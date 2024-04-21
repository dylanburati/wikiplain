use std::collections::{BTreeMap, VecDeque};
use std::fs::OpenOptions;
use std::io::{Read, Seek, SeekFrom, Write};
use std::sync::{mpsc::sync_channel, Arc, Condvar, Mutex as StdMutex};
use std::time::Instant;

use futures::io::{BufReader, BufWriter};
use memmap2::Mmap;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Deserialize;
use soketto::handshake::{server::Response as HandshakeResponse, Server};
use tokio::{net::TcpListener, runtime::Runtime, sync::Mutex, task::JoinHandle};
use tokio_stream::{wrappers::TcpListenerStream, StreamExt};
use tokio_util::compat::TokioAsyncReadCompatExt;
use zstd::{dict::DecoderDictionary, stream::write::Decoder};

use crate::{creator::TREE_FANOUT, ErrorKind, Result};

pub(crate) fn serve(path: &str, port: u16) -> Result<()> {
    let rt = Runtime::new()?;
    rt.block_on(serve_async(path, port))
}

struct WikidataNode<'a> {
    keys_native: &'a [u64],
    ptrs_native: &'a [u64],
}

impl<'a> WikidataNode<'a> {
    #[cfg(target_endian = "little")]
    fn keys(&self) -> impl Iterator<Item = u64> + 'a {
        self.keys_native.iter().map(|x| x.swap_bytes())
    }

    #[cfg(target_endian = "big")]
    fn keys(&self) -> impl Iterator<Item = u64> {
        self.keys_native.iter().copied()
    }

    #[cfg(target_endian = "little")]
    fn pointer(&self, index: usize) -> u64 {
        self.ptrs_native[index].swap_bytes()
    }

    #[cfg(target_endian = "big")]
    fn pointer(&self, index: usize) -> u64 {
        self.ptrs_native[index];
    }
}

#[repr(C)]
#[repr(packed(4))]
struct WikidataLeaf(u64, u64, u32);

impl WikidataLeaf {
    #[cfg(target_endian = "little")]
    fn key(&self) -> u64 {
        self.0.swap_bytes()
    }

    #[cfg(target_endian = "big")]
    fn key(&self) -> u64 {
        self.0
    }

    #[cfg(target_endian = "little")]
    fn data_offset(&self) -> u64 {
        self.1.swap_bytes()
    }

    #[cfg(target_endian = "big")]
    fn data_offset(&self) -> u64 {
        self.1
    }

    #[cfg(target_endian = "little")]
    fn data_length(&self) -> u32 {
        self.2.swap_bytes()
    }

    #[cfg(target_endian = "big")]
    fn data_length(&self) -> u32 {
        self.2
    }
}

struct WikidataIndex<'a> {
    raw: &'a [u8],
    n_groups: u64,
    group_offsets_native: &'a [u64],
    n_levels: u64,
    node_levels_native: &'a [u64],
    leaf_level_native: &'a [WikidataLeaf],
}

impl<'a> WikidataIndex<'a> {
    fn try_from(raw: &'a [u8]) -> Result<Self> {
        let mut buf8 = [0u8; 8];
        if raw.len() < 8 {
            return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
        }
        let (n_groups_b, rest) = raw.split_at(8);
        buf8.copy_from_slice(n_groups_b);
        let n_groups64 = u64::from_be_bytes(buf8);
        let n_groups = n_groups64 as usize;

        if rest.len() < 8 * n_groups {
            return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
        }
        let (group_offsets_b, rest) = rest.split_at(8 * n_groups);
        let group_offsets_native: &'a [u64] = unsafe {
            std::slice::from_raw_parts(std::mem::transmute(group_offsets_b.as_ptr()), n_groups)
        };

        if rest.len() < 8 {
            return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
        }
        let (n_levels_b, rest) = rest.split_at(8);
        buf8.copy_from_slice(n_levels_b);
        let n_levels = u64::from_be_bytes(buf8);

        let mut leaf_level_b = rest;
        let mut base = 8 * (1 + n_groups + 1);
        let mut curr_index = 0;
        // let base = (rest.as_ptr() as usize) - (raw.as_ptr() as usize);
        for _ in 0..n_levels - 1 {
            if leaf_level_b.len() < 8 {
                return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
            }
            buf8.copy_from_slice(&leaf_level_b[..8]);
            let node_size = u64::from_be_bytes(buf8) as usize;
            if leaf_level_b.len() < 16 * node_size {
                return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
            }
            buf8.copy_from_slice(&leaf_level_b[8 * node_size..8 * node_size + 8]);
            let first_ptr = u64::from_be_bytes(buf8) as usize;
            let first_ptr_dist = first_ptr.checked_sub(base).ok_or_else(|| {
                ErrorKind::Msg(
                    "wikidata index: node pointer must not point to a previous level".to_string(),
                )
            })?;
            if first_ptr_dist % 8 != 0 {
                return Err(ErrorKind::Msg(
                    "wikidata index: node pointer must be multiple of 8".to_string(),
                )
                .into());
            }
            if leaf_level_b.len() < first_ptr_dist {
                return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
            }
            base = first_ptr;
            leaf_level_b = &leaf_level_b[first_ptr_dist..];
            curr_index += first_ptr_dist;
        }

        let node_levels_b = &rest[..curr_index];
        let node_levels_native: &'a [u64] = unsafe {
            std::slice::from_raw_parts(
                std::mem::transmute(node_levels_b.as_ptr()),
                node_levels_b.len() / 8,
            )
        };
        let leaf_level_native: &'a [WikidataLeaf] = unsafe {
            std::slice::from_raw_parts(
                std::mem::transmute(leaf_level_b.as_ptr()),
                leaf_level_b.len() / 20,
            )
        };
        eprintln!("header_bytes = {}", 8 * (1 + n_groups + 1));
        eprintln!("node_levels_native.len() = {}", node_levels_native.len());
        eprintln!("leaf_level_native.len() = {}", leaf_level_native.len());

        Ok(WikidataIndex {
            raw,
            n_groups: n_groups64,
            group_offsets_native,
            n_levels,
            node_levels_native,
            leaf_level_native,
        })
    }

    #[cfg(target_endian = "little")]
    fn group_offsets(&self) -> impl Iterator<Item = u64> + 'a {
        self.group_offsets_native.iter().map(|x| x.swap_bytes())
    }

    #[cfg(target_endian = "big")]
    fn group_offsets(&self) -> impl Iterator<Item = u64> {
        self.group_offsets_native.iter().copied()
    }

    const fn header_bytes(&self) -> u64 {
        // n_groups (8)
        // group_offsets (8*n_groups)
        // n_levels (8)
        8 * (1 + self.n_groups + 1)
    }

    fn node_levels_bytes(&self) -> u64 {
        8 * self.node_levels_native.len() as u64
    }

    fn get_many(&self, mut query_keys: Vec<u64>) -> Vec<(u64, u32)> {
        let mut result = Vec::with_capacity(query_keys.len());
        let mut search_queue = VecDeque::new();
        query_keys.sort();
        search_queue.push_back((0, 0, &query_keys[..]));
        while let Some((level, pos, mut candidates)) = search_queue.pop_front() {
            if level < self.n_levels - 1 {
                let width = self.node_levels_native[pos] as usize;

                #[cfg(target_endian = "little")]
                let width = width.swap_bytes();

                let node = WikidataNode {
                    keys_native: &self.node_levels_native[pos + 1..pos + width],
                    ptrs_native: &self.node_levels_native[pos + width..pos + 2 * width],
                };
                for (i, k) in node.keys().enumerate() {
                    let cut = candidates
                        .iter()
                        .position(|cand| cand >= &k)
                        .unwrap_or_else(|| candidates.len());
                    if cut > 0 {
                        let matched = &candidates[..cut];
                        candidates = &candidates[cut..];
                        let p = node.pointer(i);
                        let pos = if level < self.n_levels - 2 {
                            // calculate index into node_levels_native
                            ((p - self.header_bytes()) / 8) as usize
                        } else {
                            ((p - self.header_bytes() - self.node_levels_bytes()) / 20) as usize
                        };
                        search_queue.push_back((level + 1, pos, matched));
                    }
                }
                if candidates.len() > 0 {
                    let p = node.pointer(width - 1);
                    let pos = if level < self.n_levels - 2 {
                        // calculate index into node_levels_native
                        ((p - self.header_bytes()) / 8) as usize
                    } else {
                        ((p - self.header_bytes() - self.node_levels_bytes()) / 20) as usize
                    };
                    search_queue.push_back((level + 1, pos, candidates));
                }
            } else {
                let end = (pos + TREE_FANOUT as usize).min(self.leaf_level_native.len());
                let (mut head, mut candidates) = candidates.split_first().unwrap();

                'outer: for leaf in &self.leaf_level_native[pos..end] {
                    let k = leaf.key();
                    while head < &k {
                        match candidates.split_first() {
                            Some((a, b)) => {
                                (head, candidates) = (a, b);
                            }
                            None => {
                                break 'outer;
                            }
                        }
                    }
                    if head == &k {
                        result.push((leaf.data_offset(), leaf.data_length()));
                        match candidates.split_first() {
                            Some((a, b)) => {
                                (head, candidates) = (a, b);
                            }
                            None => {
                                break;
                            }
                        }
                    }
                }
            }
        }

        result
    }
}

fn read_dicts_file(
    index: &WikidataIndex<'_>,
    path: &str,
) -> Result<BTreeMap<u64, Arc<DecoderDictionary<'static>>>> {
    let mut result = BTreeMap::new();
    let dicts_file = OpenOptions::new().read(true).open(path)?;
    let dicts_mapped = unsafe { Mmap::map(&dicts_file)? };
    let mut len_buf = [0u8; 4];
    let mut rest = &dicts_mapped[..];
    let mut group_offsets = index.group_offsets();
    while rest.len() >= 4 {
        len_buf.copy_from_slice(&rest[..4]);
        let length = u32::from_be_bytes(len_buf) as usize;
        if rest.len() < 4 + length {
            return Err(ErrorKind::Msg("wikidata dicts: unexpected EOF".to_string()).into());
        }
        let (ddict_bytes, post) = &rest[4..].split_at(length);
        let ddict = DecoderDictionary::copy(ddict_bytes);
        let offset = group_offsets.next().ok_or_else(|| {
            ErrorKind::Msg(format!(
                "wikidata dicts: more entries than groups ({})",
                result.len() + 1
            ))
        })?;
        let _ = result.insert(offset, Arc::new(ddict));
        rest = post;
    }
    if rest.len() != 0 {
        return Err(ErrorKind::Msg("wikidata dicts: unexpected EOF".to_string()).into());
    }
    Ok(result)
}

#[derive(Deserialize)]
enum QueryType {
    Id,
}

#[derive(Deserialize)]
struct Query {
    #[serde(rename = "type")]
    type_: QueryType,
    args: Vec<String>,
}

struct Pool<T> {
    data: StdMutex<Vec<T>>,
    condition: Condvar,
}

struct PoolGuard<'a, T> {
    pool: &'a Pool<T>,
    inner: Option<T>,
}

impl<'a, T> std::ops::Deref for PoolGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap()
    }
}

impl<'a, T> std::ops::DerefMut for PoolGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.as_mut().unwrap()
    }
}

impl<T> Drop for PoolGuard<'_, T> {
    fn drop(&mut self) {
        let mut vec = self.pool.data.lock().unwrap();
        vec.push(self.inner.take().unwrap());
        self.pool.condition.notify_one();
    }
}

impl<T> Pool<T> {
    fn get<'a>(&'a self) -> PoolGuard<'a, T> {
        let mut vec = self.data.lock().unwrap();
        while vec.is_empty() {
            vec = self.condition.wait(vec).unwrap();
        }
        let item = vec.pop().unwrap();
        PoolGuard {
            pool: self,
            inner: Some(item),
        }
    }

    fn new(data: Vec<T>) -> Self {
        Self {
            data: StdMutex::new(data),
            condition: Condvar::new(),
        }
    }
}

async fn serve_async(path: &str, port: u16) -> Result<()> {
    let base = path.strip_suffix(".zst").unwrap_or(path);
    let dicts_path = base.to_string() + ".zdicts";
    let index_path = base.to_string() + ".index.bin";
    //let en_index_path = base.to_string() + "-en.index.txt";

    let index_file = OpenOptions::new().read(true).open(index_path)?;
    //let en_index_file = OpenOptions::new()
    //.read(true)
    //.open(en_index_path)?;

    let index_mapped = unsafe { Mmap::map(&index_file)? };
    let wd_index = WikidataIndex::try_from(&index_mapped[..])?;
    let dicts = read_dicts_file(&wd_index, &dicts_path)?;
    let mut data_file_vec = vec![];
    for _ in 0..512 {
        let f = OpenOptions::new().read(true).open(path)?;
        data_file_vec.push(f);
    }
    let data_files = Arc::new(Pool::new(data_file_vec));
    // let summary: Vec<_> = dicts.iter().map(|(k, v)| (k, v.as_ddict().get_dict_id())).collect();
    // println!("{:?}", summary);

    let listener = TcpListener::bind(("0.0.0.0", port)).await?;
    eprintln!("Ready on http://0.0.0.0:{}", port);
    let mut server = TcpListenerStream::new(listener);
    while let Some(socket) = server.next().await {
        let socket = BufReader::with_capacity(
            8 * 1024,
            BufWriter::with_capacity(16 * 1024, socket?.compat()),
        );
        let mut websocket = Server::new(socket);
        let deflate = soketto::extension::deflate::Deflate::new(soketto::Mode::Server);
        websocket.add_extension(Box::new(deflate));

        let key = {
            let req = websocket.receive_request().await?;
            req.key()
        };
        let accept = HandshakeResponse::Accept {
            key,
            protocol: None,
        };
        websocket.send_response(&accept).await?;
        let (sender, mut receiver) = websocket.into_builder().finish();
        let sender_shared = Arc::new(Mutex::new(sender));
        let mut message = Vec::new();

        loop {
            message.clear();
            match receiver.receive_data(&mut message).await {
                Ok(soketto::Data::Binary(_)) => {
                    let mut sender = sender_shared.lock().await;
                    sender
                        .send_text("{\"error\": \"expected text message\"}")
                        .await?;
                    sender.flush().await?
                }
                Ok(soketto::Data::Text(_)) => {
                    let t0 = Instant::now();
                    let mut sender = sender_shared.lock().await;
                    let query: Query = match serde_json::from_slice(&message) {
                        Ok(q) => q,
                        Err(_) => {
                            eprintln!("could not parse query");
                            sender
                                .send_text("{\"error\": \"could not parse query\"}")
                                .await?;
                            sender.flush().await?;
                            continue;
                        }
                    };
                    let query_keys: Option<Vec<u64>> = match query.type_ {
                        QueryType::Id => query
                            .args
                            .into_iter()
                            .map(|s| {
                                if s.len() > 1 {
                                    let (kind_str, id_str) = s.split_at(1);
                                    let id_num: u64 = id_str.parse().ok()?;
                                    let kind_num = (kind_str.as_bytes()[0] as u64) << 56;
                                    Some(kind_num | id_num)
                                } else {
                                    None
                                }
                            })
                            .collect(),
                    };
                    let query_keys = match query_keys {
                        Some(lst) => lst,
                        None => {
                            eprintln!("malformed IDs in args");
                            sender
                                .send_text("{\"error\": \"malformed IDs in args\"}")
                                .await?;
                            sender.flush().await?;
                            continue;
                        }
                    };
                    let query_size = query_keys.len();
                    let mut data_ranges = wd_index.get_many(query_keys);
                    data_ranges.sort();
                    if data_ranges.is_empty() {
                        sender.send_text("").await?;
                        sender.flush().await?;
                        continue;
                    }
                    let (dict0_valid_start, mut dict0) =
                        dicts.range(..data_ranges[0].0 + 1).last().unwrap();
                    let mut workers = vec![];
                    for (dict_valid_start, dict) in
                        dicts.range(dict0_valid_start + 1..data_ranges[data_ranges.len() - 1].0 + 1)
                    {
                        let cut = data_ranges
                            .iter()
                            .position(|rng| &rng.0 >= dict_valid_start)
                            .unwrap_or_else(|| data_ranges.len());
                        if cut > 0 {
                            let mut worker_ranges = data_ranges.split_off(cut);
                            std::mem::swap(&mut data_ranges, &mut worker_ranges);
                            workers.push((Arc::clone(dict0), worker_ranges));
                            if data_ranges.is_empty() {
                                break;
                            }
                        }
                        dict0 = dict;
                    }
                    if !data_ranges.is_empty() {
                        workers.push((Arc::clone(dict0), data_ranges));
                    }
                    drop(sender);
                    let sender_shared1 = Arc::clone(&sender_shared);
                    let data_files = Arc::clone(&data_files);
                    let (data_consumer, data_supplier) = sync_channel(64);
                    let tx_task = tokio::spawn(async move {
                        workers
                            .par_iter()
                            .flat_map_iter(|(dict, rng_list)| {
                                rng_list
                                    .into_iter()
                                    .map(|(offset, length)| -> Result<Vec<u8>> {
                                        let mut data_file = data_files.get();
                                        let _ = data_file.seek(SeekFrom::Start(*offset))?;
                                        let mut buf = vec![0u8; *length as usize];
                                        data_file.read_exact(&mut buf)?;
                                        let out = Vec::new();
                                        let mut decoder =
                                            Decoder::with_prepared_dictionary(out, dict)?;
                                        decoder.write_all(&buf)?;
                                        decoder.flush()?;
                                        Ok(decoder.into_inner())
                                    })
                            })
                            .try_for_each_with(data_consumer, |ch, line_res| -> Result<()> {
                                let line = line_res?;
                                ch.send(line).unwrap();
                                Ok(())
                            })
                    });
                    let rx_task: JoinHandle<Result<()>> = tokio::spawn(async move {
                        let mut response = Vec::new();
                        let mut sender = sender_shared1.lock().await;
                        let mut n_lines = 0;
                        for line in data_supplier.into_iter() {
                            n_lines += 1;
                            eprint!(
                                "\r\x1b[Kprogress: {:>4} / {} [{}ms]",
                                n_lines,
                                query_size,
                                t0.elapsed().as_millis(),
                            );
                            response.extend(line);
                            response.push(b'\n');
                            if response.len() > 32 * 1024 * 1024 {
                                response.extend("{\"has_next\": 1}".as_bytes());
                                // safety: `response` was parsed as json in the create step, which
                                // would have errored on non-utf8 chars. Also, the send_text call only uses
                                // str::as_bytes.
                                let response_str =
                                    unsafe { std::str::from_utf8_unchecked(&response) };
                                sender.send_text(response_str).await?;
                                sender.flush().await?;
                                response.clear();
                            }
                        }
                        eprintln!();
                        let response_str = unsafe { std::str::from_utf8_unchecked(&response) };
                        sender.send_text(response_str).await?;
                        sender.flush().await?;
                        Ok(())
                    });

                    match tokio::join!(tx_task, rx_task) {
                        (Err(txe), _) => return Err(txe.into()),
                        (Ok(_), Err(rxe)) => return Err(rxe.into()),
                        (Ok(Err(txe)), Ok(_)) => return Err(txe),
                        (Ok(Ok(_)), Ok(Err(rxe))) => return Err(rxe),
                        (Ok(Ok(_)), Ok(Ok(_))) => {}
                    }
                }
                Err(soketto::connection::Error::Closed) => break,
                Err(e) => return Err(e.into()),
            }
        }
    }
    Ok(())
}
