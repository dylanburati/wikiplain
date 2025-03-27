use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fs::OpenOptions;
use std::io::{BufRead, BufReader as StdBufReader, Read, Seek, SeekFrom, Write};
use std::ops::Range;
use std::sync::{Arc, Mutex as StdMutex};
use std::time::Instant;

use futures::io::{BufReader, BufWriter};
use memmap2::Mmap;
use serde::Deserialize;
use soketto::handshake::{server::Response as HandshakeResponse, Server};
use tokio::{net::TcpListener, runtime::Runtime};
use tokio_stream::{wrappers::TcpListenerStream, StreamExt};
use tokio_util::compat::TokioAsyncReadCompatExt;
use zstd::{dict::DecoderDictionary, stream::write::Decoder};

use crate::{
    creator::{ENTITY_NUM_MASK, TREE_FANOUT},
    ErrorKind, Result,
};

pub(crate) fn serve(path: &str, port: u16) -> Result<()> {
    let rt = Runtime::new()?;
    rt.block_on(serve_async(path, port))
}

#[repr(C)]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct WikidataPropPtr(u64, u64);

impl WikidataPropPtr {
    #[cfg(target_endian = "little")]
    pub(crate) fn num_entities(&self) -> usize {
        self.0 as usize
    }

    #[cfg(target_endian = "big")]
    fn num_entities(&self) -> usize {
        self.0.swap_bytes() as usize
    }

    #[cfg(target_endian = "little")]
    pub(crate) fn data_offset(&self) -> u64 {
        self.1
    }

    #[cfg(target_endian = "big")]
    fn data_offset(&self) -> u64 {
        self.1.swap_bytes()
    }
}

struct WikidataPropIndex<'a> {
    _raw: &'a [u8],
    ptrs: &'a [WikidataPropPtr],
    data_native: &'a [u64],
}

impl<'a> WikidataPropIndex<'a> {
    fn try_from(raw: &'a [u8]) -> Result<Self> {
        let mut buf8 = [0u8; 8];
        if raw.len() < 8 {
            return Err(ErrorKind::Msg("wikidata prop index: unexpected EOF".to_string()).into());
        }
        let (n_props_b, rest) = raw.split_at(8);
        buf8.copy_from_slice(n_props_b);
        let n_props = u64::from_le_bytes(buf8) as usize;

        let n_ptrs = n_props + 1;
        let ptr_segment_size = std::mem::size_of::<WikidataPropPtr>() * n_ptrs;
        if rest.len() < ptr_segment_size {
            return Err(ErrorKind::Msg("wikidata props index: unexpected EOF".to_string()).into());
        }
        let (ptr_segment_b, data_b) = rest.split_at(ptr_segment_size);
        let ptrs: &'a [WikidataPropPtr] = unsafe {
            std::slice::from_raw_parts(std::mem::transmute(ptr_segment_b.as_ptr()), n_ptrs)
        };

        if data_b.len() % 8 != 0 {
            return Err(ErrorKind::Msg(
                "wikidata props index: size must be multiple of 8".to_string(),
            )
            .into());
        }
        let data_len = data_b.len() / 8;
        let data_native: &'a [u64] =
            unsafe { std::slice::from_raw_parts(std::mem::transmute(data_b.as_ptr()), data_len) };
        for ptr in ptrs {
            let d_offset = (ptr.data_offset() as usize)
                .checked_sub(8 + ptr_segment_size)
                .ok_or_else(|| {
                    ErrorKind::Msg("wikidata props index: ptr.data_offset is invalid".to_string())
                })?;
            if d_offset % 8 != 0 {
                return Err(ErrorKind::Msg(
                    "wikidata props index: ptr.data_offset is invalid".to_string(),
                )
                .into());
            }
            if (d_offset / 8) + ptr.num_entities() > data_len {
                return Err(
                    ErrorKind::Msg("wikidata props index: ptr is invalid".to_string()).into(),
                );
            }
        }

        Ok(WikidataPropIndex {
            _raw: raw,
            ptrs,
            data_native,
        })
    }

    fn get_entity_ids_native(&self, prop_id: u64) -> &'a [u64] {
        let ptr_segment_size = std::mem::size_of::<WikidataPropPtr>() * self.ptrs.len();

        let p_index = (prop_id & ENTITY_NUM_MASK) as usize;
        let Some(ptr) = self.ptrs.get(p_index) else {
            return &[];
        };

        let d_index = (ptr.data_offset() as usize - 8 - ptr_segment_size) / 8;

        &self.data_native[d_index..d_index + ptr.num_entities()]
    }

    #[cfg(target_endian = "little")]
    fn get_entity_ids(&self, prop_id: u64) -> impl Iterator<Item = u64> + 'a {
        return self.get_entity_ids_native(prop_id).iter().copied();
    }

    #[cfg(target_endian = "big")]
    fn get_entity_ids(&self, prop_id: u64) -> impl Iterator<Item = u64> + 'a {
        return self
            .get_entity_ids_native(prop_id)
            .iter()
            .map(|x| x.swap_bytes());
    }
}

struct WikidataNode<'a> {
    keys_native: &'a [u64],
    ptrs_native: &'a [u64],
}

impl<'a> WikidataNode<'a> {
    #[cfg(target_endian = "little")]
    fn keys(&self) -> impl Iterator<Item = u64> + 'a {
        self.keys_native.iter().copied()
    }

    #[cfg(target_endian = "big")]
    fn keys(&self) -> impl Iterator<Item = u64> + 'a {
        self.keys_native.iter().map(|x| x.swap_bytes())
    }

    #[cfg(target_endian = "little")]
    fn pointer(&self, index: usize) -> u64 {
        self.ptrs_native[index]
    }

    #[cfg(target_endian = "big")]
    fn pointer(&self, index: usize) -> u64 {
        self.ptrs_native[index].swap_bytes()
    }
}

#[repr(C)]
#[repr(packed(4))]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WikidataLeaf(u64, u64, u32);

impl WikidataLeaf {
    #[cfg(target_endian = "little")]
    pub(crate) fn new(key: u64, offset: u64, length: u32) -> Self {
        Self(key, offset, length)
    }

    #[cfg(target_endian = "big")]
    pub(crate) fn new(key: u64, offset: u64, length: u32) -> Self {
        Self(key.swap_bytes(), offset.swap_bytes(), length.swap_bytes())
    }

    #[rustfmt::skip]
    pub(crate) fn to_le_bytes(&self) -> [u8; 20] {
        // to_ne_bytes is used because the in-memory fields
        // are little-endian
        let key_b = self.0.to_ne_bytes();
        let off_b = self.1.to_ne_bytes();
        let len_b = self.2.to_ne_bytes();

        [
            key_b[0], key_b[1], key_b[2], key_b[3], key_b[4], key_b[5], key_b[6], key_b[7],
            off_b[0], off_b[1], off_b[2], off_b[3], off_b[4], off_b[5], off_b[6], off_b[7],
            len_b[0], len_b[1], len_b[2], len_b[3],
        ]
    }

    #[cfg(target_endian = "little")]
    pub(crate) fn key(&self) -> u64 {
        self.0
    }

    #[cfg(target_endian = "big")]
    fn key(&self) -> u64 {
        self.0.swap_bytes()
    }

    #[cfg(target_endian = "little")]
    pub(crate) fn data_offset(&self) -> u64 {
        self.1
    }

    #[cfg(target_endian = "big")]
    fn data_offset(&self) -> u64 {
        self.1.swap_bytes()
    }

    #[cfg(target_endian = "little")]
    pub(crate) fn data_length(&self) -> u32 {
        self.2
    }

    #[cfg(target_endian = "big")]
    fn data_length(&self) -> u32 {
        self.2.swap_bytes()
    }
}

struct WikidataIndex<'a> {
    _raw: &'a [u8],
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
        let n_groups64 = u64::from_le_bytes(buf8);
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
        let n_levels = u64::from_le_bytes(buf8);

        let mut leaf_level_b = rest;
        let mut base = 8 * (1 + n_groups + 1);
        let mut curr_index = 0;
        // let base = (rest.as_ptr() as usize) - (raw.as_ptr() as usize);
        for _ in 0..n_levels - 1 {
            if leaf_level_b.len() < 8 {
                return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
            }
            buf8.copy_from_slice(&leaf_level_b[..8]);
            let node_size = u64::from_le_bytes(buf8) as usize;
            if leaf_level_b.len() < 16 * node_size {
                return Err(ErrorKind::Msg("wikidata index: unexpected EOF".to_string()).into());
            }
            buf8.copy_from_slice(&leaf_level_b[8 * node_size..8 * node_size + 8]);
            let first_ptr = u64::from_le_bytes(buf8) as usize;
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
            _raw: raw,
            n_groups: n_groups64,
            group_offsets_native,
            n_levels,
            node_levels_native,
            leaf_level_native,
        })
    }

    #[cfg(target_endian = "little")]
    fn group_offsets(&self) -> impl Iterator<Item = u64> + 'a {
        self.group_offsets_native.iter().copied()
    }

    #[cfg(target_endian = "big")]
    fn group_offsets(&self) -> impl Iterator<Item = u64> {
        self.group_offsets_native.iter().map(|x| x.swap_bytes())
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

    /// When `is_start` is true, returns the leaf index of the first key >= the argument
    /// When `is_start` is false, returns the leaf index of the last key <= the argument
    fn get_bound(&self, key: u64, is_start: bool) -> Option<usize> {
        let mut level = 0;
        let mut pos = 0;
        while level < self.n_levels - 1 {
            let width = self.node_levels_native[pos] as usize;

            #[cfg(target_endian = "big")]
            let width = width.swap_bytes();

            let node = WikidataNode {
                keys_native: &self.node_levels_native[pos + 1..pos + width],
                ptrs_native: &self.node_levels_native[pos + width..pos + 2 * width],
            };
            let child_idx = node.keys().position(|k| key < k).unwrap_or(width - 1);
            let p = node.pointer(child_idx);
            pos = if level < self.n_levels - 2 {
                // calculate index into node_levels_native
                ((p - self.header_bytes()) / 8) as usize
            } else {
                ((p - self.header_bytes() - self.node_levels_bytes()) / 20) as usize
            };
            level += 1;
        }

        let end = (pos + TREE_FANOUT as usize).min(self.leaf_level_native.len());
        let leaves = &self.leaf_level_native[pos..end];
        let found_idx = if is_start {
            leaves.iter().position(|leaf| leaf.key() >= key)
        } else {
            leaves.iter().rposition(|leaf| leaf.key() <= key)
        };
        found_idx.map(|i| i + pos)
    }

    fn slice_leaves(&self, start: usize, end: usize) -> impl Iterator<Item = (u64, u32)> + 'a {
        self.leaf_level_native[start..end]
            .iter()
            .map(|leaf| (leaf.data_offset(), leaf.data_length()))
    }

    fn get_many(&self, query_keys: &[u64]) -> Vec<(u64, u32)> {
        let mut result = Vec::with_capacity(query_keys.len());
        let mut search_queue = VecDeque::new();
        search_queue.push_back((0, 0, &query_keys[..]));
        while let Some((level, pos, mut candidates)) = search_queue.pop_front() {
            if level < self.n_levels - 1 {
                let width = self.node_levels_native[pos] as usize;

                #[cfg(target_endian = "big")]
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
                        // Pop `matches` from the front of candidates; `matches` are the candidates
                        // which are less than the i-th node key. Since they weren't popped in a previous iteration,
                        // all must be in the half-open range from keys[i-1] to keys[i], which is
                        // the domain of ptr[i].
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
                    // Anything still in candidates is in the half-open range from keys[width-2] to infinity,
                    // which is the domain ot ptrs[width-1].
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

fn parse_entity_id(i: &str) -> Option<u64> {
    if i.len() <= 1 {
        return None;
    }
    let (kind_str, id_str) = i.split_at(1);
    let id_num: u64 = id_str.parse().ok()?;
    let kind_num = (kind_str.as_bytes()[0] as u64) << 56;
    Some(kind_num | id_num)
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
        let length = u32::from_le_bytes(len_buf) as usize;
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

fn read_en_index_file(path: &str) -> Result<HashMap<String, u64>> {
    let en_index_file = OpenOptions::new().read(true).open(path)?;
    let en_index_file = StdBufReader::with_capacity(65536, en_index_file);
    let mut result = HashMap::new();
    for line_res in en_index_file.lines() {
        let mut line = line_res?;
        let cut = line
            .find('\t')
            .ok_or_else(|| ErrorKind::EntityError("invalid line in en.index.txt"))?;
        let value_s = line.split_off(cut + 1);
        line.pop().unwrap(); // remove the '\t'
        let value = parse_entity_id(&value_s)
            .ok_or_else(|| ErrorKind::EntityError("invalid entity ID in en.index.txt"))?;
        let _ = result.insert(line, value);
    }
    Ok(result)
}

#[derive(Deserialize)]
enum QueryType {
    Id,
    IdRange,
    Title,
    HasProp,
}

#[derive(Debug, Clone)]
enum QueryArgsInternal {
    Vec(Vec<u64>),
    Range(Range<u64>),
}

impl From<Vec<u64>> for QueryArgsInternal {
    fn from(value: Vec<u64>) -> Self {
        Self::Vec(value)
    }
}

impl From<Range<u64>> for QueryArgsInternal {
    fn from(value: Range<u64>) -> Self {
        Self::Range(value)
    }
}

#[derive(Deserialize)]
struct Query {
    #[serde(rename = "type")]
    type_: QueryType,
    args: Vec<String>,
}

async fn serve_async(path: &str, port: u16) -> Result<()> {
    let base = path.strip_suffix(".zst").unwrap_or(path);
    let dicts_path = base.to_string() + ".zdicts";
    let index_path = base.to_string() + ".index.bin";
    let prop_index_path = base.to_string() + "-prop.index.bin";
    let en_index_path = base.to_string() + "-en.index.txt";

    let index_file = OpenOptions::new().read(true).open(index_path)?;
    let prop_index_file = OpenOptions::new().read(true).open(prop_index_path)?;
    let en_index = read_en_index_file(&en_index_path)?;

    let index_mapped = unsafe { Mmap::map(&index_file)? };
    let wd_index = WikidataIndex::try_from(&index_mapped[..])?;
    let prop_index_mapped = unsafe { Mmap::map(&prop_index_file)? };
    let wd_prop_index = WikidataPropIndex::try_from(&prop_index_mapped[..])?;
    let dicts = read_dicts_file(&wd_index, &dicts_path)?;
    let data_file_shared = StdMutex::new(OpenOptions::new().read(true).open(path)?);
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
        let (mut sender, mut receiver) = websocket.into_builder().finish();
        let mut message = Vec::new();

        loop {
            message.clear();
            match receiver.receive_data(&mut message).await {
                Ok(soketto::Data::Binary(_)) => {
                    sender
                        .send_text("{\"error\": \"expected text message\"}")
                        .await?;
                    sender.flush().await?
                }
                Ok(soketto::Data::Text(_)) => {
                    let t0 = Instant::now();
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
                    if matches!(query.type_, QueryType::IdRange) {
                        if query.args.len() != 2 {}
                    }
                    let query_args: Option<QueryArgsInternal> = match query.type_ {
                        QueryType::Id => query
                            .args
                            .into_iter()
                            .map(|id| parse_entity_id(&id))
                            .collect::<Option<Vec<_>>>()
                            .map(|e| e.into()),
                        QueryType::IdRange => match query.args.as_slice() {
                            [start_s, end_s] => {
                                let start_opt = parse_entity_id(start_s);
                                let end_opt = parse_entity_id(end_s);
                                start_opt.and_then(|start| {
                                    end_opt
                                        .filter(|end| *end >= start)
                                        .map(|end| Range { start, end }.into())
                                })
                            }
                            _ => {
                                sender
                                    .send_text("{\"error\": \"could not parse query\"}")
                                    .await?;
                                sender.flush().await?;
                                continue;
                            }
                        },
                        QueryType::Title => Some(
                            query
                                .args
                                .into_iter()
                                .filter_map(|title| en_index.get(&title))
                                .copied()
                                .collect::<Vec<_>>()
                                .into(),
                        ),
                        QueryType::HasProp => {
                            if let Some(prop_ids) = query
                                .args
                                .into_iter()
                                .map(|id| parse_entity_id(&id))
                                .collect::<Option<Vec<_>>>()
                            {
                                Some(
                                    prop_ids
                                        .into_iter()
                                        .flat_map(|id| wd_prop_index.get_entity_ids(id))
                                        .collect::<Vec<_>>()
                                        .into(),
                                )
                            } else {
                                None
                            }
                        }
                    };
                    let query_args = match query_args {
                        Some(a) => a,
                        None => {
                            eprintln!("malformed IDs in args");
                            sender
                                .send_text("{\"error\": \"malformed IDs in args\"}")
                                .await?;
                            sender.flush().await?;
                            continue;
                        }
                    };
                    let (query_size, mut data_ranges) = match query_args {
                        QueryArgsInternal::Vec(mut lst) => {
                            lst.sort();
                            let drs = wd_index.get_many(&lst);
                            (lst.len(), drs)
                        }
                        QueryArgsInternal::Range(Range { start, end }) => {
                            if let Some(start_leaf) = wd_index.get_bound(start, true) {
                                if let Some(end_leaf) = wd_index.get_bound(end, false) {
                                    (0, wd_index.slice_leaves(start_leaf, end_leaf).collect())
                                } else {
                                    (0, vec![])
                                }
                            } else {
                                (0, vec![])
                            }
                        }
                    };
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
                    let line_results = workers.iter().flat_map(|(dict, rng_list)| {
                        rng_list
                            .into_iter()
                            .map(|(offset, length)| -> Result<Vec<u8>> {
                                let mut data_file = data_file_shared.lock().unwrap();
                                let _ = data_file.seek(SeekFrom::Start(*offset))?;
                                let mut buf = vec![0u8; *length as usize];
                                data_file.read_exact(&mut buf)?;
                                let out = Vec::new();
                                let mut decoder = Decoder::with_prepared_dictionary(out, dict)?;
                                decoder.write_all(&buf)?;
                                decoder.flush()?;
                                Ok(decoder.into_inner())
                            })
                    });
                    let mut response = Vec::new();
                    let mut n_lines = 0;
                    for line_res in line_results {
                        let line = line_res?;
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
                            let response_str = unsafe { std::str::from_utf8_unchecked(&response) };
                            sender.send_text(response_str).await?;
                            sender.flush().await?;
                            response.clear();
                        }
                    }
                    eprintln!();
                    let response_str = unsafe { std::str::from_utf8_unchecked(&response) };
                    sender.send_text(response_str).await?;
                    sender.flush().await?;
                }
                Err(soketto::connection::Error::Closed) => break,
                Err(e) => return Err(e.into()),
            }
        }
    }
    Ok(())
}
