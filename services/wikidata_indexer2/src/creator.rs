use std::borrow::Borrow;
use std::collections::BinaryHeap;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};

use flate2::read::MultiGzDecoder;
use nom::error::VerboseError;
use zstd::Encoder as ZstdEncoder;

use crate::{Result, ErrorKind};
use crate::creator::item_parser::JsonValue;

use self::item_parser::{Selector, JsonPathItem};

mod item_parser;

pub const TREE_FANOUT: u32 = 256;
const TREE_FANOUT_LONG: u64 = TREE_FANOUT as u64;
pub const ENTITIES_PER_GROUP: u32 = 1000;

struct LevelDesc {
    /// Number of nodes; The first `width-2` have `TREE_FANOUT` pointers
    width: u64,
    /// Number of pointers in last node
    children1: u32,
    /// Number of pointers in 2nd to last node
    children2: u32,
}

/// Configuration for one level of a B+tree.
///
/// Example:
/// LevelDesc(1, 3, 0) has 3 pointers
/// LevelDesc(3, 193, 192) has 641 pointers
/// LevelDesc(641, 240, 239) has 164,063 pointers
/// LevelDesc(164063, 192, 192) has 42,000,000 pointers
///
/// The "leaf" LevelDesc has data in place of each pointer
impl LevelDesc {
    fn new(width: u64, children1: u32, children2: u32) -> LevelDesc {
        LevelDesc {
            width,
            children1,
            children2,
        }
    }

    fn total_children(&self) -> u64 {
        self.width.saturating_sub(2) * TREE_FANOUT_LONG
            + self.children1 as u64
            + self.children2 as u64
    }

    fn node_sizes(&self) -> impl Iterator<Item = u32> {
        let mut remaining = self.width;
        let children1 = self.children1;
        let children2 = self.children2;
        std::iter::from_fn(move || {
            if remaining == 0 {
                return None;
            }
            let res = if remaining == 1 {
                Some(children1)
            } else if remaining == 2 {
                Some(children2)
            } else {
                Some(TREE_FANOUT)
            };
            remaining -= 1;
            res
        })
    }

    fn from_total_children(total: u64) -> LevelDesc {
        let full_nodes = total / TREE_FANOUT_LONG;
        if full_nodes == 0 {
            return Self::new(1, u32::try_from(total).unwrap(), 0);
        }
        let remainder = u32::try_from(total % TREE_FANOUT_LONG).unwrap();
        if remainder == 0 {
            let children2 = if full_nodes == 1 { 0 } else { TREE_FANOUT };
            return Self::new(full_nodes, TREE_FANOUT, children2);
        }
        let children1 = (TREE_FANOUT + remainder) / 2;
        let children2 = TREE_FANOUT + remainder - children1;
        Self::new(full_nodes + 1, children1, children2)
    }

    fn byte_size(&self) -> u64 {
        // The nodes in this level look like:
        // [ct, ...keys (length: ct - 1), ...pointers (length: ct - 1)][ct, ...keys etc.]
        //  0,  8,                        8*ct,                        16*ct
        let mut result = 0;
        if self.width > 2 {
            result += (self.width - 2) * 16 * TREE_FANOUT_LONG;
        }
        if self.width > 1 {
            result += 16 * (self.children2 as u64);
        }
        if self.width > 0 {
            result += 16 * (self.children1 as u64);
        }
        result
    }

    // fn node_start_pos(&self, node_index: u32) -> Option<u64> {
    //     match self.width.checked_sub(node_index as u64) {
    //         None | Some(0) => None,
    //         Some(1) => {
    //             // last node pos == size of (`width-2` nodes + 2nd to last node)
    //             let full_nodes_width = self.width.saturating_sub(2) * 16 * TREE_FANOUT_LONG;
    //             let nonfull_node_width = (self.children2 as u64) * 16 * TREE_FANOUT_LONG;
    //             Some(full_nodes_width + nonfull_node_width)
    //         }
    //         _ => {
    //             // nodes before 2nd to last are all full
    //             let full_nodes_width = (node_index as u64) * 16 * TREE_FANOUT_LONG;
    //             Some(full_nodes_width)
    //         }
    //     }
    // }

    fn is_child_first_in_node(&self, child_index: u64) -> bool {
        let full_nodes_width = self.width.saturating_sub(2) * TREE_FANOUT_LONG;
        if child_index <= full_nodes_width {
            return child_index % TREE_FANOUT_LONG == 0;
        }
        child_index == full_nodes_width + self.children2 as u64
    }
}

struct DiskIter<R: Read> {
    inner: BufReader<R>,
}

impl<R: Read> DiskIter<R> {
    fn new(inner: R) -> DiskIter<R> {
        Self {
            inner: BufReader::new(inner),
        }
    }
}

impl<R: Read> Iterator for DiskIter<R> {
    type Item = std::io::Result<(u64, u64)>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = [0u8; 8];
        let a = match self.inner.read_exact(&mut buf) {
            Ok(_) => u64::from_be_bytes(buf),
            Err(err) => {
                return match err.kind() {
                    std::io::ErrorKind::UnexpectedEof => None,
                    _ => Some(Err(err)),
                };
            }
        };

        let b = match self.inner.read_exact(&mut buf) {
            Ok(_) => u64::from_be_bytes(buf),
            Err(err) => return Some(Err(err)),
        };

        Some(Ok((a, b)))
    }
}

#[derive(PartialEq, Eq)]
struct DatumWrapper((u64, u64), usize);

impl PartialOrd for DatumWrapper {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl Ord for DatumWrapper {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.0.cmp(&self.0)
    }
}

struct Segmented {
    group_offsets: Vec<u64>,
    staged_count: u64,
    ind_entries: Vec<(u64, u64)>,
    prop_staged_count: u64,
    prop_ind_entries: Vec<(u64, u64)>,
}

// a couple thousand less than 2^24, so the vector doesn't reserve past that capacity
const STG_BLOCK_LIMIT: usize = 16 * 1000 * 1024;

pub fn create(input_path: &str, output_path: &str, working_path: &str) -> Result<()> {
    let base = output_path.strip_suffix(".zst").unwrap_or(output_path);
    let output_index_path = base.to_string() + ".index.bin";
    let output_index_en_path = base.to_string() + "-en.index.txt";
    let working_path_secondary = working_path.to_string() + "2";

    let file_input = OpenOptions::new().read(true).open(input_path)?;
    let file_output = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_path)?;
    let file_en = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_index_en_path)?;
    let file_stg = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(working_path)?;
    let file_stg_secondary = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(working_path_secondary)?;

    let segmented = write_segmented(file_input, file_output, file_en, file_stg, file_stg_secondary)?;
    let file_ind = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_index_path)?;
    let mut staged_entry_files = vec![];
    let mut stg_offset = 0;
    while stg_offset < 16 * segmented.staged_count {
        let mut reader = OpenOptions::new().read(true).open(working_path)?;
        let _ = reader.seek(SeekFrom::Start(stg_offset))?;
        staged_entry_files.push(reader.take(16 * STG_BLOCK_LIMIT as u64));
        stg_offset += 16 * STG_BLOCK_LIMIT as u64;
    }
    let _ = write_index(
        segmented.ind_entries,
        segmented.group_offsets,
        staged_entry_files,
        segmented.staged_count,
        file_ind,
    )?;
    std::fs::remove_file(working_path)?;
    Ok(())
}

/// STEP 1: read .json.gz input array, rewrite as .ldjson.gz in smaller blocks (groups)
///         in memory + working file, keep (group, entity) indices keyed by Wikidata id_num
///          (Q42 -> 0x5100_0000_0000_002a)
///         in memory, keep the start byte offset of each block
fn write_segmented(
    file_input: File,
    file_output: File,
    file_en: File,
    file_stg: File,
    file_stg_secondary: File,
) -> Result<Segmented> {
    let mut input = MultiGzDecoder::new(file_input);
    let mut output_plain = Some(file_output);
    let mut output_en = BufWriter::with_capacity(131072, file_en);
    let mut file_stg = Some(file_stg);
    let mut file_stg_secondary = Some(file_stg_secondary);

    // Skip "[\n", then buffer starting from the first object / second line
    let mut array_start_buf = [0u8; 2];
    input.read_exact(&mut array_start_buf)?;
    let mut input = BufReader::new(input);

    let mut line = Vec::new();
    let mut eof = false;
    let mut curr_group_offset = 0;
    let mut enwiki_count = 0;
    let mut ind_entries: Vec<(u64, u64)> = vec![];
    let mut prop_ind_entries: Vec<(u64, u64)> = vec![];
    let mut group_offsets = vec![];
    let mut staged_count: u64 = 0;
    let mut prop_staged_count: u64 = 0;
    let mut entity_number: u64 = 0;
    let selectors = [
        Selector::new(vec![JsonPathItem::Key("id")]),
        Selector::new(vec![JsonPathItem::Key("sitelinks"), JsonPathItem::Key("enwiki"), JsonPathItem::Key("title")]),
        Selector::new(vec![JsonPathItem::Key("claims")]),
    ];
    while !eof {
        let mut output = ZstdEncoder::new(output_plain.take().unwrap(), 1)?;
        output.multithread(1)?;
        let next_group_start = entity_number + ENTITIES_PER_GROUP as u64;
        while !eof && entity_number < next_group_start {
            line.clear();
            let got = input.read_until(b'\n', &mut line)?;
            if got == 0 {
                eof = true;
                break;
            }
            let Some(json_str) = line.strip_suffix(b",\n") else { continue };
            let (_, ent) = item_parser::parse_json::<VerboseError<&[u8]>>(json_str, &selectors).map_err(|err| {
                eprintln!(
                    "{:?}",
                    err.map(|inner| inner
                        .errors
                        .into_iter()
                        .map(|(i, k)| (std::str::from_utf8(i).unwrap_or_else(|_| "<non-utf8>"), k))
                        .collect::<Vec<_>>())
                );
                ErrorKind::ParseJsonError
            })?;
            let entity_kvs = match ent {
                JsonValue::Object(kvs) => kvs,
                _ => {
                    return Err(ErrorKind::EntityError("not an object").into());
                }
            };
            let mut id_str = None;
            let mut claims = None;
            for (k, v) in entity_kvs {
                match k.borrow() {
                    "id" => match v {
                        JsonValue::Str(s) => {
                            id_str = Some(s)
                        },
                        _ => {
                            return Err(ErrorKind::EntityError("id not a string").into());
                        }
                    }
                    "claims" => match v {
                        JsonValue::Object(claim_kvs) => {
                            claims = Some(claim_kvs)
                        }
                        _ => {
                            return Err(ErrorKind::EntityError("id not a string").into());
                        }
                    }
                    "sitelinks" => match v {
                        JsonValue::Object(sitelinks_kvs) => {
                            if let Some((_, JsonValue::Object(link_kvs))) = sitelinks_kvs.into_iter().find(|(k, _)| k == "enwiki") {
                                if let Some((_, JsonValue::Str(title))) = link_kvs.into_iter().find(|(k, _)| k == "title") {
                                    writeln!(output_en, "{}\t{}", entity_number, title)?;
                                    enwiki_count += 1;
                                }
                            }
                        }
                        _ => {},
                    }
                    _ => {},
                }
            }
            let id_str = id_str.ok_or_else(|| ErrorKind::EntityError("missing id"))?;
            let (id_kind, id_num_str) = id_str.split_at(1);
            let id_num: u64 = id_num_str.parse()?;
            let id = id_num | ((id_kind.as_bytes()[0] as u64) << 56);
            output.write_all(json_str)?;
            output.write_all(b"\n")?;
            ind_entries.push((id, entity_number));
            for (prop_id_str, _) in claims.into_iter().flatten() {
                let (id_kind, id_num_str) = prop_id_str.split_at(1);
                let id_num: u64 = id_num_str.parse()?;
                let id = id_num | ((id_kind.as_bytes()[0] as u64) << 56);
                prop_ind_entries.push((id, entity_number));
            }
            entity_number += 1;
        }
        if ind_entries.len() >= STG_BLOCK_LIMIT {
            let block = &mut ind_entries[0..STG_BLOCK_LIMIT];
            block.sort_by_key(|e| e.0);

            let mut output_stg = BufWriter::with_capacity(131072, file_stg.take().unwrap());
            for (k, eidx) in ind_entries.drain(0..STG_BLOCK_LIMIT) {
                output_stg.write_all(&k.to_be_bytes())?;
                output_stg.write_all(&eidx.to_be_bytes())?;
            }
            staged_count += STG_BLOCK_LIMIT as u64;
            let _ = file_stg.insert(output_stg.into_inner().map_err(std::io::Error::from)?);
        }
        while prop_ind_entries.len() >= STG_BLOCK_LIMIT {
            let block = &mut prop_ind_entries[0..STG_BLOCK_LIMIT];
            block.sort();

            let mut output_stg_secondary = BufWriter::with_capacity(131072, file_stg_secondary.take().unwrap());
            for (k, eidx) in prop_ind_entries.drain(0..STG_BLOCK_LIMIT) {
                output_stg_secondary.write_all(&k.to_be_bytes())?;
                output_stg_secondary.write_all(&eidx.to_be_bytes())?;
            }
            prop_staged_count += STG_BLOCK_LIMIT as u64;
            let _ = file_stg_secondary.insert(output_stg_secondary.into_inner().map_err(std::io::Error::from)?);
        }

        group_offsets.push(curr_group_offset);
        eprint!(
            "\r\x1b[K{:>12}\t{:>12}\t{:>8}",
            prop_staged_count + prop_ind_entries.len() as u64,
            staged_count + ind_entries.len() as u64,
            enwiki_count
        );
        let mut output_flushed = output.finish()?;
        // output_flushed.flush()?;
        curr_group_offset = output_flushed.stream_position()?;
        let _ = output_plain.insert(output_flushed);
    }
    output_en.flush()?;
    {
        // TODO: remove when create_index_secondary is implemented
        let block = &mut prop_ind_entries[..];
        block.sort();

        let mut output_stg_secondary = BufWriter::with_capacity(131072, file_stg_secondary.take().unwrap());
        for (k, eidx) in prop_ind_entries.drain(..) {
            output_stg_secondary.write_all(&k.to_be_bytes())?;
            output_stg_secondary.write_all(&eidx.to_be_bytes())?;
        }
        prop_staged_count += STG_BLOCK_LIMIT as u64;
        let _ = file_stg_secondary.insert(output_stg_secondary.into_inner().map_err(std::io::Error::from)?);
    }

    Ok(Segmented {
        group_offsets,
        staged_count,
        ind_entries,
        prop_staged_count,
        prop_ind_entries,
    })
}

/// STEP 2: make B+tree
///
/// ```ascii
/// header  = [len(group_offsets), ...group_offsets, num_levels]
/// level 1 = root [(width=W, [W-1] keys, [W] ptrs)]  | N
///           where 2 <= W <= 256                       :'---------------------------------.
/// level _ = nodes [(width=W, [W-1] keys, [W] ptrs)] | N                                  N
///           where 128 <= W <= 256                     :'--------.---------.---[W times]# :
///                                                     :         :         :            # :
/// level N = leaves [(key, group, entity)]           | L-L-L-L # L-L-L-L # L-L- etc     # L etc
/// ```
///
/// the key of a node is the minimum/leftmost leaf key reachable from it
///
///     key(Leaf) := key in object
///     key(Node) := key(ptrs[0].deref())
///
/// the keys *in* a node refer to their direct children, corresponding to ptrs[1..W].
/// they divide the key set into W intervals, one for each pointer to follow
///
///     (-infty, keys[0])      -> ptrs[0];   [keys[0], keys[1]) -> ptrs[1]
///     [keys[W-3], keys[W-2]) -> ptrs[W-2]; [keys[W-2], infty) -> ptrs[W-1];
///
/// at level N, the leaf block reached contains all valid keys in the interval indicated
/// by its referencing pointer in level N-1
pub fn write_index<R: Read, W: Write + Seek>(
    mut entries: Vec<(u64, u64)>,
    group_offsets: Vec<u64>,
    staged_entry_files: Vec<R>,
    staged_count: u64,
    file: W,
) -> Result<W> {
    // STEP 2a: determine structure of tree (i.e. size of each level)
    let mut output = BufWriter::with_capacity(131072, file);

    let num_leaves = staged_count + entries.len() as u64;
    let build_level_descriptions =
        std::iter::successors(Some(LevelDesc::from_total_children(num_leaves)), |prev| {
            if prev.width > 1 {
                Some(LevelDesc::from_total_children(prev.width))
            } else {
                None
            }
        });
    let mut level_descriptions: Vec<_> = build_level_descriptions.collect();
    level_descriptions.reverse();

    // STEP 2b: write header, then write placeholders for all levels of the tree except
    //          the last.
    let mut offset: u64 = 0;
    output.write_all(&group_offsets.len().to_be_bytes())?;
    offset += 8;
    for v in group_offsets.iter() {
        output.write_all(&v.to_be_bytes())?;
        offset += 8;
    }
    let n_levels = level_descriptions.len();
    output.write_all(&n_levels.to_be_bytes())?;
    offset += 8;
    let mut level_offsets = vec![];
    for ld in level_descriptions[0..n_levels - 1].iter() {
        level_offsets.push(offset);
        let sz = ld.byte_size();
        let zeros = vec![0; sz as usize];
        output.write_all(&zeros)?;
        offset += sz;
    }
    level_offsets.push(offset);

    // STEP 2c: merge sort the in-memory and staging node-list chunks to form the
    //           last (leaf) level of the B+tree.
    //          save the keys and offsets of leaves which begin a leaf block
    let mut readers: Vec<&mut dyn Iterator<Item = std::io::Result<(u64, u64)>>> = vec![];
    entries.sort_by_key(|e| e.0);
    let mut entries_iter = entries.iter().copied().map(Ok);
    let mut disk_readers: Vec<_> = staged_entry_files.into_iter().map(DiskIter::new).collect();
    for dr in disk_readers.iter_mut() {
        readers.push(dr)
    }
    readers.push(&mut entries_iter);

    let mut merge_queue = BinaryHeap::new();
    for (i, reader) in readers.iter_mut().enumerate() {
        if let Some(item_res) = reader.next() {
            let item = item_res?;
            merge_queue.push(DatumWrapper(item, i));
        }
    }
    let mut ind_size = 0;
    let mut level_keys = vec![];
    let mut level_pointers = vec![];
    let leaf_level_desc = level_descriptions.last().unwrap();
    while !merge_queue.is_empty() {
        let DatumWrapper(item, i) = merge_queue.pop().unwrap();
        if leaf_level_desc.is_child_first_in_node(ind_size) {
            level_keys.push(item.0);
            level_pointers.push(offset);
        }
        output.write_all(&item.0.to_be_bytes())?;
        output.write_all(&item.1.to_be_bytes())?;
        offset += 16;
        ind_size += 1;

        if let Some(item_res) = readers[i].next() {
            let item = item_res?;
            merge_queue.push(DatumWrapper(item, i));
        }
    }

    // STEP 2d: propagate keys and pointers up the tree, one level at a time
    for (i, ld) in level_descriptions.iter().enumerate().rev().skip(1) {
        assert_eq!(level_keys.len(), ld.total_children() as usize);
        assert_eq!(level_keys.len(), level_pointers.len());
        let _ = output.seek(SeekFrom::Start(level_offsets[i]))?;
        offset = level_offsets[i];
        let mut j = 0;
        let mut next_level_keys = vec![];
        let mut next_level_pointers = vec![];
        for sz in ld.node_sizes() {
            next_level_keys.push(level_keys[j]);
            next_level_pointers.push(offset);
            output.write_all(&(sz as u64).to_be_bytes())?;
            for child_num in 1..(sz as usize) {
                output.write_all(&level_keys[j + child_num].to_be_bytes())?;
            }
            for child_num in 0..(sz as usize) {
                output.write_all(&level_pointers[j + child_num].to_be_bytes())?;
            }
            offset += 16 * sz as u64;
            j += sz as usize;
        }
        level_keys = next_level_keys;
        level_pointers = next_level_pointers;
    }
    output
        .into_inner()
        .map_err(|e| std::io::Error::from(e).into())
}

mod tests {
    use std::io::{Seek, Write};

    #[derive(Default)]
    struct SeekableVec {
        offset: usize,
        inner: Vec<u8>,
    }

    impl Write for SeekableVec {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            // self.offset <= self.inner.len() must be maintained
            let overwrite = self.inner.len() - self.offset;
            let sz = buf.len();
            if overwrite > 0 {
                if overwrite < sz {
                    // SAFETY: overwrite equals the length of `self.offset..`
                    // overwrite > 0 -> self.offset < self.inner.len()
                    unsafe {
                        self.inner
                            .get_unchecked_mut(self.offset..)
                            .copy_from_slice(&buf[..overwrite]);
                    }
                    self.inner.extend_from_slice(&buf[overwrite..]);
                } else {
                    unsafe {
                        self.inner
                            .get_unchecked_mut(self.offset..self.offset + sz)
                            .copy_from_slice(buf);
                    }
                }
            } else {
                self.inner.extend_from_slice(buf);
            }

            self.offset += sz;
            Ok(sz)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    impl Seek for SeekableVec {
        fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
            match pos {
                std::io::SeekFrom::Start(v) => {
                    let val = v.try_into().map_err(|_| std::io::ErrorKind::InvalidInput)?;
                    if val < self.inner.len() {
                        self.offset = val;
                        return Ok(v);
                    }
                    Err(std::io::ErrorKind::InvalidInput.into())
                }
                std::io::SeekFrom::End(off) => {
                    let v = u64::try_from(self.inner.len() as i64 + off)
                        .map_err(|_| std::io::ErrorKind::InvalidInput)?;
                    let val = v.try_into().map_err(|_| std::io::ErrorKind::InvalidInput)?;
                    if val < self.inner.len() {
                        self.offset = val;
                        return Ok(v);
                    }
                    Err(std::io::ErrorKind::InvalidInput.into())
                }
                std::io::SeekFrom::Current(off) => {
                    let v = u64::try_from(self.offset as i64 + off)
                        .map_err(|_| std::io::ErrorKind::InvalidInput)?;
                    let val = v.try_into().map_err(|_| std::io::ErrorKind::InvalidInput)?;
                    if val < self.inner.len() {
                        self.offset = val;
                        return Ok(v);
                    }
                    Err(std::io::ErrorKind::InvalidInput.into())
                }
            }
        }
    }

    #[test]
    #[rustfmt::skip]
    fn write_index_one_level() -> super::Result<()> {
        let ind_entries = (1u64..=20).map(|k| (k, k + 64)).collect();
        let group_offsets = vec![0, 1000, 2000, 3000];
        let staged_entry_files: Vec<std::fs::File> = vec![];
        let actual = super::write_index(ind_entries, group_offsets, staged_entry_files, 0, SeekableVec::default())?;
        let actual64: Vec<_> = actual.inner.chunks_exact(8).map(|c| {
            let mut b = [0u8; 8];
            b.copy_from_slice(c);
            u64::from_be_bytes(b)
        }).collect();
        assert_eq!(actual64, vec![
            4, 0, 1000, 2000, 3000,
            1,
            1, 0x41,
            2, 0x42,
            3, 0x43,
            4, 0x44,
            5, 0x45,
            6, 0x46,
            7, 0x47,
            8, 0x48,
            9, 0x49,
            10, 0x4a,
            11, 0x4b,
            12, 0x4c,
            13, 0x4d,
            14, 0x4e,
            15, 0x4f,
            16, 0x50,
            17, 0x51,
            18, 0x52,
            19, 0x53,
            20, 0x54,
        ]);
        Ok(())
    }

    #[test]
    #[rustfmt::skip]
    fn write_index_two_levels() -> super::Result<()> {
        let ind_entries = (1u64..=601).map(|k| (k, k + 64)).collect();
        let group_offsets = vec![0, 1000, 2000, 3000];
        let staged_entry_files: Vec<std::fs::File> = vec![];
        let actual = super::write_index(ind_entries, group_offsets, staged_entry_files, 0, SeekableVec::default())?;
        let actual64: Vec<_> = actual.inner.chunks_exact(8).map(|c| {
            let mut b = [0u8; 8];
            b.copy_from_slice(c);
            u64::from_be_bytes(b)
        }).collect();
        assert_eq!(actual64, vec![
            4, 0, 1000, 2000, 3000,
            2,
            3, 257, 430, 0x60, 0x1060, (0x60 + 429*16),
            // offset = (1+4+1+6)*8 = 0x60
            1  , 0x41, 2  , 0x42, 3  , 0x43, 4  , 0x44, 5  , 0x45, 6  , 0x46, 7  , 0x47, 8  , 0x48, 9  , 0x49, 10 , 0x4a, 11 , 0x4b, 12 , 0x4c, 13 , 0x4d, 14 , 0x4e, 15 , 0x4f, 16 , 0x50,
            17 , 0x51, 18 , 0x52, 19 , 0x53, 20 , 0x54, 21 , 0x55, 22 , 0x56, 23 , 0x57, 24 , 0x58, 25 , 0x59, 26 , 0x5a, 27 , 0x5b, 28 , 0x5c, 29 , 0x5d, 30 , 0x5e, 31 , 0x5f, 32 , 0x60,
            33 , 0x61, 34 , 0x62, 35 , 0x63, 36 , 0x64, 37 , 0x65, 38 , 0x66, 39 , 0x67, 40 , 0x68, 41 , 0x69, 42 , 0x6a, 43 , 0x6b, 44 , 0x6c, 45 , 0x6d, 46 , 0x6e, 47 , 0x6f, 48 , 0x70,
            49 , 0x71, 50 , 0x72, 51 , 0x73, 52 , 0x74, 53 , 0x75, 54 , 0x76, 55 , 0x77, 56 , 0x78, 57 , 0x79, 58 , 0x7a, 59 , 0x7b, 60 , 0x7c, 61 , 0x7d, 62 , 0x7e, 63 , 0x7f, 64 , 0x80,
            65 , 0x81, 66 , 0x82, 67 , 0x83, 68 , 0x84, 69 , 0x85, 70 , 0x86, 71 , 0x87, 72 , 0x88, 73 , 0x89, 74 , 0x8a, 75 , 0x8b, 76 , 0x8c, 77 , 0x8d, 78 , 0x8e, 79 , 0x8f, 80 , 0x90,
            81 , 0x91, 82 , 0x92, 83 , 0x93, 84 , 0x94, 85 , 0x95, 86 , 0x96, 87 , 0x97, 88 , 0x98, 89 , 0x99, 90 , 0x9a, 91 , 0x9b, 92 , 0x9c, 93 , 0x9d, 94 , 0x9e, 95 , 0x9f, 96 , 0xa0,
            97 , 0xa1, 98 , 0xa2, 99 , 0xa3, 100, 0xa4, 101, 0xa5, 102, 0xa6, 103, 0xa7, 104, 0xa8, 105, 0xa9, 106, 0xaa, 107, 0xab, 108, 0xac, 109, 0xad, 110, 0xae, 111, 0xaf, 112, 0xb0,
            113, 0xb1, 114, 0xb2, 115, 0xb3, 116, 0xb4, 117, 0xb5, 118, 0xb6, 119, 0xb7, 120, 0xb8, 121, 0xb9, 122, 0xba, 123, 0xbb, 124, 0xbc, 125, 0xbd, 126, 0xbe, 127, 0xbf, 128, 0xc0,
            129, 0xc1, 130, 0xc2, 131, 0xc3, 132, 0xc4, 133, 0xc5, 134, 0xc6, 135, 0xc7, 136, 0xc8, 137, 0xc9, 138, 0xca, 139, 0xcb, 140, 0xcc, 141, 0xcd, 142, 0xce, 143, 0xcf, 144, 0xd0,
            145, 0xd1, 146, 0xd2, 147, 0xd3, 148, 0xd4, 149, 0xd5, 150, 0xd6, 151, 0xd7, 152, 0xd8, 153, 0xd9, 154, 0xda, 155, 0xdb, 156, 0xdc, 157, 0xdd, 158, 0xde, 159, 0xdf, 160, 0xe0,
            161, 0xe1, 162, 0xe2, 163, 0xe3, 164, 0xe4, 165, 0xe5, 166, 0xe6, 167, 0xe7, 168, 0xe8, 169, 0xe9, 170, 0xea, 171, 0xeb, 172, 0xec, 173, 0xed, 174, 0xee, 175, 0xef, 176, 0xf0,
            177, 0xf1, 178, 0xf2, 179, 0xf3, 180, 0xf4, 181, 0xf5, 182, 0xf6, 183, 0xf7, 184, 0xf8, 185, 0xf9, 186, 0xfa, 187, 0xfb, 188, 0xfc, 189, 0xfd, 190, 0xfe, 191, 0xff, 192, 0x100,
            193, 0x101, 194, 0x102, 195, 0x103, 196, 0x104, 197, 0x105, 198, 0x106, 199, 0x107, 200, 0x108, 201, 0x109, 202, 0x10a, 203, 0x10b, 204, 0x10c, 205, 0x10d, 206, 0x10e, 207, 0x10f, 208, 0x110,
            209, 0x111, 210, 0x112, 211, 0x113, 212, 0x114, 213, 0x115, 214, 0x116, 215, 0x117, 216, 0x118, 217, 0x119, 218, 0x11a, 219, 0x11b, 220, 0x11c, 221, 0x11d, 222, 0x11e, 223, 0x11f, 224, 0x120,
            225, 0x121, 226, 0x122, 227, 0x123, 228, 0x124, 229, 0x125, 230, 0x126, 231, 0x127, 232, 0x128, 233, 0x129, 234, 0x12a, 235, 0x12b, 236, 0x12c, 237, 0x12d, 238, 0x12e, 239, 0x12f, 240, 0x130,
            241, 0x131, 242, 0x132, 243, 0x133, 244, 0x134, 245, 0x135, 246, 0x136, 247, 0x137, 248, 0x138, 249, 0x139, 250, 0x13a, 251, 0x13b, 252, 0x13c, 253, 0x13d, 254, 0x13e, 255, 0x13f, 256, 0x140,
            257, 0x141, 258, 0x142, 259, 0x143, 260, 0x144, 261, 0x145, 262, 0x146, 263, 0x147, 264, 0x148, 265, 0x149, 266, 0x14a, 267, 0x14b, 268, 0x14c, 269, 0x14d, 270, 0x14e, 271, 0x14f, 272, 0x150,
            273, 0x151, 274, 0x152, 275, 0x153, 276, 0x154, 277, 0x155, 278, 0x156, 279, 0x157, 280, 0x158, 281, 0x159, 282, 0x15a, 283, 0x15b, 284, 0x15c, 285, 0x15d, 286, 0x15e, 287, 0x15f, 288, 0x160,
            289, 0x161, 290, 0x162, 291, 0x163, 292, 0x164, 293, 0x165, 294, 0x166, 295, 0x167, 296, 0x168, 297, 0x169, 298, 0x16a, 299, 0x16b, 300, 0x16c, 301, 0x16d, 302, 0x16e, 303, 0x16f, 304, 0x170,
            305, 0x171, 306, 0x172, 307, 0x173, 308, 0x174, 309, 0x175, 310, 0x176, 311, 0x177, 312, 0x178, 313, 0x179, 314, 0x17a, 315, 0x17b, 316, 0x17c, 317, 0x17d, 318, 0x17e, 319, 0x17f, 320, 0x180,
            321, 0x181, 322, 0x182, 323, 0x183, 324, 0x184, 325, 0x185, 326, 0x186, 327, 0x187, 328, 0x188, 329, 0x189, 330, 0x18a, 331, 0x18b, 332, 0x18c, 333, 0x18d, 334, 0x18e, 335, 0x18f, 336, 0x190,
            337, 0x191, 338, 0x192, 339, 0x193, 340, 0x194, 341, 0x195, 342, 0x196, 343, 0x197, 344, 0x198, 345, 0x199, 346, 0x19a, 347, 0x19b, 348, 0x19c, 349, 0x19d, 350, 0x19e, 351, 0x19f, 352, 0x1a0,
            353, 0x1a1, 354, 0x1a2, 355, 0x1a3, 356, 0x1a4, 357, 0x1a5, 358, 0x1a6, 359, 0x1a7, 360, 0x1a8, 361, 0x1a9, 362, 0x1aa, 363, 0x1ab, 364, 0x1ac, 365, 0x1ad, 366, 0x1ae, 367, 0x1af, 368, 0x1b0,
            369, 0x1b1, 370, 0x1b2, 371, 0x1b3, 372, 0x1b4, 373, 0x1b5, 374, 0x1b6, 375, 0x1b7, 376, 0x1b8, 377, 0x1b9, 378, 0x1ba, 379, 0x1bb, 380, 0x1bc, 381, 0x1bd, 382, 0x1be, 383, 0x1bf, 384, 0x1c0,
            385, 0x1c1, 386, 0x1c2, 387, 0x1c3, 388, 0x1c4, 389, 0x1c5, 390, 0x1c6, 391, 0x1c7, 392, 0x1c8, 393, 0x1c9, 394, 0x1ca, 395, 0x1cb, 396, 0x1cc, 397, 0x1cd, 398, 0x1ce, 399, 0x1cf, 400, 0x1d0,
            401, 0x1d1, 402, 0x1d2, 403, 0x1d3, 404, 0x1d4, 405, 0x1d5, 406, 0x1d6, 407, 0x1d7, 408, 0x1d8, 409, 0x1d9, 410, 0x1da, 411, 0x1db, 412, 0x1dc, 413, 0x1dd, 414, 0x1de, 415, 0x1df, 416, 0x1e0,
            417, 0x1e1, 418, 0x1e2, 419, 0x1e3, 420, 0x1e4, 421, 0x1e5, 422, 0x1e6, 423, 0x1e7, 424, 0x1e8, 425, 0x1e9, 426, 0x1ea, 427, 0x1eb, 428, 0x1ec, 429, 0x1ed, 430, 0x1ee, 431, 0x1ef, 432, 0x1f0,
            433, 0x1f1, 434, 0x1f2, 435, 0x1f3, 436, 0x1f4, 437, 0x1f5, 438, 0x1f6, 439, 0x1f7, 440, 0x1f8, 441, 0x1f9, 442, 0x1fa, 443, 0x1fb, 444, 0x1fc, 445, 0x1fd, 446, 0x1fe, 447, 0x1ff, 448, 0x200,
            449, 0x201, 450, 0x202, 451, 0x203, 452, 0x204, 453, 0x205, 454, 0x206, 455, 0x207, 456, 0x208, 457, 0x209, 458, 0x20a, 459, 0x20b, 460, 0x20c, 461, 0x20d, 462, 0x20e, 463, 0x20f, 464, 0x210,
            465, 0x211, 466, 0x212, 467, 0x213, 468, 0x214, 469, 0x215, 470, 0x216, 471, 0x217, 472, 0x218, 473, 0x219, 474, 0x21a, 475, 0x21b, 476, 0x21c, 477, 0x21d, 478, 0x21e, 479, 0x21f, 480, 0x220,
            481, 0x221, 482, 0x222, 483, 0x223, 484, 0x224, 485, 0x225, 486, 0x226, 487, 0x227, 488, 0x228, 489, 0x229, 490, 0x22a, 491, 0x22b, 492, 0x22c, 493, 0x22d, 494, 0x22e, 495, 0x22f, 496, 0x230,
            497, 0x231, 498, 0x232, 499, 0x233, 500, 0x234, 501, 0x235, 502, 0x236, 503, 0x237, 504, 0x238, 505, 0x239, 506, 0x23a, 507, 0x23b, 508, 0x23c, 509, 0x23d, 510, 0x23e, 511, 0x23f, 512, 0x240,
            513, 0x241, 514, 0x242, 515, 0x243, 516, 0x244, 517, 0x245, 518, 0x246, 519, 0x247, 520, 0x248, 521, 0x249, 522, 0x24a, 523, 0x24b, 524, 0x24c, 525, 0x24d, 526, 0x24e, 527, 0x24f, 528, 0x250,
            529, 0x251, 530, 0x252, 531, 0x253, 532, 0x254, 533, 0x255, 534, 0x256, 535, 0x257, 536, 0x258, 537, 0x259, 538, 0x25a, 539, 0x25b, 540, 0x25c, 541, 0x25d, 542, 0x25e, 543, 0x25f, 544, 0x260,
            545, 0x261, 546, 0x262, 547, 0x263, 548, 0x264, 549, 0x265, 550, 0x266, 551, 0x267, 552, 0x268, 553, 0x269, 554, 0x26a, 555, 0x26b, 556, 0x26c, 557, 0x26d, 558, 0x26e, 559, 0x26f, 560, 0x270,
            561, 0x271, 562, 0x272, 563, 0x273, 564, 0x274, 565, 0x275, 566, 0x276, 567, 0x277, 568, 0x278, 569, 0x279, 570, 0x27a, 571, 0x27b, 572, 0x27c, 573, 0x27d, 574, 0x27e, 575, 0x27f, 576, 0x280,
            577, 0x281, 578, 0x282, 579, 0x283, 580, 0x284, 581, 0x285, 582, 0x286, 583, 0x287, 584, 0x288, 585, 0x289, 586, 0x28a, 587, 0x28b, 588, 0x28c, 589, 0x28d, 590, 0x28e, 591, 0x28f, 592, 0x290,
            593, 0x291, 594, 0x292, 595, 0x293, 596, 0x294, 597, 0x295, 598, 0x296, 599, 0x297, 600, 0x298, 601, 0x299,
        ]);
        Ok(())
    }
}
