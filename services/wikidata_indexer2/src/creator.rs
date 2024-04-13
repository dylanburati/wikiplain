use std::collections::BinaryHeap;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};

use flate2::read::MultiGzDecoder;
use zstd::Encoder as ZstdEncoder;
use serde::Deserialize;

use crate::Result;

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
    type Item = std::io::Result<(u64, u32, u32)>;

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

        let mut buf = [0u8; 4];
        let b = match self.inner.read_exact(&mut buf) {
            Ok(_) => u32::from_be_bytes(buf),
            Err(err) => return Some(Err(err)),
        };

        let mut buf = [0u8; 4];
        let c = match self.inner.read_exact(&mut buf) {
            Ok(_) => u32::from_be_bytes(buf),
            Err(err) => return Some(Err(err)),
        };

        Some(Ok((a, b, c)))
    }
}

#[derive(PartialEq, Eq)]
struct DatumWrapper((u64, u32, u32), usize);

impl PartialOrd for DatumWrapper {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.0 .0.partial_cmp(&self.0 .0)
    }
}

impl Ord for DatumWrapper {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.0 .0.cmp(&self.0 .0)
    }
}

#[derive(Deserialize)]
struct HasTitle {
    title: String,
}

#[derive(Deserialize)]
struct WikidataSitelinks {
    enwiki: Option<HasTitle>,
}

#[derive(Deserialize)]
struct WikidataEntityJson {
    id: String,
    sitelinks: Option<WikidataSitelinks>,
}

struct Segmented {
    staged_count: u64,
    ind_entries: Vec<(u64, u32, u32)>,
    group_offsets: Vec<u64>,
}

// a couple thousand less than 2^24, so the vector doesn't reserve past that capacity
const STG_BLOCK_LIMIT: usize = 16 * 1000 * 1024;

pub fn create(input_path: &str, output_path: &str, working_path: &str) -> Result<()> {
    let base = output_path.strip_suffix(".zst").unwrap_or(output_path);
    let output_index_path = base.to_string() + ".index.bin";
    let output_index_en_path = base.to_string() + "-en.index.txt";

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

    let segmented = write_segmented(file_input, file_output, file_en, file_stg)?;
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
) -> Result<Segmented> {
    let mut input = MultiGzDecoder::new(file_input);
    let mut output_plain = Some(file_output);
    let mut output_en = BufWriter::with_capacity(131072, file_en);
    let mut file_stg = Some(file_stg);

    // Skip "[\n", then buffer starting from the first object / second line
    let mut array_start_buf = [0u8; 2];
    input.read_exact(&mut array_start_buf)?;
    let mut input = BufReader::new(input);

    let mut line = Vec::new();
    let mut eof = false;
    let mut group_number: u32 = 0;
    let mut curr_group_offset = 0;
    let mut enwiki_count = 0;
    let mut ind_entries: Vec<(u64, u32, u32)> = vec![];
    let mut group_offsets = vec![];
    let mut staged_count: u64 = 0;
    while !eof {
        let mut output = ZstdEncoder::new(output_plain.take().unwrap(), 1)?;
        output.multithread(1)?;
        let mut entity_number: u32 = 0;
        while !eof && entity_number < ENTITIES_PER_GROUP {
            line.clear();
            let got = input.read_until(b'\n', &mut line)?;
            if got == 0 {
                eof = true;
                break;
            }
            let Some(json_str) = line.strip_suffix(b",\n") else { continue };
            let ent: WikidataEntityJson = wikidata_item::parse(json_str)?;
            if let Some(title) = ent.sitelinks.and_then(|s| s.enwiki).map(|e| e.title) {
                writeln!(output_en, "{}\t{}\t{}", group_number, entity_number, title)?;
                enwiki_count += 1;
            }
            let (id_kind, id_num_str) = ent.id.split_at(1);
            let id_num: u64 = id_num_str.parse()?;
            let id = id_num | ((id_kind.as_bytes()[0] as u64) << 56);
            output.write_all(json_str)?;
            output.write_all(b"\n")?;
            ind_entries.push((id, group_number, entity_number));
            entity_number += 1;
        }
        if ind_entries.len() >= STG_BLOCK_LIMIT {
            let block = &mut ind_entries[0..STG_BLOCK_LIMIT];
            block.sort_by_key(|e| e.0);

            let mut output_stg = BufWriter::with_capacity(131072, file_stg.take().unwrap());
            for (k, gidx, eidx) in ind_entries.drain(0..STG_BLOCK_LIMIT) {
                output_stg.write_all(&k.to_be_bytes())?;
                output_stg.write_all(&gidx.to_be_bytes())?;
                output_stg.write_all(&eidx.to_be_bytes())?;
            }
            staged_count += STG_BLOCK_LIMIT as u64;
            let _ = file_stg.insert(output_stg.into_inner().map_err(std::io::Error::from)?);
        }

        group_offsets.push(curr_group_offset);
        group_number += 1;
        eprint!(
            "\r\x1b[K{:>12}\t{:>8}",
            staged_count + ind_entries.len() as u64,
            enwiki_count
        );
        let mut output_flushed = output.finish()?;
        // output_flushed.flush()?;
        curr_group_offset = output_flushed.stream_position()?;
        let _ = output_plain.insert(output_flushed);
    }
    output_en.flush()?;

    Ok(Segmented {
        staged_count,
        ind_entries,
        group_offsets,
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
    mut entries: Vec<(u64, u32, u32)>,
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
    let mut readers: Vec<&mut dyn Iterator<Item = std::io::Result<(u64, u32, u32)>>> = vec![];
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
        output.write_all(&item.2.to_be_bytes())?;
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
        let ind_entries = (1u64..=20).map(|k| (k, (k + 32) as u32, (k + 64) as u32)).collect();
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
            1, 0x21_00000041,
            2, 0x22_00000042,
            3, 0x23_00000043,
            4, 0x24_00000044,
            5, 0x25_00000045,
            6, 0x26_00000046,
            7, 0x27_00000047,
            8, 0x28_00000048,
            9, 0x29_00000049,
            10, 0x2a_0000004a,
            11, 0x2b_0000004b,
            12, 0x2c_0000004c,
            13, 0x2d_0000004d,
            14, 0x2e_0000004e,
            15, 0x2f_0000004f,
            16, 0x30_00000050,
            17, 0x31_00000051,
            18, 0x32_00000052,
            19, 0x33_00000053,
            20, 0x34_00000054,
        ]);
        Ok(())
    }

    #[test]
    #[rustfmt::skip]
    fn write_index_two_levels() -> super::Result<()> {
        let ind_entries = (1u64..=601).map(|k| (k, (k + 32) as u32, (k + 64) as u32)).collect();
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
            1  , 0x021_00000041, 2  , 0x022_00000042, 3  , 0x023_00000043, 4  , 0x024_00000044, 5  , 0x025_00000045, 6  , 0x026_00000046, 7  , 0x027_00000047, 8  , 0x028_00000048, 9  , 0x029_00000049, 10 , 0x02a_0000004a, 11 , 0x02b_0000004b, 12 , 0x02c_0000004c, 13 , 0x02d_0000004d, 14 , 0x02e_0000004e, 15 , 0x02f_0000004f, 16 , 0x030_00000050,
            17 , 0x031_00000051, 18 , 0x032_00000052, 19 , 0x033_00000053, 20 , 0x034_00000054, 21 , 0x035_00000055, 22 , 0x036_00000056, 23 , 0x037_00000057, 24 , 0x038_00000058, 25 , 0x039_00000059, 26 , 0x03a_0000005a, 27 , 0x03b_0000005b, 28 , 0x03c_0000005c, 29 , 0x03d_0000005d, 30 , 0x03e_0000005e, 31 , 0x03f_0000005f, 32 , 0x040_00000060,
            33 , 0x041_00000061, 34 , 0x042_00000062, 35 , 0x043_00000063, 36 , 0x044_00000064, 37 , 0x045_00000065, 38 , 0x046_00000066, 39 , 0x047_00000067, 40 , 0x048_00000068, 41 , 0x049_00000069, 42 , 0x04a_0000006a, 43 , 0x04b_0000006b, 44 , 0x04c_0000006c, 45 , 0x04d_0000006d, 46 , 0x04e_0000006e, 47 , 0x04f_0000006f, 48 , 0x050_00000070,
            49 , 0x051_00000071, 50 , 0x052_00000072, 51 , 0x053_00000073, 52 , 0x054_00000074, 53 , 0x055_00000075, 54 , 0x056_00000076, 55 , 0x057_00000077, 56 , 0x058_00000078, 57 , 0x059_00000079, 58 , 0x05a_0000007a, 59 , 0x05b_0000007b, 60 , 0x05c_0000007c, 61 , 0x05d_0000007d, 62 , 0x05e_0000007e, 63 , 0x05f_0000007f, 64 , 0x060_00000080,
            65 , 0x061_00000081, 66 , 0x062_00000082, 67 , 0x063_00000083, 68 , 0x064_00000084, 69 , 0x065_00000085, 70 , 0x066_00000086, 71 , 0x067_00000087, 72 , 0x068_00000088, 73 , 0x069_00000089, 74 , 0x06a_0000008a, 75 , 0x06b_0000008b, 76 , 0x06c_0000008c, 77 , 0x06d_0000008d, 78 , 0x06e_0000008e, 79 , 0x06f_0000008f, 80 , 0x070_00000090,
            81 , 0x071_00000091, 82 , 0x072_00000092, 83 , 0x073_00000093, 84 , 0x074_00000094, 85 , 0x075_00000095, 86 , 0x076_00000096, 87 , 0x077_00000097, 88 , 0x078_00000098, 89 , 0x079_00000099, 90 , 0x07a_0000009a, 91 , 0x07b_0000009b, 92 , 0x07c_0000009c, 93 , 0x07d_0000009d, 94 , 0x07e_0000009e, 95 , 0x07f_0000009f, 96 , 0x080_000000a0,
            97 , 0x081_000000a1, 98 , 0x082_000000a2, 99 , 0x083_000000a3, 100, 0x084_000000a4, 101, 0x085_000000a5, 102, 0x086_000000a6, 103, 0x087_000000a7, 104, 0x088_000000a8, 105, 0x089_000000a9, 106, 0x08a_000000aa, 107, 0x08b_000000ab, 108, 0x08c_000000ac, 109, 0x08d_000000ad, 110, 0x08e_000000ae, 111, 0x08f_000000af, 112, 0x090_000000b0,
            113, 0x091_000000b1, 114, 0x092_000000b2, 115, 0x093_000000b3, 116, 0x094_000000b4, 117, 0x095_000000b5, 118, 0x096_000000b6, 119, 0x097_000000b7, 120, 0x098_000000b8, 121, 0x099_000000b9, 122, 0x09a_000000ba, 123, 0x09b_000000bb, 124, 0x09c_000000bc, 125, 0x09d_000000bd, 126, 0x09e_000000be, 127, 0x09f_000000bf, 128, 0x0a0_000000c0,
            129, 0x0a1_000000c1, 130, 0x0a2_000000c2, 131, 0x0a3_000000c3, 132, 0x0a4_000000c4, 133, 0x0a5_000000c5, 134, 0x0a6_000000c6, 135, 0x0a7_000000c7, 136, 0x0a8_000000c8, 137, 0x0a9_000000c9, 138, 0x0aa_000000ca, 139, 0x0ab_000000cb, 140, 0x0ac_000000cc, 141, 0x0ad_000000cd, 142, 0x0ae_000000ce, 143, 0x0af_000000cf, 144, 0x0b0_000000d0,
            145, 0x0b1_000000d1, 146, 0x0b2_000000d2, 147, 0x0b3_000000d3, 148, 0x0b4_000000d4, 149, 0x0b5_000000d5, 150, 0x0b6_000000d6, 151, 0x0b7_000000d7, 152, 0x0b8_000000d8, 153, 0x0b9_000000d9, 154, 0x0ba_000000da, 155, 0x0bb_000000db, 156, 0x0bc_000000dc, 157, 0x0bd_000000dd, 158, 0x0be_000000de, 159, 0x0bf_000000df, 160, 0x0c0_000000e0,
            161, 0x0c1_000000e1, 162, 0x0c2_000000e2, 163, 0x0c3_000000e3, 164, 0x0c4_000000e4, 165, 0x0c5_000000e5, 166, 0x0c6_000000e6, 167, 0x0c7_000000e7, 168, 0x0c8_000000e8, 169, 0x0c9_000000e9, 170, 0x0ca_000000ea, 171, 0x0cb_000000eb, 172, 0x0cc_000000ec, 173, 0x0cd_000000ed, 174, 0x0ce_000000ee, 175, 0x0cf_000000ef, 176, 0x0d0_000000f0,
            177, 0x0d1_000000f1, 178, 0x0d2_000000f2, 179, 0x0d3_000000f3, 180, 0x0d4_000000f4, 181, 0x0d5_000000f5, 182, 0x0d6_000000f6, 183, 0x0d7_000000f7, 184, 0x0d8_000000f8, 185, 0x0d9_000000f9, 186, 0x0da_000000fa, 187, 0x0db_000000fb, 188, 0x0dc_000000fc, 189, 0x0dd_000000fd, 190, 0x0de_000000fe, 191, 0x0df_000000ff, 192, 0x0e0_00000100,
            193, 0x0e1_00000101, 194, 0x0e2_00000102, 195, 0x0e3_00000103, 196, 0x0e4_00000104, 197, 0x0e5_00000105, 198, 0x0e6_00000106, 199, 0x0e7_00000107, 200, 0x0e8_00000108, 201, 0x0e9_00000109, 202, 0x0ea_0000010a, 203, 0x0eb_0000010b, 204, 0x0ec_0000010c, 205, 0x0ed_0000010d, 206, 0x0ee_0000010e, 207, 0x0ef_0000010f, 208, 0x0f0_00000110,
            209, 0x0f1_00000111, 210, 0x0f2_00000112, 211, 0x0f3_00000113, 212, 0x0f4_00000114, 213, 0x0f5_00000115, 214, 0x0f6_00000116, 215, 0x0f7_00000117, 216, 0x0f8_00000118, 217, 0x0f9_00000119, 218, 0x0fa_0000011a, 219, 0x0fb_0000011b, 220, 0x0fc_0000011c, 221, 0x0fd_0000011d, 222, 0x0fe_0000011e, 223, 0x0ff_0000011f, 224, 0x100_00000120,
            225, 0x101_00000121, 226, 0x102_00000122, 227, 0x103_00000123, 228, 0x104_00000124, 229, 0x105_00000125, 230, 0x106_00000126, 231, 0x107_00000127, 232, 0x108_00000128, 233, 0x109_00000129, 234, 0x10a_0000012a, 235, 0x10b_0000012b, 236, 0x10c_0000012c, 237, 0x10d_0000012d, 238, 0x10e_0000012e, 239, 0x10f_0000012f, 240, 0x110_00000130,
            241, 0x111_00000131, 242, 0x112_00000132, 243, 0x113_00000133, 244, 0x114_00000134, 245, 0x115_00000135, 246, 0x116_00000136, 247, 0x117_00000137, 248, 0x118_00000138, 249, 0x119_00000139, 250, 0x11a_0000013a, 251, 0x11b_0000013b, 252, 0x11c_0000013c, 253, 0x11d_0000013d, 254, 0x11e_0000013e, 255, 0x11f_0000013f, 256, 0x120_00000140,
            257, 0x121_00000141, 258, 0x122_00000142, 259, 0x123_00000143, 260, 0x124_00000144, 261, 0x125_00000145, 262, 0x126_00000146, 263, 0x127_00000147, 264, 0x128_00000148, 265, 0x129_00000149, 266, 0x12a_0000014a, 267, 0x12b_0000014b, 268, 0x12c_0000014c, 269, 0x12d_0000014d, 270, 0x12e_0000014e, 271, 0x12f_0000014f, 272, 0x130_00000150,
            273, 0x131_00000151, 274, 0x132_00000152, 275, 0x133_00000153, 276, 0x134_00000154, 277, 0x135_00000155, 278, 0x136_00000156, 279, 0x137_00000157, 280, 0x138_00000158, 281, 0x139_00000159, 282, 0x13a_0000015a, 283, 0x13b_0000015b, 284, 0x13c_0000015c, 285, 0x13d_0000015d, 286, 0x13e_0000015e, 287, 0x13f_0000015f, 288, 0x140_00000160,
            289, 0x141_00000161, 290, 0x142_00000162, 291, 0x143_00000163, 292, 0x144_00000164, 293, 0x145_00000165, 294, 0x146_00000166, 295, 0x147_00000167, 296, 0x148_00000168, 297, 0x149_00000169, 298, 0x14a_0000016a, 299, 0x14b_0000016b, 300, 0x14c_0000016c, 301, 0x14d_0000016d, 302, 0x14e_0000016e, 303, 0x14f_0000016f, 304, 0x150_00000170,
            305, 0x151_00000171, 306, 0x152_00000172, 307, 0x153_00000173, 308, 0x154_00000174, 309, 0x155_00000175, 310, 0x156_00000176, 311, 0x157_00000177, 312, 0x158_00000178, 313, 0x159_00000179, 314, 0x15a_0000017a, 315, 0x15b_0000017b, 316, 0x15c_0000017c, 317, 0x15d_0000017d, 318, 0x15e_0000017e, 319, 0x15f_0000017f, 320, 0x160_00000180,
            321, 0x161_00000181, 322, 0x162_00000182, 323, 0x163_00000183, 324, 0x164_00000184, 325, 0x165_00000185, 326, 0x166_00000186, 327, 0x167_00000187, 328, 0x168_00000188, 329, 0x169_00000189, 330, 0x16a_0000018a, 331, 0x16b_0000018b, 332, 0x16c_0000018c, 333, 0x16d_0000018d, 334, 0x16e_0000018e, 335, 0x16f_0000018f, 336, 0x170_00000190,
            337, 0x171_00000191, 338, 0x172_00000192, 339, 0x173_00000193, 340, 0x174_00000194, 341, 0x175_00000195, 342, 0x176_00000196, 343, 0x177_00000197, 344, 0x178_00000198, 345, 0x179_00000199, 346, 0x17a_0000019a, 347, 0x17b_0000019b, 348, 0x17c_0000019c, 349, 0x17d_0000019d, 350, 0x17e_0000019e, 351, 0x17f_0000019f, 352, 0x180_000001a0,
            353, 0x181_000001a1, 354, 0x182_000001a2, 355, 0x183_000001a3, 356, 0x184_000001a4, 357, 0x185_000001a5, 358, 0x186_000001a6, 359, 0x187_000001a7, 360, 0x188_000001a8, 361, 0x189_000001a9, 362, 0x18a_000001aa, 363, 0x18b_000001ab, 364, 0x18c_000001ac, 365, 0x18d_000001ad, 366, 0x18e_000001ae, 367, 0x18f_000001af, 368, 0x190_000001b0,
            369, 0x191_000001b1, 370, 0x192_000001b2, 371, 0x193_000001b3, 372, 0x194_000001b4, 373, 0x195_000001b5, 374, 0x196_000001b6, 375, 0x197_000001b7, 376, 0x198_000001b8, 377, 0x199_000001b9, 378, 0x19a_000001ba, 379, 0x19b_000001bb, 380, 0x19c_000001bc, 381, 0x19d_000001bd, 382, 0x19e_000001be, 383, 0x19f_000001bf, 384, 0x1a0_000001c0,
            385, 0x1a1_000001c1, 386, 0x1a2_000001c2, 387, 0x1a3_000001c3, 388, 0x1a4_000001c4, 389, 0x1a5_000001c5, 390, 0x1a6_000001c6, 391, 0x1a7_000001c7, 392, 0x1a8_000001c8, 393, 0x1a9_000001c9, 394, 0x1aa_000001ca, 395, 0x1ab_000001cb, 396, 0x1ac_000001cc, 397, 0x1ad_000001cd, 398, 0x1ae_000001ce, 399, 0x1af_000001cf, 400, 0x1b0_000001d0,
            401, 0x1b1_000001d1, 402, 0x1b2_000001d2, 403, 0x1b3_000001d3, 404, 0x1b4_000001d4, 405, 0x1b5_000001d5, 406, 0x1b6_000001d6, 407, 0x1b7_000001d7, 408, 0x1b8_000001d8, 409, 0x1b9_000001d9, 410, 0x1ba_000001da, 411, 0x1bb_000001db, 412, 0x1bc_000001dc, 413, 0x1bd_000001dd, 414, 0x1be_000001de, 415, 0x1bf_000001df, 416, 0x1c0_000001e0,
            417, 0x1c1_000001e1, 418, 0x1c2_000001e2, 419, 0x1c3_000001e3, 420, 0x1c4_000001e4, 421, 0x1c5_000001e5, 422, 0x1c6_000001e6, 423, 0x1c7_000001e7, 424, 0x1c8_000001e8, 425, 0x1c9_000001e9, 426, 0x1ca_000001ea, 427, 0x1cb_000001eb, 428, 0x1cc_000001ec, 429, 0x1cd_000001ed, 430, 0x1ce_000001ee, 431, 0x1cf_000001ef, 432, 0x1d0_000001f0,
            433, 0x1d1_000001f1, 434, 0x1d2_000001f2, 435, 0x1d3_000001f3, 436, 0x1d4_000001f4, 437, 0x1d5_000001f5, 438, 0x1d6_000001f6, 439, 0x1d7_000001f7, 440, 0x1d8_000001f8, 441, 0x1d9_000001f9, 442, 0x1da_000001fa, 443, 0x1db_000001fb, 444, 0x1dc_000001fc, 445, 0x1dd_000001fd, 446, 0x1de_000001fe, 447, 0x1df_000001ff, 448, 0x1e0_00000200,
            449, 0x1e1_00000201, 450, 0x1e2_00000202, 451, 0x1e3_00000203, 452, 0x1e4_00000204, 453, 0x1e5_00000205, 454, 0x1e6_00000206, 455, 0x1e7_00000207, 456, 0x1e8_00000208, 457, 0x1e9_00000209, 458, 0x1ea_0000020a, 459, 0x1eb_0000020b, 460, 0x1ec_0000020c, 461, 0x1ed_0000020d, 462, 0x1ee_0000020e, 463, 0x1ef_0000020f, 464, 0x1f0_00000210,
            465, 0x1f1_00000211, 466, 0x1f2_00000212, 467, 0x1f3_00000213, 468, 0x1f4_00000214, 469, 0x1f5_00000215, 470, 0x1f6_00000216, 471, 0x1f7_00000217, 472, 0x1f8_00000218, 473, 0x1f9_00000219, 474, 0x1fa_0000021a, 475, 0x1fb_0000021b, 476, 0x1fc_0000021c, 477, 0x1fd_0000021d, 478, 0x1fe_0000021e, 479, 0x1ff_0000021f, 480, 0x200_00000220,
            481, 0x201_00000221, 482, 0x202_00000222, 483, 0x203_00000223, 484, 0x204_00000224, 485, 0x205_00000225, 486, 0x206_00000226, 487, 0x207_00000227, 488, 0x208_00000228, 489, 0x209_00000229, 490, 0x20a_0000022a, 491, 0x20b_0000022b, 492, 0x20c_0000022c, 493, 0x20d_0000022d, 494, 0x20e_0000022e, 495, 0x20f_0000022f, 496, 0x210_00000230,
            497, 0x211_00000231, 498, 0x212_00000232, 499, 0x213_00000233, 500, 0x214_00000234, 501, 0x215_00000235, 502, 0x216_00000236, 503, 0x217_00000237, 504, 0x218_00000238, 505, 0x219_00000239, 506, 0x21a_0000023a, 507, 0x21b_0000023b, 508, 0x21c_0000023c, 509, 0x21d_0000023d, 510, 0x21e_0000023e, 511, 0x21f_0000023f, 512, 0x220_00000240,
            513, 0x221_00000241, 514, 0x222_00000242, 515, 0x223_00000243, 516, 0x224_00000244, 517, 0x225_00000245, 518, 0x226_00000246, 519, 0x227_00000247, 520, 0x228_00000248, 521, 0x229_00000249, 522, 0x22a_0000024a, 523, 0x22b_0000024b, 524, 0x22c_0000024c, 525, 0x22d_0000024d, 526, 0x22e_0000024e, 527, 0x22f_0000024f, 528, 0x230_00000250,
            529, 0x231_00000251, 530, 0x232_00000252, 531, 0x233_00000253, 532, 0x234_00000254, 533, 0x235_00000255, 534, 0x236_00000256, 535, 0x237_00000257, 536, 0x238_00000258, 537, 0x239_00000259, 538, 0x23a_0000025a, 539, 0x23b_0000025b, 540, 0x23c_0000025c, 541, 0x23d_0000025d, 542, 0x23e_0000025e, 543, 0x23f_0000025f, 544, 0x240_00000260,
            545, 0x241_00000261, 546, 0x242_00000262, 547, 0x243_00000263, 548, 0x244_00000264, 549, 0x245_00000265, 550, 0x246_00000266, 551, 0x247_00000267, 552, 0x248_00000268, 553, 0x249_00000269, 554, 0x24a_0000026a, 555, 0x24b_0000026b, 556, 0x24c_0000026c, 557, 0x24d_0000026d, 558, 0x24e_0000026e, 559, 0x24f_0000026f, 560, 0x250_00000270,
            561, 0x251_00000271, 562, 0x252_00000272, 563, 0x253_00000273, 564, 0x254_00000274, 565, 0x255_00000275, 566, 0x256_00000276, 567, 0x257_00000277, 568, 0x258_00000278, 569, 0x259_00000279, 570, 0x25a_0000027a, 571, 0x25b_0000027b, 572, 0x25c_0000027c, 573, 0x25d_0000027d, 574, 0x25e_0000027e, 575, 0x25f_0000027f, 576, 0x260_00000280,
            577, 0x261_00000281, 578, 0x262_00000282, 579, 0x263_00000283, 580, 0x264_00000284, 581, 0x265_00000285, 582, 0x266_00000286, 583, 0x267_00000287, 584, 0x268_00000288, 585, 0x269_00000289, 586, 0x26a_0000028a, 587, 0x26b_0000028b, 588, 0x26c_0000028c, 589, 0x26d_0000028d, 590, 0x26e_0000028e, 591, 0x26f_0000028f, 592, 0x270_00000290,
            593, 0x271_00000291, 594, 0x272_00000292, 595, 0x273_00000293, 596, 0x274_00000294, 597, 0x275_00000295, 598, 0x276_00000296, 599, 0x277_00000297, 600, 0x278_00000298, 601, 0x279_00000299,
        ]);
        Ok(())
    }
}
