use std::collections::BinaryHeap;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::result::Result as StdResult;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};

use flate2::read::MultiGzDecoder;
use itertools::Itertools;
use memmap2::Mmap;
use rand::Rng;
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use serde::Deserialize;
use simd_json::prelude::ArrayTrait;
use zstd::bulk::Compressor;
use zstd::dict::EncoderDictionary;

use crate::querier::WikidataLeaf;
use crate::{Error, ErrorKind, Result};

#[derive(Deserialize)]
struct HasTitle {
    title: String,
}

#[derive(Deserialize)]
struct WikidataClaimValue {}

#[derive(Deserialize)]
struct WikidataSitelinks {
    enwiki: Option<HasTitle>,
}

#[derive(Deserialize)]
struct WikidataEntityJson {
    id: String,
    sitelinks: Option<WikidataSitelinks>,
    claims: FxHashMap<String, Vec<WikidataClaimValue>>,
}

struct WikidataEntity {
    id: u64,
    id_str: String,
    enwiki_title: Option<String>,
    claims: Vec<u64>,
}

const ENTITY_KIND_SHIFT: u64 = 56;
const ENTITY_NUM_MASK: u64 = (1 << 56) - 1;

impl TryFrom<WikidataEntityJson> for WikidataEntity {
    type Error = Error;

    fn try_from(value: WikidataEntityJson) -> StdResult<Self, Self::Error> {
        let WikidataEntityJson {
            id: id_str,
            sitelinks,
            claims: claims_raw,
        } = value;
        let (id_kind, id_num_str) = id_str.split_at(1);
        let id_num: u64 = id_num_str.parse()?;
        let id = id_num | ((id_kind.as_bytes()[0] as u64) << ENTITY_KIND_SHIFT);
        let enwiki_title = sitelinks.and_then(|obj| obj.enwiki).map(|obj| obj.title);
        let claims = claims_raw
            .into_keys()
            .map(|id_str| {
                let (id_kind, id_num_str) = id_str.split_at(1);
                let id_num: u64 = id_num_str.parse()?;
                Ok(id_num | ((id_kind.as_bytes()[0] as u64) << ENTITY_KIND_SHIFT))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(WikidataEntity {
            id,
            id_str,
            enwiki_title,
            claims,
        })
    }
}

pub const TREE_FANOUT: u64 = 256;
pub const CDICT_SIZE: usize = 128 * 1024;
pub const ENTITIES_PER_CDICT: usize = 100000;

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
        self.width.saturating_sub(2) * TREE_FANOUT
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
                Some(TREE_FANOUT as u32)
            };
            remaining -= 1;
            res
        })
    }

    fn from_total_children(total: u64) -> LevelDesc {
        let full_nodes = total / TREE_FANOUT;
        if full_nodes == 0 {
            return Self::new(1, u32::try_from(total).unwrap(), 0);
        }
        let remainder = (total % TREE_FANOUT) as u32;
        if remainder == 0 {
            let children2 = if full_nodes == 1 { 0 } else { TREE_FANOUT as u32 };
            return Self::new(full_nodes, TREE_FANOUT as u32, children2);
        }
        let children1 = (TREE_FANOUT as u32 + remainder) / 2;
        let children2 = TREE_FANOUT as u32 + remainder - children1;
        Self::new(full_nodes + 1, children1, children2)
    }

    /// Only valid for the root and intermediate levels; the leaf levels have size (node_size * sizeof(LeafData))
    fn byte_size(&self) -> u64 {
        // `is_leaf` false --> the nodes in this level look like:
        // [ct, ...keys (length: ct - 1), ...pointers (length: ct - 1)][ct, ...keys etc.]
        //  0,  8,                        8*ct,                        16*ct
        let mut result = 0;
        if self.width > 2 {
            result += (self.width - 2) * 16 * TREE_FANOUT;
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
    //             let full_nodes_width = self.width.saturating_sub(2) * 16 * TREE_FANOUT;
    //             let nonfull_node_width = (self.children2 as u64) * 16 * TREE_FANOUT;
    //             Some(full_nodes_width + nonfull_node_width)
    //         }
    //         _ => {
    //             // nodes before 2nd to last are all full
    //             let full_nodes_width = (node_index as u64) * 16 * TREE_FANOUT;
    //             Some(full_nodes_width)
    //         }
    //     }
    // }

    fn is_child_first_in_node(&self, child_index: u64) -> bool {
        let full_nodes_width = self.width.saturating_sub(2) * TREE_FANOUT;
        if child_index <= full_nodes_width {
            return child_index % TREE_FANOUT == 0;
        }
        child_index == full_nodes_width + self.children2 as u64
    }
}

struct Segmented {
    group_offsets: Vec<u64>,
    /// file_stg contains a multiple of STG_BLOCK_LIMIT WikidataLeaf records
    file_stg: File,
    /// ind_entries contains the unsorted WikidataLeaf records which are not in file_stg
    ind_entries: Vec<WikidataLeaf>,

    /// file_stg_secondary contains a multiple of STG_BLOCK_LIMIT (property id, entity id) pairs
    file_stg_secondary: File,
    /// prop_ind_entries contains the unsorted (property id, entity id) pairs which are not in file_stg
    prop_ind_entries: Vec<(u64, u64)>,
    prop_max_id: u64,
}

// a couple thousand less than 2^24, so the vector doesn't reserve past that capacity
const STG_BLOCK_LIMIT: usize = 16 * 1000 * 1024;

pub fn create(input_path: &str, output_path: &str, working_path: &str) -> Result<()> {
    let base = output_path.strip_suffix(".zst").unwrap_or(output_path);
    let output_dicts_path = base.to_string() + ".zdicts";
    let output_index_path = base.to_string() + ".index.bin";
    let output_en_index_path = base.to_string() + "-en.index.txt";
    let output_prop_index_path = base.to_string() + "-prop.index.bin";
    let working_path_secondary = working_path.to_string() + "2";

    let file_input = OpenOptions::new().read(true).open(input_path)?;
    let file_input2 = OpenOptions::new().read(true).open(input_path)?;
    let file_output = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_path)?;
    let file_dicts = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_dicts_path)?;
    let file_en = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_en_index_path)?;
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
        .open(&working_path_secondary)?;

    let (dict_consumer, dict_supplier) = sync_channel::<Result<Vec<u8>>>(4);
    std::thread::scope(|s| -> Result<()> {
        s.spawn(
            || match get_zstd_dicts(file_input2, dict_consumer.clone()) {
                Ok(_) => {}
                Err(e) => dict_consumer.send(Err(e)).unwrap(),
            },
        );

        let mut segmented = write_segmented(
            file_input,
            file_output,
            file_dicts,
            file_en,
            file_stg,
            file_stg_secondary,
            dict_supplier,
        )?;
        segmented.ind_entries.sort();
        let mut stg_entries = vec![segmented.ind_entries.as_slice()];
        let stg_bytes = unsafe { Mmap::map(&segmented.file_stg) }?;
        let mut stg_offset = 0;
        let stride = std::mem::size_of::<WikidataLeaf>();
        while stg_offset < stg_bytes.len() {
            let entry_chunk: &[WikidataLeaf] = unsafe {
                std::slice::from_raw_parts(
                    std::mem::transmute(stg_bytes.as_ptr().add(stg_offset)),
                    STG_BLOCK_LIMIT,
                )
            };
            stg_entries.push(entry_chunk);
            stg_offset += stride * STG_BLOCK_LIMIT;
        }
        let file_ind = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(output_index_path)?;
        let _ = write_index(stg_entries, segmented.group_offsets, file_ind)?;
        drop(stg_bytes);

        segmented.prop_ind_entries.sort();
        let mut stg_secondary_entries = vec![segmented.prop_ind_entries.as_slice()];
        let stg_secondary_bytes = unsafe { Mmap::map(&segmented.file_stg_secondary) }?;
        let mut stg_secondary_offset = 0;
        let stride = std::mem::size_of::<(u64, u64)>();
        while (stg_secondary_offset + stride * STG_BLOCK_LIMIT) <= stg_secondary_bytes.len() {
            let entry_chunk: &[(u64, u64)] = unsafe {
                std::slice::from_raw_parts(
                    std::mem::transmute(stg_secondary_bytes.as_ptr().add(stg_secondary_offset)),
                    STG_BLOCK_LIMIT,
                )
            };
            stg_secondary_entries.push(entry_chunk);
            stg_secondary_offset += stride * STG_BLOCK_LIMIT;
        }
        let file_prop_ind = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(output_prop_index_path)?;
        let _ = write_prop_index(stg_secondary_entries, segmented.prop_max_id, file_prop_ind)?;
        drop(stg_secondary_bytes);

        Ok(())
    })?;
    std::fs::remove_file(working_path)?;
    std::fs::remove_file(working_path_secondary)?;
    Ok(())
}

/// STEP 0: train the compressor and create a dictionary which will be shared for all items
fn get_zstd_dicts(file_input: File, dict_consumer: SyncSender<Result<Vec<u8>>>) -> Result<()> {
    let mut input = MultiGzDecoder::new(file_input);
    // Skip "[\n", then buffer starting from the first object / second line
    let mut array_start_buf = [0u8; 2];
    input.read_exact(&mut array_start_buf)?;
    let mut input = BufReader::new(input);
    let mut eof = false;
    let reservoir_size = 200;

    while !eof {
        let mut reservoir = Vec::new();
        for _ in 0..reservoir_size {
            let mut line = Vec::new();
            let got = input.read_until(b'\n', &mut line)?;
            if got == 0 {
                eof = true;
                break;
            }
            reservoir.push(line);
        }
        let mut line_num = reservoir_size;
        if !eof {
            let mut line = Vec::new();
            for _ in reservoir_size..ENTITIES_PER_CDICT {
                let got = input.read_until(b'\n', &mut line)?;
                if got == 0 {
                    eof = true;
                    break;
                }
                let i = rand::thread_rng().gen_range(0..=line_num);
                if i < reservoir_size {
                    std::mem::swap(&mut reservoir[i], &mut line);
                }
                line.clear();
                line_num += 1;
            }
        }

        let cdict_bytes = zstd::dict::from_samples(&reservoir, CDICT_SIZE)?;
        dict_consumer.send(Ok(cdict_bytes)).unwrap();
    }
    Ok(())
}

/// STEP 1: read .json.gz input array, rewrite as .ldjson.zst
///         in memory + working file, keep (group, entity) indices keyed by Wikidata id_num
///          (Q42 -> 0x5100_0000_0000_002a)
///         in memory, keep the start byte offset of each block
fn write_segmented(
    file_input: File,
    file_output: File,
    file_output_dicts: File,
    file_en: File,
    file_stg: File,
    file_stg_secondary: File,
    dict_supplier: Receiver<Result<Vec<u8>>>,
) -> Result<Segmented> {
    let mut input = MultiGzDecoder::new(file_input);
    let mut output_plain = BufWriter::with_capacity(131072, file_output);
    let mut output_dicts = BufWriter::with_capacity(131072, file_output_dicts);
    let mut output_en = BufWriter::with_capacity(131072, file_en);
    let mut file_stg = Some(file_stg);
    let mut file_stg_secondary = Some(file_stg_secondary);

    // Skip "[\n", then buffer starting from the first object / second line
    let mut array_start_buf = [0u8; 2];
    input.read_exact(&mut array_start_buf)?;
    let input = BufReader::new(input);

    let mut eof = false;
    let mut enwiki_count = 0;
    let mut ind_entries: Vec<WikidataLeaf> = vec![];
    let mut prop_ind_entries: Vec<(u64, u64)> = vec![];
    let mut staged_count: u64 = 0;
    let mut prop_staged_count: u64 = 0;
    let mut prop_max_id: u64 = 0;
    let mut entity_number: u64 = 0;
    let mut compressed_offset: u64 = 0;
    let mut group_offsets = vec![];
    let mut compressed_buf = Vec::with_capacity(1024 * 1024);

    const LINES_PER_CHUNK: usize = 200;
    assert_eq!(ENTITIES_PER_CDICT % LINES_PER_CHUNK, 0);

    let lines_lazy_iterable = input.split(b'\n').chunks(LINES_PER_CHUNK);
    let mut lines_lazy_iter = lines_lazy_iterable.into_iter();
    while !eof {
        let next_group_start = entity_number + ENTITIES_PER_CDICT as u64;
        let cdict_bytes = dict_supplier.recv().unwrap()?;
        let cdict_size = cdict_bytes.len() as u32;
        output_dicts.write_all(&cdict_size.to_le_bytes())?;
        output_dicts.write_all(&cdict_bytes)?;
        let cdict = EncoderDictionary::copy(&cdict_bytes, 5);
        group_offsets.push(compressed_offset);

        while entity_number < next_group_start {
            let Some(lines_lazy) = lines_lazy_iter.next() else {
                eof = true;
                break;
            };
            let lines = lines_lazy.collect::<StdResult<Vec<_>, _>>()?;
            let entity_results: Vec<_> = lines
                .par_iter()
                .filter(|line| line.starts_with(b"{"))
                .map(|line| -> Result<(&[u8], WikidataEntityJson)> {
                    let json_str = line.strip_suffix(b",").unwrap_or(&line[..]);
                    let ent = serde_json::from_slice(json_str)?;
                    Ok((json_str, ent))
                })
                .collect();
            let entities: Vec<(&[u8], WikidataEntity)> = entity_results.into_iter().try_fold(
                Vec::with_capacity(LINES_PER_CHUNK),
                |mut acc, res| -> Result<_> {
                    let (json, ent_raw) = res?;
                    let ent = ent_raw.try_into()?;
                    acc.push((json, ent));
                    Ok(acc)
                },
            )?;
            for (json_str, ent) in entities {
                if let Some(title) = ent.enwiki_title {
                    writeln!(output_en, "{}\t{}", title, ent.id_str)?;
                    enwiki_count += 1;
                }
                let mut enc = Compressor::with_prepared_dictionary(&cdict)?;
                let csize = enc.compress_to_buffer(json_str, &mut compressed_buf)?;
                output_plain.write_all(&compressed_buf)?;
                let csize_32 = csize
                    .try_into()
                    .map_err(|_| ErrorKind::EntityError("compressed size > 4GiB"))?;
                ind_entries.push(WikidataLeaf::new(ent.id, compressed_offset, csize_32));
                for prop_id in ent.claims.into_iter() {
                    prop_ind_entries.push((prop_id, ent.id));
                    prop_max_id = prop_max_id.max(prop_id);
                }
                compressed_offset += csize as u64;
                entity_number += 1;
                if entity_number % 1000 == 0 {
                    eprint!(
                        "\r\x1b[K{:>12}\t{:>12}\t{:>8}",
                        prop_staged_count + prop_ind_entries.len() as u64,
                        staged_count + ind_entries.len() as u64,
                        enwiki_count
                    );
                }
            }
        }
        if !eof && ind_entries.len() >= STG_BLOCK_LIMIT {
            let block = &mut ind_entries[0..STG_BLOCK_LIMIT];
            block.sort();

            let mut output_stg = BufWriter::with_capacity(131072, file_stg.take().unwrap());
            for item in ind_entries.drain(0..STG_BLOCK_LIMIT) {
                output_stg.write_all(&item.to_le_bytes())?;
            }
            staged_count += STG_BLOCK_LIMIT as u64;
            let _ = file_stg.insert(output_stg.into_inner().map_err(std::io::Error::from)?);
        }
        if !eof && prop_ind_entries.len() >= STG_BLOCK_LIMIT {
            let block = &mut prop_ind_entries[0..STG_BLOCK_LIMIT];
            block.sort();

            let mut output_stg_secondary =
                BufWriter::with_capacity(131072, file_stg_secondary.take().unwrap());
            for (k, eid) in prop_ind_entries.drain(0..STG_BLOCK_LIMIT) {
                output_stg_secondary.write_all(&k.to_le_bytes())?;
                output_stg_secondary.write_all(&eid.to_le_bytes())?;
            }
            prop_staged_count += STG_BLOCK_LIMIT as u64;
            let _ = file_stg_secondary.insert(
                output_stg_secondary
                    .into_inner()
                    .map_err(std::io::Error::from)?,
            );
        }
        // FIXME
        if eof && prop_ind_entries.len() > 0 {
            let mut output_stg_secondary =
                BufWriter::with_capacity(131072, file_stg_secondary.take().unwrap());
            for (k, eid) in prop_ind_entries.iter() {
                output_stg_secondary.write_all(&k.to_le_bytes())?;
                output_stg_secondary.write_all(&eid.to_le_bytes())?;
            }
            let _ = file_stg_secondary.insert(
                output_stg_secondary
                    .into_inner()
                    .map_err(std::io::Error::from)?,
            );
        }
    }
    output_en.flush()?;

    Ok(Segmented {
        group_offsets,
        file_stg: file_stg.unwrap(),
        ind_entries,
        file_stg_secondary: file_stg_secondary.unwrap(),
        prop_ind_entries,
        prop_max_id,
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
/// level N = leaves [(key, entity_start)]            | L-L-L-L # L-L-L-L # L-L- etc     # L etc
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
pub fn write_index<W: Write + Seek>(
    entries: Vec<&[WikidataLeaf]>,
    group_offsets: Vec<u64>,
    file: W,
) -> Result<W> {
    // STEP 2a: determine structure of tree (i.e. size of each level)
    let mut output = BufWriter::with_capacity(131072, file);

    let num_leaves = entries.iter().map(|chunk| chunk.len() as u64).sum();
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
    let n_groups = group_offsets.len();
    output.write_all(&u64::to_le_bytes(n_groups as u64))?;
    offset += 8;
    for v in group_offsets.iter() {
        output.write_all(&v.to_le_bytes())?;
        offset += 8;
    }
    let n_levels = level_descriptions.len();
    output.write_all(&u64::to_le_bytes(n_levels as u64))?;
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
    let mut readers: Vec<std::slice::Iter<WikidataLeaf>> =
        entries.iter().map(|chunk| chunk.into_iter()).collect();
    let mut merge_queue = BinaryHeap::new();
    for (i, reader) in readers.iter_mut().enumerate() {
        if let Some(item) = reader.next() {
            merge_queue.push((item, i));
        }
    }
    let mut ind_size = 0;
    let mut level_keys = vec![];
    let mut level_pointers = vec![];
    let leaf_level_desc = level_descriptions.last().unwrap();
    while !merge_queue.is_empty() {
        let (item, i) = merge_queue.pop().unwrap();
        if leaf_level_desc.is_child_first_in_node(ind_size) {
            level_keys.push(item.key());
            level_pointers.push(offset);
        }
        output.write_all(&item.to_le_bytes())?;
        offset += 20;
        ind_size += 1;

        if let Some(item) = readers[i].next() {
            merge_queue.push((item, i));
        }
    }

    // STEP 2d: propagate keys and pointers up the tree, one level at a time
    for level_id in (0 .. level_descriptions.len()-1).rev() {
        let ld = &level_descriptions[level_id];
        assert_eq!(level_keys.len(), ld.total_children() as usize);
        assert_eq!(level_keys.len(), level_pointers.len());
        let _ = output.seek(SeekFrom::Start(level_offsets[level_id]))?;
        offset = level_offsets[level_id];
        let mut j = 0;
        let mut next_level_keys = vec![];
        let mut next_level_pointers = vec![];
        for sz in ld.node_sizes() {
            next_level_keys.push(level_keys[j]);
            next_level_pointers.push(offset);
            output.write_all(&(sz as u64).to_le_bytes())?;
            for child_num in 1..(sz as usize) {
                output.write_all(&level_keys[j + child_num].to_le_bytes())?;
            }
            for child_num in 0..(sz as usize) {
                output.write_all(&level_pointers[j + child_num].to_le_bytes())?;
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

/// STEP 2: make propery-to-entity index
///
/// ```ascii
/// header  = len(props)
/// props   = [P0 = (entity_count, byte_offset), ..., P{one-past-last} = (0, byte_offset)]
///           where 2 <= W <= 256                       :'---------------------------------.
/// level _ = nodes [(width=W, [W-1] keys, [W] ptrs)] | N                                  N
///           where 128 <= W <= 256                     :'--------.---------.---[W times]# :
///                                                     :         :         :            # :
/// level N = leaves [(key, entity_start)]            | L-L-L-L # L-L-L-L # L-L- etc     # L etc
/// ```
pub fn write_prop_index<W: Write + Seek>(
    entries: Vec<&[(u64, u64)]>,
    prop_max_id: u64,
    file: W,
) -> Result<W> {
    let mut output = BufWriter::with_capacity(131072, file);
    let n_props = (prop_max_id & ENTITY_NUM_MASK) + 1;

    // STEP 3a: write header, then write a placeholder for the array of prop metadata
    let mut offset: u64 = 0;
    output.write_all(&n_props.to_le_bytes())?;
    offset += 8;
    let zeros = vec![0; 16 * (n_props + 1) as usize];
    output.write_all(&zeros)?;
    offset += zeros.len() as u64;

    // STEP 3b: merge sort the in-memory and staging (prop_id, entity_id) chunks to form the
    //           last (leaf) level of the B+tree.
    //          save the keys and offsets of leaves which begin a leaf block
    let mut readers: Vec<std::slice::Iter<(u64, u64)>> =
        entries.iter().map(|chunk| chunk.into_iter()).collect();
    let mut merge_queue = BinaryHeap::new();
    for (i, reader) in readers.iter_mut().enumerate() {
        if let Some(item) = reader.next() {
            merge_queue.push((item, i));
        }
    }
    let mut metadata = vec![];
    let mut entity_ids: Vec<u64> = vec![];
    let mut last_prop_id: u64 = 0;
    let mut prop_card: u64 = 0;
    while !merge_queue.is_empty() {
        let (item, i) = merge_queue.pop().unwrap();
        while last_prop_id < item.0 {
            metadata.push((prop_card, offset));
            prop_card = 0;
            if !entity_ids.is_empty() {
                offset += std::mem::size_of_val(&entity_ids[..]) as u64;
                for v in entity_ids.iter() {
                    output.write_all(&v.to_le_bytes())?;
                }
                entity_ids.clear();
            }
            last_prop_id += 1;
        }

        prop_card += 1;
        entity_ids.push(item.1);
        if let Some(item) = readers[i].next() {
            merge_queue.push((item, i));
        }
    }
    if last_prop_id != prop_max_id {
        return Err(ErrorKind::Msg(format!("want prop_max_id = {}, got {}", prop_max_id, last_prop_id)).into())
    }
    metadata.push((prop_card, offset));
    offset += std::mem::size_of_val(&entity_ids[..]) as u64;
    for v in &entity_ids {
        output.write_all(&v.to_le_bytes())?;
    }
    metadata.push((0, offset));

    let _ = output.seek(SeekFrom::Start(8))?;
    for (card, offset) in metadata {
        output.write_all(&card.to_le_bytes())?;
        output.write_all(&offset.to_le_bytes())?;
    }
    output
        .into_inner()
        .map_err(|e| std::io::Error::from(e).into())
}

#[cfg(test)]
mod tests {
    use crate::querier::WikidataLeaf;

    struct IndexReader<'a> {
        data: &'a [u8],
    }

    impl<'a> IndexReader<'a> {
        fn new(data: &'a [u8]) -> Self {
            Self { data }
        }

        fn le_to_u64(src: &[u8]) -> u64 {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(src);
            u64::from_le_bytes(buf)
        }

        fn le_to_leafdata(src: &[u8]) -> (u64, u64, u32) {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(&src[..8]);
            let a = u64::from_le_bytes(buf);
            buf.copy_from_slice(&src[8..16]);
            let b = u64::from_le_bytes(buf);
            let mut buf = [0u8; 4];
            buf.copy_from_slice(&src[16..]);
            (a, b, u32::from_le_bytes(buf))
        }

        fn n_groups(&self) -> u64 {
            Self::le_to_u64(&self.data[..8])
        }

        fn group_offsets(&self, n_groups: u64) -> Vec<u64> {
            self.data[8..]
                .chunks_exact(8)
                .map(Self::le_to_u64)
                .take(n_groups as usize)
                .collect()
        }

        fn root_node_pos(&self, n_groups: u64) -> u64 {
            // n_groups, group_offsets, n_levels
            8 + 8 * n_groups + 8
        }

        fn node_level(&self, pos: u64, n_nodes: u64) -> (Vec<u64>, u64, u64) {
            let mut base = pos as usize;
            let mut res = vec![];
            let mut n_ptrs = 0;
            for _ in 0..n_nodes {
                let w = Self::le_to_u64(&self.data[base..base + 8]);
                n_ptrs += w;
                let w = w as usize;
                res.extend(
                    self.data[base..]
                        .chunks_exact(8)
                        .map(Self::le_to_u64)
                        .take(2 * w),
                );
                base += 16 * w;
            }
            (res, base as u64, n_ptrs)
        }

        fn leaf_level(&self, pos: u64) -> Vec<(u64, u64, u32)> {
            let base = pos as usize;
            self.data[base..]
                .chunks_exact(20)
                .map(Self::le_to_leafdata)
                .collect()
        }
    }

    #[test]
    #[rustfmt::skip]
    fn write_index_one_level() -> super::Result<()> {
        let ind_entries: Vec<_> = (1u64..=20).map(|k| WikidataLeaf::new(k, k + 64, 1u32)).collect();
        let group_offsets = vec![0, 1000, 2000, 3000];
        let actual = super::write_index(vec![&ind_entries], group_offsets, std::io::Cursor::new(Vec::new()))?.into_inner();
        let ir = IndexReader::new(&actual);
        let n_groups = ir.n_groups();
        assert_eq!(n_groups, 4);
        assert_eq!(ir.group_offsets(n_groups), vec![0, 1000, 2000, 3000]);
        assert_eq!(ir.leaf_level(ir.root_node_pos(n_groups)), vec![
            (1, 0x41, 1),
            (2, 0x42, 1),
            (3, 0x43, 1),
            (4, 0x44, 1),
            (5, 0x45, 1),
            (6, 0x46, 1),
            (7, 0x47, 1),
            (8, 0x48, 1),
            (9, 0x49, 1),
            (10, 0x4a, 1),
            (11, 0x4b, 1),
            (12, 0x4c, 1),
            (13, 0x4d, 1),
            (14, 0x4e, 1),
            (15, 0x4f, 1),
            (16, 0x50, 1),
            (17, 0x51, 1),
            (18, 0x52, 1),
            (19, 0x53, 1),
            (20, 0x54, 1),
        ]);
        Ok(())
    }

    #[test]
    #[rustfmt::skip]
    fn write_index_two_levels() -> super::Result<()> {
        let ind_entries: Vec<_> = (1u64..=601).map(|k| WikidataLeaf::new(k, k + 64, 1u32)).collect();
        let group_offsets = vec![0, 1000, 2000, 3000];
        let actual = super::write_index(vec![&ind_entries], group_offsets, std::io::Cursor::new(Vec::new()))?.into_inner();
        let ir = IndexReader::new(&actual);
        let n_groups = ir.n_groups();
        assert_eq!(n_groups, 4);
        assert_eq!(ir.group_offsets(n_groups), vec![0, 1000, 2000, 3000]);
        let (node_level, leaf_level_pos, _) = ir.node_level(ir.root_node_pos(n_groups), 1);
        assert_eq!(node_level, vec![3, 257, 430, 0x60, (0x60 + 256*20), (0x60 + 429*20)]);
        assert_eq!(ir.leaf_level(leaf_level_pos), vec![
            (  1,  0x41, 1), (  2,  0x42, 1), (  3,  0x43, 1), (  4,  0x44, 1), (  5,  0x45, 1), (  6,  0x46, 1), (  7,  0x47, 1), (  8,  0x48, 1), (  9,  0x49, 1), ( 10,  0x4a, 1), ( 11,  0x4b, 1), ( 12,  0x4c, 1), ( 13,  0x4d, 1), ( 14,  0x4e, 1), ( 15,  0x4f, 1), ( 16,  0x50, 1),
            ( 17,  0x51, 1), ( 18,  0x52, 1), ( 19,  0x53, 1), ( 20,  0x54, 1), ( 21,  0x55, 1), ( 22,  0x56, 1), ( 23,  0x57, 1), ( 24,  0x58, 1), ( 25,  0x59, 1), ( 26,  0x5a, 1), ( 27,  0x5b, 1), ( 28,  0x5c, 1), ( 29,  0x5d, 1), ( 30,  0x5e, 1), ( 31,  0x5f, 1), ( 32,  0x60, 1),
            ( 33,  0x61, 1), ( 34,  0x62, 1), ( 35,  0x63, 1), ( 36,  0x64, 1), ( 37,  0x65, 1), ( 38,  0x66, 1), ( 39,  0x67, 1), ( 40,  0x68, 1), ( 41,  0x69, 1), ( 42,  0x6a, 1), ( 43,  0x6b, 1), ( 44,  0x6c, 1), ( 45,  0x6d, 1), ( 46,  0x6e, 1), ( 47,  0x6f, 1), ( 48,  0x70, 1),
            ( 49,  0x71, 1), ( 50,  0x72, 1), ( 51,  0x73, 1), ( 52,  0x74, 1), ( 53,  0x75, 1), ( 54,  0x76, 1), ( 55,  0x77, 1), ( 56,  0x78, 1), ( 57,  0x79, 1), ( 58,  0x7a, 1), ( 59,  0x7b, 1), ( 60,  0x7c, 1), ( 61,  0x7d, 1), ( 62,  0x7e, 1), ( 63,  0x7f, 1), ( 64,  0x80, 1),
            ( 65,  0x81, 1), ( 66,  0x82, 1), ( 67,  0x83, 1), ( 68,  0x84, 1), ( 69,  0x85, 1), ( 70,  0x86, 1), ( 71,  0x87, 1), ( 72,  0x88, 1), ( 73,  0x89, 1), ( 74,  0x8a, 1), ( 75,  0x8b, 1), ( 76,  0x8c, 1), ( 77,  0x8d, 1), ( 78,  0x8e, 1), ( 79,  0x8f, 1), ( 80,  0x90, 1),
            ( 81,  0x91, 1), ( 82,  0x92, 1), ( 83,  0x93, 1), ( 84,  0x94, 1), ( 85,  0x95, 1), ( 86,  0x96, 1), ( 87,  0x97, 1), ( 88,  0x98, 1), ( 89,  0x99, 1), ( 90,  0x9a, 1), ( 91,  0x9b, 1), ( 92,  0x9c, 1), ( 93,  0x9d, 1), ( 94,  0x9e, 1), ( 95,  0x9f, 1), ( 96,  0xa0, 1),
            ( 97,  0xa1, 1), ( 98,  0xa2, 1), ( 99,  0xa3, 1), (100,  0xa4, 1), (101,  0xa5, 1), (102,  0xa6, 1), (103,  0xa7, 1), (104,  0xa8, 1), (105,  0xa9, 1), (106,  0xaa, 1), (107,  0xab, 1), (108,  0xac, 1), (109,  0xad, 1), (110,  0xae, 1), (111,  0xaf, 1), (112,  0xb0, 1),
            (113,  0xb1, 1), (114,  0xb2, 1), (115,  0xb3, 1), (116,  0xb4, 1), (117,  0xb5, 1), (118,  0xb6, 1), (119,  0xb7, 1), (120,  0xb8, 1), (121,  0xb9, 1), (122,  0xba, 1), (123,  0xbb, 1), (124,  0xbc, 1), (125,  0xbd, 1), (126,  0xbe, 1), (127,  0xbf, 1), (128,  0xc0, 1),
            (129,  0xc1, 1), (130,  0xc2, 1), (131,  0xc3, 1), (132,  0xc4, 1), (133,  0xc5, 1), (134,  0xc6, 1), (135,  0xc7, 1), (136,  0xc8, 1), (137,  0xc9, 1), (138,  0xca, 1), (139,  0xcb, 1), (140,  0xcc, 1), (141,  0xcd, 1), (142,  0xce, 1), (143,  0xcf, 1), (144,  0xd0, 1),
            (145,  0xd1, 1), (146,  0xd2, 1), (147,  0xd3, 1), (148,  0xd4, 1), (149,  0xd5, 1), (150,  0xd6, 1), (151,  0xd7, 1), (152,  0xd8, 1), (153,  0xd9, 1), (154,  0xda, 1), (155,  0xdb, 1), (156,  0xdc, 1), (157,  0xdd, 1), (158,  0xde, 1), (159,  0xdf, 1), (160,  0xe0, 1),
            (161,  0xe1, 1), (162,  0xe2, 1), (163,  0xe3, 1), (164,  0xe4, 1), (165,  0xe5, 1), (166,  0xe6, 1), (167,  0xe7, 1), (168,  0xe8, 1), (169,  0xe9, 1), (170,  0xea, 1), (171,  0xeb, 1), (172,  0xec, 1), (173,  0xed, 1), (174,  0xee, 1), (175,  0xef, 1), (176,  0xf0, 1),
            (177,  0xf1, 1), (178,  0xf2, 1), (179,  0xf3, 1), (180,  0xf4, 1), (181,  0xf5, 1), (182,  0xf6, 1), (183,  0xf7, 1), (184,  0xf8, 1), (185,  0xf9, 1), (186,  0xfa, 1), (187,  0xfb, 1), (188,  0xfc, 1), (189,  0xfd, 1), (190,  0xfe, 1), (191,  0xff, 1), (192, 0x100, 1),
            (193, 0x101, 1), (194, 0x102, 1), (195, 0x103, 1), (196, 0x104, 1), (197, 0x105, 1), (198, 0x106, 1), (199, 0x107, 1), (200, 0x108, 1), (201, 0x109, 1), (202, 0x10a, 1), (203, 0x10b, 1), (204, 0x10c, 1), (205, 0x10d, 1), (206, 0x10e, 1), (207, 0x10f, 1), (208, 0x110, 1),
            (209, 0x111, 1), (210, 0x112, 1), (211, 0x113, 1), (212, 0x114, 1), (213, 0x115, 1), (214, 0x116, 1), (215, 0x117, 1), (216, 0x118, 1), (217, 0x119, 1), (218, 0x11a, 1), (219, 0x11b, 1), (220, 0x11c, 1), (221, 0x11d, 1), (222, 0x11e, 1), (223, 0x11f, 1), (224, 0x120, 1),
            (225, 0x121, 1), (226, 0x122, 1), (227, 0x123, 1), (228, 0x124, 1), (229, 0x125, 1), (230, 0x126, 1), (231, 0x127, 1), (232, 0x128, 1), (233, 0x129, 1), (234, 0x12a, 1), (235, 0x12b, 1), (236, 0x12c, 1), (237, 0x12d, 1), (238, 0x12e, 1), (239, 0x12f, 1), (240, 0x130, 1),
            (241, 0x131, 1), (242, 0x132, 1), (243, 0x133, 1), (244, 0x134, 1), (245, 0x135, 1), (246, 0x136, 1), (247, 0x137, 1), (248, 0x138, 1), (249, 0x139, 1), (250, 0x13a, 1), (251, 0x13b, 1), (252, 0x13c, 1), (253, 0x13d, 1), (254, 0x13e, 1), (255, 0x13f, 1), (256, 0x140, 1),
            (257, 0x141, 1), (258, 0x142, 1), (259, 0x143, 1), (260, 0x144, 1), (261, 0x145, 1), (262, 0x146, 1), (263, 0x147, 1), (264, 0x148, 1), (265, 0x149, 1), (266, 0x14a, 1), (267, 0x14b, 1), (268, 0x14c, 1), (269, 0x14d, 1), (270, 0x14e, 1), (271, 0x14f, 1), (272, 0x150, 1),
            (273, 0x151, 1), (274, 0x152, 1), (275, 0x153, 1), (276, 0x154, 1), (277, 0x155, 1), (278, 0x156, 1), (279, 0x157, 1), (280, 0x158, 1), (281, 0x159, 1), (282, 0x15a, 1), (283, 0x15b, 1), (284, 0x15c, 1), (285, 0x15d, 1), (286, 0x15e, 1), (287, 0x15f, 1), (288, 0x160, 1),
            (289, 0x161, 1), (290, 0x162, 1), (291, 0x163, 1), (292, 0x164, 1), (293, 0x165, 1), (294, 0x166, 1), (295, 0x167, 1), (296, 0x168, 1), (297, 0x169, 1), (298, 0x16a, 1), (299, 0x16b, 1), (300, 0x16c, 1), (301, 0x16d, 1), (302, 0x16e, 1), (303, 0x16f, 1), (304, 0x170, 1),
            (305, 0x171, 1), (306, 0x172, 1), (307, 0x173, 1), (308, 0x174, 1), (309, 0x175, 1), (310, 0x176, 1), (311, 0x177, 1), (312, 0x178, 1), (313, 0x179, 1), (314, 0x17a, 1), (315, 0x17b, 1), (316, 0x17c, 1), (317, 0x17d, 1), (318, 0x17e, 1), (319, 0x17f, 1), (320, 0x180, 1),
            (321, 0x181, 1), (322, 0x182, 1), (323, 0x183, 1), (324, 0x184, 1), (325, 0x185, 1), (326, 0x186, 1), (327, 0x187, 1), (328, 0x188, 1), (329, 0x189, 1), (330, 0x18a, 1), (331, 0x18b, 1), (332, 0x18c, 1), (333, 0x18d, 1), (334, 0x18e, 1), (335, 0x18f, 1), (336, 0x190, 1),
            (337, 0x191, 1), (338, 0x192, 1), (339, 0x193, 1), (340, 0x194, 1), (341, 0x195, 1), (342, 0x196, 1), (343, 0x197, 1), (344, 0x198, 1), (345, 0x199, 1), (346, 0x19a, 1), (347, 0x19b, 1), (348, 0x19c, 1), (349, 0x19d, 1), (350, 0x19e, 1), (351, 0x19f, 1), (352, 0x1a0, 1),
            (353, 0x1a1, 1), (354, 0x1a2, 1), (355, 0x1a3, 1), (356, 0x1a4, 1), (357, 0x1a5, 1), (358, 0x1a6, 1), (359, 0x1a7, 1), (360, 0x1a8, 1), (361, 0x1a9, 1), (362, 0x1aa, 1), (363, 0x1ab, 1), (364, 0x1ac, 1), (365, 0x1ad, 1), (366, 0x1ae, 1), (367, 0x1af, 1), (368, 0x1b0, 1),
            (369, 0x1b1, 1), (370, 0x1b2, 1), (371, 0x1b3, 1), (372, 0x1b4, 1), (373, 0x1b5, 1), (374, 0x1b6, 1), (375, 0x1b7, 1), (376, 0x1b8, 1), (377, 0x1b9, 1), (378, 0x1ba, 1), (379, 0x1bb, 1), (380, 0x1bc, 1), (381, 0x1bd, 1), (382, 0x1be, 1), (383, 0x1bf, 1), (384, 0x1c0, 1),
            (385, 0x1c1, 1), (386, 0x1c2, 1), (387, 0x1c3, 1), (388, 0x1c4, 1), (389, 0x1c5, 1), (390, 0x1c6, 1), (391, 0x1c7, 1), (392, 0x1c8, 1), (393, 0x1c9, 1), (394, 0x1ca, 1), (395, 0x1cb, 1), (396, 0x1cc, 1), (397, 0x1cd, 1), (398, 0x1ce, 1), (399, 0x1cf, 1), (400, 0x1d0, 1),
            (401, 0x1d1, 1), (402, 0x1d2, 1), (403, 0x1d3, 1), (404, 0x1d4, 1), (405, 0x1d5, 1), (406, 0x1d6, 1), (407, 0x1d7, 1), (408, 0x1d8, 1), (409, 0x1d9, 1), (410, 0x1da, 1), (411, 0x1db, 1), (412, 0x1dc, 1), (413, 0x1dd, 1), (414, 0x1de, 1), (415, 0x1df, 1), (416, 0x1e0, 1),
            (417, 0x1e1, 1), (418, 0x1e2, 1), (419, 0x1e3, 1), (420, 0x1e4, 1), (421, 0x1e5, 1), (422, 0x1e6, 1), (423, 0x1e7, 1), (424, 0x1e8, 1), (425, 0x1e9, 1), (426, 0x1ea, 1), (427, 0x1eb, 1), (428, 0x1ec, 1), (429, 0x1ed, 1), (430, 0x1ee, 1), (431, 0x1ef, 1), (432, 0x1f0, 1),
            (433, 0x1f1, 1), (434, 0x1f2, 1), (435, 0x1f3, 1), (436, 0x1f4, 1), (437, 0x1f5, 1), (438, 0x1f6, 1), (439, 0x1f7, 1), (440, 0x1f8, 1), (441, 0x1f9, 1), (442, 0x1fa, 1), (443, 0x1fb, 1), (444, 0x1fc, 1), (445, 0x1fd, 1), (446, 0x1fe, 1), (447, 0x1ff, 1), (448, 0x200, 1),
            (449, 0x201, 1), (450, 0x202, 1), (451, 0x203, 1), (452, 0x204, 1), (453, 0x205, 1), (454, 0x206, 1), (455, 0x207, 1), (456, 0x208, 1), (457, 0x209, 1), (458, 0x20a, 1), (459, 0x20b, 1), (460, 0x20c, 1), (461, 0x20d, 1), (462, 0x20e, 1), (463, 0x20f, 1), (464, 0x210, 1),
            (465, 0x211, 1), (466, 0x212, 1), (467, 0x213, 1), (468, 0x214, 1), (469, 0x215, 1), (470, 0x216, 1), (471, 0x217, 1), (472, 0x218, 1), (473, 0x219, 1), (474, 0x21a, 1), (475, 0x21b, 1), (476, 0x21c, 1), (477, 0x21d, 1), (478, 0x21e, 1), (479, 0x21f, 1), (480, 0x220, 1),
            (481, 0x221, 1), (482, 0x222, 1), (483, 0x223, 1), (484, 0x224, 1), (485, 0x225, 1), (486, 0x226, 1), (487, 0x227, 1), (488, 0x228, 1), (489, 0x229, 1), (490, 0x22a, 1), (491, 0x22b, 1), (492, 0x22c, 1), (493, 0x22d, 1), (494, 0x22e, 1), (495, 0x22f, 1), (496, 0x230, 1),
            (497, 0x231, 1), (498, 0x232, 1), (499, 0x233, 1), (500, 0x234, 1), (501, 0x235, 1), (502, 0x236, 1), (503, 0x237, 1), (504, 0x238, 1), (505, 0x239, 1), (506, 0x23a, 1), (507, 0x23b, 1), (508, 0x23c, 1), (509, 0x23d, 1), (510, 0x23e, 1), (511, 0x23f, 1), (512, 0x240, 1),
            (513, 0x241, 1), (514, 0x242, 1), (515, 0x243, 1), (516, 0x244, 1), (517, 0x245, 1), (518, 0x246, 1), (519, 0x247, 1), (520, 0x248, 1), (521, 0x249, 1), (522, 0x24a, 1), (523, 0x24b, 1), (524, 0x24c, 1), (525, 0x24d, 1), (526, 0x24e, 1), (527, 0x24f, 1), (528, 0x250, 1),
            (529, 0x251, 1), (530, 0x252, 1), (531, 0x253, 1), (532, 0x254, 1), (533, 0x255, 1), (534, 0x256, 1), (535, 0x257, 1), (536, 0x258, 1), (537, 0x259, 1), (538, 0x25a, 1), (539, 0x25b, 1), (540, 0x25c, 1), (541, 0x25d, 1), (542, 0x25e, 1), (543, 0x25f, 1), (544, 0x260, 1),
            (545, 0x261, 1), (546, 0x262, 1), (547, 0x263, 1), (548, 0x264, 1), (549, 0x265, 1), (550, 0x266, 1), (551, 0x267, 1), (552, 0x268, 1), (553, 0x269, 1), (554, 0x26a, 1), (555, 0x26b, 1), (556, 0x26c, 1), (557, 0x26d, 1), (558, 0x26e, 1), (559, 0x26f, 1), (560, 0x270, 1),
            (561, 0x271, 1), (562, 0x272, 1), (563, 0x273, 1), (564, 0x274, 1), (565, 0x275, 1), (566, 0x276, 1), (567, 0x277, 1), (568, 0x278, 1), (569, 0x279, 1), (570, 0x27a, 1), (571, 0x27b, 1), (572, 0x27c, 1), (573, 0x27d, 1), (574, 0x27e, 1), (575, 0x27f, 1), (576, 0x280, 1),
            (577, 0x281, 1), (578, 0x282, 1), (579, 0x283, 1), (580, 0x284, 1), (581, 0x285, 1), (582, 0x286, 1), (583, 0x287, 1), (584, 0x288, 1), (585, 0x289, 1), (586, 0x28a, 1), (587, 0x28b, 1), (588, 0x28c, 1), (589, 0x28d, 1), (590, 0x28e, 1), (591, 0x28f, 1), (592, 0x290, 1),
            (593, 0x291, 1), (594, 0x292, 1), (595, 0x293, 1), (596, 0x294, 1), (597, 0x295, 1), (598, 0x296, 1), (599, 0x297, 1), (600, 0x298, 1), (601, 0x299, 1),
        ]);
        Ok(())
    }
}
