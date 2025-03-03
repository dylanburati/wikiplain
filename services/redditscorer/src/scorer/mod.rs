use std::{
    fs::{File, OpenOptions},
    io::{BufRead, BufReader, BufWriter, Write},
    ops::Add,
    path::PathBuf,
    result::Result as StdResult,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use anyhow::{anyhow, Result};
use caches::{Cache, LRUCache};
use crossbeam::channel::{bounded, unbounded, Receiver, Sender};
use itertools::Itertools;
use nalgebra::{DMatrix, DVector};
use pyo3::prelude::*;
use rayon::prelude::*;
use redis::Commands;
use rusqlite::Connection;
use rustc_hash::{FxHashMap, FxHashSet};
use serde::Deserialize;
use zstd::stream::Decoder;

use crate::pos_tagger::{self, AveragedPerceptron, PENN_TAG_NNP, PENN_TAG_NNPS};

mod domain;

struct SpanList {
    sentence: Vec<String>,
    max_span_widths: Vec<usize>,
    min_span_ends: Vec<usize>,

    pos: usize,
    ww: usize,
}

impl SpanList {
    pub fn new(sentence: Vec<String>, max_span_map: &FxHashMap<String, usize>) -> Self {
        #[allow(non_snake_case)]
        let W = sentence.len();
        let mut max_span_widths = vec![0; W];
        for (word_index, word) in sentence.iter().enumerate() {
            let Some(width) = max_span_map.get(word) else {
                continue;
            };
            let width = usize::min(*width, W - word_index);
            for i in 0..width {
                max_span_widths[word_index + i] = max_span_widths[word_index + i].max(width - i)
            }
        }
        let min_span_ends = (1..=W).into_iter().collect();
        let pos = 0;
        let ww = *max_span_widths.get(0).unwrap_or(&0);

        Self {
            sentence,
            max_span_widths,
            min_span_ends,
            pos,
            ww,
        }
    }

    pub fn all<'a>(&'a self) -> impl Iterator<Item = String> + 'a {
        let sentence = &self.sentence;
        self.max_span_widths
            .iter()
            .enumerate()
            .flat_map(move |(i, max_width)| {
                (1..=*max_width).map(move |w| {
                    let slice = &sentence[i..i + w];
                    slice.join(" ")
                })
            })
    }

    /// Returns the position, width, and content of the next span. The position and
    /// width are guaranteed to be safe for `self.mark_found`
    pub fn next(&mut self) -> Option<(usize, usize, String)> {
        if self.pos >= self.sentence.len() {
            return None;
        }

        while self.pos + self.ww < self.min_span_ends[self.pos] {
            self.pos += 1;
            if self.pos >= self.sentence.len() {
                return None;
            }
            self.ww = self.max_span_widths[self.pos];
        }

        let ww = self.ww;
        let slice = &self.sentence[self.pos..self.pos + self.ww];
        self.ww -= 1;
        Some((self.pos, ww, slice.join(" ")))
    }

    /// Instructs `next` to skip any spans contained by the given (pos, width).
    /// Panics if `pos+width > len(sentence)`
    pub fn mark_found(&mut self, pos: usize, width: usize) {
        for i in 0..width {
            self.min_span_ends[pos + i] = self.min_span_ends[pos + i].min(pos + width)
        }
    }
}

const N_PRODUCERS: usize = 3;

type ChannelPair<T> = (Sender<T>, Receiver<T>);

struct ReaderContext {
    file: File,
    db_path: String,
    task_id: String,
}

#[derive(Clone)]
struct WorkerContext {
    file_path: PathBuf,
    db_path: String,
    redis_uri: String,
    num_articles: u32,
    output_dir: String,
    pos_model: Arc<Mutex<AveragedPerceptron>>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct ProgressMsg {
    task_id: String,
    titles: usize,
    bytes: u64,
}

#[derive(Deserialize)]
struct SubmissionMeta {
    #[serde(default)]
    url: Option<String>,
    #[serde(default)]
    title: Option<String>,
}

fn produce_titles(
    reader_ctx: ReaderContext,
    titles_push: Sender<Vec<String>>,
    progress_push: Arc<Sender<ProgressMsg>>,
) -> Result<()> {
    let sqlconn = Connection::open(&reader_ctx.db_path)?;
    let mut top_domains_stmt = sqlconn.prepare("SELECT k FROM top_domains")?;
    let mut top_domains_iter = top_domains_stmt.query_map((), |row| {
        let k: String = row.get(0)?;
        Ok(k)
    })?;
    let domains = top_domains_iter.try_fold(FxHashSet::default(), |mut acc, res| -> Result<_> {
        let k = res?;
        acc.insert(k);
        Ok(acc)
    })?;

    let stream = BufReader::with_capacity(256 * 1024, reader_ctx.file);
    let mut stream = Decoder::new(stream)?;
    stream.window_log_max(31)?;
    let stream = BufReader::new(stream);
    let mut dedup_cache: LRUCache<String, (), _> =
        LRUCache::new(64 * 1024).expect("can not create LRU cache");

    let title_batch_size = 250;
    let mut title_batch = Vec::with_capacity(title_batch_size);
    let mut titles_sent = 0;
    for lines_lazy in stream.split(b'\n').chunks(1024).into_iter() {
        let mut lines = lines_lazy.collect::<StdResult<Vec<_>, _>>()?;
        let submission_meta_results: Vec<StdResult<SubmissionMeta, _>> = lines
            .par_iter_mut()
            .map(|json| simd_json::from_slice(&mut json[..]))
            .collect();
        let submission_metas = submission_meta_results.into_iter().try_fold(
            Vec::with_capacity(1024),
            |mut acc, res| -> Result<_> {
                let meta = res?;
                if let SubmissionMeta {
                    url: Some(u),
                    title: Some(t),
                } = meta
                {
                    acc.push((u, t));
                }
                Ok(acc)
            },
        )?;

        for (url, title) in submission_metas {
            let url_without_scheme = url
                .strip_prefix("https://")
                .or_else(|| url.strip_prefix("http://"))
                .unwrap_or(&url);
            let (full_domain, path) = match url_without_scheme.split_once('/') {
                Some((domain, rest)) => {
                    let path_and_query = rest.split('#').next().unwrap();
                    let path = path_and_query.split('?').next().unwrap();
                    (domain, path)
                }
                None => (url_without_scheme, ""),
            };
            let chars_iter =
                full_domain
                    .char_indices()
                    .rev()
                    .scan(full_domain.len(), |idx_after, (idx, c)| {
                        let idx_after_copy = *idx_after;
                        *idx_after = idx;
                        Some((idx_after_copy, c))
                    });
            let mut dot_iter = chars_iter.filter(|(_, c)| *c == '.').skip(1);
            let domain = match dot_iter.next() {
                Some((i, _)) => {
                    let mut cut = i;
                    if domain::is_second_level_domain(&full_domain[i..]) {
                        cut = dot_iter.next().map_or(0, |pair| pair.0);
                    }
                    &full_domain[cut..]
                }
                None => full_domain,
            };
            if !domains.contains(domain) {
                continue;
            }
            let dedup_key = full_domain.to_string() + "/" + path;
            if let caches::PutResult::Update(_) = dedup_cache.put(dedup_key, ()) {
                continue;
            }

            title_batch.push(title.to_string());
            if title_batch.len() >= title_batch_size {
                titles_sent += title_batch.len();
                progress_push.send(ProgressMsg {
                    task_id: reader_ctx.task_id.clone(),
                    titles: titles_sent,
                    bytes: 0,
                })?;

                let mut send_batch = Vec::with_capacity(title_batch_size);
                std::mem::swap(&mut title_batch, &mut send_batch);
                titles_push.send(send_batch)?;
            }
        }
    }
    if title_batch.len() > 0 {
        titles_sent += title_batch.len();
        progress_push.send(ProgressMsg {
            task_id: reader_ctx.task_id.clone(),
            titles: titles_sent,
            bytes: 0,
        })?;
        titles_push.send(title_batch)?;
    }

    Ok(())
}

#[derive(FromPyObject)]
struct SpacyToken {
    norm_: String,
    is_left_punct: bool,
    is_right_punct: bool,
    is_space: bool,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct WeightEntry {
    node_id: u32,
    weight: f32,
}

fn consume_titles(
    worker_ctx: WorkerContext,
    titles_pull: Arc<Receiver<Vec<String>>>,
) -> Result<()> {
    let mut output_path = PathBuf::from(worker_ctx.output_dir);
    output_path.push(worker_ctx.file_path.file_name().unwrap());
    output_path.set_extension("npy");

    let client = redis::Client::open(worker_ctx.redis_uri)?;
    let mut rconn = client.get_connection()?;

    let sqlconn = Connection::open(&worker_ctx.db_path)?;
    rusqlite::vtab::array::load_module(&sqlconn)?;
    let mut max_span_map_stmt = sqlconn.prepare("SELECT k, v FROM max_span_map")?;
    let mut max_span_map_iter = max_span_map_stmt.query_map((), |row| {
        let k: String = row.get(0)?;
        let v: usize = row.get(1)?;
        Ok((k, v))
    })?;
    let max_span_map =
        max_span_map_iter.try_fold(FxHashMap::default(), |mut acc, res| -> Result<_> {
            let (k, v) = res?;
            acc.insert(k, v);
            Ok(acc)
        })?;
    let mut relevance: Vec<f32> = vec![0.0; worker_ctx.num_articles as usize];
    let nlp_cell: Py<PyAny> = Python::with_gil(|py| {
        let english_module = PyModule::import(py, "spacy.lang.en")?;
        let nlp = english_module.getattr("English")?.call0()?;
        nlp.extract()
    })?;

    for titles in titles_pull.iter() {
        let t0 = Instant::now();
        let token_lists = Python::with_gil(|py| {
            let nlp = nlp_cell.as_ref(py);
            titles
                .iter()
                .map(|title| {
                    let token_pylist = nlp.getattr("tokenizer")?.call1((title,))?;
                    let token_list: Vec<SpacyToken> = token_pylist.extract()?;
                    Ok(token_list)
                })
                .collect::<PyResult<Vec<_>>>()
        })
        .map_err(|e| anyhow!("python error: {:?}", e))?;
        let dur0 = t0.elapsed();

        let span_lists: Vec<_> = token_lists
            .into_iter()
            .map(|tl| {
                let sent = tl
                    .into_iter()
                    .filter_map(|t| {
                        if !t.is_left_punct && !t.is_right_punct && !t.is_space {
                            Some(t.norm_)
                        } else {
                            None
                        }
                    })
                    .collect();
                SpanList::new(sent, &max_span_map)
            })
            .collect();

        let model = worker_ctx.pos_model.lock().unwrap();
        let dur1 = t0.elapsed();
        let tag_resp: Vec<_> = span_lists
            .iter()
            .map(|sl| pos_tagger::predict(&model, &sl.sentence))
            .collect();
        drop(model);
        let dur2 = t0.elapsed();

        let all_terms: FxHashSet<_> = span_lists.iter().flat_map(SpanList::all).collect();
        let all_terms_count = all_terms.len();
        let all_terms: Vec<_> = all_terms.into_iter().collect();
        let postings_raw: Vec<Vec<u8>> = rconn.mget(&all_terms)?;
        let postings: FxHashMap<_, _> = all_terms
            .into_iter()
            .zip(postings_raw.iter())
            .map(|(k, vb)| {
                if std::mem::size_of::<WeightEntry>() != 8 {
                    panic!("static assert failed")
                }
                let len = vb.len() / 8;
                if vb.len() % 8 != 0 {
                    panic!("redis corrupted: {}", k);
                }
                let v: &[WeightEntry] = unsafe {
                    let byte_ptr = vb.as_slice().as_ptr();
                    let entry_ptr: *const WeightEntry = std::mem::transmute(byte_ptr);
                    std::slice::from_raw_parts(entry_ptr, len)
                };
                (k, v)
            })
            .collect();
        let dur3 = t0.elapsed();

        for (mut sl, taginfo) in span_lists.into_iter().zip(tag_resp) {
            let mut scores = DMatrix::from_row_iterator(
                taginfo.len(),
                pos_tagger::C,
                taginfo.iter().flat_map(|e| e.scores.iter().copied()),
            );
            let observations: DVector<f32> =
                DVector::from_iterator(scores.nrows(), taginfo.iter().map(|e| e.observations))
                    .cast();
            let priors = DVector::from_iterator(
                scores.nrows(),
                sl.sentence.iter().map(|word| {
                    if word.chars().all(|c| c.is_alphabetic()) {
                        1.0
                    } else {
                        0.0
                    }
                }),
            );
            for mut row in scores.row_iter_mut() {
                row.add_scalar_mut(-row.max());
            }
            scores *= 0.5;
            scores = scores.map(f32::exp);
            for mut row in scores.row_iter_mut() {
                row /= row.sum();
            }
            let mut score = scores.column(PENN_TAG_NNP).clone_owned();
            score.zip_apply(&scores.column(PENN_TAG_NNPS), |x, y| *x = x.max(y));
            let denominator = score.add_scalar(20.0);
            score =
                (score.component_mul(&observations) + priors * 20.0).component_div(&denominator);

            while let Some((pos, width, term)) = sl.next() {
                let Some(weights) = postings.get(&term) else {
                    continue;
                };
                sl.mark_found(pos, width);

                let noun_factor = score.rows(pos, width).mean();
                for WeightEntry { node_id, weight } in weights.iter().copied() {
                    relevance[node_id as usize] += weight * noun_factor;
                }
            }
        }

        eprintln!(
            "{} tok={} lock={} pos={} db={} math={} | qsize={}",
            worker_ctx
                .file_path
                .file_stem()
                .and_then(std::ffi::OsStr::to_str)
                .unwrap_or(""),
            dur0.as_nanos(),
            (dur1 - dur0).as_nanos(),
            (dur2 - dur1).as_nanos(),
            (dur3 - dur2).as_nanos(),
            (t0.elapsed() - dur3).as_nanos(),
            all_terms_count,
        )
    }

    let numpy_header_const = b"\x93NUMPY\x01\x00";
    let numpy_header_dyn_str = format!(
        "{{'descr': '<f4', 'fortran_order': False, 'shape': ({},), }}",
        relevance.len()
    );
    let numpy_header_used_len = numpy_header_const.len() + numpy_header_dyn_str.len() + 1;
    let numpy_header_total_len = numpy_header_used_len.next_multiple_of(64);
    let mut numpy_header = Vec::with_capacity(numpy_header_total_len);
    numpy_header.extend_from_slice(numpy_header_const);
    let numpy_header_len_tag =
        ((numpy_header_total_len - numpy_header_const.len()) as u16).to_le_bytes();
    numpy_header.extend_from_slice(&numpy_header_len_tag);
    numpy_header.extend_from_slice(numpy_header_dyn_str.as_bytes());
    numpy_header
        .extend(std::iter::repeat(b' ').take(numpy_header_total_len - numpy_header_used_len));
    numpy_header.push(b'\n');

    let outfile = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(output_path)?;
    let mut outfile = BufWriter::new(outfile);
    outfile.write_all(&numpy_header)?;
    for v in relevance {
        let vb = v.to_le_bytes();
        outfile.write_all(&vb)?;
    }

    Ok(())
}

fn run_scorer_for_file(
    reader_ctx: ReaderContext,
    worker_ctx: WorkerContext,
    progress_push: Arc<Sender<ProgressMsg>>,
) -> Result<()> {
    let (titles_push, titles_pull): ChannelPair<Vec<String>> = bounded(1);
    let titles_pull = Arc::new(titles_pull);

    std::thread::scope(|s| {
        let mut handles = vec![];
        handles.push(s.spawn(move || produce_titles(reader_ctx, titles_push, progress_push)));
        handles.push(s.spawn(move || consume_titles(worker_ctx, titles_pull)));

        let mut handles: Vec<_> = handles.into_iter().map(Some).collect();
        while !handles.is_empty() {
            for handle_box in handles.iter_mut() {
                let handle = handle_box.take().unwrap();
                if handle.is_finished() {
                    match handle.join() {
                        Err(err) => {
                            eprintln!("{:?}", err);
                            return Err(anyhow!("thread crashed!"));
                        }
                        Ok(Err(err)) => return Err(err),
                        _ => {}
                    }
                } else {
                    let _ = handle_box.insert(handle);
                }
            }
            handles.retain(|handle_box| handle_box.is_some());

            std::thread::sleep(Duration::from_millis(100));
        }
        Ok(())
    })
}

pub fn run_scorer(
    input_filenames: Vec<PathBuf>,
    db_path: String,
    pos_model_path: String,
    num_articles: u32,
    output_dir: String,
) -> Result<()> {
    pyo3::prepare_freethreaded_python();

    let output_pattern = if output_dir.ends_with('/') {
        output_dir.clone() + "RS_*.npy"
    } else {
        output_dir.clone() + "/RS_*.npy"
    };
    let finished = glob::glob(&output_pattern)?.try_fold(
        FxHashSet::default(),
        |mut acc, res| -> Result<_> {
            let p = res?;
            if let Some(stem) = p.file_stem() {
                acc.insert(stem.to_owned());
            }
            Ok(acc)
        },
    )?;
    let input_filenames: Vec<_> = input_filenames
        .into_iter()
        .filter(|p| p.file_stem().is_some_and(|stem| !finished.contains(stem)))
        .collect();
    eprintln!("{} input files found", input_filenames.len());

    let pos_model_file = File::open(pos_model_path)?;
    let pos_model: AveragedPerceptron = serde_json::from_reader(BufReader::new(pos_model_file))?;
    let pos_model = Arc::new(Mutex::new(pos_model));

    let (progress_push, progress_pull): ChannelPair<ProgressMsg> = bounded(1);
    let (filename_push, filename_pull): ChannelPair<PathBuf> = unbounded();
    let filename_pull = Arc::new(filename_pull);
    let progress_push = Arc::new(progress_push);

    // TODO load domains from database
    std::thread::scope(|s| {
        let mut handles = vec![];
        // Producer thread
        handles.push(s.spawn(|| {
            for fname in input_filenames {
                filename_push.send(fname).unwrap();
            }
            drop(filename_push);
            Ok(())
        }));

        // Processors
        for _ in 0..N_PRODUCERS {
            let filename_pull_ref = Arc::clone(&filename_pull);
            let progress_push_ref = Arc::clone(&progress_push);
            let pos_model_ref = Arc::clone(&pos_model);
            let db_path_copy = db_path.clone();
            let output_dir_copy = output_dir.clone();
            handles.push(s.spawn(move || -> Result<()> {
                for filename in filename_pull_ref.iter() {
                    let file = File::open(&filename)?;
                    let reader_ctx = ReaderContext {
                        file,
                        db_path: db_path_copy.clone(),
                        task_id: filename
                            .file_stem()
                            .and_then(std::ffi::OsStr::to_str)
                            .unwrap_or("")
                            .to_owned(),
                    };
                    let worker_ctx = WorkerContext {
                        file_path: filename,
                        num_articles,
                        db_path: db_path_copy.clone(),
                        redis_uri: "redis://127.0.0.1/".to_string(),
                        output_dir: output_dir_copy.clone(),
                        pos_model: pos_model_ref.clone(),
                    };
                    run_scorer_for_file(reader_ctx, worker_ctx, progress_push_ref.clone())?;
                }

                Ok(())
            }))
        }

        // Close the channel, otherwise sink will never
        // exit the for-loop
        drop(progress_push);

        let mut handles: Vec<_> = handles.into_iter().map(Some).collect();
        while !handles.is_empty() {
            for handle_box in handles.iter_mut() {
                let handle = handle_box.take().unwrap();
                if handle.is_finished() {
                    match handle.join() {
                        Err(err) => {
                            eprintln!("{:?}", err);
                            return Err(anyhow!("thread crashed!"));
                        }
                        Ok(Err(err)) => return Err(err),
                        _ => {}
                    }
                } else {
                    let _ = handle_box.insert(handle);
                }
            }
            handles.retain(|handle_box| handle_box.is_some());

            let mut next_timeout = Some(Duration::from_millis(1000));
            let end_of_wait = Instant::now().add(Duration::from_millis(1000));
            while let Some(timeout) = next_timeout {
                match progress_pull.recv_timeout(timeout) {
                    Ok(msg) => {
                        eprintln!("{:?}", msg);
                    }
                    Err(err) => {
                        if err.is_timeout() || err.is_disconnected() {
                            break;
                        }
                        return Err(err.into());
                    }
                }

                next_timeout = end_of_wait.checked_duration_since(Instant::now());
            }
        }

        Ok(())
    })
}
