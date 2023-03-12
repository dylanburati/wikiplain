use std::{
    cmp::{Eq, Ord, Ordering::*, PartialEq},
    fmt::{Debug, Display},
    fs::File,
    io,
    io::{BufRead, BufReader, Seek},
    path::PathBuf,
    str::from_utf8,
    sync::{Arc, Mutex},
};

use arrow::{
    array::*,
    datatypes::{Field, Schema},
    record_batch::RecordBatch,
};
use bzip2::read::*;
use crossbeam::channel::{bounded, Receiver, Sender};
use parquet::{
    arrow::ArrowWriter, basic::Compression, file::properties::WriterProperties,
    schema::types::ColumnPath,
};
use quick_xml::events::{BytesStart, BytesText, Event};

use crate::{ErrorKind, Result, ResultExt};

fn parse_index_bz2(index_path: PathBuf) -> Result<(Vec<u64>, Vec<usize>)> {
    let file = File::open(index_path)?;
    let decompressed_reader = BzDecoder::new(file);
    let buffered_reader = BufReader::with_capacity(131072, decompressed_reader);
    let lines = BufRead::lines(buffered_reader);
    let mut offsets: Vec<u64> = Vec::new();
    let mut counts: Vec<usize> = Vec::new();
    let mut current_offset: u64 = 0;
    let mut current_count: usize = 0;
    for line_res in lines {
        let line = line_res?;
        if let Some(delim) = line.find(':') {
            let off: u64 = line[0..delim].parse()?;
            match off.cmp(&current_offset) {
                Greater => {
                    if current_count > 0 {
                        offsets.push(current_offset);
                        counts.push(current_count);
                    }
                    current_offset = off;
                    current_count = 1;
                }
                Equal => {
                    current_count += 1;
                }
                Less => {
                    return Err(ErrorKind::InvalidIndexFile.into());
                }
            }
        } else {
            break;
        }
    }
    offsets.push(current_offset);
    counts.push(current_count);
    Ok((offsets, counts))
}

#[derive(PartialEq, Eq)]
enum Tag {
    Id,
    Ns,
    Page,
    Redirect,
    Revision,
    Text,
    Timestamp,
    Title,
    Other,
}

impl From<&[u8]> for Tag {
    fn from(value: &[u8]) -> Self {
        match value {
            b"id" => Tag::Id,
            b"ns" => Tag::Ns,
            b"page" => Tag::Page,
            b"redirect" => Tag::Redirect,
            b"revision" => Tag::Revision,
            b"text" => Tag::Text,
            b"timestamp" => Tag::Timestamp,
            b"title" => Tag::Title,
            _ => Tag::Other,
        }
    }
}

#[derive(Clone, Debug)]
struct WikiPage {
    id: i64,
    ns: i64, // could be smaller but 8 bytes keeps alignment
    title: String,
    redirect: Option<String>,
    timestamp: String,
    text: String,
}

impl Display for WikiPage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "<WikiPage ({}, {}, {:?}, {:?}, {}, {})>",
            self.id,
            self.ns,
            self.title,
            self.redirect,
            self.timestamp,
            self.text.len()
        ))
    }
}

struct WikiPagePartial {
    id: Option<i64>,
    ns: Option<i64>,
    title: Option<String>,
    redirect: Option<String>,
    timestamp: Option<String>,
    text: Option<String>,
}

impl WikiPagePartial {
    fn new() -> WikiPagePartial {
        Self {
            id: None,
            ns: None,
            title: None,
            redirect: None,
            timestamp: None,
            text: None,
        }
    }

    fn finish(&mut self) -> Option<WikiPage> {
        if let (Some(id), Some(ns), Some(title), redirect, Some(timestamp), Some(text)) = (
            self.id.take(),
            self.ns.take(),
            self.title.take(),
            self.redirect.take(),
            self.timestamp.take(),
            self.text.take(),
        ) {
            let wp = WikiPage {
                id,
                ns,
                title,
                redirect,
                timestamp,
                text,
            };
            Some(wp)
        } else {
            None
        }
    }

    fn on_start(&mut self, path: &[Tag], data: BytesStart) {
        if let [Tag::Page, Tag::Redirect] = path {
            data.attributes().for_each(|attr_res| {
                if let Ok(attr) = attr_res {
                    if attr.key.as_ref() == b"title" {
                        let target = attr.unescape_value();
                        self.redirect =
                            Some(from_utf8(target.unwrap().as_bytes()).unwrap().to_owned());
                    }
                }
            });
        }
    }

    fn on_end(&mut self, path: &[Tag]) -> Option<WikiPage> {
        match path {
            [Tag::Page, Tag::Revision, Tag::Text] => {
                self.text.get_or_insert("".to_owned());
                None
            }
            [Tag::Page] => self.finish(),
            _ => None,
        }
    }

    fn on_text(&mut self, path: &[Tag], data: BytesText) {
        match path {
            [Tag::Page, Tag::Id] => {
                self.id = Some(data.unescape().unwrap().parse().unwrap());
            }
            [Tag::Page, Tag::Ns] => {
                self.ns = Some(data.unescape().unwrap().parse().unwrap());
            }
            [Tag::Page, Tag::Title] => {
                self.title = Some(
                    from_utf8(data.unescape().unwrap().as_bytes())
                        .unwrap()
                        .to_owned(),
                );
            }
            [Tag::Page, Tag::Revision, Tag::Timestamp] => {
                self.timestamp = Some(
                    from_utf8(data.unescape().unwrap().as_bytes())
                        .unwrap()
                        .to_owned(),
                );
            }
            [Tag::Page, Tag::Revision, Tag::Text] => {
                self.text = Some(
                    from_utf8(data.unescape().unwrap().as_bytes())
                        .unwrap()
                        .to_owned(),
                );
            }
            _ => {}
        }
    }
}

fn load_xml_bz2_stream(dump_path: &str, offset: u64, count: usize) -> Result<Vec<WikiPage>> {
    let mut file = File::open(dump_path)?;
    file.seek(io::SeekFrom::Start(offset))?;
    let decompressed_reader = BzDecoder::new(file);
    let buffered_reader = BufReader::new(decompressed_reader);
    let mut xml_reader = quick_xml::Reader::from_reader(buffered_reader);
    let mut buffer: Vec<u8> = Vec::new();
    let mut partial: WikiPagePartial = WikiPagePartial::new();
    let mut result: Vec<WikiPage> = Vec::with_capacity(count);
    let mut elem_path: Vec<Tag> = Vec::new();
    loop {
        match xml_reader.read_event_into(&mut buffer) {
            Ok(event) => match event {
                Event::Start(start) => {
                    elem_path.push(Tag::from(start.name().as_ref()));
                    partial.on_start(&elem_path, start);
                }
                Event::End(end) => {
                    if let Some(complete) = partial.on_end(&elem_path) {
                        result.push(complete);
                    }
                    elem_path
                        .pop()
                        .filter(|e| *e == Tag::from(end.name().as_ref()))
                        .expect("Unbalanced");
                }
                Event::Empty(start) => {
                    elem_path.push(Tag::from(start.name().as_ref()));
                    partial.on_start(&elem_path, start);
                    partial.on_end(&elem_path);
                    elem_path.pop();
                }
                Event::Text(text) => {
                    partial.on_text(&elem_path, text);
                }
                Event::Comment(_) => {}
                Event::CData(_) => {}
                Event::Decl(_) => {}
                Event::PI(_) => {}
                Event::DocType(_) => {}
                Event::Eof => return Ok(result),
            },
            Err(error) => {
                eprintln!(
                    "XML parsing error {:?} @ (streamStart={}, streamPos={})",
                    error,
                    offset,
                    xml_reader.buffer_position()
                );
                return Err(error.into());
            }
        }
    }
}

fn arrow_arrays(pages: Vec<WikiPage>) -> [(&'static str, Arc<dyn Array>, bool); 6] {
    [
        (
            "id",
            Arc::new(Int64Array::from_iter(pages.iter().map(|wp| wp.id))) as _,
            false,
        ),
        (
            "ns",
            Arc::new(Int64Array::from_iter(pages.iter().map(|wp| wp.ns))) as _,
            false,
        ),
        (
            "title",
            Arc::new(StringArray::from_iter_values(
                pages.iter().cloned().map(|wp| wp.title),
            )) as _,
            false,
        ),
        (
            "redirect",
            Arc::new(StringArray::from_iter(
                pages.iter().cloned().map(|wp| wp.redirect),
            )) as _,
            true,
        ),
        (
            "timestamp",
            Arc::new(StringArray::from_iter_values(
                pages.iter().cloned().map(|wp| wp.timestamp),
            )) as _,
            false,
        ),
        (
            "text",
            Arc::new(StringArray::from_iter_values(
                pages.into_iter().map(|wp| wp.text),
            )) as _,
            false,
        ),
    ]
}

type ChannelPair<T> = (Sender<T>, Receiver<T>);

pub fn load(dump_path: &str, output_path: &str) -> Result<()> {
    let mut index_path = PathBuf::from(dump_path);
    let dump_fname = index_path
        .file_name()
        .ok_or("dump_path must be a normal file path")?
        .to_str()
        .ok_or("non-UTF8 file path???")?;
    if dump_fname != "pages-articles-multistream.xml.bz2" {
        return Err(ErrorKind::InvalidArgument("dump_path".to_owned()).into());
    }
    index_path.pop();
    index_path.push("pages-articles-multistream-index.txt.bz2");
    // let total_size = file_size(dump_path)?;
    // Length is not needed because BzDecoder stops correctly
    let file = File::create(output_path)?;
    let schema = Schema::new(vec![
        Field::new("id", arrow::datatypes::DataType::Int64, false),
        Field::new("ns", arrow::datatypes::DataType::Int64, false),
        Field::new("title", arrow::datatypes::DataType::Utf8, false),
        Field::new("redirect", arrow::datatypes::DataType::Utf8, true),
        Field::new("timestamp", arrow::datatypes::DataType::Utf8, false),
        Field::new("text", arrow::datatypes::DataType::Utf8, false),
    ]);
    let props = WriterProperties::builder()
        .set_column_compression(ColumnPath::from("text"), Compression::ZSTD)
        .build();
    let schema_ref = Arc::new(schema);
    let writer = ArrowWriter::try_new(file, schema_ref, Some(props))?;
    let mutex = Arc::new(Mutex::new(Some(writer)));

    let (offsets, counts) = parse_index_bz2(index_path)?;
    let total_count: usize = counts.iter().cloned().sum();
    eprintln!("Total articles: {}", total_count);

    let (src_push, src_pull) = bounded(1);
    let (sink_push, sink_pull): ChannelPair<Result<(u64, usize, usize)>> = bounded(1);

    let n_workers = 10;
    crossbeam::scope(|s| {
        // Producer thread
        s.spawn(|_| {
            for pair in offsets.into_iter().zip(counts.into_iter()) {
                src_push.send(pair).unwrap();
            }
            drop(src_push);
        });

        // Processors
        for _ in 0..n_workers {
            let mutex_ref = Arc::clone(&mutex);
            let (sink_push_ref, src_pull_ref) = (sink_push.clone(), src_pull.clone());
            s.spawn(move |_| {
                // Receive until channel closes
                for (offset, count) in src_pull_ref.iter() {
                    match load_xml_bz2_stream(dump_path, offset, count) {
                        Ok(pages) => {
                            let length = pages.len();
                            let arrays = arrow_arrays(pages);
                            let batch = RecordBatch::try_from_iter_with_nullable(arrays).unwrap();
                            let mut maybe_writer_ref = mutex_ref.lock().unwrap();
                            if let Some(writer_ref) = maybe_writer_ref.as_mut() {
                                let write_res = writer_ref
                                    .write(&batch)
                                    .and_then(|_| writer_ref.flush())
                                    .chain_err(|| "Worker could not write to parquet")
                                    .map(|_| (offset, count, length));
                                sink_push_ref.send(write_res).unwrap();
                            } else {
                                sink_push_ref.send(Err("ArrowWriter gone".into())).unwrap();
                            }
                        }
                        Err(error) => {
                            sink_push_ref.send(Err(error)).unwrap();
                        }
                    }
                }
            });
        }
        // Close the channel, otherwise sink will never
        // exit the for-loop
        drop(sink_push);

        // Sink
        let mut progress: usize = 0;
        for msg in sink_pull.iter() {
            match msg {
                Ok((offset, index_count, article_count)) => {
                    progress += article_count;
                    eprint!("\r\x1b[K{}", progress);
                    if article_count != index_count {
                        eprintln!("\n[WARN] got {} from streamStart={}", article_count, offset);
                    }
                }
                Err(error) => {
                    eprintln!("\n{}", error);
                }
            }
        }

        let writer_done = mutex
            .lock()
            .unwrap()
            .take()
            .expect("Could not close ArrowWriter (double-close?)");
        writer_done.close().expect("Could not close ArrowWriter")
    })
    .unwrap();

    Ok(())
}
