use anyhow::Result;
use clap::{arg, value_parser, Command};

mod pos_tagger;
mod scorer;

fn cli() -> Command {
    Command::new("redditscorer")
        .about("Estimate the relevance of Wikipedia pages on the English-speaking web.")
        .arg_required_else_help(true)
        .allow_external_subcommands(true)
        .subcommand(
            Command::new("train")
                .about("Trains the simple part-of-speech recognition model")
                .arg(arg!(ipattern: -i <globpattern> "Input files").required(true))
                .arg(arg!(opath: -o <path> "Path to the output file").required(true)),
        )
        .subcommand(
            Command::new("score")
                .about("Computes a score array from each input reddit-submissions dump")
                .arg(arg!(ipattern: -i <globpattern> "Input .zst files").required(true))
                .arg(arg!(db: --db <path> "Path to the tokenized enwiki sqlite db").required(true))
                .arg(arg!(pos: --pos <path> "Path to the trained part-of-speech recognition model").required(true))
                .arg(
                    arg!(--size <size> "Number of articles in enwiki parquet (== length of each score array)")
                        .value_parser(value_parser!(u32))
                        .required(true)
                )
                .arg(arg!(opath: -o <path> "Path to the output directory").required(true))
        )
}

fn main() -> Result<()> {
    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("train", sub_matches)) => {
            let input_pattern = sub_matches.get_one::<String>("ipattern").unwrap().as_str();
            let output_path = sub_matches.get_one::<String>("opath").unwrap().as_str();
            todo!("{} {}", input_pattern, output_path)
        }
        Some(("score", sub_matches)) => {
            let input_pattern = sub_matches.get_one::<String>("ipattern").unwrap().to_string();
            let db_path = sub_matches.get_one::<String>("db").unwrap().to_string();
            let pos_model_path = sub_matches.get_one::<String>("pos").unwrap().to_string();
            let num_articles = sub_matches.get_one::<u32>("size").unwrap();
            let output_dir = sub_matches.get_one::<String>("opath").unwrap().to_string();
            scorer::run_scorer(input_pattern, db_path, pos_model_path, *num_articles, output_dir)
        }
        _ => unreachable!(),
    }
}

