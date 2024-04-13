use std::env::temp_dir;

use clap::{arg, Command, value_parser};
use error_chain::error_chain;

mod creator;

error_chain! {
    foreign_links {
        IoError(std::io::Error);
        ParseIntError(std::num::ParseIntError);
        DeserError(nom::error::VerboseError);
    }
    errors {
        EntityError(m: &'static str) {
            description("wikidata entity error"),
            display("wikidata entity error: {}", m)
        }
    }
}

fn cli() -> Command {
    Command::new("wikidata_indexer2")
        .about("Deal with Wikidata .ldjson.gz files")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .allow_external_subcommands(true)
        .subcommand(
            Command::new("create")
                .about("Converts a raw Wikidata dump to a searchable one")
                .arg(arg!(ipath: -i <path> "Path to the input file").required(true))
                .arg(arg!(opath: -o <path> "Path to the output file").required(true))
        )
        .subcommand(
            Command::new("query")
                .about("Starts a server for querying a processed Wikidata dump")
                .arg(arg!(<path> "Path to the main processed .ldjson.gz file").required(true))
                .arg(
                    arg!(-p <port> "TCP port number for the server")
                    .value_parser(value_parser!(u16))
                    .default_value("12345")
                )
                .arg_required_else_help(true),
        )
}

fn main() -> Result<()> {
    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("create", sub_matches)) => {
            let input_path = sub_matches.get_one::<String>("ipath").unwrap().as_str();
            let output_path = sub_matches.get_one::<String>("opath").unwrap().as_str();
            let working_path = temp_dir().join(format!("{}.bin", std::time::UNIX_EPOCH.elapsed().unwrap().as_millis()));
            creator::create(input_path, output_path, working_path.as_os_str().to_str().unwrap())
        }
        Some(("query", sub_matches)) => {
            let path = sub_matches.get_one::<String>("path").unwrap().as_str();
            let port = sub_matches.get_one::<u16>("port").unwrap();
            todo!("{} {}", path, port)
        }
        _ => unreachable!()
    }
}
