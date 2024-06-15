use std::{path::PathBuf, process::exit};

use reqwest::Client;

use clap::Parser;

use crate::{cache::Cache, config::Config, config::Model};
use crate::api::{get_res, simulate_browser_reqs};

mod cache;
mod config;
mod api;

pub const GREEN: &str = "\x1b[1;32m";
pub const RED:   &str = "\x1b[1;31m";
pub const BLUE:  &str = "\x1b[34m";
pub const WARN:  &str = "\x1b[33m";
pub const RESET: &str = "\x1b[0m";

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, default_value = "false", required = false, help = "If you want to agree to the DuckDuckGo TOS")]
    pub agree_tos: bool,
    #[arg(long, required = false, help = "Remove Chat history file")]
    pub remove_cache: bool,
    #[arg(long, required = false, help = "Print current 'model' setting")]
    pub print_model: bool,
    #[arg(long, required = false, help = "Set AI model")]
    pub set_model: Option<String>,
    #[arg()]
    pub query: Vec<String>,
}

#[tokio::main]
async fn main() {
    femme::start();

    let args = Args::parse();
    let query = args.query.join(" ");

    let mut cache = Cache::load().unwrap();
    let mut config = Config::load().unwrap();

    if args.agree_tos {
        if ! config.tos {
            println!("{GREEN}TOS accepted{RESET}");
        }
        config.tos = true;
        config.save().expect("Error saving config");
        exit(0);
    }

    if args.remove_cache {
        match cache.remove() {
            Ok(_) => {},
            Err(err) => {
                eprintln!("{RED}Error removing cache: {err}{RESET}");
                exit(2);
            }
        }
        eprintln!("{GREEN}Cache removed{RESET}");
        exit(0);
    }

    if args.print_model {
        println!("{}", config.model.to_string());
        exit(0);
    }

    if let Some(model) = args.set_model {
        let parsed: Result<Model, ()> = model.parse();
        match parsed {
            Ok(m) => {
                config.model = m;
                config.save().expect("Error saving config");
                exit(0);
            },
            Err(_) => {
                eprintln!("{RED}Invalid model name: {model}{RESET}");
                exit(2);
            }
        }
    }

    if ! config.tos {
        eprintln!("{RED}You need to agree to duckduckgo AI chat TOS to continue.{RESET}");
        eprintln!("{RED}Visit it on this URL: {RESET}{BLUE}https://duckduckgo.com/?q=duckduckgo+ai+chat&ia=chat{RESET}");
        eprintln!("Once you read it, pass --agree-tos parameter to agree.");
        eprintln!("{WARN}Note: if you want to, modify `tos` parameter in {}{RESET}", Config::get_path::<PathBuf>().join(Config::get_file_name::<String>()).display());
        exit(3);
    }

    println!("{GREEN}Contacting DuckDuckGo chat AI...{RESET}");

    let cli = Client::new();
    simulate_browser_reqs(&cli).await.unwrap();

    get_res(&cli, query, &mut cache, &config).await;

}
