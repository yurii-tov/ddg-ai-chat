use std::process::exit;

use reqwest::Client;

use clap::Parser;

use crate::api::{get_res, simulate_browser_reqs};
use crate::{cache::Cache, config::Config, config::HINT_AVAILABLE};

mod api;
mod cache;
mod config;

pub const GREEN: &str = "\x1b[1;32m";
pub const RED: &str = "\x1b[1;31m";
pub const BLUE: &str = "\x1b[34m";
pub const WARN: &str = "\x1b[33m";
pub const RESET: &str = "\x1b[0m";

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, required = false, help = "Remove Chat history file")]
    pub remove_cache: bool,
    #[arg(long, required = false, help = "Print current 'model' setting")]
    pub print_model: bool,
    #[arg(long, required = false, help = "Set AI model")]
    pub set_model: Option<String>,
    #[arg(
        long,
        required = false,
        help = "Use given AI model for current session"
    )]
    pub model: Option<String>,
    #[arg(long, required = false, help = "List models available")]
    pub list_models: bool,
    #[arg(long, required = false, help = "Don't use cache")]
    pub no_cache: bool,
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

    if let Some(model) = args.model {
        match model.parse() {
            Ok(m) => {
                config.model = m;
            }
            Err(_) => {
                eprintln!("{RED}Invalid model name: {model}{RESET}");
                exit(2);
            }
        }
    }

    if args.remove_cache {
        match cache.remove() {
            Ok(_) => {}
            Err(err) => {
                eprintln!("{RED}Error removing cache: {err}{RESET}");
                exit(2);
            }
        }
        eprintln!("{GREEN}Cache removed{RESET}");
        exit(0);
    }

    if args.print_model {
        println!("{}", config.model);
        exit(0);
    }

    if args.list_models {
        println!("{}", HINT_AVAILABLE);
        exit(0);
    }

    if args.no_cache {
        config.no_cache = true;
    }

    if let Some(model) = args.set_model {
        match model.parse() {
            Ok(m) => {
                config.model = m;
                config.save().expect("Error saving config");
                exit(0);
            }
            Err(_) => {
                eprintln!("{RED}Invalid model name: {model}{RESET}");
                exit(2);
            }
        }
    }

    println!("{GREEN}Contacting DuckDuckGo chat AI...{RESET}");

    let cli = Client::new();
    simulate_browser_reqs(&cli).await.unwrap();

    get_res(&cli, query, &mut cache, &config).await;
}
