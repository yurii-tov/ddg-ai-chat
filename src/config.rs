use std::{env, error::Error, fmt::Display, fs, io, path::PathBuf, str::FromStr};

use home::home_dir;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Model {
    Claude,
    GPT3,
    Llama,
    Mixtral,
}

pub const HINT_AVAILABLE: &str = "Claude, GPT3, Llama, Mixtral";

impl Display for Model {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Claude => write!(f, "claude-3-haiku-20240307"),
            Self::GPT3 => write!(f, "gpt-3.5-turbo-0125"),
            Self::Llama => write!(f, "meta-llama/Llama-3-70b-chat-hf"),
            Self::Mixtral => write!(f, "mistralai/Mixtral-8x7B-Instruct-v0.1")
        }
    }
}

impl FromStr for Model {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Claude" => Ok(Model::Claude),
            "GPT3" => Ok(Model::GPT3),
            "Llama" => Ok(Model::Llama),
            "Mixtral" => Ok(Model::Mixtral),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub model: Model,
    pub tos: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            model: Model::Claude,
            tos: false,
        }
    }
}

impl Config {
    pub fn get_path<T: From<String>>() -> T {
        match env::var("HEY_CONFIG_PATH") {
            Ok(v) => v,
            Err(_) => match home_dir() {
                Some(home) => home
                    .join(".config/hey")
                    .as_os_str()
                    .as_encoded_bytes()
                    .iter()
                    .map(|x| char::from(*x))
                    .collect::<String>(),
                None => panic!("Cannot detect your home directory!"),
            },
        }
        .into()
    }

    pub fn get_file_name<T: From<String>>() -> T {
        match env::var("HEY_CONFIG_FILENAME") {
            Ok(v) => v,
            Err(_) => "conf.toml".into(),
        }
        .into()
    }

    fn ensure_dir_exists() -> io::Result<()> {
        let path: PathBuf = Self::get_path();
        if !path.is_dir() {
            fs::create_dir_all(path)?
        }
        Ok(())
    }

    pub fn save(&self) -> Result<(), Box<dyn Error>> {
        let path = Self::get_path::<PathBuf>();
        Self::ensure_dir_exists()?;

        let file_path = path.join(Self::get_file_name::<String>());
        fs::write(file_path, toml::to_string_pretty(self)?)?;
        Ok(())
    }

    pub fn load() -> Result<Self, Box<dyn Error>> {
        let path = Self::get_path::<PathBuf>();

        let file_path = path.join(Self::get_file_name::<String>());
        if !file_path.is_file() {
            Ok(Self::default())
        } else {
            let conf: Config = toml::from_str(&fs::read_to_string(file_path)?)?;
            conf.model.to_string(); // so that it would panic if the config is outdated

            Ok(conf)
        }
    }
}
