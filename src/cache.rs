use crate::api::ChatMessagePayload;
use home::home_dir;
use serde::{Deserialize, Serialize};
use std::{env, error::Error, fs, io, path::PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cache {
    pub last_vqd: String,
    pub messages: Vec<ChatMessagePayload>,
}

impl Default for Cache {
    fn default() -> Self {
        Self {
            last_vqd: "".into(),
            messages: Vec::new(),
        }
    }
}

impl Cache {
    pub fn get_path<T: From<String>>() -> T {
        match env::var("DDG_AI_CHAT_CACHE_PATH") {
            Ok(v) => v,
            Err(_) => match home_dir() {
                Some(home) => home
                    .join(".cache/ddg-ai-chat")
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
        match env::var("DDG_AI_CHAT_CACHE_FILENAME") {
            Ok(v) => v,
            Err(_) => "cache.json".into(),
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

    pub fn load() -> Result<Self, Box<dyn Error>> {
        let path: PathBuf = Self::get_path();
        Self::ensure_dir_exists()?;

        let file_path = path.join(Self::get_file_name::<PathBuf>());
        if !file_path.is_file() {
            let def = Self::default();
            def.save()?;
            Ok(def)
        } else {
            let file = fs::read_to_string(file_path)?;
            Ok(serde_json::from_str(&file)?)
        }
    }

    pub fn save(&self) -> Result<(), Box<dyn Error>> {
        let path: PathBuf = Self::get_path();
        Self::ensure_dir_exists()?;

        let file_path = path.join(Self::get_file_name::<PathBuf>());
        fs::write(file_path, serde_json::to_string_pretty(self)?)?;
        Ok(())
    }

    pub fn remove(&self) -> Result<(), Box<dyn Error>> {
        let path: PathBuf = Self::get_path();
        fs::remove_file(path.join(Self::get_file_name::<PathBuf>()))?;
        Ok(())
    }

    pub fn get_messages(&self) -> &Vec<ChatMessagePayload> {
        &self.messages
    }

    pub fn set_messages(
        &mut self,
        messages: Vec<ChatMessagePayload>,
    ) -> Result<(), Box<dyn Error>> {
        self.messages = messages;
        self.save()?;
        Ok(())
    }

    pub fn set_last_vqd<T: Into<String>>(&mut self, vqd: T) -> Result<(), Box<dyn Error>> {
        self.last_vqd = vqd.into();
        self.save()?;
        Ok(())
    }

    pub fn get_last_vqd<'a, T: From<&'a String>>(&'a self) -> Option<T> {
        if &self.last_vqd == "" {
            None
        } else {
            Some((&self.last_vqd).into())
        }
    }
}
