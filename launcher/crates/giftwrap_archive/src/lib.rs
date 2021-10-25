use std::convert::TryInto;
use std::ffi::CString;
use std::fmt::{self, Display};
use std::fs::File;
use std::io::prelude::*;
use std::io::SeekFrom;
use std::iter::{self, Iterator};
use std::path::{Path, PathBuf};
use std::ptr;

use serde::Deserialize;

/// Giftwrap archive appended to a launcher program.
///
/// The layout of the has archive has four sections:
///
/// - `content`: a tar file containing the program to execute and all of its
///   dependencies (optionally compressed).
/// - `config`: a JSON entry containing for configuration on how to boot into
///   to the content's program.
/// - `tag`: the tag for the archive, typically a hash of the launcher, content,
///   and config.
/// - `trailer`: a binary trailer indiciating the Giftwrap magic and trailer
///   version and the config and content lengths.
///
/// # Trailer Binary Layout
///
/// Below shows a table with the byte ranges in the trailer with their meaning
/// with integers being big-endian:
///
/// | Byte Range | Type       | Description           |
/// |------------|------------|-----------------------|
/// | 12 - 15    | `[u8; 4]`  | magic (`b"GIFT"`)     |
/// | 11         | `u8`       | trailer version (`1`) |
/// | 10         | `u8`       | tag length            |
/// | 8 - 9      | `u16`      | config length         |
/// | 0 - 7      | `u64`      | content length        |
pub struct Archive<'a> {
    file: &'a File,
    config: Config,
    tag: String,
    dest: PathBuf,
    content_start: i64,
    content_length: u64,
}

impl<'a> Archive<'a> {
    pub fn from_file(file: &'a File) -> Result<Self> {
        let mut file = file;
        let mut offset = -(TRAILER_LENGTH as i64);

        let trailer = Self::read_trailer(&mut file, offset)?;

        offset -= trailer.tag_length as i64;

        let tag = Self::read_tag(&mut file, offset, trailer.tag_length as u64)?;

        offset -= trailer.config_length as i64;

        let config = Self::read_config(&mut file, offset, trailer.config_length as u64)?;

        offset -= trailer.content_length as i64;

        let version = if let Some(v) = &config.version {
            v.as_str()
        } else {
            tag.as_str()
        };

        let dest = Path::new(&config.cache_dir).join(version);

        let archive = Self {
            file,
            config,
            tag,
            dest,
            content_start: offset,
            content_length: trailer.content_length,
        };
        Ok(archive)
    }

    fn read_trailer<T: Read + Seek>(file: &mut T, offset: i64) -> Result<Trailer> {
        Self::seek(file, offset);
        Trailer::from_reader(file)
    }

    fn read_tag<T: Read + Seek>(file: &mut T, offset: i64, length: u64) -> Result<String> {
        Self::seek(file, offset);
        let mut tag = String::new();

        match file.take(length).read_to_string(&mut tag) {
            Ok(_) => Ok(tag.to_string()),
            Err(_) => Err(Error::InvalidTag(String::from(
                "failed to read archive tag",
            ))),
        }
    }

    fn read_config<T: Read + Seek>(file: &mut T, offset: i64, length: u64) -> Result<Config> {
        Self::seek(file, offset);
        Config::from_reader(file.take(length))
    }

    fn seek<T: Read + Seek>(file: &mut T, offset: i64) {
        file.seek(SeekFrom::End(offset)).unwrap();
    }

    pub fn unpack_content(&mut self) -> Result<()> {
        let completion_path = self.dest.as_path().join(".tag");

        if completion_path.is_file() {
            return Ok(());
        }

        if let Ok(mut f) = File::open(&completion_path) {
            let mut existing = String::new();

            f.read_to_string(&mut existing)
                .map_err(|e| Error::UnpackError(format!("failed to read tag file: {}", e)))?;

            if existing != self.tag {
                return Err(Error::UnpackError(
                    "found conflicting tag in tag file".to_string(),
                ));
            }
        }

        Self::seek(&mut self.file, self.content_start);

        let mut tar_reader = if self.config.compression == Some(Compression::Zstd) {
            unimplemented!()
        } else {
            self.file.take(self.content_length)
        };

        let mut tar_archive = tar::Archive::new(&mut tar_reader);

        tar_archive.unpack(&self.dest).map_err(|e| {
            let msg = format!("failed to unpack content: {}", e);
            Error::UnpackError(msg)
        })?;

        File::create(&completion_path)
            .map_err(|e| Error::UnpackError(format!("failed to create tag file: {}", e)))?
            .write_all(self.tag.as_bytes())
            .map_err(|e| Error::UnpackError(format!("failed to write to tag file: {}", e)))?;

        Ok(())
    }

    pub fn launch(&mut self, args: &[String]) -> ! {
        let path_buf = self.dest.join(self.config.entry_point.as_str());
        let path_string = path_buf.into_os_string().into_string().unwrap();
        let path = CString::new(path_string).unwrap();

        let args_c_strings: Vec<CString> = args
            .iter()
            .map(|s| CString::new(s.as_str()).unwrap())
            .collect();
        let args_ptrs = Self::exec_array(&path, &args_c_strings);

        unsafe {
            libc::execv(path.as_ptr(), args_ptrs.as_ptr());
        }

        panic!("execv: {}", errno::errno());
    }

    fn exec_array(path: &CString, args: &[CString]) -> Vec<*const libc::c_char> {
        iter::once(path.as_ptr())
            .chain(args.iter().map(|s| s.as_ptr()))
            .chain(iter::once(ptr::null()))
            .collect()
    }
}

/// Archive result.
pub type Result<T> = std::result::Result<T, Error>;

/// Archive error.
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    UnexpectedMagic,
    UnexpectedVersion,
    InvalidTag(String),
    InvalidConfig(String),
    UnpackError(String),
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let message = match self {
            Error::UnexpectedMagic => "unexpected magic in trailer",
            Error::UnexpectedVersion => "unexpected version in trailer",
            Error::InvalidTag(msg) => msg,
            Error::InvalidConfig(msg) => msg,
            Error::UnpackError(msg) => msg,
        };

        formatter.write_str(message)
    }
}

impl std::error::Error for Error {}

const TRAILER_MAGIC: &[u8; 4] = b"GIFT";
const TRAILER_VERSION: u8 = 1;
const TRAILER_LENGTH: usize = 16;

struct Trailer {
    pub tag_length: u8,
    pub config_length: u16,
    pub content_length: u64,
}

impl Trailer {
    pub fn from_reader<R: Read>(reader: &mut R) -> Result<Self> {
        let mut bytes = [0; TRAILER_LENGTH];
        reader.read_exact(&mut bytes).unwrap();

        let magic: &[u8; 4] = bytes[12..16].try_into().unwrap();
        let version = bytes[11];

        if magic != TRAILER_MAGIC {
            return Err(Error::UnexpectedMagic);
        }

        if version != TRAILER_VERSION {
            return Err(Error::UnexpectedVersion);
        }

        let tag_length = bytes[10];
        let config_length = u16::from_be_bytes(bytes[8..10].try_into().unwrap());
        let content_length = u64::from_be_bytes(bytes[0..8].try_into().unwrap());

        let trailer = Self {
            tag_length,
            content_length,
            config_length,
        };
        Ok(trailer)
    }
}

#[derive(Deserialize)]
struct Config {
    pub app: Option<String>,
    pub version: Option<String>,
    pub compression: Option<Compression>,
    #[serde(default)]
    pub cache_dir: String,
    pub entry_point: String,
}

impl Config {
    pub fn from_reader<R: Read>(reader: R) -> Result<Self> {
        match serde_json::from_reader::<R, Self>(reader) {
            Ok(mut config) => {
                if let Some(app) = &config.app {
                    if app.eq_ignore_ascii_case(cache::SHARED) {
                        return Err(Error::InvalidConfig(
                            "invalid config: invalid app name".to_string(),
                        ));
                    }
                }

                if config.cache_dir.is_empty() {
                    let app = if let Some(app) = &config.app {
                        app.as_str()
                    } else {
                        cache::SHARED
                    };

                    config.cache_dir = cache::default_cache_dir(app);
                }

                if config.version.is_some() && config.app.is_none() {
                    return Err(Error::InvalidConfig(
                        "invalid config: app must be provided if version is provided".to_string(),
                    ));
                }

                config.cache_dir = cache::expand_cache_dir(&config.cache_dir)?;

                Ok(config)
            }
            Err(error) => {
                let msg = format!("invalid config: {}", error);
                Err(Error::InvalidConfig(msg))
            }
        }
    }
}

#[derive(Deserialize, PartialEq)]
enum Compression {
    Zstd,
}

mod cache {
    extern crate dirs_next as dirs;

    use std::path::PathBuf;

    pub const SHARED: &str = "shared";

    #[cfg(all(unix, not(target_os = "macos")))]
    pub fn default_cache_dir(app_name: &str) -> String {
        format!("~/.cache/giftwrap/{}", app_name)
    }

    #[cfg(target_os = "macos")]
    pub fn default_cache_dir(app_name: &str) -> String {
        format!("~/Library/Caches/Giftwrap/{}", app_name)
    }

    #[cfg(windows)]
    pub fn default_cache_dir(app_name: &str) -> String {
        format!("$APPDATA\\Local\\Giftwrap\\cache\\{}", app_name)
    }

    #[cfg(unix)]
    pub fn expand_cache_dir(cache_dir: &str) -> super::Result<String> {
        Ok(shellexpand::tilde_with_context(cache_dir, home_dir).to_string())
    }

    #[cfg(windows)]
    pub fn expand_cache_dir(cache_dir: &str) -> super::Result<String> {
        shellexpand::full_with_context(cache_dir, home_dir, appdata_env)
            .map(|dir| dir.to_string())
            .map_err(|error| error.cause)
    }

    fn home_dir() -> Option<PathBuf> {
        dirs::home_dir()
    }

    #[cfg(windows)]
    fn appdata_env(name: &str) -> super::Result<Option<String>> {
        match name {
            "APPDATA" => std::env::var("APPDATA")
                .map(|s| Some(s))
                .map_err(|_| super::Error::InvalidConfig("unable to expand APPDATA".to_string())),
            _ => Err(super::Error::InvalidConfig(format!(
                "unsupported env var {} in cache dir",
                name
            ))),
        }
    }
}
