use std::env;
use std::fs::File;
use std::panic;
use std::process;

mod archive;

use archive::{Archive, Result};

fn extract<'a>(file: &'a File) -> Result<Archive<'a>> {
    let mut archive = Archive::from_file(&file)?;
    archive.unpack_content()?;

    Ok(archive)
}

fn main() {
    panic::set_hook(Box::new(|panic_info| {
        if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            eprintln!("Panic occurred while launching program: {}", s);
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            eprintln!("Panic occurred while launching program: {}", s);
        } else {
            eprintln!("Panic occurred while launching program");
        }
        process::exit(1);
    }));

    let path = env::current_exe().expect("Could not get current executable");
    let file = File::open(path).expect("Could not open file");

    match extract(&file) {
        Ok(mut archive) => {
            archive.launch(&env::args().collect::<Vec<_>>());
        }
        Err(error) => {
            eprintln!("Error occurred while launching program: {}", error);
            process::exit(1);
        }
    }
}
