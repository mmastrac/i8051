use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use i8051_disassembler::address::AddressValue;
use i8051_disassembler::commands::Environment;

/// Reads mapped files from a base directory.
pub struct FsEnvironment {
    base: PathBuf,
}

impl FsEnvironment {
    /// Root reads at this directory.
    pub fn new(base: impl Into<PathBuf>) -> Self {
        Self { base: base.into() }
    }
}

impl Environment for FsEnvironment {
    fn load_file_bytes(
        &self,
        file: &str,
        offset: usize,
        size: AddressValue,
    ) -> Result<Vec<u8>, io::Error> {
        let data = std::fs::read(self.base.join(file))?;
        slice(&data, offset, size)
    }
}

#[derive(Default)]
/// Serves mapped files from memory.
pub struct MemoryEnvironment {
    files: HashMap<String, Vec<u8>>,
}

impl MemoryEnvironment {
    /// An empty environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a file's contents.
    pub fn insert(&mut self, name: impl Into<String>, bytes: Vec<u8>) {
        self.files.insert(name.into(), bytes);
    }

    /// Builder form of `insert`.
    pub fn with_file(mut self, name: impl Into<String>, bytes: Vec<u8>) -> Self {
        self.insert(name, bytes);
        self
    }
}

impl Environment for MemoryEnvironment {
    fn load_file_bytes(
        &self,
        file: &str,
        offset: usize,
        size: AddressValue,
    ) -> Result<Vec<u8>, io::Error> {
        let data = self
            .files
            .get(file)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("no such file `{file}`")))?;
        slice(data, offset, size)
    }
}

/// Shared bounds-checked slice of `[offset, offset+size)`.
fn slice(data: &[u8], offset: usize, size: AddressValue) -> Result<Vec<u8>, io::Error> {
    let end = offset.saturating_add(size as usize);
    if end > data.len() {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "read past end of file",
        ));
    }
    Ok(data[offset..end].to_vec())
}
