use std::io;

use crate::address::{AddressSpace, AddressValue};
use crate::db::{Equivalent, Error, Function};
use serde::{Deserialize, Serialize};

pub trait Environment {
    fn load_file_bytes(
        &self,
        file: &str,
        offset: usize,
        size: AddressValue,
    ) -> Result<Vec<u8>, io::Error>;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Command {
    SetLabel {
        space: AddressSpace,
        offset: AddressValue,
        label: Option<String>,
    },
    SetEquivalent {
        space: AddressSpace,
        offset: AddressValue,
        equivalent: Equivalent,
    },
    AutoDisassemble {
        space: AddressSpace,
        start: AddressValue,
    },
    ClearEquivalents {
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
    },
    SetComment {
        space: AddressSpace,
        offset: AddressValue,
        comment: Option<String>,
    },
    MapBytes {
        space: AddressSpace,
        offset: AddressValue,
        file: String,
        file_offset: usize,
        size: AddressValue,
    },
    ClearBytes {
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
    },
    SetConstantBytes {
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
        value: u8,
    },
    SetFunction {
        space: AddressSpace,
        offset: AddressValue,
        function: Option<Function>,
    },
}

impl Command {
    pub fn set_label(space: AddressSpace, offset: AddressValue, label: impl Into<String>) -> Self {
        Self::SetLabel {
            space,
            offset,
            label: Some(label.into()),
        }
    }

    pub fn clear_label(space: AddressSpace, offset: AddressValue) -> Self {
        Self::SetLabel {
            space,
            offset,
            label: None,
        }
    }

    pub fn auto_disassemble(space: AddressSpace, start: AddressValue) -> Self {
        Self::AutoDisassemble { space, start }
    }

    pub fn set_equivalent(
        space: AddressSpace,
        offset: AddressValue,
        equivalent: Equivalent,
    ) -> Self {
        Self::SetEquivalent {
            space,
            offset,
            equivalent,
        }
    }

    pub fn clear_equivalents(
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
    ) -> Self {
        Self::ClearEquivalents {
            space,
            offset,
            size,
        }
    }

    pub fn set_comment(
        space: AddressSpace,
        offset: AddressValue,
        comment: impl Into<String>,
    ) -> Self {
        Self::SetComment {
            space,
            offset,
            comment: Some(comment.into()),
        }
    }

    pub fn clear_comment(space: AddressSpace, offset: AddressValue) -> Self {
        Self::SetComment {
            space,
            offset,
            comment: None,
        }
    }

    pub fn map_bytes(
        space: AddressSpace,
        offset: AddressValue,
        file: impl Into<String>,
        file_offset: usize,
        size: AddressValue,
    ) -> Self {
        Self::MapBytes {
            space,
            offset,
            file: file.into(),
            file_offset,
            size,
        }
    }

    pub fn clear_bytes(space: AddressSpace, offset: AddressValue, size: AddressValue) -> Self {
        Self::ClearBytes {
            space,
            offset,
            size,
        }
    }

    pub fn set_constant_bytes(
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
        value: u8,
    ) -> Self {
        Self::SetConstantBytes {
            space,
            offset,
            size,
            value,
        }
    }

    pub fn set_function(space: AddressSpace, offset: AddressValue, function: Function) -> Self {
        Self::SetFunction {
            space,
            offset,
            function: Some(function),
        }
    }

    pub fn clear_function(space: AddressSpace, offset: AddressValue) -> Self {
        Self::SetFunction {
            space,
            offset,
            function: None,
        }
    }

    pub fn apply(
        self,
        db: &mut crate::db::Db,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Command>, crate::db::Error> {
        match self {
            Command::SetLabel {
                space,
                offset,
                label,
            } => {
                let region = db.region_mut(space);
                let before = region.get_label(offset).map(str::to_owned);
                match label.as_deref() {
                    Some(label) => region.set_label(offset, label),
                    None => region.clear_label(offset),
                }
                Ok(vec![Command::SetLabel {
                    space,
                    offset,
                    label: before,
                }])
            }
            Command::SetEquivalent {
                space,
                offset,
                equivalent,
            } => {
                let region = db.region_mut(space);
                let span = region.equivalent_span(offset, &equivalent)?;
                let before = region.snapshot_equivalents(offset, span);
                region.set_equivalent(offset, equivalent)?;
                let mut undo = vec![Command::ClearEquivalents {
                    space,
                    offset,
                    size: span,
                }];
                for (start, range) in before {
                    undo.push(Command::SetEquivalent {
                        space,
                        offset: start,
                        equivalent: range.equivalent,
                    });
                }
                Ok(undo)
            }
            Command::AutoDisassemble { space, start } => {
                let region = db.region_mut(space);
                let addresses = region.auto_disassemble(start).success;
                Ok(addresses
                    .into_iter()
                    .map(|addr| Command::ClearEquivalents {
                        space,
                        offset: addr,
                        size: 1,
                    })
                    .collect())
            }
            Command::ClearEquivalents {
                space,
                offset,
                size,
            } => {
                let region = db.region_mut(space);
                let before = region.snapshot_equivalents(offset, size);
                region.clear_equivalents(offset, size);
                let mut undo = Vec::new();
                for (start, range) in before {
                    undo.push(Command::SetEquivalent {
                        space,
                        offset: start,
                        equivalent: range.equivalent,
                    });
                }
                Ok(undo)
            }
            Command::SetComment {
                space,
                offset,
                comment,
            } => {
                let region = db.region_mut(space);
                let before = region.get_comment(offset).map(str::to_owned);
                match comment.as_deref() {
                    Some(comment) => region.set_comment(offset, comment),
                    None => region.clear_comment(offset),
                }
                Ok(vec![Command::SetComment {
                    space,
                    offset,
                    comment: before,
                }])
            }
            Command::MapBytes {
                space,
                offset,
                file,
                file_offset,
                size,
            } => {
                let region = db.region_mut(space);
                let Some(env) = env else {
                    return Err(Error::NoEnvironment);
                };
                let bytes = env
                    .load_file_bytes(&file, file_offset, size)
                    .map_err(Error::Io)?;
                let size = bytes.len() as AddressValue;
                let before = region.snapshot_byte_ranges(offset, size);
                region.map_bytes(&file, file_offset, offset, &bytes);
                Ok(undo_byte_ranges(space, offset, size, before))
            }
            Command::ClearBytes {
                space,
                offset,
                size,
            } => {
                let region = db.region_mut(space);
                let before = region.snapshot_byte_ranges(offset, size);
                region.clear_bytes(offset, size);
                Ok(undo_byte_ranges(space, offset, size, before))
            }
            Command::SetConstantBytes {
                space,
                offset,
                size,
                value,
            } => {
                let region = db.region_mut(space);
                let before = region.snapshot_byte_ranges(offset, size);
                region.set_constant(offset, size, value);
                Ok(undo_byte_ranges(space, offset, size, before))
            }
            Command::SetFunction {
                space,
                offset,
                function,
            } => {
                let region = db.region_mut(space);
                let before = region.get_function(offset).cloned();
                match function {
                    Some(function) => region.set_function(function),
                    None => region.clear_function(offset),
                }
                Ok(vec![Command::SetFunction {
                    space,
                    offset,
                    function: before,
                }])
            }
        }
    }
}

fn undo_byte_ranges(
    space: AddressSpace,
    offset: AddressValue,
    size: AddressValue,
    ranges: Vec<(AddressValue, crate::region::ByteRange)>,
) -> Vec<Command> {
    use crate::region::ByteRange;

    let mut undo = vec![Command::ClearBytes {
        space,
        offset,
        size,
    }];
    for (start, range) in ranges {
        match range {
            ByteRange::Mapped(file, file_offset, data) => {
                undo.push(Command::MapBytes {
                    space,
                    offset: start,
                    file,
                    file_offset,
                    size: data.len() as AddressValue,
                });
            }
            ByteRange::Constant(count, value) => {
                undo.push(Command::SetConstantBytes {
                    space,
                    offset: start,
                    size: count as AddressValue,
                    value,
                });
            }
        }
    }
    undo
}
