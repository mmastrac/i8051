use crate::{address::AddressValue, db::DataType};

pub mod data;
pub mod sdas;

#[derive(Debug, Clone)]
pub enum Line {
    Org {
        addr: AddressValue,
    },
    Blank,
    Comment {
        addr: AddressValue,
        text: String,
    },
    Label {
        addr: AddressValue,
        name: String,
    },
    Instruction {
        addr: AddressValue,
        text: String,
        bytes: Vec<u8>,
    },
    Data {
        addr: AddressValue,
        data_type: DataType,
        bytes: Vec<u8>,
    },
    Raw {
        addr: AddressValue,
        bytes: Vec<u8>,
    },
    Function {
        addr: AddressValue,
        name: String,
        signature: Option<String>,
        length: AddressValue,
        noreturn: bool,
    },
}

impl Line {
    pub fn addr(&self) -> AddressValue {
        match self {
            Self::Org { addr, .. }
            | Self::Comment { addr, .. }
            | Self::Label { addr, .. }
            | Self::Function { addr, .. }
            | Self::Instruction { addr, .. }
            | Self::Data { addr, .. }
            | Self::Raw { addr, .. } => *addr,
            Self::Blank => 0,
        }
    }

    pub fn to_sdas(&self) -> String {
        sdas::line_to_sdas(self)
    }
}
