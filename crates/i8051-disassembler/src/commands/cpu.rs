use crate::db::{Db, Error};
use crate::platform::{self, PlatformRef};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetCpu {
    pub name: String,
}

register!(SetCpu(
    /// Select the processor whose instruction set the DB disassembles against.
    /// Must run before any disassembly. The inverse restores the prior CPU, or
    /// clears it when none was set.
    name: String,
));

impl Apply for SetCpu {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let driver =
            platform::by_name(&self.name).ok_or_else(|| Error::UnknownCpu(self.name.clone()))?;
        let prev = db.set_platform(Some(driver));
        Ok(vec![restore(prev)])
    }
}

#[derive(Debug, Clone, Default, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearCpu {}

register!(ClearCpu(
    /// Clear the selected processor. The inverse of `set_cpu` when no CPU was
    /// set beforehand.
));

impl Apply for ClearCpu {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        match db.set_platform(None) {
            Some(prev) => Ok(vec![restore(Some(prev))]),
            None => Ok(vec![]),
        }
    }
}

/// The command that restores CPU selection `prev`: re-select it, or clear.
fn restore(prev: Option<PlatformRef>) -> Box<dyn Command> {
    match prev {
        Some(p) => boxed(SetCpu {
            name: p.name().to_string(),
        }),
        None => boxed(ClearCpu {}),
    }
}

serialize_test!(set_cpu, r#"set_cpu(name="m6805")"#, SetCpu { name: "m6805".into() });
serialize_test!(clear_cpu, "clear_cpu()", ClearCpu {});

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::{AutoDisassemble, boxed};
    use crate::platform::i8051;

    #[test]
    fn disassembly_requires_a_cpu() {
        let mut db = Db::new();
        let auto = boxed(AutoDisassemble::new((i8051::CODE, 0u32)));
        assert!(matches!(db.apply(auto, None), Err(Error::NoCpu)));
    }

    #[test]
    fn set_cpu_selects_and_undo_clears() {
        let mut db = Db::new();
        assert!(db.platform().is_none());

        let undo = db.apply(boxed(SetCpu::new("m6805")), None).unwrap();
        assert_eq!(db.platform().unwrap().name(), "m6805");

        // The inverse of setting a CPU on a fresh DB is `clear_cpu`.
        assert_eq!(undo.len(), 1);
        assert_eq!(undo[0].name(), "clear_cpu");
        db.apply(undo.into_iter().next().unwrap(), None).unwrap();
        assert!(db.platform().is_none());
    }

    #[test]
    fn set_cpu_over_existing_undo_restores_previous() {
        let mut db = Db::with_platform(i8051::platform());
        let undo = db.apply(boxed(SetCpu::new("m6805")), None).unwrap();
        assert_eq!(db.platform().unwrap().name(), "m6805");
        db.apply(undo.into_iter().next().unwrap(), None).unwrap();
        assert_eq!(db.platform().unwrap().name(), "i8051");
    }

    #[test]
    fn unknown_cpu_is_rejected() {
        let mut db = Db::new();
        assert!(matches!(
            db.apply(boxed(SetCpu::new("z80")), None),
            Err(Error::UnknownCpu(_))
        ));
    }
}
