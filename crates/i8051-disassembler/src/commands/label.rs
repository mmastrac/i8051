use crate::address::{SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetLabel {
    pub address: SpaceAddressValue,
    pub label: String,
    /// A working guess rather than a finalized name. If true, stays on the
    /// naming worklist.
    pub provisional: bool,
    /// Scoped to the routine containing it.
    pub local: bool,
}

register!(SetLabel(
    /// Name the code `address` with `label`. Pass `provisional=True` for a
    /// working guess: it is stored and shown like any name, but the address
    /// stays on the naming worklist so a later pass can adjust it.
    address: SpaceAddressValue,
    label: String,
    provisional: bool,
    local: bool,
));

impl Apply for SetLabel {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self {
            address,
            label,
            provisional,
            local,
        } = self;
        let label = normalize_label(&label)?;
        if crate::labels::is_provisional_name(&label) {
            return Ok(Vec::new());
        }
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_label(offset).map(str::to_owned);
        let was_draft = region.is_draft_label(offset);
        let was_local = region.is_local_label(offset);
        region.set_label(
            offset,
            &label,
            crate::region::LabelAttrs { provisional, local },
        );
        Ok(match before {
            Some(label) => vec![boxed(SetLabel {
                address,
                label,
                provisional: was_draft,
                local: was_local,
            })],
            None => vec![boxed(ClearLabel::new((space, offset)))],
        })
    }
}

/// Clean a caller-supplied label into something an assembler can accept.
pub fn normalize_label(label: &str) -> Result<String, Error> {
    let trimmed = label.trim();
    let unquoted = trimmed
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .unwrap_or(trimmed)
        .trim();
    let invalid = |reason| {
        Err(Error::InvalidLabel {
            label: label.to_string(),
            reason,
        })
    };
    if unquoted.is_empty() {
        return invalid("a label cannot be empty");
    }
    if unquoted.chars().any(char::is_whitespace) {
        return invalid("a label cannot contain whitespace");
    }
    if unquoted.contains('"') {
        return invalid("a label cannot contain quote characters");
    }
    if unquoted.starts_with(|c: char| c.is_ascii_digit()) {
        return invalid("a label cannot start with a digit");
    }
    Ok(unquoted.to_string())
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearLabel {
    pub addresses: SpaceAddressSet,
}

register!(ClearLabel(
    /// Remove labels at the given `addresses`.
    addresses: SpaceAddressSet,
));

impl Apply for ClearLabel {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { addresses } = self;
        let space = addresses.space;
        let region = db.region_mut(space);
        let mut undo = Vec::new();
        for range in addresses.ranges() {
            for (offset, label) in region.clear_labels_in(range) {
                undo.push(boxed(SetLabel::new((space, offset), label, false, false)));
            }
        }
        Ok(undo)
    }
}

// A clear command's `SpaceAddressSet` renders as the optimal `CODE:{...}` form.
serialize_test!(
    clear_label_address_set,
    "clear_label(addresses=CODE:{0x10, 0x20})",
    ClearLabel {
        addresses: {
            let mut set = SpaceAddressSet::new(crate::platform::i8051::CODE);
            set.insert_address(0x10);
            set.insert_address(0x20);
            set
        },
    }
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::labels::is_provisional_name;
    use crate::platform::i8051::CODE;

    #[test]
    fn strips_extra_quotes() {
        assert_eq!(normalize_label("\"uart_init\"").unwrap(), "uart_init");
        assert_eq!(normalize_label("uart_init").unwrap(), "uart_init");
        assert_eq!(normalize_label("  spaced_out  ").unwrap(), "spaced_out");
        assert!(is_provisional_name(&normalize_label("\"loc_0071\"").unwrap()));

        for bad in ["", "  ", "\"\"", "two words", "mid\"quote", "9lives"] {
            assert!(normalize_label(bad).is_err(), "{bad:?} should be rejected");
        }
    }

    #[test]
    fn recognizes_generated_names_only() {
        for name in ["sub_002C", "loc_03A9", "sub_002c", "sub_10000"] {
            assert!(is_provisional_name(name), "{name} should be provisional");
        }
        for name in ["uart_init", "sub_", "sub_2C", "loc_INIT", "subtract_a", "sub_00GG"] {
            assert!(!is_provisional_name(name), "{name} should not be provisional");
        }
    }

    #[test]
    fn generated_name_is_noop() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        let undo = db
            .apply(
                boxed(SetLabel::new((CODE, 0x2C), "sub_002C".to_string(), false, false)),
                None,
            )
            .expect("a generated name is ignored, not an error");
        assert!(undo.is_empty(), "a no-op contributes nothing to undo");
        assert_eq!(db.region_mut(CODE).get_label(0x2C), None, "nothing stored");

        // A real name still lands...
        db.apply(
            boxed(SetLabel::new((CODE, 0x2C), "uart_init".to_string(), false, false)),
            None,
        )
        .unwrap();
        assert_eq!(db.region_mut(CODE).get_label(0x2C), Some("uart_init"));

        // ...and a generated one does not overwrite it.
        db.apply(
            boxed(SetLabel::new((CODE, 0x2C), "sub_002C".to_string(), false, false)),
            None,
        )
        .unwrap();
        assert_eq!(db.region_mut(CODE).get_label(0x2C), Some("uart_init"));
    }

    /// A provisional name is shown like any other, but the address stays on the
    /// naming worklist.
    #[test]
    fn provisional_stays_draft() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(SetLabel::new((CODE, 0x40), "maybe_crc".to_string(), true, false)),
            None,
        )
        .unwrap();
        assert!(db.region_mut(CODE).is_draft_label(0x40));

        let dsl = crate::store::to_dsl_many(&db.to_commands());
        assert!(dsl.contains("provisional=True"), "{dsl}");
        let mut replayed = Db::new();
        for command in crate::store::from_dsl_many(&dsl).expect("export parses") {
            replayed.apply(command, None).expect("export replays");
        }
        assert!(replayed.region_mut(CODE).is_draft_label(0x40), "{dsl}");

        // Re-naming it without the flag finalizes it.
        db.apply(
            boxed(SetLabel::new((CODE, 0x40), "crc16".to_string(), false, false)),
            None,
        )
        .unwrap();
        assert!(!db.region_mut(CODE).is_draft_label(0x40));
    }
}
