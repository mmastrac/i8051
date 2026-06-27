use crate::address::SpaceAddressRange;
use crate::db::{Db, Error, Note, NoteId};
use crate::store::fields;

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetNote {
    pub address: SpaceAddressRange,
    #[serde(with = "fields::note")]
    pub note: Note,
}

register!(SetNote(
    /// Attach `note` to the code address range `address`.
    address: SpaceAddressRange,
    note: Note,
));

impl Apply for SetNote {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address, note } = self;
        let SpaceAddressRange { space, range } = address;
        let id = note.id.clone();
        let before = db
            .notes
            .note_range(space, &id)
            .zip(db.notes.get(&id).cloned());
        db.notes.set_address(space, range, note);
        match before {
            Some((old_range, old_note)) => Ok(vec![boxed(SetNote {
                address: (space, old_range).into(),
                note: old_note,
            })]),
            None => Ok(vec![boxed(ClearNote { id })]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearNote {
    pub id: NoteId,
}

register!(ClearNote(
    /// Remove the note with the given `id`.
    id: NoteId,
));

impl Apply for ClearNote {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { id } = self;
        let Some((space, range, note)) = db.notes.clear_address(&id) else {
            return Ok(vec![]);
        };
        Ok(vec![boxed(SetNote {
            address: (space, range).into(),
            note,
        })])
    }
}
