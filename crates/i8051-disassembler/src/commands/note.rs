use crate::address::SpaceAddressRange;
use crate::db::{Db, Error, Note, NoteId};
use crate::store::fields;

use super::Command;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetNote {
    #[serde(with = "fields::space_address_range")]
    pub address: SpaceAddressRange,
    #[serde(with = "fields::note")]
    pub note: Note,
}

impl SetNote {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address, note } = self;
        let (space, range) = address;
        let id = note.id.clone();
        let before = db.notes.note_range(space, &id).and_then(|old_range| {
            db.notes
                .get(&id)
                .cloned()
                .map(|old_note| (old_range, old_note))
        });
        db.notes.set_address(space, range, note);
        match before {
            Some((old_range, old_note)) => Ok(vec![Command::SetNote(SetNote {
                address: (space, old_range),
                note: old_note,
            })]),
            None => Ok(vec![Command::ClearNote(ClearNote { id })]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearNote {
    pub id: NoteId,
}

impl ClearNote {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { id } = self;
        let Some((space, range, note)) = db.notes.clear_address(&id) else {
            return Ok(vec![]);
        };
        Ok(vec![Command::SetNote(SetNote {
            address: (space, range),
            note,
        })])
    }
}
