//! The remaining `#[serde(with = ...)]` adapter: note-id allocation. Address
//! syntax now lives on the address types themselves (see [`crate::address`]).

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::note::{Note, NoteField, NoteId};

/// A [`Note`] whose `id` may be omitted in hand-written DSL, in which case a
/// fresh id is allocated from the content.
pub mod note {
    use super::*;

    #[derive(Deserialize)]
    struct Repr {
        #[serde(default)]
        id: Option<NoteId>,
        content: String,
        #[serde(default)]
        tags: BTreeSet<String>,
        #[serde(default)]
        fields: BTreeMap<String, NoteField>,
        #[serde(default)]
        links: BTreeSet<NoteId>,
    }

    pub fn serialize<S: Serializer>(note: &Note, s: S) -> Result<S::Ok, S::Error> {
        note.serialize(s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Note, D::Error> {
        let repr = Repr::deserialize(d)?;
        let mut note = match repr.id {
            Some(id) => Note {
                id,
                content: repr.content,
                tags: BTreeSet::new(),
                fields: BTreeMap::new(),
                links: BTreeSet::new(),
            },
            None => Note::new(None, repr.content),
        };
        note.tags = repr.tags;
        note.fields = repr.fields;
        note.links = repr.links;
        Ok(note)
    }
}
