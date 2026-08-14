//! The remaining `#[serde(with = ...)]` adapter: note-id allocation. Address
//! syntax now lives on the address types themselves (see [`crate::address`]).

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::note::{Note, NoteField, NoteId};

/// A [`Note`].
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

    struct NoteVisitor;

    impl<'de> serde::de::Visitor<'de> for NoteVisitor {
        type Value = Note;

        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("a note: Note(content=\"...\", tags=[\"...\"]) or bare \"text\"")
        }

        // Bare text turns into a note.
        fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Note, E> {
            Ok(Note::new(None, v.to_owned()))
        }

        fn visit_string<E: serde::de::Error>(self, v: String) -> Result<Note, E> {
            Ok(Note::new(None, v))
        }

        fn visit_map<A: serde::de::MapAccess<'de>>(self, map: A) -> Result<Note, A::Error> {
            let repr = Repr::deserialize(serde::de::value::MapAccessDeserializer::new(map))?;
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

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Note, D::Error> {
        d.deserialize_any(NoteVisitor)
    }
}
