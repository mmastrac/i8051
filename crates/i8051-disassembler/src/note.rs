use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::ops::RangeBounds;
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize};
use xxhash_rust::xxh64::xxh64;

use crate::address::{AddressRange, AddressSpace, AddressValue};

const BASE32: &[u8; 32] = b"0123456789ABCDEFGHJKMNPQRSTVWXYZ";

/// A lexicographically sortable note identifier (fixed-width base32, 26 chars).
///
/// The 128-bit value uses Lamport depth in the high 64 bits and a content hash in
/// the low 64 bits. The ID is assigned once and never recomputed. Fixed-width
/// encoding makes string order identical to numeric Lamport order.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
#[serde(transparent)]
pub struct NoteId(String);

impl NoteId {
    pub const ENCODED_LEN: usize = 26;

    /// Allocate the next ID after `previous_tip` for `content`.
    pub fn allocate(previous_tip: Option<&Self>, content: &str) -> Self {
        let depth = previous_tip.map(Self::depth).unwrap_or(0).saturating_add(1);
        let hash = hash_payload(previous_tip, content);
        Self::from_parts(depth, hash)
    }

    /// Return the Lamport tip from an iterator of existing note IDs.
    pub fn tip<'a>(ids: impl IntoIterator<Item = &'a Self>) -> Option<Self> {
        ids.into_iter().max().cloned()
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn depth(&self) -> u64 {
        decode(self.as_str()).0
    }

    pub fn hash(&self) -> u64 {
        decode(self.as_str()).1
    }

    fn from_parts(depth: u64, hash: u64) -> Self {
        let value = ((depth as u128) << 64) | (hash as u128);
        Self(encode(value))
    }
}

impl<'de> Deserialize<'de> for NoteId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Self::from_str(&value).map_err(serde::de::Error::custom)
    }
}

impl fmt::Debug for NoteId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("NoteId").field(&self.0).finish()
    }
}

impl fmt::Display for NoteId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for NoteId {
    type Err = NoteIdError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        if value.is_empty() || value.len() > Self::ENCODED_LEN {
            return Err(NoteIdError::InvalidLength);
        }
        let (depth, hash) = try_decode(value)?;
        Ok(Self::from_parts(depth, hash))
    }
}

impl AsRef<str> for NoteId {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteIdError {
    InvalidLength,
    InvalidCharacter,
}

impl fmt::Display for NoteIdError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidLength => write!(
                f,
                "note id must be 1..={} base32 characters",
                NoteId::ENCODED_LEN
            ),
            Self::InvalidCharacter => write!(f, "note id contains invalid base32 character"),
        }
    }
}

impl std::error::Error for NoteIdError {}

/// A dotted global path (`a.b.c`) into the shared note namespace.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct NotePath(String);

impl NotePath {
    pub fn parse(path: &str) -> Result<Self, NotePathError> {
        if path.is_empty() {
            return Err(NotePathError::Empty);
        }
        if path.starts_with('.') || path.ends_with('.') || path.contains("..") {
            return Err(NotePathError::Invalid);
        }
        for segment in path.split('.') {
            if segment.is_empty() {
                return Err(NotePathError::Invalid);
            }
        }
        Ok(Self(path.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.0.split('.')
    }
}

impl fmt::Display for NotePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for NotePath {
    type Err = NotePathError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::parse(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotePathError {
    Empty,
    Invalid,
}

impl fmt::Display for NotePathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "note path must not be empty"),
            Self::Invalid => write!(f, "note path must be dot-separated segments"),
        }
    }
}

impl std::error::Error for NotePathError {}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum NoteField {
    String(String),
    List(Vec<String>),
}

/// A disassembly note. The [`NoteId`] is fixed at creation; edits change
/// [`content`](Self::content), tags, fields, and links only.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Note {
    pub id: NoteId,
    pub content: String,
    pub tags: BTreeSet<String>,
    pub fields: BTreeMap<String, NoteField>,
    pub links: BTreeSet<NoteId>,
}

impl Note {
    pub fn new(previous_tip: Option<&NoteId>, content: impl Into<String>) -> Self {
        let content = content.into();
        let id = NoteId::allocate(previous_tip, &content);
        Self {
            id,
            content,
            tags: BTreeSet::new(),
            fields: BTreeMap::new(),
            links: BTreeSet::new(),
        }
    }

    pub fn with_metadata(
        previous_tip: Option<&NoteId>,
        content: impl Into<String>,
        tags: BTreeSet<String>,
        fields: BTreeMap<String, NoteField>,
        links: BTreeSet<NoteId>,
    ) -> Self {
        let mut note = Self::new(previous_tip, content);
        note.tags = tags;
        note.fields = fields;
        note.links = links;
        note
    }

    pub fn set_content(&mut self, content: impl Into<String>) {
        self.content = content.into();
    }
}

/// All notes keyed by opaque ID.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Notes {
    by_id: BTreeMap<NoteId, Note>,
}

impl Notes {
    pub fn insert(&mut self, note: Note) -> Option<Note> {
        self.by_id.insert(note.id.clone(), note)
    }

    pub fn remove(&mut self, id: &NoteId) -> Option<Note> {
        self.by_id.remove(id)
    }

    pub fn get(&self, id: &NoteId) -> Option<&Note> {
        self.by_id.get(id)
    }

    pub fn tip(&self) -> Option<NoteId> {
        self.by_id.keys().next_back().cloned()
    }

    /// Allocate the next ID from the current tip and insert atomically.
    pub fn create(&mut self, content: impl Into<String>) -> Note {
        let note = Note::new(self.tip().as_ref(), content);
        assert!(
            self.by_id.insert(note.id.clone(), note.clone()).is_none(),
            "duplicate note id"
        );
        note
    }

    pub fn iter(&self) -> impl Iterator<Item = (&NoteId, &Note)> {
        self.by_id.iter()
    }

    pub fn len(&self) -> usize {
        self.by_id.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }
}

/// A note located by a proximity query, tagged with how far the probe address
/// sits from the note's range.
#[derive(Debug, Clone, Copy)]
pub struct ProximateNote<'a> {
    /// Bytes between the probe address and the note's range (`0` if inside).
    pub distance: AddressValue,
    /// The address range the note is attached to.
    pub range: AddressRange,
    pub note: &'a Note,
}

/// Distance from a point to a half-open range: `0` when the point is inside.
fn point_distance(range: AddressRange, addr: AddressValue) -> AddressValue {
    if addr < range.start {
        range.start - addr
    } else if addr >= range.end {
        addr - range.end + 1
    } else {
        0
    }
}

/// Maps address ranges to note IDs within one address space.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NoteAddressIndex {
    positions: rangemap::RangeMap<AddressValue, Vec<NoteId>>,
    ranges: BTreeMap<NoteId, AddressRange>,
}

impl NoteAddressIndex {
    pub fn insert(&mut self, range: AddressRange, id: NoteId) {
        if let Some(old_range) = self.ranges.remove(&id) {
            self.remove_id_from_positions(old_range, &id);
        }
        self.ranges.insert(id.clone(), range);
        self.add_id_to_positions(range, id);
    }

    pub fn remove(&mut self, id: &NoteId) -> Option<AddressRange> {
        let range = self.ranges.remove(id)?;
        self.remove_id_from_positions(range, id);
        Some(range)
    }

    pub fn range(&self, id: &NoteId) -> Option<AddressRange> {
        self.ranges.get(id).copied()
    }

    pub fn overlapping<'a>(&'a self, range: AddressRange, notes: &'a Notes) -> Vec<&'a Note> {
        self.positions
            .overlapping(range.start..range.end)
            .flat_map(|(_, ids)| ids.iter())
            .filter_map(|id| notes.get(id))
            .collect()
    }

    pub fn inside<'a>(&'a self, range: AddressRange, notes: &'a Notes) -> Vec<&'a Note> {
        self.positions
            .overlapping(range.start..range.end)
            .flat_map(|(indexed, ids)| {
                if range.start <= indexed.start && indexed.end <= range.end {
                    ids.iter()
                } else {
                    [].iter()
                }
            })
            .filter_map(|id| notes.get(id))
            .collect()
    }

    /// Notes within `window` bytes of `addr`, paired with their distance
    /// (`0` = the address falls inside the note's range), nearest first.
    pub fn near<'a>(
        &'a self,
        addr: AddressValue,
        window: AddressValue,
        notes: &'a Notes,
    ) -> Vec<ProximateNote<'a>> {
        let lo = addr.saturating_sub(window);
        let hi = addr.saturating_add(window).saturating_add(1);
        let mut seen = BTreeSet::new();
        let mut out = Vec::new();
        for (_, ids) in self.positions.overlapping(lo..hi) {
            for id in ids {
                if !seen.insert(id.clone()) {
                    continue;
                }
                let (Some(&range), Some(note)) = (self.ranges.get(id), notes.get(id)) else {
                    continue;
                };
                let distance = point_distance(range, addr);
                if distance <= window {
                    out.push(ProximateNote {
                        distance,
                        range,
                        note,
                    });
                }
            }
        }
        out.sort_by(|a, b| {
            a.distance
                .cmp(&b.distance)
                .then_with(|| a.note.id.cmp(&b.note.id))
        });
        out
    }

    fn add_id_to_positions(&mut self, range: AddressRange, id: NoteId) {
        let mut ids = self.ids_at_exact_range(range);
        if !ids.contains(&id) {
            ids.push(id);
        }
        self.positions.remove(range.start..range.end);
        self.positions.insert(range.start..range.end, ids);
    }

    fn remove_id_from_positions(&mut self, range: AddressRange, id: &NoteId) {
        let ids: Vec<NoteId> = self
            .ids_at_exact_range(range)
            .into_iter()
            .filter(|existing| existing != id)
            .collect();
        self.positions.remove(range.start..range.end);
        if !ids.is_empty() {
            self.positions.insert(range.start..range.end, ids);
        }
    }

    fn ids_at_exact_range(&self, range: AddressRange) -> Vec<NoteId> {
        self.positions
            .overlapping(range.start..range.end)
            .find(|(indexed, _)| indexed.start == range.start && indexed.end == range.end)
            .map(|(_, ids)| ids.clone())
            .unwrap_or_default()
    }
}

/// Maps dotted global paths to note IDs.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NoteGlobalIndex {
    paths: BTreeMap<NotePath, NoteId>,
    by_id: BTreeMap<NoteId, NotePath>,
}

impl NoteGlobalIndex {
    pub fn insert(&mut self, path: NotePath, id: NoteId) {
        if let Some(old_path) = self.by_id.remove(&id) {
            self.paths.remove(&old_path);
        }
        if let Some(old_id) = self.paths.remove(&path) {
            self.by_id.remove(&old_id);
        }
        self.paths.insert(path.clone(), id.clone());
        self.by_id.insert(id, path);
    }

    pub fn remove(&mut self, id: &NoteId) -> Option<NotePath> {
        let path = self.by_id.remove(id)?;
        self.paths.remove(&path);
        Some(path)
    }

    pub fn path(&self, id: &NoteId) -> Option<&NotePath> {
        self.by_id.get(id)
    }

    pub fn get<'a>(&'a self, path: &NotePath, notes: &'a Notes) -> Option<&'a Note> {
        let id = self.paths.get(path)?;
        notes.get(id)
    }
}

/// Note storage and indexes sharing one ID namespace.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NoteDb {
    pub notes: Notes,
    pub global: NoteGlobalIndex,
    address: BTreeMap<AddressSpace, NoteAddressIndex>,
}

impl NoteDb {
    pub fn tip(&self) -> Option<NoteId> {
        self.notes.tip()
    }

    pub fn create(&mut self, content: impl Into<String>) -> Note {
        self.notes.create(content)
    }

    pub fn get(&self, id: &NoteId) -> Option<&Note> {
        self.notes.get(id)
    }

    pub fn get_by_path(&self, path: &NotePath) -> Option<&Note> {
        self.global.get(path, &self.notes)
    }

    pub fn address_index(&self, space: AddressSpace) -> Option<&NoteAddressIndex> {
        self.address.get(&space)
    }

    pub fn set_address(&mut self, space: AddressSpace, range: AddressRange, note: Note) {
        let id = note.id.clone();
        self.notes.insert(note);
        self.address.entry(space).or_default().insert(range, id);
    }

    pub fn set_global(&mut self, path: NotePath, note: Note) {
        let id = note.id.clone();
        self.notes.insert(note);
        self.global.insert(path, id);
    }

    pub fn clear(&mut self, id: &NoteId) -> Option<Note> {
        let note = self.notes.remove(id)?;
        self.global.remove(id);
        for index in self.address.values_mut() {
            index.remove(id);
        }
        Some(note)
    }

    pub fn clear_address(&mut self, id: &NoteId) -> Option<(AddressSpace, AddressRange, Note)> {
        let mut located = None;
        for (&space, index) in &mut self.address {
            if let Some(range) = index.remove(id) {
                located = Some((space, range));
                break;
            }
        }
        let (space, range) = located?;
        let note = self.notes.remove(id)?;
        self.global.remove(id);
        Some((space, range, note))
    }

    pub fn note_range(&self, space: AddressSpace, id: &NoteId) -> Option<AddressRange> {
        self.address.get(&space)?.range(id)
    }

    pub fn get_notes_overlapping(
        &self,
        space: AddressSpace,
        range: impl RangeBounds<AddressValue>,
    ) -> Vec<&Note> {
        let range = AddressRange::from(range);
        self.address
            .get(&space)
            .map(|index| index.overlapping(range, &self.notes))
            .unwrap_or_default()
    }

    pub fn get_notes_inside(
        &self,
        space: AddressSpace,
        range: impl RangeBounds<AddressValue>,
    ) -> Vec<&Note> {
        let range = AddressRange::from(range);
        self.address
            .get(&space)
            .map(|index| index.inside(range, &self.notes))
            .unwrap_or_default()
    }

    /// Notes within `window` bytes of `addr` in `space`, nearest first, each
    /// tagged with its distance (`0` = the address is inside the note).
    pub fn notes_near(
        &self,
        space: AddressSpace,
        addr: AddressValue,
        window: AddressValue,
    ) -> Vec<ProximateNote<'_>> {
        self.address
            .get(&space)
            .map(|index| index.near(addr, window, &self.notes))
            .unwrap_or_default()
    }

    /// Find where a note is attached, if it has an address location.
    pub fn location(&self, id: &NoteId) -> Option<(AddressSpace, AddressRange)> {
        self.address
            .iter()
            .find_map(|(&space, index)| index.range(id).map(|range| (space, range)))
    }

    /// Notes whose content, tags, or field values contain `query`
    /// (case-insensitive). An empty query matches every note. Results are in
    /// note-id (Lamport) order.
    pub fn search(&self, query: &str) -> Vec<&Note> {
        let needle = query.to_lowercase();
        self.notes
            .iter()
            .map(|(_, note)| note)
            .filter(|note| note_matches(note, &needle))
            .collect()
    }
}

/// Whether a note matches an already-lowercased search needle.
fn note_matches(note: &Note, needle: &str) -> bool {
    if needle.is_empty() {
        return true;
    }
    if note.content.to_lowercase().contains(needle) {
        return true;
    }
    if note.tags.iter().any(|tag| tag.to_lowercase().contains(needle)) {
        return true;
    }
    note.fields.iter().any(|(key, value)| {
        key.to_lowercase().contains(needle)
            || match value {
                NoteField::String(s) => s.to_lowercase().contains(needle),
                NoteField::List(items) => {
                    items.iter().any(|item| item.to_lowercase().contains(needle))
                }
            }
    })
}

fn hash_payload(previous_tip: Option<&NoteId>, content: &str) -> u64 {
    match previous_tip {
        None => xxh64(content.as_bytes(), 0),
        Some(tip) => {
            let tip_bytes = tip.as_str().as_bytes();
            let content_bytes = content.as_bytes();
            let mut combined = Vec::with_capacity(2 + tip_bytes.len() + 2 + content_bytes.len());
            combined.extend_from_slice(&(tip_bytes.len() as u16).to_le_bytes());
            combined.extend_from_slice(tip_bytes);
            combined.extend_from_slice(&(content_bytes.len() as u16).to_le_bytes());
            combined.extend_from_slice(content_bytes);
            xxh64(&combined, 0)
        }
    }
}

fn encode(mut value: u128) -> String {
    let mut buf = [0u8; NoteId::ENCODED_LEN];
    for slot in buf.iter_mut().rev() {
        *slot = BASE32[(value & 0x1F) as usize];
        value >>= 5;
    }
    String::from_utf8(buf.to_vec()).expect("base32 alphabet is ascii")
}

fn decode(value: &str) -> (u64, u64) {
    try_decode(value).expect("note id is canonical fixed-width")
}

fn try_decode(value: &str) -> Result<(u64, u64), NoteIdError> {
    if value.is_empty() || value.len() > NoteId::ENCODED_LEN {
        return Err(NoteIdError::InvalidLength);
    }
    let padded = format!(
        "{:0>width$}",
        value.to_ascii_uppercase(),
        width = NoteId::ENCODED_LEN
    );
    let bytes = padded.as_bytes();
    let msb = decode_base32(bytes[0]).ok_or(NoteIdError::InvalidCharacter)? as u128;
    if msb > 0x7 {
        return Err(NoteIdError::InvalidCharacter);
    }
    let mut lower = 0u128;
    for (i, &byte) in bytes[1..].iter().enumerate() {
        let digit = decode_base32(byte).ok_or(NoteIdError::InvalidCharacter)? as u128;
        let shift = 5 * (bytes.len() - 2 - i);
        lower |= digit << shift;
    }
    let bits = (msb << 125) | lower;
    Ok(((bits >> 64) as u64, bits as u64))
}

fn decode_base32(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'A'..=b'H' => Some(byte - b'A' + 10),
        b'J' | b'K' => Some(byte - b'A' + 10 - 1),
        b'M' | b'N' => Some(byte - b'A' + 10 - 2),
        b'P'..=b'T' => Some(byte - b'A' + 10 - 3),
        b'V'..=b'Z' => Some(byte - b'A' + 10 - 4),
        b'a'..=b'h' => Some(byte - b'a' + 10),
        b'j' | b'k' => Some(byte - b'a' + 10 - 1),
        b'm' | b'n' => Some(byte - b'a' + 10 - 2),
        b'p'..=b't' => Some(byte - b'a' + 10 - 3),
        b'v'..=b'z' => Some(byte - b'a' + 10 - 4),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ids_are_fixed_width() {
        let id = NoteId::allocate(None, "hello");
        assert_eq!(id.as_str().len(), NoteId::ENCODED_LEN);
    }

    #[test]
    fn ids_sort_in_lamport_order() {
        let first = Note::new(None, "alpha");
        let second = Note::new(Some(&first.id), "beta");
        let third = Note::new(Some(&second.id), "gamma");

        assert!(first.id < second.id);
        assert!(second.id < third.id);
        assert_eq!(
            NoteId::tip([&first.id, &second.id, &third.id]),
            Some(third.id.clone())
        );
    }

    #[test]
    fn ids_sort_lexicographically_as_strings() {
        let first = Note::new(None, "alpha");
        let second = Note::new(Some(&first.id), "beta");
        let third = Note::new(Some(&second.id), "gamma");

        let mut sorted = [
            first.id.to_string(),
            second.id.to_string(),
            third.id.to_string(),
        ];
        sorted.sort();
        assert_eq!(
            sorted,
            [
                first.id.to_string(),
                second.id.to_string(),
                third.id.to_string()
            ]
        );
        assert_eq!(sorted.last().unwrap(), &third.id.to_string());
    }

    #[test]
    fn concurrent_notes_from_same_tip_differ_by_hash() {
        let first = Note::new(None, "shared tip");
        let left = Note::new(Some(&first.id), "branch a");
        let right = Note::new(Some(&first.id), "branch b");

        assert_eq!(left.id.depth(), right.id.depth());
        assert_ne!(left.id, right.id);
        assert_ne!(left.id.hash(), right.id.hash());
    }

    #[test]
    fn create_advances_tip() {
        let mut notes = Notes::default();
        let first = notes.create("one");
        let second = notes.create("two");
        assert_ne!(first.id, second.id);
        assert_eq!(notes.tip(), Some(second.id));
    }

    #[test]
    fn editing_content_does_not_change_id() {
        let mut note = Note::new(None, "original");
        let id = note.id.clone();
        note.set_content("edited body");
        note.tags.insert("todo".into());
        assert_eq!(note.id, id);
    }

    #[test]
    fn golden_replay_is_deterministic() {
        let steps = ["map rom", "disassemble reset vector", "label main"];
        let mut tip = None;
        let mut ids = Vec::new();

        for content in steps {
            let note = Note::new(tip.as_ref(), content);
            ids.push(note.id.to_string());
            tip = Some(note.id);
        }

        assert_eq!(
            ids,
            [
                "0000000000000G6MX6SW56W8JJ",
                "00000000000016Q7FK68C75XDV",
                "0000000000001PH81Q8ZKYJY1J",
            ]
        );
    }

    #[test]
    fn encode_decode_roundtrip_values() {
        for depth in [1u64, 2, 100, u64::MAX] {
            for hash in [0u64, 1, 12345, u64::MAX] {
                let value = ((depth as u128) << 64) | (hash as u128);
                let encoded = encode(value);
                let (decoded_depth, decoded_hash) = try_decode(&encoded).unwrap();
                assert_eq!(decoded_depth, depth, "depth for {encoded}");
                assert_eq!(decoded_hash, hash, "hash for {encoded}");
                assert_eq!(
                    encode(((decoded_depth as u128) << 64) | (decoded_hash as u128)),
                    encoded,
                    "re-encode for {encoded}"
                );
            }
        }
    }

    #[test]
    fn roundtrip_base32_encoding() {
        let id = NoteId::allocate(None, "hello");
        assert_eq!(id.as_str().len(), NoteId::ENCODED_LEN);
        let parsed = id.as_str().parse::<NoteId>().unwrap();
        assert_eq!(parsed, id);
        assert_eq!(parsed.depth(), id.depth());
        assert_eq!(parsed.hash(), id.hash());
    }

    #[test]
    fn deserialize_canonicalizes_input() {
        let id = NoteId::allocate(None, "hello");
        let trimmed = id.as_str().trim_start_matches('0');
        let parsed = trimmed.parse::<NoteId>().unwrap();
        assert_eq!(parsed, id);
    }

    #[test]
    fn two_notes_at_same_address_are_both_retrievable() {
        let mut db = NoteDb::default();
        let range = AddressRange::from(0..4);
        let human = db.create("human note on ISR");
        let llm = db.create("llm note on ISR");
        db.set_address(AddressSpace::Code, range, human.clone());
        db.set_address(AddressSpace::Code, range, llm.clone());

        let found = db.get_notes_inside(AddressSpace::Code, 0..4);
        assert_eq!(found.len(), 2);
        let ids: BTreeSet<_> = found.iter().map(|note| &note.id).collect();
        assert!(ids.contains(&human.id));
        assert!(ids.contains(&llm.id));
    }

    #[test]
    fn notes_near_orders_by_distance_and_reports_zero_inside() {
        let mut db = NoteDb::default();
        let a = db.create("note at 0x10");
        let b = db.create("note at 0x40");
        let c = db.create("far note at 0x200");
        db.set_address(AddressSpace::Code, AddressRange::from(0x10..0x14), a.clone());
        db.set_address(AddressSpace::Code, AddressRange::from(0x40..0x44), b.clone());
        db.set_address(AddressSpace::Code, AddressRange::from(0x200..0x204), c.clone());

        // Probe inside `a`'s range, with a window that reaches `b` but not `c`.
        let near = db.notes_near(AddressSpace::Code, 0x12, 0x40);
        assert_eq!(near.len(), 2, "c is outside the window");
        assert_eq!(near[0].note.id, a.id);
        assert_eq!(near[0].distance, 0, "probe is inside a");
        assert_eq!(near[1].note.id, b.id);
        assert_eq!(near[1].distance, 0x40 - 0x12);

        // A different space has no notes.
        assert!(db.notes_near(AddressSpace::Xdata, 0x12, 0x1000).is_empty());
    }

    #[test]
    fn search_matches_content_tags_and_is_case_insensitive() {
        let mut db = NoteDb::default();
        let mut isr = db.create("Reset Vector handler");
        isr.tags.insert("isr".into());
        db.notes.insert(isr.clone());
        db.create("loop body");

        assert_eq!(db.search("reset").len(), 1);
        assert_eq!(db.search("ISR").len(), 1, "tag match, case-insensitive");
        assert_eq!(db.search("body").len(), 1);
        assert_eq!(db.search("").len(), 2, "empty query matches all");
        assert!(db.search("nonexistent").is_empty());
    }

    #[test]
    fn note_db_shares_namespace_between_indexes() {
        let mut db = NoteDb::default();
        let note = db.create("global and local");
        let id = note.id.clone();
        db.global
            .insert(NotePath::parse("project.vt420").unwrap(), id.clone());
        db.address
            .entry(AddressSpace::Code)
            .or_default()
            .insert(AddressRange::from(0..4), id.clone());

        assert_eq!(db.get(&id).unwrap().content, "global and local");
        assert_eq!(
            db.get_by_path(&NotePath::parse("project.vt420").unwrap())
                .unwrap()
                .id,
            id
        );
        assert_eq!(db.get_notes_inside(AddressSpace::Code, 0..4).len(), 1);

        db.clear(&id);
        assert!(db.notes.is_empty());
        assert!(
            db.get_by_path(&NotePath::parse("project.vt420").unwrap())
                .is_none()
        );
        assert!(
            db.get_notes_overlapping(AddressSpace::Code, 0..4)
                .is_empty()
        );
    }
}
