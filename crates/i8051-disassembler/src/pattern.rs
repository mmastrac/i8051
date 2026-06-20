use std::range::Range;

#[derive(Debug, Clone)]
pub struct BytePattern {
    regex: regex::bytes::Regex,
}

impl BytePattern {
    /// Parses a byte pattern.
    ///
    ///  - `00 11 22` -> matches `0x00 0x11 0x22`
    ///  - `?? ?? ??` -> matches any three bytes
    pub fn new(pattern: &str) -> Result<Self, String> {
        Ok(Self {
            regex: compile_pattern(pattern)?,
        })
    }

    pub fn find(&self, data: &[u8]) -> Option<usize> {
        self.regex.find(data).map(|m| m.start())
    }

    pub fn find_all(&self, data: &[u8]) -> impl Iterator<Item = Range<usize>> {
        self.regex
            .find_iter(data)
            .map(|m| (m.start()..m.end()).into())
    }
}

fn compile_pattern(pattern: &str) -> Result<regex::bytes::Regex, String> {
    let mut regex = String::with_capacity(pattern.len());
    regex.push_str("(?s-u)");
    for part in pattern.split_whitespace() {
        if part == "??" {
            regex.push_str(".");
        } else if let Ok(byte) = u8::from_str_radix(part, 16) {
            regex.push_str(&format!(r#"\x{:02X}"#, byte));
        } else {
            return Err(format!("invalid byte pattern: {}", part));
        }
    }

    regex::bytes::Regex::new(&regex).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn test_byte_pattern_short() {
        let pattern = BytePattern::new("11 22 33").unwrap();
        assert_eq!(pattern.find(&hex!("11 22 33")), Some(0));
        assert_eq!(pattern.find(&hex!("00 11 22 33")), Some(1));
        assert_eq!(pattern.find(&hex!("01 02 03 44")), None);
    }

    #[test]
    fn test_byte_pattern_long() {
        let pattern = BytePattern::new("12 ?? ?? e4 93 fa a3 e4 93 f5 83 8a 82 e4 73").unwrap();
        assert_eq!(
            pattern.find(&hex!("12 01 02 e4 93 fa a3 e4 93 f5 83 8a 82 e4 73")),
            Some(0)
        );
        assert_eq!(
            pattern.find(&hex!("00 12 01 02 e4 93 fa a3 e4 93 f5 83 8a 82 e4 73")),
            Some(1)
        );
        assert_eq!(
            pattern.find(&hex!("01 02 e4 93 fa a3 e4 93 f5 83 8a 82 e4 73")),
            None
        );
    }
}
