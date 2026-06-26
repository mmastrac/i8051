use crate::store::error::DslError;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Int(u64),
    String(String),
    True,
    False,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    ColonColon,
    DotDot,
    Equals,
    Eof,
}

pub struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    pos: usize,
    peeked: Option<Result<Token, DslError>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            pos: 0,
            peeked: None,
        }
    }

    pub fn offset(&self) -> usize {
        self.pos
    }

    pub fn next_token(&mut self) -> Result<Token, DslError> {
        if let Some(token) = self.peeked.take() {
            return token;
        }
        self.skip_whitespace();
        let offset = self.pos;
        let Some(ch) = self.peek_char() else {
            return Ok(Token::Eof);
        };

        match ch {
            '(' => {
                self.bump();
                Ok(Token::LParen)
            }
            ')' => {
                self.bump();
                Ok(Token::RParen)
            }
            '[' => {
                self.bump();
                Ok(Token::LBracket)
            }
            ']' => {
                self.bump();
                Ok(Token::RBracket)
            }
            '{' => {
                self.bump();
                Ok(Token::LBrace)
            }
            '}' => {
                self.bump();
                Ok(Token::RBrace)
            }
            ',' => {
                self.bump();
                Ok(Token::Comma)
            }
            '=' => {
                self.bump();
                Ok(Token::Equals)
            }
            ':' => {
                self.bump();
                if self.peek_char() == Some(':') {
                    self.bump();
                    Ok(Token::ColonColon)
                } else {
                    Ok(Token::Colon)
                }
            }
            '.' => {
                self.bump();
                if self.peek_char() == Some('.') {
                    self.bump();
                    Ok(Token::DotDot)
                } else {
                    Err(DslError::new(offset, "unexpected '.'"))
                }
            }
            '"' => {
                self.bump();
                self.read_quoted_string(offset, 0)
            }
            'r' if self.raw_string_start() => self.read_raw_string(offset),
            '0'..='9' => self.read_number(offset),
            'a'..='z' | 'A'..='Z' | '_' => self.read_ident(offset),
            _ => Err(DslError::new(
                offset,
                format!("unexpected character {ch:?}"),
            )),
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(' ' | '\t' | '\n' | '\r')) {
            self.bump();
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn raw_string_start(&mut self) -> bool {
        let mut peek = self.chars.clone();
        peek.next();
        matches!(peek.next(), Some('"' | '#'))
    }

    fn read_ident(&mut self, _offset: usize) -> Result<Token, DslError> {
        let mut ident = String::new();
        while matches!(
            self.peek_char(),
            Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_')
        ) {
            ident.push(self.bump().unwrap());
        }
        match ident.as_str() {
            "True" => Ok(Token::True),
            "False" => Ok(Token::False),
            _ => Ok(Token::Ident(ident)),
        }
    }

    fn read_number(&mut self, offset: usize) -> Result<Token, DslError> {
        let mut text = String::new();
        if self.peek_char() == Some('0') {
            text.push(self.bump().unwrap());
            if matches!(self.peek_char(), Some('x' | 'X')) {
                text.push(self.bump().unwrap());
                while matches!(
                    self.peek_char(),
                    Some('0'..='9' | 'a'..='f' | 'A'..='F' | '_')
                ) {
                    let ch = self.bump().unwrap();
                    if ch != '_' {
                        text.push(ch);
                    }
                }
                if text.len() <= 2 {
                    return Err(DslError::new(offset, "expected hex digits"));
                }
                let value = u64::from_str_radix(&text[2..], 16)
                    .map_err(|_| DslError::new(offset, format!("invalid integer {text}")))?;
                return Ok(Token::Int(value));
            }
        }

        while matches!(self.peek_char(), Some('0'..='9' | '_')) {
            let ch = self.bump().unwrap();
            if ch != '_' {
                text.push(ch);
            }
        }

        if text.is_empty() {
            return Err(DslError::new(offset, "expected digits"));
        }

        let value = text
            .parse::<u64>()
            .map_err(|_| DslError::new(offset, format!("invalid integer {text}")))?;
        Ok(Token::Int(value))
    }

    fn read_quoted_string(&mut self, offset: usize, hashes: usize) -> Result<Token, DslError> {
        let mut value = String::new();
        loop {
            let Some(ch) = self.bump() else {
                return Err(DslError::new(offset, "unterminated string"));
            };
            match ch {
                '"' if hashes == 0 => return Ok(Token::String(value)),
                '"' => {
                    let mut end_hashes = 0;
                    while self.peek_char() == Some('#') {
                        end_hashes += 1;
                        self.bump();
                    }
                    if end_hashes == hashes {
                        return Ok(Token::String(value));
                    }
                    value.push('"');
                    for _ in 0..end_hashes {
                        value.push('#');
                    }
                }
                '\\' if hashes == 0 => {
                    let esc = self
                        .bump()
                        .ok_or_else(|| DslError::new(offset, "unterminated escape in string"))?;
                    value.push(match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        '0' => '\0',
                        'x' => {
                            let hi = self.read_hex_digit(offset)?;
                            let lo = self.read_hex_digit(offset)?;
                            char::from_u32((hi << 4 | lo) as u32)
                                .ok_or_else(|| DslError::new(offset, "invalid hex escape"))?
                        }
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }
    }

    fn read_raw_string(&mut self, offset: usize) -> Result<Token, DslError> {
        self.bump();
        let mut hashes = 0;
        while self.peek_char() == Some('#') {
            hashes += 1;
            self.bump();
        }
        if self.bump() != Some('"') {
            return Err(DslError::new(offset, "expected opening quote after r"));
        }
        self.read_quoted_string(offset, hashes)
    }

    fn read_hex_digit(&mut self, offset: usize) -> Result<u8, DslError> {
        let ch = self
            .bump()
            .ok_or_else(|| DslError::new(offset, "expected hex digit"))?;
        ch.to_digit(16)
            .map(|d| d as u8)
            .ok_or_else(|| DslError::new(offset, "expected hex digit"))
    }
}
