use std::collections::BTreeMap;

use crate::store::error::DslError;
use crate::store::lexer::{Lexer, Token};
use crate::store::value::{EnumArgs, Value};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<Token, DslError>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            peeked: None,
        }
    }

    fn bump(&mut self) -> Result<Token, DslError> {
        if let Some(token) = self.peeked.take() {
            token
        } else {
            self.lexer.next_token()
        }
    }

    fn peek(&mut self) -> Result<&Token, DslError> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next_token());
        }
        match &self.peeked {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err.clone()),
            None => unreachable!(),
        }
    }

    fn expect_eof(&mut self) -> Result<(), DslError> {
        match self.bump()? {
            Token::Eof => Ok(()),
            _ => Err(DslError::at(
                self.lexer.offset(),
                "unexpected trailing input",
            )),
        }
    }

    fn parse_value(&mut self) -> Result<Value, DslError> {
        match self.peek()? {
            Token::True => {
                self.bump()?;
                Ok(Value::Bool(true))
            }
            Token::False => {
                self.bump()?;
                Ok(Value::Bool(false))
            }
            Token::Int(_) => {
                let Token::Int(value) = self.bump()? else {
                    unreachable!()
                };
                Ok(Value::Int(value))
            }
            Token::String(_) => {
                let Token::String(value) = self.bump()? else {
                    unreachable!()
                };
                Ok(Value::String(value))
            }
            Token::LBracket => self.parse_list(),
            Token::LBrace => self.parse_brace(),
            Token::Ident(_) => self.parse_ident_value(),
            _ => {
                let offset = self.lexer.offset();
                let token = self.bump()?;
                Err(DslError::at(offset, format!("unexpected token {token:?}")))
            }
        }
    }

    fn parse_ident_value(&mut self) -> Result<Value, DslError> {
        let Token::Ident(first) = self.bump()? else {
            unreachable!()
        };
        self.parse_ident_tail(first)
    }

    /// Parse a value whose leading identifier has already been consumed. Used
    /// when distinguishing `ident=value` from a value like `DataType::Byte`
    /// requires looking past the identifier first.
    fn parse_ident_tail(&mut self, first: String) -> Result<Value, DslError> {
        if first == "None" {
            return Ok(Value::Null);
        }

        match self.peek()? {
            Token::ColonColon => {
                self.bump()?;
                let Token::Ident(variant) = self.bump()? else {
                    return Err(DslError::at(
                        self.lexer.offset(),
                        "expected enum variant name",
                    ));
                };
                let args = if matches!(self.peek()?, Token::LParen) {
                    self.parse_enum_args()?
                } else {
                    EnumArgs::Unit
                };
                Ok(Value::Enum {
                    type_name: first,
                    variant,
                    args,
                })
            }
            Token::Colon => self.parse_address(first),
            Token::LParen => {
                self.bump()?;
                let fields = self.parse_kwargs()?;
                self.expect(Token::RParen, "expected ')'")?;
                Ok(Value::Struct {
                    name: first,
                    fields,
                })
            }
            _ => Err(DslError::at(
                self.lexer.offset(),
                format!("unexpected token after identifier {first}"),
            )),
        }
    }

    fn parse_address(&mut self, space: String) -> Result<Value, DslError> {
        self.expect(Token::Colon, "expected ':' in address")?;
        if matches!(self.peek()?, Token::LBrace) {
            return self.parse_address_set(space);
        }
        let offset = self.parse_u64_value()?;
        if matches!(self.peek()?, Token::DotDot) {
            self.bump()?;
            let end = self.parse_u64_value()?;
            Ok(Value::AddressRange {
                space,
                start: offset,
                end,
            })
        } else {
            Ok(Value::Address { space, offset })
        }
    }

    fn parse_address_set(&mut self, space: String) -> Result<Value, DslError> {
        self.expect(Token::LBrace, "expected '{'")?;
        let mut ranges = Vec::new();
        if !matches!(self.peek()?, Token::RBrace) {
            loop {
                let start = self.parse_u64_value()?;
                let end = if matches!(self.peek()?, Token::DotDot) {
                    self.bump()?;
                    self.parse_u64_value()?
                } else {
                    start + 1
                };
                ranges.push((start, end));
                match self.peek()? {
                    Token::Comma => {
                        self.bump()?;
                        if matches!(self.peek()?, Token::RBrace) {
                            break;
                        }
                    }
                    Token::RBrace => break,
                    _ => {
                        return Err(DslError::at(
                            self.lexer.offset(),
                            "expected ',' or '}' in address set",
                        ));
                    }
                }
            }
        }
        self.expect(Token::RBrace, "expected '}'")?;
        Ok(Value::AddressSet { space, ranges })
    }

    fn parse_u64_value(&mut self) -> Result<u64, DslError> {
        match self.parse_value()? {
            Value::Int(value) => Ok(value),
            other => Err(DslError::at(
                self.lexer.offset(),
                format!("expected integer, got {other:?}"),
            )),
        }
    }

    fn parse_list(&mut self) -> Result<Value, DslError> {
        self.expect(Token::LBracket, "expected '['")?;
        let mut items = Vec::new();
        if !matches!(self.peek()?, Token::RBracket) {
            loop {
                items.push(self.parse_value()?);
                match self.peek()? {
                    Token::Comma => {
                        self.bump()?;
                    }
                    Token::RBracket => break,
                    _ => {
                        return Err(DslError::at(
                            self.lexer.offset(),
                            "expected ',' or ']' in list",
                        ));
                    }
                }
            }
        }
        self.expect(Token::RBracket, "expected ']'")?;
        Ok(Value::List(items))
    }

    fn parse_brace(&mut self) -> Result<Value, DslError> {
        self.expect(Token::LBrace, "expected '{{'")?;
        if matches!(self.peek()?, Token::RBrace) {
            self.bump()?;
            return Ok(Value::Set(Vec::new()));
        }

        let first = self.parse_value()?;
        match self.peek()? {
            Token::Colon => {
                let mut map = BTreeMap::new();
                map.insert(self.key_from_value(first)?, self.parse_value_after_colon()?);
                while matches!(self.peek()?, Token::Comma) {
                    self.bump()?;
                    if matches!(self.peek()?, Token::RBrace) {
                        break;
                    }
                    let key = self.parse_map_key()?;
                    self.expect(Token::Colon, "expected ':' in map")?;
                    map.insert(key, self.parse_value()?);
                }
                self.expect(Token::RBrace, "expected '}}'")?;
                Ok(Value::Map(map))
            }
            Token::Comma | Token::RBrace => {
                let mut set = vec![first];
                while matches!(self.peek()?, Token::Comma) {
                    self.bump()?;
                    if matches!(self.peek()?, Token::RBrace) {
                        break;
                    }
                    set.push(self.parse_value()?);
                }
                self.expect(Token::RBrace, "expected '}}'")?;
                Ok(Value::Set(set))
            }
            _ => Err(DslError::at(
                self.lexer.offset(),
                "expected ':' or ',' in brace expression",
            )),
        }
    }

    fn parse_map_key(&mut self) -> Result<String, DslError> {
        let value = self.parse_value()?;
        self.key_from_value(value)
    }

    fn key_from_value(&self, value: Value) -> Result<String, DslError> {
        match value {
            Value::String(s) => Ok(s),
            Value::Int(v) => Ok(v.to_string()),
            other => Err(DslError::new(format!("invalid map key {other:?}"))),
        }
    }

    fn parse_value_after_colon(&mut self) -> Result<Value, DslError> {
        self.expect(Token::Colon, "expected ':'")?;
        self.parse_value()
    }

    fn parse_enum_args(&mut self) -> Result<EnumArgs, DslError> {
        self.expect(Token::LParen, "expected '('")?;
        if matches!(self.peek()?, Token::RParen) {
            self.bump()?;
            return Ok(EnumArgs::Unit);
        }

        // Named (`field=value`) and positional args both can start with an
        // identifier (`label=...` vs `DataType::Byte`), so decide by peeking at
        // the token after a leading identifier.
        let first = if matches!(self.peek()?, Token::Ident(_)) {
            let Token::Ident(name) = self.bump()? else {
                unreachable!()
            };
            if matches!(self.peek()?, Token::Equals) {
                let mut fields = BTreeMap::new();
                fields.insert(name, self.parse_value_after_equals()?);
                while matches!(self.peek()?, Token::Comma) {
                    self.bump()?;
                    if matches!(self.peek()?, Token::RParen) {
                        break;
                    }
                    let key = self.parse_kwarg_key()?;
                    fields.insert(key, self.parse_value_after_equals()?);
                }
                self.expect(Token::RParen, "expected ')'")?;
                return Ok(EnumArgs::Named(fields));
            }
            self.parse_ident_tail(name)?
        } else {
            self.parse_value()?
        };

        let mut args = vec![first];
        while matches!(self.peek()?, Token::Comma) {
            self.bump()?;
            if matches!(self.peek()?, Token::RParen) {
                break;
            }
            args.push(self.parse_value()?);
        }
        self.expect(Token::RParen, "expected ')'")?;
        Ok(EnumArgs::Positional(args))
    }

    fn parse_kwargs(&mut self) -> Result<BTreeMap<String, Value>, DslError> {
        let mut fields = BTreeMap::new();
        if matches!(self.peek()?, Token::RParen) {
            return Ok(fields);
        }
        loop {
            let key = self.parse_kwarg_key()?;
            fields.insert(key, self.parse_value_after_equals()?);
            match self.peek()? {
                Token::Comma => {
                    self.bump()?;
                    if matches!(self.peek()?, Token::RParen) {
                        break;
                    }
                }
                Token::RParen => break,
                _ => {
                    return Err(DslError::at(
                        self.lexer.offset(),
                        "expected ',' or ')' in argument list",
                    ));
                }
            }
        }
        Ok(fields)
    }

    fn parse_kwarg_key(&mut self) -> Result<String, DslError> {
        match self.bump()? {
            Token::Ident(name) => Ok(name),
            other => Err(DslError::at(
                self.lexer.offset(),
                format!("expected keyword argument name, got {other:?}"),
            )),
        }
    }

    fn parse_value_after_equals(&mut self) -> Result<Value, DslError> {
        self.expect(Token::Equals, "expected '='")?;
        self.parse_value()
    }

    fn expect(&mut self, expected: Token, message: &str) -> Result<(), DslError> {
        let offset = self.lexer.offset();
        let token = self.bump()?;
        if std::mem::discriminant(&token) == std::mem::discriminant(&expected) {
            Ok(())
        } else {
            Err(DslError::at(offset, message))
        }
    }
}

pub fn parse_command(input: &str) -> Result<Value, DslError> {
    let mut parser = Parser::new(input);
    let value = parser.parse_value()?;
    parser.expect_eof()?;
    match value {
        Value::Struct { name, fields } => Ok(Value::Call {
            name: snake_case(&name),
            kwargs: fields,
        }),
        Value::Call { .. } => Ok(value),
        other => Err(DslError::new(format!(
            "expected command call, got {other:?}"
        ))),
    }
}

/// Parse a single bare DSL value (e.g. an address `CODE:0x84`, a range, or a
/// string), used for the standalone value entry points like
/// [`from_dsl_value`](super::from_dsl_value).
pub fn parse_value(input: &str) -> Result<Value, DslError> {
    let mut parser = Parser::new(input);
    let value = parser.parse_value()?;
    parser.expect_eof()?;
    Ok(value)
}

fn snake_case(name: &str) -> String {
    let mut out = String::new();
    for (i, ch) in name.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 {
                out.push('_');
            }
            out.extend(ch.to_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_auto_disassemble() {
        let value = parse_command("auto_disassemble(address=CODE:0x1234)").unwrap();
        assert_eq!(
            value,
            Value::Call {
                name: "auto_disassemble".into(),
                kwargs: BTreeMap::from([(
                    "address".into(),
                    Value::Address {
                        space: "CODE".into(),
                        offset: 0x1234,
                    }
                )]),
            }
        );
    }

    #[test]
    fn parse_address_range() {
        let value = parse_command("clear_bytes(range=CODE:0x10..0x20)").unwrap();
        assert!(matches!(
            value.field("range"),
            Some(Value::AddressRange { .. })
        ));
    }
}
