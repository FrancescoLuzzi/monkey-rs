use std::{iter::Peekable, str::CharIndices};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::token::{self, Token, TokenType};

type StepRead = (usize, char);
type LexResult<'a> = Result<Token<'a>, LexerError>;

#[derive(Debug, Error, Diagnostic)]
pub enum LexerError {
    #[error("unexpected character `{found}`")]
    #[diagnostic(
        code(monkey::lexer::unexpected_character),
        help("remove it or replace it with a valid Monkey token")
    )]
    UnexpectedCharacter {
        found: char,
        #[source_code]
        src: NamedSource<String>,
        #[label("this character is not valid Monkey syntax")]
        span: SourceSpan,
    },

    #[error("unterminated string literal")]
    #[diagnostic(
        code(monkey::lexer::unterminated_string),
        help("add a closing `\"` to finish the string literal")
    )]
    UnterminatedString {
        #[source_code]
        src: NamedSource<String>,
        #[label("string starts here but never closes")]
        span: SourceSpan,
    },

    #[error("unterminated character literal")]
    #[diagnostic(
        code(monkey::lexer::unterminated_char),
        help("character literals must end with a closing `'`")
    )]
    UnterminatedCharacter {
        #[source_code]
        src: NamedSource<String>,
        #[label("character literal starts here but never closes")]
        span: SourceSpan,
    },

    #[error("empty character literal")]
    #[diagnostic(
        code(monkey::lexer::empty_char),
        help("character literals must contain exactly one character or escape sequence")
    )]
    EmptyCharacter {
        #[source_code]
        src: NamedSource<String>,
        #[label("there is no character between these quotes")]
        span: SourceSpan,
    },

    #[error("character literal must contain exactly one character")]
    #[diagnostic(
        code(monkey::lexer::char_too_long),
        help("use a string literal if you need more than one character")
    )]
    CharacterTooLong {
        #[source_code]
        src: NamedSource<String>,
        #[label("this literal contains more than one character")]
        span: SourceSpan,
    },
}

pub struct Lexer<'a> {
    original_input: &'a str,
    input: Peekable<CharIndices<'a>>,
    peeked: Option<LexResult<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            original_input: input,
            input: input.char_indices().peekable(),
            peeked: None,
        }
    }

    pub fn source(&self) -> &'a str {
        self.original_input
    }

    pub fn next_token(&mut self) -> LexResult<'a> {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        self.read_token()
    }

    pub fn peek_token(&mut self) -> Result<&Token<'a>, &LexerError> {
        if self.peeked.is_none() {
            self.peeked = Some(self.read_token());
        }

        self.peeked
            .as_ref()
            .expect("peeked token must be populated")
            .as_ref()
    }

    fn read_token(&mut self) -> LexResult<'a> {
        let Some(read) = self.skip_white_space_and_read_char() else {
            return Ok(Token::eof());
        };

        let (start_index, ch) = read;
        let end_index = start_index;

        match ch {
            '?' => Ok(self.create_token(TokenType::QuestionMark, start_index, end_index)),
            ',' => Ok(self.create_token(TokenType::Comma, start_index, end_index)),
            ';' => Ok(self.create_token(TokenType::Semicolon, start_index, end_index)),
            '.' => Ok(self.create_token(TokenType::Dot, start_index, end_index)),
            ':' => Ok(self.create_token(TokenType::Colon, start_index, end_index)),
            '(' => Ok(self.create_token(TokenType::Lparen, start_index, end_index)),
            ')' => Ok(self.create_token(TokenType::Rparen, start_index, end_index)),
            '[' => Ok(self.create_token(TokenType::Lsquare, start_index, end_index)),
            ']' => Ok(self.create_token(TokenType::Rsquare, start_index, end_index)),
            '{' => Ok(self.create_token(TokenType::Lsquirly, start_index, end_index)),
            '}' => Ok(self.create_token(TokenType::Rsquirly, start_index, end_index)),
            '/' => Ok(self.create_token(TokenType::Slash, start_index, end_index)),
            '\\' => Ok(self.create_token(TokenType::BackSlash, start_index, end_index)),
            '\'' => self.read_char_literal(start_index),
            '"' => self.read_string(read),
            '-' => Ok(self.create_token(TokenType::Minus, start_index, end_index)),
            '+' => Ok(self.create_token(TokenType::Plus, start_index, end_index)),
            '*' => Ok(self.create_token(TokenType::Asterisk, start_index, end_index)),
            '&' => {
                if let Some(&(end_index, '&')) = self.peek_char() {
                    self.read_char();
                    Ok(self.create_token(TokenType::And, start_index, end_index))
                } else {
                    Ok(self.create_token(TokenType::BitAnd, start_index, end_index))
                }
            }
            '|' => {
                if let Some(&(end_index, '|')) = self.peek_char() {
                    self.read_char();
                    Ok(self.create_token(TokenType::Or, start_index, end_index))
                } else {
                    Ok(self.create_token(TokenType::BitOr, start_index, end_index))
                }
            }
            '=' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    Ok(self.create_token(TokenType::Eq, start_index, end_index))
                } else {
                    Ok(self.create_token(TokenType::Assign, start_index, end_index))
                }
            }
            '!' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    Ok(self.create_token(TokenType::Neq, start_index, end_index))
                } else {
                    Ok(self.create_token(TokenType::Bang, start_index, end_index))
                }
            }
            '>' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    Ok(self.create_token(TokenType::Ge, start_index, end_index))
                } else {
                    Ok(self.create_token(TokenType::Gt, start_index, end_index))
                }
            }
            '<' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    Ok(self.create_token(TokenType::Le, start_index, end_index))
                } else {
                    Ok(self.create_token(TokenType::Lt, start_index, end_index))
                }
            }
            ch if ch.is_ascii_digit() => Ok(self.read_number(read)),
            ch if is_ident_start(ch) => Ok(self.read_ident(read)),
            _ => Err(self.unexpected_character(start_index, ch)),
        }
    }

    fn skip_white_space_and_read_char(&mut self) -> Option<StepRead> {
        while let Some(step_read) = self.read_char() {
            if !step_read.1.is_whitespace() {
                return Some(step_read);
            }
        }
        None
    }

    #[inline]
    fn read_char(&mut self) -> Option<StepRead> {
        self.input.next()
    }

    #[inline]
    fn peek_char(&mut self) -> Option<&StepRead> {
        self.input.peek()
    }

    #[inline]
    fn read_ident(&mut self, first_read: StepRead) -> Token<'a> {
        let (start_index, _) = first_read;
        let mut end_index = start_index;
        while let Some(&step_read) = self.peek_char() {
            let (next_index, ch) = step_read;
            if ch.is_alphanumeric() || ch == '_' {
                end_index = next_index;
                self.read_char();
            } else {
                break;
            }
        }
        let slice = &self.original_input[start_index..=end_index];
        let token_type = token::lookup_ident_token_type(slice);
        Token::new(token_type, start_index, end_index, slice)
    }

    #[inline]
    fn read_number(&mut self, first_read: StepRead) -> Token<'a> {
        let (start_index, _) = first_read;
        let mut end_index = start_index;
        let mut has_dot = false;
        while let Some(&step_read) = self.peek_char() {
            let (next_index, ch) = step_read;
            if ch.is_ascii_digit() {
                end_index = next_index;
                self.read_char();
            } else if ch == '.' && !has_dot {
                let mut lookahead = self.input.clone();
                lookahead.next();
                if let Some((digit_index, digit)) = lookahead.next() {
                    if digit.is_ascii_digit() {
                        has_dot = true;
                        self.read_char();
                        self.read_char();
                        end_index = digit_index;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        let token_type = if has_dot {
            TokenType::Floating
        } else {
            TokenType::Integer
        };
        self.create_token(token_type, start_index, end_index)
    }

    #[inline]
    fn read_string(&mut self, (quote_index, quote): StepRead) -> LexResult<'a> {
        let start_index = quote_index + quote.len_utf8();
        while let Some((index, ch)) = self.read_char() {
            match ch {
                '\\' => {
                    if self.read_char().is_none() {
                        return Err(self.unterminated_string(quote_index));
                    }
                }
                current if current == quote => {
                    return Ok(self.create_token(TokenType::String, start_index, index));
                }
                _ => {}
            }
        }

        Err(self.unterminated_string(quote_index))
    }

    #[inline]
    fn read_char_literal(&mut self, quote_index: usize) -> LexResult<'a> {
        let Some((value_index, value)) = self.read_char() else {
            return Err(self.unterminated_character(quote_index));
        };

        if value == '\'' {
            return Err(self.empty_character(quote_index, value_index));
        }

        if value == '\\' {
            if self.read_char().is_none() {
                return Err(self.unterminated_character(quote_index));
            };
        }

        match self.read_char() {
            Some((end_index, '\'')) => {
                Ok(self.create_token(TokenType::Char, quote_index, end_index))
            }
            Some((next_index, _)) => Err(self.character_too_long(quote_index, next_index)),
            None => Err(self.unterminated_character(quote_index)),
        }
    }

    #[inline]
    fn create_token(&self, _type: TokenType, start_index: usize, end_index: usize) -> Token<'a> {
        let slice = &self.original_input[start_index..=end_index];
        Token::new(_type, start_index, end_index, slice)
    }

    fn unexpected_character(&self, index: usize, found: char) -> LexerError {
        LexerError::UnexpectedCharacter {
            found,
            src: named_source(self.original_input),
            span: inclusive_span(index, index),
        }
    }

    fn unterminated_string(&self, quote_index: usize) -> LexerError {
        LexerError::UnterminatedString {
            src: named_source(self.original_input),
            span: span_to_end(self.original_input, quote_index),
        }
    }

    fn unterminated_character(&self, quote_index: usize) -> LexerError {
        LexerError::UnterminatedCharacter {
            src: named_source(self.original_input),
            span: span_to_end(self.original_input, quote_index),
        }
    }

    fn empty_character(&self, start_index: usize, end_index: usize) -> LexerError {
        LexerError::EmptyCharacter {
            src: named_source(self.original_input),
            span: inclusive_span(start_index, end_index),
        }
    }

    fn character_too_long(&self, start_index: usize, end_index: usize) -> LexerError {
        LexerError::CharacterTooLong {
            src: named_source(self.original_input),
            span: inclusive_span(start_index, end_index),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token {
                _type: TokenType::Eof,
                ..
            }) => None,
            result => Some(result),
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn named_source(input: &str) -> NamedSource<String> {
    NamedSource::new("input", input.to_string())
}

fn inclusive_span(start: usize, end: usize) -> SourceSpan {
    (start, end.saturating_sub(start) + 1).into()
}

fn span_to_end(input: &str, start: usize) -> SourceSpan {
    let len = input.len().saturating_sub(start).max(1);
    (start, len).into()
}

#[cfg(test)]
mod test {
    use crate::lexer::{Lexer, LexerError};
    use crate::token::TokenType;

    #[test]
    fn test_tokenizer() {
        let input = r#"
        let x = 5;
        let y = x + (4 - 3) * 2.2 / 1;
        fn test(a,b,c){
            if a {
                return a;
            }else if b {
                return b;
            } else {
                return c;
            }
        }
        let second_test = fn(a, b) {
            a & b | 1
        }
        test(' ',x,y) || false && true;
        let s_test = "test";
        let s_test = "\"test";
        [] >= <= != ! == > < \ && ||
        null
        "#;
        let expected_tokens = vec![
            // let x = 5;
            (TokenType::Let, "let"),
            (TokenType::Ident, "x"),
            (TokenType::Assign, "="),
            (TokenType::Integer, "5"),
            (TokenType::Semicolon, ";"),
            // let y = x + (4 - 3) * 2.2 / 1;
            (TokenType::Let, "let"),
            (TokenType::Ident, "y"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Lparen, "("),
            (TokenType::Integer, "4"),
            (TokenType::Minus, "-"),
            (TokenType::Integer, "3"),
            (TokenType::Rparen, ")"),
            (TokenType::Asterisk, "*"),
            (TokenType::Floating, "2.2"),
            (TokenType::Slash, "/"),
            (TokenType::Integer, "1"),
            (TokenType::Semicolon, ";"),
            // fn test(a,b,c){
            //    if a {
            //        return a;
            //    }else if b {
            //        return b;
            //    } else {
            //        return c;
            //    }
            // }
            (TokenType::Function, "fn"),
            (TokenType::Ident, "test"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "a"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "b"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "c"),
            (TokenType::Rparen, ")"),
            (TokenType::Lsquirly, "{"),
            (TokenType::If, "if"),
            (TokenType::Ident, "a"),
            (TokenType::Lsquirly, "{"),
            (TokenType::Return, "return"),
            (TokenType::Ident, "a"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rsquirly, "}"),
            (TokenType::Else, "else"),
            (TokenType::If, "if"),
            (TokenType::Ident, "b"),
            (TokenType::Lsquirly, "{"),
            (TokenType::Return, "return"),
            (TokenType::Ident, "b"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rsquirly, "}"),
            (TokenType::Else, "else"),
            (TokenType::Lsquirly, "{"),
            (TokenType::Return, "return"),
            (TokenType::Ident, "c"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rsquirly, "}"),
            (TokenType::Rsquirly, "}"),
            // let second_test = fn(a, b) {
            //     a & b | 1
            // }
            (TokenType::Let, "let"),
            (TokenType::Ident, "second_test"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "a"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "b"),
            (TokenType::Rparen, ")"),
            (TokenType::Lsquirly, "{"),
            (TokenType::Ident, "a"),
            (TokenType::BitAnd, "&"),
            (TokenType::Ident, "b"),
            (TokenType::BitOr, "|"),
            (TokenType::Integer, "1"),
            (TokenType::Rsquirly, "}"),
            // test(true,x,y) || false && true;
            (TokenType::Ident, "test"),
            (TokenType::Lparen, "("),
            (TokenType::Char, "' '"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::Rparen, ")"),
            (TokenType::Or, "||"),
            (TokenType::False, "false"),
            (TokenType::And, "&&"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            // let s_test = "test";
            (TokenType::Let, "let"),
            (TokenType::Ident, "s_test"),
            (TokenType::Assign, "="),
            (TokenType::String, "test\""),
            (TokenType::Semicolon, ";"),
            // let s_test = "\"test";
            (TokenType::Let, "let"),
            (TokenType::Ident, "s_test"),
            (TokenType::Assign, "="),
            (TokenType::String, "\\\"test\""),
            (TokenType::Semicolon, ";"),
            // [] >= <= != ! == > < \ && ||
            (TokenType::Lsquare, "["),
            (TokenType::Rsquare, "]"),
            (TokenType::Ge, ">="),
            (TokenType::Le, "<="),
            (TokenType::Neq, "!="),
            (TokenType::Bang, "!"),
            (TokenType::Eq, "=="),
            (TokenType::Gt, ">"),
            (TokenType::Lt, "<"),
            (TokenType::BackSlash, "\\"),
            (TokenType::And, "&&"),
            (TokenType::Or, "||"),
            // null
            (TokenType::Null, "null"),
            // end
            (TokenType::Eof, "EOF"),
        ];
        let mut lex = Lexer::new(input);
        for (expected_type, expected_slice) in expected_tokens {
            let new_token = lex
                .next_token()
                .expect("tokenizing test input should succeed");
            assert_eq!(expected_type, new_token._type);
            assert_eq!(expected_slice, new_token.slice);
        }
    }

    #[test]
    fn reports_unexpected_character() {
        let error = Lexer::new("@")
            .next_token()
            .expect_err("`@` should be rejected");
        assert!(matches!(
            error,
            LexerError::UnexpectedCharacter { found: '@', .. }
        ));
    }

    #[test]
    fn reports_unterminated_string() {
        let error = Lexer::new("\"unterminated")
            .next_token()
            .expect_err("unterminated strings should error");
        assert!(matches!(error, LexerError::UnterminatedString { .. }));
    }

    #[test]
    fn peek_token_does_not_advance() {
        let mut lexer = Lexer::new("let value = 1");

        let peeked = lexer.peek_token().expect("peek should succeed");
        assert_eq!(peeked._type, TokenType::Let);
        assert_eq!(peeked.slice, "let");

        let consumed = lexer.next_token().expect("next should reuse peeked token");
        assert_eq!(consumed._type, TokenType::Let);
        assert_eq!(consumed.slice, "let");

        let second_peek = lexer.peek_token().expect("second peek should succeed");
        assert_eq!(second_peek._type, TokenType::Ident);
        assert_eq!(second_peek.slice, "value");
    }
}
