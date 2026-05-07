use std::{iter::Peekable, str::CharIndices};

use crate::token::{self, Token, TokenType};

type StepRead = (usize, char);

pub struct Lexer<'a> {
    original_input: &'a str,
    input: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            original_input: input,
            input: input.char_indices().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        let maybe_read = self.skip_white_space_and_read_char();
        if maybe_read.is_none() {
            return Token::eof();
        }
        let read = maybe_read.unwrap();
        let (start_index, mut ch) = read;
        let end_index = start_index;
        match ch {
            '?' => self.create_token(TokenType::QuestionMark, start_index, end_index),
            ',' => self.create_token(TokenType::Comma, start_index, end_index),
            ';' => self.create_token(TokenType::Semicolon, start_index, end_index),
            '.' => self.create_token(TokenType::Dot, start_index, end_index),
            ':' => self.create_token(TokenType::Colon, start_index, end_index),
            '(' => self.create_token(TokenType::Lparen, start_index, end_index),
            ')' => self.create_token(TokenType::Rparen, start_index, end_index),
            '[' => self.create_token(TokenType::Lsquare, start_index, end_index),
            ']' => self.create_token(TokenType::Rsquare, start_index, end_index),
            '{' => self.create_token(TokenType::Lsquirly, start_index, end_index),
            '}' => self.create_token(TokenType::Rsquirly, start_index, end_index),
            '/' => self.create_token(TokenType::Slash, start_index, end_index),
            '\\' => self.create_token(TokenType::BackSlash, start_index, end_index),
            '\'' => {
                if let Some(read) = self.read_char() {
                    (_, ch) = read;
                    if ch == '\\' {
                        if self.read_char().is_none() {
                            return self.create_token(TokenType::Illegal, start_index, end_index);
                        }
                    }
                    if let Some((end_index, '\'')) = self.read_char() {
                        self.create_token(TokenType::Char, start_index, end_index)
                    } else {
                        self.create_token(TokenType::Illegal, start_index, end_index)
                    }
                } else {
                    Token::illegal(start_index, start_index, "\\")
                }
            }
            '"' => self.read_string(read),
            '-' => self.create_token(TokenType::Minus, start_index, end_index),
            '+' => self.create_token(TokenType::Plus, start_index, end_index),
            '*' => self.create_token(TokenType::Asterisk, start_index, end_index),
            '&' => {
                if let Some(&(end_index, '&')) = self.peek_char() {
                    self.read_char();
                    self.create_token(TokenType::And, start_index, end_index)
                } else {
                    self.create_token(TokenType::BitAnd, start_index, end_index)
                }
            }
            '|' => {
                if let Some(&(end_index, '|')) = self.peek_char() {
                    self.read_char();
                    self.create_token(TokenType::Or, start_index, end_index)
                } else {
                    self.create_token(TokenType::BitOr, start_index, end_index)
                }
            }
            '=' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    self.create_token(TokenType::Eq, start_index, end_index)
                } else {
                    self.create_token(TokenType::Assign, start_index, end_index)
                }
            }
            '!' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    self.create_token(TokenType::Neq, start_index, end_index)
                } else {
                    self.create_token(TokenType::Bang, start_index, end_index)
                }
            }
            '>' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    self.create_token(TokenType::Ge, start_index, end_index)
                } else {
                    self.create_token(TokenType::Gt, start_index, end_index)
                }
            }
            '<' => {
                if let Some(&(end_index, '=')) = self.peek_char() {
                    self.read_char();
                    self.create_token(TokenType::Le, start_index, end_index)
                } else {
                    self.create_token(TokenType::Lt, start_index, end_index)
                }
            }
            ch if ch.is_ascii_digit() => self.read_number(read),
            _ => self.read_ident(read),
        }
    }

    fn skip_white_space_and_read_char(&mut self) -> Option<StepRead> {
        while let Some(step_read) = self.read_char() {
            let ch = step_read.1;
            if !ch.is_whitespace() {
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
    fn read_string(&mut self, step_read: StepRead) -> Token<'a> {
        let (quote_index, quote) = step_read;
        let start_index = quote_index + 1;
        let mut end_index = start_index;
        let mut backslash_found = false;
        while let Some(step_read) = self.read_char() {
            let ch;
            (end_index, ch) = step_read;
            if ch == '\\' && !backslash_found {
                backslash_found = true;
            } else if ch != quote || backslash_found {
                backslash_found = false;
            } else {
                break;
            }
        }
        self.create_token(TokenType::String, start_index, end_index)
    }

    #[inline]
    fn create_token(&self, _type: TokenType, start_index: usize, end_index: usize) -> Token<'a> {
        let slice = &self.original_input[start_index..=end_index];
        Token::new(_type, start_index, end_index, slice)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token {
                _type: TokenType::Eof,
                ..
            } => None,
            default => Some(default),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
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
            let new_token = lex.next_token();
            assert_eq!(expected_type, new_token._type);
            assert_eq!(expected_slice, new_token.slice);
        }
    }
}
