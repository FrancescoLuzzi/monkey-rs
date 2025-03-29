use std::{iter::Peekable, str::Chars};

use crate::token::{self, Token};

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        let ch = self.skip_white_space_and_read_char();
        if ch.is_none() {
            return Token::Eof;
        }
        let ch = ch.unwrap();
        match ch {
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '[' => Token::Lsquare,
            ']' => Token::Rsquare,
            '{' => Token::Lsquirly,
            '}' => Token::Rsquirly,
            '/' => Token::Slash,
            '\\' => Token::BackSlash,
            '\'' => {
                if let Some(ch) = self.read_char() {
                    let t = Token::Char(ch);
                    if let Some('\'') = self.read_char() {
                        t
                    } else {
                        Token::Illegal
                    }
                } else {
                    Token::Illegal
                }
            }
            '"' => self.read_string(ch),
            '-' => Token::Minus,
            '+' => Token::Plus,
            '*' => Token::Asterisk,
            '.' => Token::Dot,
            '&' => {
                if let Some('&') = self.peek_char() {
                    self.read_char();
                    Token::And
                } else {
                    Token::BitAnd
                }
            }
            '|' => {
                if let Some('|') = self.peek_char() {
                    self.read_char();
                    Token::Or
                } else {
                    Token::BitOr
                }
            }
            '=' => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            }
            '>' => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Ge
                } else {
                    Token::Gt
                }
            }
            '<' => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Le
                } else {
                    Token::Lt
                }
            }
            ch if ch.is_ascii_digit() => self.read_number(ch),
            ch => self.read_ident(ch),
        }
    }

    fn skip_white_space_and_read_char(&mut self) -> Option<char> {
        while let Some(ch) = self.read_char() {
            if !ch.is_whitespace() {
                return Some(ch);
            }
        }
        None
    }

    #[inline]
    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    #[inline]
    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    #[inline]
    fn read_ident(&mut self, first_char: char) -> Token {
        let mut buf = String::from(first_char);
        while let Some(&ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' {
                self.read_char();
                buf.push(ch);
            } else {
                break;
            }
        }
        token::lookup_ident(buf)
    }

    #[inline]
    fn read_number(&mut self, first_digit: char) -> Token {
        let mut buf = String::from(first_digit);
        let mut has_dot = false;
        while let Some(&ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.read_char();
                buf.push(ch);
            } else if ch == '.' {
                self.read_char();
                buf.push(ch);
                has_dot = true;
            } else {
                break;
            }
        }
        if has_dot {
            match buf.parse::<f64>() {
                Ok(n) => Token::Floating(n),
                Err(_) => Token::Illegal,
            }
        } else {
            match buf.parse::<i64>() {
                Ok(n) => Token::Integer(n),
                Err(_) => Token::Illegal,
            }
        }
    }

    #[inline]
    fn read_string(&mut self, quote: char) -> Token {
        let mut buf = String::new();
        let mut backslash_found = false;
        while let Some(ch) = self.read_char() {
            if ch == '\\' && !backslash_found {
                backslash_found = true;
            } else if ch != quote || backslash_found {
                buf.push(ch);
                backslash_found = false;
            } else {
                break;
            }
        }
        Token::String(buf)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
            default => Some(default),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::Token;

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
        [] >= <= != ! == > < \
        "#;
        let expected_tokens = vec![
            // let x = 5;
            Token::Let,
            Token::Ident("x".into()),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            // let y = x + (4 - 3) * 2.2 / 1;
            Token::Let,
            Token::Ident("y".into()),
            Token::Assign,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Lparen,
            Token::Integer(4),
            Token::Minus,
            Token::Integer(3),
            Token::Rparen,
            Token::Asterisk,
            Token::Floating(2.2),
            Token::Slash,
            Token::Integer(1),
            Token::Semicolon,
            // fn test(a,b,c){
            //    if a {
            //        return a;
            //    }else if b {
            //        return b;
            //    } else {
            //        return c;
            //    }
            // }
            Token::Function,
            Token::Ident("test".into()),
            Token::Lparen,
            Token::Ident("a".into()),
            Token::Comma,
            Token::Ident("b".into()),
            Token::Comma,
            Token::Ident("c".into()),
            Token::Rparen,
            Token::Lsquirly,
            Token::If,
            Token::Ident("a".into()),
            Token::Lsquirly,
            Token::Return,
            Token::Ident("a".into()),
            Token::Semicolon,
            Token::Rsquirly,
            Token::Else,
            Token::If,
            Token::Ident("b".into()),
            Token::Lsquirly,
            Token::Return,
            Token::Ident("b".into()),
            Token::Semicolon,
            Token::Rsquirly,
            Token::Else,
            Token::Lsquirly,
            Token::Return,
            Token::Ident("c".into()),
            Token::Semicolon,
            Token::Rsquirly,
            Token::Rsquirly,
            // let second_test = fn(a, b) {
            //     a & b | 1
            // }
            Token::Let,
            Token::Ident("second_test".into()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("a".into()),
            Token::Comma,
            Token::Ident("b".into()),
            Token::Rparen,
            Token::Lsquirly,
            Token::Ident("a".into()),
            Token::BitAnd,
            Token::Ident("b".into()),
            Token::BitOr,
            Token::Integer(1),
            Token::Rsquirly,
            // test(true,x,y) || false && true;
            Token::Ident("test".into()),
            Token::Lparen,
            Token::Char(' '),
            Token::Comma,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::Rparen,
            Token::Or,
            Token::False,
            Token::And,
            Token::True,
            Token::Semicolon,
            // let s_test = "test";
            Token::Let,
            Token::Ident("s_test".into()),
            Token::Assign,
            Token::String("test".into()),
            Token::Semicolon,
            // let s_test = "\"test";
            Token::Let,
            Token::Ident("s_test".into()),
            Token::Assign,
            Token::String("\"test".into()),
            Token::Semicolon,
            // [] >= <= != ! == > < \
            Token::Lsquare,
            Token::Rsquare,
            Token::Ge,
            Token::Le,
            Token::Neq,
            Token::Bang,
            Token::Eq,
            Token::Gt,
            Token::Lt,
            Token::BackSlash,
            // end
            Token::Eof,
        ];
        let mut lex = Lexer::new(input);
        for exp_token in expected_tokens {
            let new_token = lex.next_token();
            assert_eq!(exp_token, new_token);
        }
    }
}
