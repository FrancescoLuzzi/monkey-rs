use std::fmt::Write as _;

#[derive(PartialEq, Clone, Debug)]
#[repr(u16)]
pub enum Token {
    Illegal,
    Eof,
    // basic tokens
    Colon,
    Semicolon,
    Dot,
    Lparen,
    Rparen,
    Lsquare,
    Rsquare,
    Lsquirly,
    Rsquirly,
    Slash,
    BackSlash,
    Plus,
    Minus,
    Bang,
    Asterisk,
    And,
    BitAnd,
    Or,
    BitOr,
    Assign,
    Eq,
    Neq,
    Gt,
    Lt,
    Ge,
    Le,
    // keywords
    Let,
    If,
    Else,
    Return,
    Function,
    True,
    False,
    // literals
    Integer(i64),
    Floating(f64),
    Ident(String),
    String(String),
    Char(char),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => f.write_str("ILLEGAL"),
            Token::Eof => f.write_str("EOF"),
            Token::Colon => f.write_str(","),
            Token::Semicolon => f.write_str(";"),
            Token::Dot => f.write_str("."),
            Token::Lparen => f.write_str("("),
            Token::Rparen => f.write_str(")"),
            Token::Lsquare => f.write_str("["),
            Token::Rsquare => f.write_str("]"),
            Token::Lsquirly => f.write_str("{"),
            Token::Rsquirly => f.write_str("}"),
            Token::Slash => f.write_str("/"),
            Token::BackSlash => f.write_str("\\"),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Bang => f.write_str("!"),
            Token::Asterisk => f.write_str("*"),
            Token::And => f.write_str("&&"),
            Token::BitAnd => f.write_str("&"),
            Token::Or => f.write_str("||"),
            Token::BitOr => f.write_str("|"),
            Token::Assign => f.write_str("="),
            Token::Eq => f.write_str("=="),
            Token::Neq => f.write_str("!="),
            Token::Gt => f.write_str(">"),
            Token::Lt => f.write_str("<"),
            Token::Ge => f.write_str(">="),
            Token::Le => f.write_str("<="),
            Token::Let => f.write_str("let"),
            Token::If => f.write_str("if"),
            Token::Else => f.write_str("else"),
            Token::Return => f.write_str("return"),
            Token::Function => f.write_str("fn"),
            Token::True => f.write_str("true"),
            Token::False => f.write_str("false"),
            Token::Integer(n) => f.write_fmt(format_args!("{}", n)),
            Token::Floating(n) => f.write_fmt(format_args!("{}", n)),
            Token::Ident(ident) => f.write_str(ident),
            Token::String(s) => f.write_str(s),
            Token::Char(c) => f.write_char(*c),
        }
    }
}

pub fn lookup_ident(ident: String) -> Token {
    match ident.as_str() {
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(ident),
    }
}
