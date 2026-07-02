#[derive(Copy, PartialEq, Eq, Clone, Debug)]
#[repr(u8)]
pub enum TokenType {
    Illegal,
    Eof,
    // basic tokens
    QuestionMark,
    Comma,
    Semicolon,
    Dot,
    DoubleDot,
    Colon,
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
    Null,
    Integer,
    Floating,
    Ident,
    String,
    Char,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub _type: TokenType,
    pub start_index: usize,
    pub end_index: usize,
    pub slice: &'a str,
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self._type == other._type && self.slice == other.slice
    }
}

impl<'a> Token<'a> {
    pub fn new(
        _type: TokenType,
        start_index: usize,
        end_index: usize,
        slice: &'a str,
    ) -> Token<'a> {
        Self {
            _type,
            start_index,
            end_index,
            slice,
        }
    }

    pub fn eof() -> Token<'static> {
        Token::new(TokenType::Eof, 0, 0, "EOF")
    }

    pub fn illegal(start_index: usize, end_index: usize, slice: &'a str) -> Token<'a> {
        Self::new(TokenType::Illegal, start_index, end_index, slice)
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self._type {
            TokenType::Illegal => write!(f, "ILLEGAL({})", self.slice),
            TokenType::Eof => f.write_str("EOF"),
            _ => f.write_str(self.slice),
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",
            TokenType::QuestionMark => "?",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Dot => ".",
            TokenType::DoubleDot => "..",
            TokenType::Colon => ":",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lsquare => "[",
            TokenType::Rsquare => "]",
            TokenType::Lsquirly => "{",
            TokenType::Rsquirly => "}",
            TokenType::Slash => "/",
            TokenType::BackSlash => "\\",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Asterisk => "*",
            TokenType::And => "&&",
            TokenType::BitAnd => "&",
            TokenType::Or => "||",
            TokenType::BitOr => "|",
            TokenType::Assign => "=",
            TokenType::Eq => "==",
            TokenType::Neq => "!=",
            TokenType::Gt => ">",
            TokenType::Lt => "<",
            TokenType::Ge => ">=",
            TokenType::Le => "<=",
            TokenType::Let => "let",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
            TokenType::Function => "fn",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::Null => "null",
            TokenType::Integer => "integer",
            TokenType::Floating => "float",
            TokenType::Ident => "ident",
            TokenType::String => "string",
            TokenType::Char => "char",
        };

        f.write_str(value)
    }
}

pub fn lookup_ident_token_type(ident: &str) -> TokenType {
    match ident {
        "let" => TokenType::Let,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        "fn" => TokenType::Function,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "null" => TokenType::Null,
        _ => TokenType::Ident,
    }
}
