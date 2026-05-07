use crate::ast;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::iter::Peekable;

pub fn escape_str(input: &str) -> String {
    let mut escaped = String::new();
    for ch in input.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            '\0' => escaped.push_str("\\0"),
            '"' => escaped.push_str("\\\""),
            '\'' => escaped.push_str("\\\'"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

pub fn unescape_str(input: &str) -> Option<String> {
    let mut unescaped = String::new();
    let mut chars = input.chars();

    while let Some(ch) = chars.next() {
        if ch != '\\' {
            unescaped.push(ch);
            continue;
        }

        match chars.next()? {
            '\\' => unescaped.push('\\'),
            'n' => unescaped.push('\n'),
            'r' => unescaped.push('\r'),
            't' => unescaped.push('\t'),
            '0' => unescaped.push('\0'),
            '"' => unescaped.push('"'),
            '\'' => unescaped.push('\''),
            _ => return None,
        }
    }

    Some(unescaped)
}

pub fn unenscape_str(input: &str) -> Option<String> {
    unescape_str(input)
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Min = 0,
    Equals,
    Diff,
    Sum,
    Prod,
    Prefix,
    Call,
    Index,
}

#[inline]
fn get_token_precedence(tok: &Token<'_>) -> Precedence {
    match tok._type {
        TokenType::Eq | TokenType::Neq => Precedence::Equals,
        TokenType::Gt
        | TokenType::Lt
        | TokenType::Ge
        | TokenType::Le
        | TokenType::And
        | TokenType::Or => Precedence::Diff,
        TokenType::Plus | TokenType::BitOr | TokenType::BitAnd => Precedence::Sum,
        TokenType::Asterisk | TokenType::Slash => Precedence::Prod,
        TokenType::Bang | TokenType::Minus => Precedence::Prefix,
        TokenType::Lparen => Precedence::Call,
        TokenType::Lsquare | TokenType::Dot => Precedence::Index,
        _ => Precedence::Min,
    }
}

macro_rules! assert_token_type {
    ($t:expr,$tt:expr) => {
        assert_eq!($t._type, $tt)
    };
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.into_iter().peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program = ast::Program::new();
        while let Some(token) = self.next_token() {
            program.statements.push(self.parse(token)?);
        }
        Some(program)
    }

    fn parse(&mut self, token: Token<'_>) -> Option<ast::Node> {
        let res = match token._type {
            TokenType::Let => self.parse_let_stmt(token),
            TokenType::Return => self.parse_return_stmt(token),
            _ => Some(ast::Node::Expression(
                self.parse_expr(token, Precedence::Min)?,
            )),
        }?;
        self.skip_optional_semicolon();
        Some(res)
    }

    fn parse_let_stmt(&mut self, token: Token<'_>) -> Option<ast::Node> {
        assert_token_type!(token, TokenType::Let);

        let ident = if let Token {
            _type: TokenType::Ident,
            slice,
            ..
        } = self.next_token()?
        {
            Some(slice.to_string())
        } else {
            None
        }?;

        if self.next_token()?._type != TokenType::Assign {
            return None;
        }

        let token = self.next_token()?;

        Some(ast::Node::Let {
            name: ident.to_string(),
            value: self.parse_expr(token, Precedence::Min)?,
        })
    }

    fn parse_return_stmt(&mut self, token: Token<'_>) -> Option<ast::Node> {
        assert_token_type!(token, TokenType::Return);
        let token = self.next_token()?;

        Some(ast::Node::Return {
            value: self.parse_expr(token, Precedence::Min)?,
        })
    }

    fn parse_expr(&mut self, token: Token<'_>, precedence: Precedence) -> Option<ast::Expression> {
        let mut expr = self.parse_expr_prefix(token)?;
        while !self.skip_optional_semicolon() && precedence < self.peek_precedence() {
            let next_token = self.next_token()?;
            expr = self.parse_expr_infix(next_token, expr)?;
        }
        Some(expr)
    }

    fn parse_expr_prefix(&mut self, token: Token<'_>) -> Option<ast::Expression> {
        match token._type {
            TokenType::Ident => self.parse_ident(token),
            TokenType::Integer => self.parse_int(token),
            TokenType::Floating => self.parse_float(token),
            TokenType::String => self.parse_string(token),
            TokenType::True | TokenType::False => Some(ast::Expression::Bool {
                value: token._type == TokenType::True,
            }),
            TokenType::Null => Some(ast::Expression::Null),
            TokenType::Char => self.parse_char(token),
            TokenType::Minus => self.parse_minus(token),
            TokenType::Bang => self.parse_negated(token),
            TokenType::Function => self.parse_function(token),
            TokenType::Lparen => self.parse_group(token),
            TokenType::If => self.parse_if(token),
            TokenType::Lsquare => self.parse_array(token),
            TokenType::Lsquirly => self.parse_dict(token),
            _ => None,
        }
    }

    fn parse_ident(&mut self, token: Token<'_>) -> Option<ast::Expression> {
        if let Token {
            _type: TokenType::Ident,
            slice,
            ..
        } = token
        {
            Some(ast::Expression::Identifier {
                name: slice.to_string(),
            })
        } else {
            panic!("Ident expected to parse_ident")
        }
    }

    fn parse_int(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token {
            _type: TokenType::Integer,
            slice,
            ..
        } = token
        {
            let num = match slice.parse::<i64>() {
                Ok(n) => Some(n),
                Err(_) => None,
            }?;
            Some(ast::Expression::Integer { value: num })
        } else {
            panic!("Integer expected to parse_int")
        }
    }

    fn parse_float(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token {
            _type: TokenType::Floating,
            slice,
            ..
        } = token
        {
            let num = match slice.parse::<f64>() {
                Ok(n) => Some(n),
                Err(_) => None,
            }?;
            Some(ast::Expression::Float { value: num })
        } else {
            panic!("Floating expected to parse_float")
        }
    }

    fn parse_string(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token {
            _type: TokenType::String,
            slice,
            ..
        } = token
        {
            let value = unescape_str(slice.strip_suffix('"')?)?;
            Some(ast::Expression::String { value })
        } else {
            panic!("String expected to parse_string")
        }
    }

    fn parse_char(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token {
            _type: TokenType::Char,
            slice,
            ..
        } = token
        {
            let value = unescape_str(slice.strip_prefix('\'')?.strip_suffix('\'')?)?;
            let mut chars = value.chars();
            let value = chars.next()?;
            if chars.next().is_some() {
                return None;
            }
            Some(ast::Expression::Char { value })
        } else {
            panic!("Char expected to parse_char")
        }
    }

    fn parse_minus(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::Minus {
            panic!("Minus expected to parse_minus")
        }

        let next_token = self.next_token()?;
        Some(ast::Expression::Minus {
            value: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        })
    }

    fn parse_negated(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::Bang {
            panic!("Bang expected to parse_negated")
        }

        let next_token = self.next_token()?;
        Some(ast::Expression::Negated {
            value: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        })
    }

    fn parse_group(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::Lparen {
            panic!("Lparen expected to parse_group")
        }
        let next_token = self.next_token()?;
        let result = self.parse_expr(next_token, Precedence::Min);
        match self.next_token()?._type {
            TokenType::Rparen => result,
            _ => None,
        }
    }

    fn parse_function(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::Function {
            panic!("Bang expected to parse_negated")
        }
        if self.next_token()?._type != TokenType::Lparen {
            return None;
        }
        let params = self.parse_function_params()?;

        let next_token = self.next_token()?;
        Some(ast::Expression::Function {
            parameters: params,
            body: self.parse_block(next_token)?,
        })
    }

    fn parse_if(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::If {
            panic!("If expected to parse_if")
        }
        let next_token = self.next_token()?;
        // TODO: support ifs without parentheses
        let condition = self.parse_group(next_token)?;
        let next_token = self.next_token()?;
        let consequence = if next_token._type == TokenType::Lsquirly {
            self.parse_block(next_token)?
        } else {
            ast::Block {
                statements: vec![self.parse(next_token)?],
            }
        };
        if self.peek_is(TokenType::Else) {
            self.next_token()?;
            let next_token = self.next_token()?;
            let alternative = if next_token._type == TokenType::Lsquirly {
                self.parse_block(next_token)?
            } else {
                ast::Block {
                    statements: vec![self.parse(next_token)?],
                }
            };
            Some(ast::Expression::If {
                condition: Box::new(condition),
                consequence,
                alternative: Some(alternative),
            })
        } else {
            Some(ast::Expression::If {
                condition: Box::new(condition),
                consequence,
                alternative: None,
            })
        }
    }

    fn parse_array(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::Lsquare {
            panic!("Lsquare expected to parse_array")
        }
        let mut values: Vec<ast::Expression> = Vec::new();
        if self.peek_is(TokenType::Rsquare) {
            self.next_token()?;
            return Some(ast::Expression::Array { values });
        }
        loop {
            let next_token = self.next_token()?;
            values.push(self.parse_expr(next_token, Precedence::Min)?);
            if !self.skip_optional_comma() {
                break;
            }
        }
        match self.next_token()?._type {
            TokenType::Rsquare => Some(ast::Expression::Array { values }),
            _ => None,
        }
    }

    fn parse_dict(&mut self, token: Token) -> Option<ast::Expression> {
        if token._type != TokenType::Lsquirly {
            panic!("Lsquirly expected to parse_dict")
        }
        let mut values: Vec<(ast::Expression, ast::Expression)> = Vec::new();
        if self.peek_is(TokenType::Rsquirly) {
            self.next_token()?;
            return Some(ast::Expression::Dict { values });
        }
        loop {
            let next_token = self.next_token()?;
            let key = self.parse_expr(next_token, Precedence::Min)?;
            if !self.skip_required_colon() {
                return None;
            }
            let next_token = self.next_token()?;
            let value = self.parse_expr(next_token, Precedence::Min)?;
            values.push((key, value));
            if !self.skip_optional_comma() {
                break;
            }
        }
        match self.next_token()?._type {
            TokenType::Rsquirly => Some(ast::Expression::Dict { values }),
            _ => None,
        }
    }

    fn parse_function_params(&mut self) -> Option<Vec<String>> {
        let mut result: Vec<String> = Vec::new();
        if self.peek_is(TokenType::Rparen) {
            self.next_token()?;
            return Some(result);
        }
        loop {
            let token = self.next_token()?;
            if token._type == TokenType::Ident {
                result.push(token.slice.to_string());
            } else {
                return None;
            }
            if !self.skip_optional_comma() {
                break;
            }
        }
        match self.next_token()?._type {
            TokenType::Rparen => Some(result),
            _ => None,
        }
    }

    fn parse_block(&mut self, token: Token) -> Option<ast::Block> {
        if token._type != TokenType::Lsquirly {
            panic!("Lsquirly expected to parse_block")
        }
        let mut statements: Vec<ast::Node> = Vec::new();
        let mut next_token = self.next_token()?;
        while next_token._type != TokenType::Rsquirly && next_token._type != TokenType::Eof {
            statements.push(self.parse(next_token)?);
            next_token = self.next_token()?;
        }
        Some(ast::Block { statements })
    }

    fn parse_expr_infix(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        match token._type.clone() {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Eq
            | TokenType::Neq
            | TokenType::Gt
            | TokenType::Lt
            | TokenType::Ge
            | TokenType::Le
            | TokenType::Or
            | TokenType::And
            | TokenType::BitOr
            | TokenType::BitAnd => {
                let precedence = get_token_precedence(&token);
                let next_token = self.next_token()?;
                Some(ast::Expression::Infix {
                    left: Box::new(left),
                    op: token._type,
                    right: Box::new(self.parse_expr(next_token, precedence)?),
                })
            }
            TokenType::Lparen => self.parse_call(token, left),
            TokenType::Lsquare => self.parse_index_square(token, left),
            TokenType::Dot => {
                if self.peek_is(TokenType::Lparen) {
                    let call_token = self.next_token()?;
                    self.parse_call(call_token, left)
                } else {
                    self.parse_index_dot(token, left)
                }
            }
            // TokenType::QuestionMark => self.parse_if(),
            _ => None,
        }
    }

    fn parse_index_square(
        &mut self,
        token: Token,
        left: ast::Expression,
    ) -> Option<ast::Expression> {
        if token._type != TokenType::Lsquare {
            panic!("Lsquare expected to parse_index_square")
        }
        let next_token = self.next_token()?;
        let index = self.parse_expr(next_token, Precedence::Min)?;

        match self.next_token()?._type {
            TokenType::Rsquare => Some(ast::Expression::Index {
                value: left.into(),
                index: index.into(),
            }),

            _ => None,
        }
    }

    fn parse_index_dot(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        if token._type != TokenType::Dot {
            panic!("Dot expected to parse_index_dot")
        }
        let next_token = self.next_token()?;
        let index = self.parse_expr(next_token, Precedence::Index)?;
        Some(ast::Expression::Index {
            value: left.into(),
            index: index.into(),
        })
    }

    fn parse_call(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        if token._type != TokenType::Lparen {
            panic!("Lparen expected to parse_call")
        }
        let function = Box::new(left);
        let mut parameters = Vec::new();
        if self.peek_is(TokenType::Rparen) {
            self.next_token()?;
            return Some(ast::Expression::Call {
                function,
                parameters,
            });
        }

        loop {
            let next_token = self.next_token()?;
            parameters.push(self.parse_expr(next_token, Precedence::Min)?);
            if !self.skip_optional_comma() {
                break;
            }
        }
        match self.next_token()?._type {
            TokenType::Rparen => Some(ast::Expression::Call {
                function,
                parameters,
            }),

            _ => None,
        }
    }

    fn skip_required_colon(&mut self) -> bool {
        if self.peek_is(TokenType::Colon) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn skip_optional_comma(&mut self) -> bool {
        if self.peek_is(TokenType::Comma) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn skip_optional_semicolon(&mut self) -> bool {
        if let Some(token) = self.lexer.peek() {
            if token._type == TokenType::Semicolon {
                self.next_token();
                true
            } else {
                false
            }
        } else {
            true
        }
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        self.lexer.next()
    }

    fn peek_is(&mut self, token_type: TokenType) -> bool {
        matches!(self.lexer.peek(), Some(token) if token._type == token_type)
    }

    fn peek_precedence(&mut self) -> Precedence {
        match self.lexer.peek() {
            Some(t) => get_token_precedence(t),
            None => Precedence::Min,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{ast, lexer, token::TokenType};

    use super::{escape_str, unescape_str, Parser};

    fn token(_type: TokenType, _slice: &'static str) -> TokenType {
        _type
    }

    #[test]
    fn parse_let_stmt() {
        let tests: [(&str, ast::Node); 9] = [
            (
                "let some = 3;",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::Integer { value: 3 },
                },
            ),
            (
                "let some = \"some\";",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::String {
                        value: "some".into(),
                    },
                },
            ),
            (
                "let some = 's';",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::Char { value: 's' },
                },
            ),
            (
                "let some = test;",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::Identifier {
                        name: "test".into(),
                    },
                },
            ),
            (
                "let some = 1 + 2 * 3;",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::Infix {
                        left: ast::Expression::Integer { value: 1 }.into(),
                        op: token(TokenType::Plus, "+"),
                        right: ast::Expression::Infix {
                            left: ast::Expression::Integer { value: 2 }.into(),
                            op: token(TokenType::Asterisk, "*"),
                            right: ast::Expression::Integer { value: 3 }.into(),
                        }
                        .into(),
                    },
                },
            ),
            (
                "let some = fn(){};",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::Function {
                        parameters: Vec::new(),
                        body: ast::Block {
                            statements: Vec::new(),
                        },
                    },
                },
            ),
            (
                "let some = fn(a,b){let x = a+b; return x;};",
                ast::Node::Let {
                    name: "some".into(),
                    value: ast::Expression::Function {
                        parameters: vec!["a".into(), "b".into()],
                        body: ast::Block {
                            statements: vec![
                                ast::Node::Let {
                                    name: "x".into(),
                                    value: ast::Expression::Infix {
                                        left: ast::Expression::Identifier { name: "a".into() }
                                            .into(),
                                        op: token(TokenType::Plus, "+"),
                                        right: ast::Expression::Identifier { name: "b".into() }
                                            .into(),
                                    },
                                },
                                ast::Node::Return {
                                    value: ast::Expression::Identifier { name: "x".into() },
                                },
                            ],
                        },
                    },
                },
            ),
            (
                "let res = test();",
                ast::Node::Let {
                    name: "res".into(),
                    value: ast::Expression::Call {
                        function: ast::Expression::Identifier {
                            name: "test".into(),
                        }
                        .into(),
                        parameters: Vec::new(),
                    },
                },
            ),
            (
                "let res = test(a, -(b+1)*2 );",
                ast::Node::Let {
                    name: "res".into(),
                    value: ast::Expression::Call {
                        function: ast::Expression::Identifier {
                            name: "test".into(),
                        }
                        .into(),
                        parameters: vec![
                            ast::Expression::Identifier { name: "a".into() },
                            ast::Expression::Infix {
                                left: ast::Expression::Minus {
                                    value: ast::Expression::Infix {
                                        left: ast::Expression::Identifier { name: "b".into() }
                                            .into(),
                                        op: token(TokenType::Plus, "+"),
                                        right: ast::Expression::Integer { value: 1 }.into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                op: token(TokenType::Asterisk, "*"),
                                right: ast::Expression::Integer { value: 2 }.into(),
                            },
                        ],
                    },
                },
            ),
        ];
        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse let stmt: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_return() {
        let source = "return \"hello\";";
        let lex = lexer::Lexer::new(source);
        let mut parser = Parser::new(lex);
        let program = parser.parse_program().expect("couldn't parse let stmt");
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            ast::Node::Return {
                value: ast::Expression::String {
                    value: "hello".into(),
                }
            }
        )
    }

    macro_rules! define_infix_test_case {
        (
            $test_string:expr,
            $left_value:expr,
            $operator:expr,
            $right_value:expr
        ) => {
            (
                $test_string,
                ast::Node::Expression(ast::Expression::Infix {
                    left: ast::Expression::Integer { value: $left_value }.into(),
                    op: $operator,
                    right: ast::Expression::Integer {
                        value: $right_value,
                    }
                    .into(),
                }),
            )
        };
    }

    #[test]
    fn parse_ops() {
        let tests: [(&str, ast::Node); 13] = [
            define_infix_test_case!("1 / 2", 1, token(TokenType::Slash, "/"), 2),
            define_infix_test_case!("1 + 2", 1, token(TokenType::Plus, "+"), 2),
            define_infix_test_case!("1 * 2", 1, token(TokenType::Asterisk, "*"), 2),
            define_infix_test_case!("1 && 2", 1, token(TokenType::And, "&&"), 2),
            define_infix_test_case!("1 & 2", 1, token(TokenType::BitAnd, "&"), 2),
            define_infix_test_case!("1 || 2", 1, token(TokenType::Or, "||"), 2),
            define_infix_test_case!("1 | 2", 1, token(TokenType::BitOr, "|"), 2),
            define_infix_test_case!("1 == 2", 1, token(TokenType::Eq, "=="), 2),
            define_infix_test_case!("1 != 2", 1, token(TokenType::Neq, "!="), 2),
            define_infix_test_case!("1 > 2", 1, token(TokenType::Gt, ">"), 2),
            define_infix_test_case!("1 < 2", 1, token(TokenType::Lt, "<"), 2),
            define_infix_test_case!("1 >= 2", 1, token(TokenType::Ge, ">="), 2),
            define_infix_test_case!("1 <= 2", 1, token(TokenType::Le, "<="), 2),
        ];
        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse let stmt: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_if() {
        let tests: [(&str, ast::Node); 6] = [
            (
                "if(a > b){ return a; } else { return b;}",
                ast::Node::Expression(ast::Expression::If {
                    condition: ast::Expression::Infix {
                        left: ast::Expression::Identifier { name: "a".into() }.into(),
                        op: token(TokenType::Gt, ">"),
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Identifier { name: "a".into() },
                        }],
                    },
                    alternative: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Identifier { name: "b".into() },
                        }],
                    }
                    .into(),
                }),
            ),
            (
                "if(a > b) a else b;",
                ast::Node::Expression(ast::Expression::If {
                    condition: ast::Expression::Infix {
                        left: ast::Expression::Identifier { name: "a".into() }.into(),
                        op: token(TokenType::Gt, ">"),
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Expression(ast::Expression::Identifier {
                            name: "a".into(),
                        })],
                    },
                    alternative: Some(ast::Block {
                        statements: vec![ast::Node::Expression(ast::Expression::Identifier {
                            name: "b".into(),
                        })],
                    }),
                }),
            ),
            (
                "if(a > b) { a }",
                ast::Node::Expression(ast::Expression::If {
                    condition: ast::Expression::Infix {
                        left: ast::Expression::Identifier { name: "a".into() }.into(),
                        op: token(TokenType::Gt, ">"),
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Expression(ast::Expression::Identifier {
                            name: "a".into(),
                        })],
                    },
                    alternative: None,
                }),
            ),
            (
                "if(a > b) a",
                ast::Node::Expression(ast::Expression::If {
                    condition: ast::Expression::Infix {
                        left: ast::Expression::Identifier { name: "a".into() }.into(),
                        op: token(TokenType::Gt, ">"),
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Expression(ast::Expression::Identifier {
                            name: "a".into(),
                        })],
                    },
                    alternative: None,
                }),
            ),
            (
                "if(a > b) a else { return b }",
                ast::Node::Expression(ast::Expression::If {
                    condition: ast::Expression::Infix {
                        left: ast::Expression::Identifier { name: "a".into() }.into(),
                        op: token(TokenType::Gt, ">"),
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Expression(ast::Expression::Identifier {
                            name: "a".into(),
                        })],
                    },
                    alternative: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Identifier { name: "b".into() },
                        }],
                    }
                    .into(),
                }),
            ),
            (
                "if(a > b) { return a/b; } else b;",
                ast::Node::Expression(ast::Expression::If {
                    condition: ast::Expression::Infix {
                        left: ast::Expression::Identifier { name: "a".into() }.into(),
                        op: token(TokenType::Gt, ">"),
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Infix {
                                left: ast::Expression::Identifier { name: "a".into() }.into(),
                                right: ast::Expression::Identifier { name: "b".into() }.into(),
                                op: token(TokenType::Slash, "/"),
                            },
                        }],
                    },
                    alternative: ast::Block {
                        statements: vec![ast::Node::Expression(ast::Expression::Identifier {
                            name: "b".into(),
                        })],
                    }
                    .into(),
                }),
            ),
        ];
        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse let stmt: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_array() {
        let tests: [(&str, ast::Node); 1] = [(
            r#"[1,"test",'c',(1+3)*2,fn(test){" "+test}]"#,
            ast::Node::Expression(ast::Expression::Array {
                values: vec![
                    ast::Expression::Integer { value: 1 },
                    ast::Expression::String {
                        value: "test".into(),
                    },
                    ast::Expression::Char { value: 'c' },
                    ast::Expression::Infix {
                        left: ast::Expression::Infix {
                            left: ast::Expression::Integer { value: 1 }.into(),
                            op: token(TokenType::Plus, "+"),
                            right: ast::Expression::Integer { value: 3 }.into(),
                        }
                        .into(),
                        op: token(TokenType::Asterisk, "*"),
                        right: ast::Expression::Integer { value: 2 }.into(),
                    },
                    ast::Expression::Function {
                        parameters: vec!["test".into()],
                        body: ast::Block {
                            statements: vec![ast::Node::Expression(ast::Expression::Infix {
                                left: ast::Expression::String { value: " ".into() }.into(),
                                right: ast::Expression::Identifier {
                                    name: "test".into(),
                                }
                                .into(),
                                op: token(TokenType::Plus, "+"),
                            })],
                        },
                    },
                ],
            }),
        )];
        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse let stmt: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_dict() {
        let tests: [(&str, ast::Node); 1] = [(
            r#"{1 : 1, "test": "test", 'c': 'c', "func": fn(test){" "+test} }"#,
            ast::Node::Expression(ast::Expression::Dict {
                values: vec![
                    (
                        ast::Expression::Integer { value: 1 },
                        ast::Expression::Integer { value: 1 },
                    ),
                    (
                        ast::Expression::String {
                            value: "test".into(),
                        },
                        ast::Expression::String {
                            value: "test".into(),
                        },
                    ),
                    (
                        ast::Expression::Char { value: 'c' },
                        ast::Expression::Char { value: 'c' },
                    ),
                    (
                        ast::Expression::String {
                            value: "func".into(),
                        },
                        ast::Expression::Function {
                            parameters: vec!["test".into()],
                            body: ast::Block {
                                statements: vec![ast::Node::Expression(ast::Expression::Infix {
                                    left: ast::Expression::String { value: " ".into() }.into(),
                                    right: ast::Expression::Identifier {
                                        name: "test".into(),
                                    }
                                    .into(),
                                    op: token(TokenType::Plus, "+"),
                                })],
                            },
                        },
                    ),
                ],
            }),
        )];
        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse let stmt: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_index() {
        let tests: [(&str, ast::Node); 9] = [
            (
                "index[1]",
                ast::Node::Expression(ast::Expression::Index {
                    value: ast::Expression::Identifier {
                        name: "index".into(),
                    }
                    .into(),
                    index: ast::Expression::Integer { value: 1 }.into(),
                }),
            ),
            (
                "index[1]()",
                ast::Node::Expression(ast::Expression::Call {
                    function: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Integer { value: 1 }.into(),
                    }
                    .into(),
                    parameters: vec![],
                }),
            ),
            (
                r#"index[1]("test")"#,
                ast::Node::Expression(ast::Expression::Call {
                    function: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Integer { value: 1 }.into(),
                    }
                    .into(),
                    parameters: vec![ast::Expression::String {
                        value: "test".into(),
                    }],
                }),
            ),
            (
                "index.name",
                ast::Node::Expression(ast::Expression::Index {
                    value: ast::Expression::Identifier {
                        name: "index".into(),
                    }
                    .into(),
                    index: ast::Expression::Identifier {
                        name: "name".into(),
                    }
                    .into(),
                }),
            ),
            (
                "index.name()",
                ast::Node::Expression(ast::Expression::Call {
                    function: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Identifier {
                            name: "name".into(),
                        }
                        .into(),
                    }
                    .into(),
                    parameters: vec![],
                }),
            ),
            (
                r#"index.1.("test")"#,
                ast::Node::Expression(ast::Expression::Call {
                    function: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Integer { value: 1 }.into(),
                    }
                    .into(),
                    parameters: vec![ast::Expression::String {
                        value: "test".into(),
                    }],
                }),
            ),
            (
                "index.name + \"test\"",
                ast::Node::Expression(ast::Expression::Infix {
                    left: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Identifier {
                            name: "name".into(),
                        }
                        .into(),
                    }
                    .into(),
                    right: ast::Expression::String {
                        value: "test".into(),
                    }
                    .into(),
                    op: token(TokenType::Plus, "+"),
                }),
            ),
            (
                "index[1][0]",
                ast::Node::Expression(ast::Expression::Index {
                    value: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Integer { value: 1 }.into(),
                    }
                    .into(),
                    index: ast::Expression::Integer { value: 0 }.into(),
                }),
            ),
            (
                "index[1].0",
                ast::Node::Expression(ast::Expression::Index {
                    value: ast::Expression::Index {
                        value: ast::Expression::Identifier {
                            name: "index".into(),
                        }
                        .into(),
                        index: ast::Expression::Integer { value: 1 }.into(),
                    }
                    .into(),
                    index: ast::Expression::Integer { value: 0 }.into(),
                }),
            ),
        ];
        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse let stmt: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_string_and_char_escapes() {
        let tests: [(&str, ast::Node); 2] = [
            (
                r#""line\nnext\rrow\tcol\\quote\"""#,
                ast::Node::Expression(ast::Expression::String {
                    value: "line\nnext\rrow\tcol\\quote\"".into(),
                }),
            ),
            (
                r#"'\n'"#,
                ast::Node::Expression(ast::Expression::Char { value: '\n' }),
            ),
        ];

        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("couldn't parse escaped literal: {source}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node);
        }
    }

    #[test]
    fn escape_roundtrip() {
        let original = "line\nnext\rrow\tcol\\quote\"tick'\0";
        let escaped = escape_str(original);

        assert_eq!(unescape_str(&escaped), Some(original.to_string()));
    }
}
