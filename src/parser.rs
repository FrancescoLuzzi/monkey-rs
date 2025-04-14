use crate::ast;
use crate::lexer::Lexer;
use crate::token::Token;
use std::iter::Peekable;

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
fn get_token_precedence(tok: &Token) -> Precedence {
    match tok {
        Token::Eq | Token::Neq => Precedence::Equals,
        Token::Gt | Token::Lt | Token::Ge | Token::Le | Token::And | Token::Or => Precedence::Diff,
        Token::Plus | Token::BitOr | Token::BitAnd => Precedence::Sum,
        Token::Asterisk | Token::Slash => Precedence::Prod,
        Token::Bang | Token::Minus => Precedence::Prefix,
        Token::Lparen => Precedence::Call,
        Token::Lsquare | Token::Dot => Precedence::Index,
        _ => Precedence::Min,
    }
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

    fn parse(&mut self, token: Token) -> Option<ast::Node> {
        let res = match token {
            Token::Let => self.parse_let_stmt(token),
            Token::Return => self.parse_return_stmt(token),
            _ => Some(ast::Node::Expression(
                self.parse_expr(token, Precedence::Min)?,
            )),
        }?;
        self.skip_optional_semicolon();
        Some(res)
    }

    fn parse_let_stmt(&mut self, token: Token) -> Option<ast::Node> {
        assert_eq!(token, Token::Let);

        let ident = if let Token::Ident(ident) = self.next_token()? {
            Some(ident)
        } else {
            None
        }?;

        if self.next_token()? != Token::Assign {
            return None;
        }

        let token = self.next_token()?;

        Some(ast::Node::Let {
            name: ident,
            value: self.parse_expr(token, Precedence::Min)?,
        })
    }

    fn parse_return_stmt(&mut self, token: Token) -> Option<ast::Node> {
        assert_eq!(token, Token::Return);
        let token = self.next_token()?;

        Some(ast::Node::Return {
            value: self.parse_expr(token, Precedence::Min)?,
        })
    }

    fn parse_expr(&mut self, token: Token, precedence: Precedence) -> Option<ast::Expression> {
        let mut expr = self.parse_expr_prefix(token)?;
        while !self.skip_optional_semicolon() && precedence < self.peek_precedence() {
            let next_token = self.next_token()?;
            expr = self.parse_expr_infix(next_token, expr)?;
        }
        Some(expr)
    }

    fn parse_expr_prefix(&mut self, token: Token) -> Option<ast::Expression> {
        match token {
            Token::Ident(_) => self.parse_ident(token),
            Token::Integer(_) => self.parse_int(token),
            Token::Floating(_) => self.parse_float(token),
            Token::String(_) => self.parse_string(token),
            Token::True | Token::False => Some(ast::Expression::Bool {
                value: token == Token::True,
            }),
            Token::Char(_) => self.parse_char(token),
            Token::Minus => self.parse_minus(token),
            Token::Bang => self.parse_negated(token),
            Token::Function => self.parse_function(token),
            Token::Lparen => self.parse_group(token),
            Token::If => self.parse_if(token),
            Token::Lsquare => self.parse_array(token),
            Token::Lsquirly => self.parse_dict(token),
            _ => None,
        }
    }

    fn parse_ident(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Ident(ident) = token {
            Some(ast::Expression::Identifier { name: ident })
        } else {
            panic!("Ident expected to parse_ident")
        }
    }

    fn parse_int(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Integer(num) = token {
            Some(ast::Expression::Integer { value: num })
        } else {
            panic!("Integer expected to parse_int")
        }
    }

    fn parse_float(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Floating(num) = token {
            Some(ast::Expression::Float { value: num })
        } else {
            panic!("Floating expected to parse_float")
        }
    }

    fn parse_string(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::String(value) = token {
            Some(ast::Expression::String { value })
        } else {
            panic!("String expected to parse_string")
        }
    }

    fn parse_char(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Char(value) = token {
            Some(ast::Expression::Char { value })
        } else {
            panic!("Char expected to parse_char")
        }
    }

    fn parse_minus(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Minus) {
            panic!("Minus expected to parse_minus")
        }

        let next_token = self.next_token()?;
        Some(ast::Expression::Minus {
            value: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        })
    }

    fn parse_negated(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Bang) {
            panic!("Bang expected to parse_negated")
        }

        let next_token = self.next_token()?;
        Some(ast::Expression::Negated {
            value: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        })
    }

    fn parse_group(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Lparen) {
            panic!("Lparen expected to parse_group")
        }
        let next_token = self.next_token()?;
        let result = self.parse_expr(next_token, Precedence::Min);
        match self.next_token()? {
            Token::Rparen => result,
            _ => None,
        }
    }

    fn parse_function(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Function) {
            panic!("Bang expected to parse_negated")
        }
        if !matches!(self.next_token()?, Token::Lparen) {
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
        if !matches!(token, Token::If) {
            panic!("If expected to parse_if")
        }
        let next_token = self.next_token()?;
        // TODO: support ifs without parentheses
        let condition = self.parse_group(next_token)?;
        let next_token = self.next_token()?;
        let consequence = if matches!(next_token, Token::Lsquirly) {
            self.parse_block(next_token)?
        } else {
            ast::Block {
                statements: vec![self.parse(next_token)?],
            }
        };
        if matches!(self.lexer.peek(), Some(Token::Else)) {
            self.next_token()?;
            let next_token = self.next_token()?;
            let alternative = if matches!(next_token, Token::Lsquirly) {
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
        if !matches!(token, Token::Lsquare) {
            panic!("Lsquare expected to parse_array")
        }
        let mut values: Vec<ast::Expression> = Vec::new();
        if matches!(self.lexer.peek()?, Token::Rsquare) {
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
        match self.next_token()? {
            Token::Rsquare => Some(ast::Expression::Array { values }),
            _ => None,
        }
    }

    fn parse_dict(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Lsquirly) {
            panic!("Lsquirly expected to parse_dict")
        }
        let mut values: Vec<(ast::Expression, ast::Expression)> = Vec::new();
        if matches!(self.lexer.peek()?, Token::Rsquirly) {
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
        match self.next_token()? {
            Token::Rsquirly => Some(ast::Expression::Dict { values }),
            _ => None,
        }
    }

    fn parse_function_params(&mut self) -> Option<Vec<String>> {
        let mut result: Vec<String> = Vec::new();
        if matches!(self.lexer.peek()?, Token::Rparen) {
            self.next_token()?;
            return Some(result);
        }
        loop {
            if let Token::Ident(ident) = self.next_token()? {
                result.push(ident);
            } else {
                return None;
            }
            if !self.skip_optional_comma() {
                break;
            }
        }
        match self.next_token()? {
            Token::Rparen => Some(result),
            _ => None,
        }
    }

    fn parse_block(&mut self, token: Token) -> Option<ast::Block> {
        if !matches!(token, Token::Lsquirly) {
            panic!("Lsquirly expected to parse_block")
        }
        let mut statements: Vec<ast::Node> = Vec::new();
        let mut next_token = self.next_token()?;
        while !matches!(next_token, Token::Rsquirly | Token::Eof) {
            statements.push(self.parse(next_token)?);
            next_token = self.next_token()?;
        }
        Some(ast::Block { statements })
    }

    fn parse_expr_infix(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Eq
            | Token::Neq
            | Token::Gt
            | Token::Lt
            | Token::Ge
            | Token::Le
            | Token::Or
            | Token::And
            | Token::BitOr
            | Token::BitAnd => {
                let precedence = get_token_precedence(&token);
                let next_token = self.next_token()?;
                Some(ast::Expression::Infix {
                    left: Box::new(left),
                    op: token.clone(),
                    right: Box::new(self.parse_expr(next_token, precedence)?),
                })
            }
            Token::Lparen => self.parse_call(token, left),
            Token::Lsquare => self.parse_index_square(token, left),
            Token::Dot => self.parse_index_dot(token, left),
            // Token::QuestionMark => self.parse_if(),
            _ => None,
        }
    }

    fn parse_index_square(
        &mut self,
        token: Token,
        left: ast::Expression,
    ) -> Option<ast::Expression> {
        if !matches!(token, Token::Lsquare) {
            panic!("Lsquare expected to parse_index_square")
        }
        let next_token = self.next_token()?;
        let index = self.parse_expr(next_token, Precedence::Min)?;

        match self.next_token()? {
            Token::Rsquare => Some(ast::Expression::Index {
                value: left.into(),
                index: index.into(),
            }),

            _ => None,
        }
    }

    fn parse_index_dot(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        if !matches!(token, Token::Dot) {
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
        if !matches!(token, Token::Lparen) {
            panic!("Lparen expected to parse_call")
        }
        let function = if let ast::Expression::Identifier { name } = left {
            name
        } else {
            return None;
        };

        let mut parameters = Vec::new();
        if matches!(self.lexer.peek()?, Token::Rparen) {
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
        match self.next_token()? {
            Token::Rparen => Some(ast::Expression::Call {
                function,
                parameters,
            }),

            _ => None,
        }
    }

    fn skip_required_colon(&mut self) -> bool {
        if let Some(Token::Colon) = self.lexer.peek() {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn skip_optional_comma(&mut self) -> bool {
        if let Some(Token::Comma) = self.lexer.peek() {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn skip_optional_semicolon(&mut self) -> bool {
        if let Some(token) = self.lexer.peek() {
            if matches!(token, Token::Semicolon) {
                self.next_token();
                true
            } else {
                false
            }
        } else {
            true
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.lexer.next()
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
    use crate::{ast, lexer, token};

    use super::Parser;

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
                        op: token::Token::Plus,
                        right: ast::Expression::Infix {
                            left: ast::Expression::Integer { value: 2 }.into(),
                            op: token::Token::Asterisk,
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
                                        op: token::Token::Plus,
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
                        function: "test".into(),
                        parameters: Vec::new(),
                    },
                },
            ),
            (
                "let res = test(a, -(b+1)*2 );",
                ast::Node::Let {
                    name: "res".into(),
                    value: ast::Expression::Call {
                        function: "test".into(),
                        parameters: vec![
                            ast::Expression::Identifier { name: "a".into() },
                            ast::Expression::Infix {
                                left: ast::Expression::Minus {
                                    value: ast::Expression::Infix {
                                        left: ast::Expression::Identifier { name: "b".into() }
                                            .into(),
                                        op: token::Token::Plus,
                                        right: ast::Expression::Integer { value: 1 }.into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                op: token::Token::Asterisk,
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
            $operator:path,
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
            define_infix_test_case!("1 / 2", 1, token::Token::Slash, 2),
            define_infix_test_case!("1 + 2", 1, token::Token::Plus, 2),
            define_infix_test_case!("1 * 2", 1, token::Token::Asterisk, 2),
            define_infix_test_case!("1 && 2", 1, token::Token::And, 2),
            define_infix_test_case!("1 & 2", 1, token::Token::BitAnd, 2),
            define_infix_test_case!("1 || 2", 1, token::Token::Or, 2),
            define_infix_test_case!("1 | 2", 1, token::Token::BitOr, 2),
            define_infix_test_case!("1 == 2", 1, token::Token::Eq, 2),
            define_infix_test_case!("1 != 2", 1, token::Token::Neq, 2),
            define_infix_test_case!("1 > 2", 1, token::Token::Gt, 2),
            define_infix_test_case!("1 < 2", 1, token::Token::Lt, 2),
            define_infix_test_case!("1 >= 2", 1, token::Token::Ge, 2),
            define_infix_test_case!("1 <= 2", 1, token::Token::Le, 2),
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
                        op: token::Token::Gt,
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
                        op: token::Token::Gt,
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
                        op: token::Token::Gt,
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
                        op: token::Token::Gt,
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
                        op: token::Token::Gt,
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
                        op: token::Token::Gt,
                        right: ast::Expression::Identifier { name: "b".into() }.into(),
                    }
                    .into(),
                    consequence: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Infix {
                                left: ast::Expression::Identifier { name: "a".into() }.into(),
                                right: ast::Expression::Identifier { name: "b".into() }.into(),
                                op: token::Token::Slash,
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
            "[1,\"test\",'c',(1+3)*2]",
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
                            op: token::Token::Plus,
                            right: ast::Expression::Integer { value: 3 }.into(),
                        }
                        .into(),
                        op: token::Token::Asterisk,
                        right: ast::Expression::Integer { value: 2 }.into(),
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
    fn parse_index() {
        let tests: [(&str, ast::Node); 5] = [
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
                    op: token::Token::Plus,
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
}
