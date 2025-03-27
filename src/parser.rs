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
}

#[inline]
fn get_token_precedence(tok: &Token) -> Precedence {
    match tok {
        Token::Eq | Token::Neq => Precedence::Equals,
        Token::Gt | Token::Lt | Token::Ge | Token::Le => Precedence::Diff,
        Token::Plus => Precedence::Sum,
        Token::Asterisk => Precedence::Prod,
        Token::Bang | Token::Minus => Precedence::Prefix,
        Token::Lparen => Precedence::Call,
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
            // support fn test(){...}
            // it needs to be represented as Let(name,FunctionExpr(block))
            //Token::Function => self.parse_function_stmt(),
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

        Some(ast::Node::Let(ast::LetStatement {
            name: ident,
            value: self.parse_expr(token, Precedence::Min)?,
        }))
    }

    fn parse_return_stmt(&mut self, token: Token) -> Option<ast::Node> {
        assert_eq!(token, Token::Return);
        let token = self.next_token()?;

        Some(ast::Node::Return(ast::ReturnStatement {
            value: self.parse_expr(token, Precedence::Min)?,
        }))
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
            Token::Char(_) => self.parse_char(token),
            Token::Bang => self.parse_negated(token),
            Token::Function => self.parse_function(token),
            Token::Lparen => self.parse_group(token),
            _ => None,
        }
    }

    fn parse_ident(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Ident(ident) = token {
            Some(ast::Expression::Identifier(ast::IdentifierExpression {
                name: ident,
            }))
        } else {
            panic!("Ident expected to parse_ident")
        }
    }

    fn parse_int(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Integer(num) = token {
            Some(ast::Expression::Integer(ast::IntegerExpression {
                number: num,
            }))
        } else {
            panic!("Integer expected to parse_int")
        }
    }

    fn parse_float(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Floating(num) = token {
            Some(ast::Expression::Float(ast::FloatExpression { number: num }))
        } else {
            panic!("Floating expected to parse_float")
        }
    }

    fn parse_string(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::String(value) = token {
            Some(ast::Expression::String(ast::StringExpression { value }))
        } else {
            panic!("String expected to parse_string")
        }
    }

    fn parse_char(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Char(value) = token {
            Some(ast::Expression::Char(ast::CharExpression { value }))
        } else {
            panic!("Char expected to parse_char")
        }
    }

    fn parse_negated(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Bang) {
            panic!("Bang expected to parse_negated")
        }

        let next_token = self.next_token()?;
        Some(ast::Expression::Negated(ast::NegatedExpression {
            expr: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        }))
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
        if let ast::Expression::Block(block) = self.parse_block(next_token)? {
            Some(ast::Expression::Function(ast::FunctionExpression {
                parameters: params,
                body: block,
            }))
        } else {
            panic!("parse_block returne something that wasnt a block")
        }
    }

    fn parse_function_params(&mut self) -> Option<Vec<ast::IdentifierExpression>> {
        let mut result: Vec<ast::IdentifierExpression> = Vec::new();
        if matches!(self.lexer.peek()?, Token::Rparen) {
            self.next_token()?;
            return Some(result);
        }
        loop {
            if let Token::Ident(ident) = self.next_token()? {
                result.push(ast::IdentifierExpression { name: ident });
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

    fn parse_block(&mut self, token: Token) -> Option<ast::Expression> {
        if !matches!(token, Token::Lsquirly) {
            panic!("Lsquirly expected to parse_block")
        }
        let mut statements: Vec<ast::Node> = Vec::new();
        let mut next_token = self.next_token()?;
        while !matches!(next_token, Token::Rsquirly | Token::Eof) {
            statements.push(self.parse(next_token)?);
            next_token = self.next_token()?;
        }
        Some(ast::Expression::Block(ast::BlockExpression { statements }))
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
                Some(ast::Expression::Infix(ast::InfixExpression {
                    left: Box::new(left),
                    op: token.clone(),
                    right: Box::new(self.parse_expr(next_token, precedence)?),
                }))
            }
            Token::Lparen => self.parse_call(token, left),
            _ => None,
        }
    }

    fn parse_call(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        if !matches!(token, Token::Lparen) {
            panic!("Lparen expected to parse_call")
        }
        let function = if let ast::Expression::Identifier(func_name) = left {
            func_name.name
        } else {
            return None;
        };

        let mut parameters = Vec::new();
        if matches!(self.lexer.peek()?, Token::Rparen) {
            self.next_token()?;
            return Some(ast::Expression::Call(ast::CallExpression {
                function,
                parameters,
            }));
        }

        loop {
            let next_token = self.next_token()?;
            parameters.push(self.parse_expr(next_token, Precedence::Min)?);
            if !self.skip_optional_comma() {
                break;
            }
        }
        match self.next_token()? {
            Token::Rparen => Some(ast::Expression::Call(ast::CallExpression {
                function,
                parameters,
            })),

            _ => None,
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
        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.next_token();
            true
        } else {
            false
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
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::Integer(ast::IntegerExpression { number: 3 }),
                }),
            ),
            (
                "let some = \"some\";",
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::String(ast::StringExpression {
                        value: "some".into(),
                    }),
                }),
            ),
            (
                "let some = 's';",
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::Char(ast::CharExpression { value: 's' }),
                }),
            ),
            (
                "let some = test;",
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::Identifier(ast::IdentifierExpression {
                        name: "test".into(),
                    }),
                }),
            ),
            (
                "let some = 1 + 2;",
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::Infix(ast::InfixExpression {
                        left: ast::Expression::Integer(ast::IntegerExpression { number: 1 }).into(),
                        op: token::Token::Plus,
                        right: ast::Expression::Integer(ast::IntegerExpression { number: 2 })
                            .into(),
                    }),
                }),
            ),
            (
                "let some = fn(){};",
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::Function(ast::FunctionExpression {
                        parameters: Vec::new(),
                        body: ast::BlockExpression {
                            statements: Vec::new(),
                        },
                    }),
                }),
            ),
            (
                "let some = fn(a,b){let x = a+b; return x;};",
                ast::Node::Let(ast::LetStatement {
                    name: "some".into(),
                    value: ast::Expression::Function(ast::FunctionExpression {
                        parameters: vec![
                            ast::IdentifierExpression { name: "a".into() },
                            ast::IdentifierExpression { name: "b".into() },
                        ],
                        body: ast::BlockExpression {
                            statements: vec![
                                ast::Node::Let(ast::LetStatement {
                                    name: "x".into(),
                                    value: ast::Expression::Infix(ast::InfixExpression {
                                        left: ast::Expression::Identifier(
                                            ast::IdentifierExpression { name: "a".into() },
                                        )
                                        .into(),
                                        op: token::Token::Plus,
                                        right: ast::Expression::Identifier(
                                            ast::IdentifierExpression { name: "b".into() },
                                        )
                                        .into(),
                                    }),
                                }),
                                ast::Node::Return(ast::ReturnStatement {
                                    value: ast::Expression::Identifier(ast::IdentifierExpression {
                                        name: "x".into(),
                                    }),
                                }),
                            ],
                        },
                    }),
                }),
            ),
            (
                "let res = test();",
                ast::Node::Let(ast::LetStatement {
                    name: "res".into(),
                    value: ast::Expression::Call(ast::CallExpression {
                        function: "test".into(),
                        parameters: Vec::new(),
                    }),
                }),
            ),
            (
                "let res = test(a, (b+1)*2 );",
                ast::Node::Let(ast::LetStatement {
                    name: "res".into(),
                    value: ast::Expression::Call(ast::CallExpression {
                        function: "test".into(),
                        parameters: vec![
                            ast::Expression::Identifier(ast::IdentifierExpression {
                                name: "a".into(),
                            }),
                            ast::Expression::Infix(ast::InfixExpression {
                                left: ast::Expression::Infix(ast::InfixExpression {
                                    left: ast::Expression::Identifier(ast::IdentifierExpression {
                                        name: "b".into(),
                                    })
                                    .into(),
                                    op: token::Token::Plus,
                                    right: ast::Expression::Integer(ast::IntegerExpression {
                                        number: 1,
                                    })
                                    .into(),
                                })
                                .into(),
                                op: token::Token::Asterisk,
                                right: ast::Expression::Integer(ast::IntegerExpression {
                                    number: 2,
                                })
                                .into(),
                            }),
                        ],
                    }),
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
    fn parse_return() {
        let source = "return \"hello\";";
        let lex = lexer::Lexer::new(source);
        let mut parser = Parser::new(lex);
        let program = parser.parse_program().expect("couldn't parse let stmt");
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            ast::Node::Return(ast::ReturnStatement {
                value: ast::Expression::String(ast::StringExpression {
                    value: "hello".into(),
                })
            })
        )
    }
}
