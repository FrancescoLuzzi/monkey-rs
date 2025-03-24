use crate::ast;
use crate::lexer::Lexer;
use crate::token::Token;
use std::iter::Peekable;

#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
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
            panic!("Ident expected to parse_int")
        }
    }

    fn parse_float(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::Floating(num) = token {
            Some(ast::Expression::Float(ast::FloatExpression { number: num }))
        } else {
            panic!("Ident expected to parse_float")
        }
    }

    fn parse_string(&mut self, token: Token) -> Option<ast::Expression> {
        if let Token::String(value) = token {
            Some(ast::Expression::String(ast::StringExpression { value }))
        } else {
            panic!("Ident expected to parse_string")
        }
    }

    fn parse_expr_infix(&mut self, token: Token, left: ast::Expression) -> Option<ast::Expression> {
        None
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
    use crate::{ast, lexer};

    use super::Parser;

    #[test]
    fn parse_let_stmt() {
        let source = "let some = 3;";
        let lex = lexer::Lexer::new(source);
        let mut parser = Parser::new(lex);
        let program = parser.parse_program().expect("couldn't parse let stmt");
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            ast::Node::Let(ast::LetStatement {
                name: "some".into(),
                value: ast::Expression::Integer(ast::IntegerExpression { number: 3 })
            })
        )
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
