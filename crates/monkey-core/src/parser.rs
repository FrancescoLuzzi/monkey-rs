use crate::ast;
use crate::lexer::{Lexer, LexerError};
use crate::token::{Token, TokenType};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

pub fn escape_str(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len());
    for ch in input.chars() {
        push_escaped_char(&mut escaped, ch);
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

fn unescape_char(input: &str) -> Option<char> {
    let mut chars = input.chars();
    let value = match chars.next()? {
        '\\' => match chars.next()? {
            '\\' => '\\',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '0' => '\0',
            '"' => '"',
            '\'' => '\'',
            _ => return None,
        },
        ch => ch,
    };

    if chars.next().is_none() {
        Some(value)
    } else {
        None
    }
}

fn push_escaped_char(output: &mut String, ch: char) {
    match ch {
        '\\' => output.push_str("\\\\"),
        '\n' => output.push_str("\\n"),
        '\r' => output.push_str("\\r"),
        '\t' => output.push_str("\\t"),
        '\0' => output.push_str("\\0"),
        '"' => output.push_str("\\\""),
        '\'' => output.push_str("\\\'"),
        _ => output.push(ch),
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
fn get_token_precedence(token_type: TokenType) -> Precedence {
    match token_type {
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

type ParseResult<T> = Result<T, ParserError>;

#[derive(Debug, Error, Diagnostic)]
pub enum ParserError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lex(#[from] LexerError),

    #[error("expected {expected}, found {found}")]
    #[diagnostic(
        code(monkey::parser::unexpected_token),
        help(
            "check the syntax around this token and make sure delimiters and separators are balanced"
        )
    )]
    UnexpectedToken {
        expected: &'static str,
        found: TokenType,
        #[label("unexpected token")]
        span: SourceSpan,
    },

    #[error("expected {expected}, but reached the end of input")]
    #[diagnostic(
        code(monkey::parser::unexpected_eof),
        help("the input ended before this construct was complete")
    )]
    UnexpectedEof {
        expected: &'static str,
        #[label("input ends here")]
        span: SourceSpan,
    },

    #[error("expected an expression, found {found}")]
    #[diagnostic(
        code(monkey::parser::expected_expression),
        help(
            "expressions can start with identifiers, literals, prefix operators, `fn`, `if`, `(`, `[` or `{{`"
        )
    )]
    ExpectedExpression {
        found: TokenType,
        #[label("this token cannot start an expression")]
        span: SourceSpan,
    },

    #[error("invalid integer literal")]
    #[diagnostic(
        code(monkey::parser::invalid_integer),
        help("make sure the integer fits into a signed 64-bit value")
    )]
    InvalidIntegerLiteral {
        #[label("this integer cannot be parsed")]
        span: SourceSpan,
    },

    #[error("invalid float literal")]
    #[diagnostic(
        code(monkey::parser::invalid_float),
        help("make sure this is a valid floating-point literal")
    )]
    InvalidFloatLiteral {
        #[label("this float cannot be parsed")]
        span: SourceSpan,
    },

    #[error("invalid string literal")]
    #[diagnostic(
        code(monkey::parser::invalid_string),
        help("check the escape sequences in this string literal")
    )]
    InvalidStringLiteral {
        #[label("this string literal could not be decoded")]
        span: SourceSpan,
    },

    #[error("invalid character literal")]
    #[diagnostic(
        code(monkey::parser::invalid_char),
        help("character literals must decode to exactly one character")
    )]
    InvalidCharacterLiteral {
        #[label("this character literal is not valid")]
        span: SourceSpan,
    },
}

pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            source: lexer.source(),
            lexer,
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<ast::Program> {
        let mut program = ast::Program::new();
        while let Some(token) = self.next_token()? {
            program.statements.push(self.parse(token)?);
        }
        Ok(program)
    }

    fn parse(&mut self, token: Token<'a>) -> ParseResult<ast::Node> {
        let result = match token._type {
            TokenType::Let => self.parse_let_stmt(token),
            TokenType::Return => self.parse_return_stmt(token),
            TokenType::Function => self.parse_function_decl(token),
            _ => Ok(ast::Node::Expression(
                self.parse_expr(token, Precedence::Min)?,
            )),
        }?;
        self.skip_optional_semicolon()?;
        Ok(result)
    }

    fn parse_let_stmt(&mut self, token: Token<'a>) -> ParseResult<ast::Node> {
        debug_assert_eq!(token._type, TokenType::Let);

        let ident_token = self.next_required_token("an identifier after `let`")?;
        let ident = if ident_token._type == TokenType::Ident {
            ident_token.slice.to_string()
        } else {
            return self.unexpected_token(ident_token, "an identifier after `let`");
        };

        self.expect_next_token(TokenType::Assign, "`=` after the variable name")?;
        let value_token = self.next_required_token("an expression after `=`")?;

        if value_token._type == TokenType::Function && self.peek_is(TokenType::Ident)? {
            let name_token = self.next_required_token("an anonymous function in a let binding")?;
            return self.unexpected_token(name_token, "an anonymous function in a let binding");
        }

        let parsed_expr = self.parse_expr(value_token, Precedence::Min)?;

        match parsed_expr {
            ast::Expression::Function {
                name,
                parameters,
                body,
            } => {
                if name.is_some() {
                    self.unexpected_token(token, "can't let define a function that has a name")
                } else {
                    Ok(ast::Node::Let {
                        name: ident.clone(),
                        value: ast::Expression::Function {
                            name: Some(ident),
                            parameters,
                            body,
                        },
                    })
                }
            }
            value => Ok(ast::Node::Let { name: ident, value }),
        }
    }

    fn parse_return_stmt(&mut self, token: Token<'a>) -> ParseResult<ast::Node> {
        debug_assert_eq!(token._type, TokenType::Return);

        let value_token = self.next_required_token("an expression after `return`")?;
        Ok(ast::Node::Return {
            value: self.parse_expr(value_token, Precedence::Min)?,
        })
    }

    fn parse_expr(
        &mut self,
        token: Token<'a>,
        precedence: Precedence,
    ) -> ParseResult<ast::Expression> {
        let mut expr = self.parse_expr_prefix(token)?;
        while !self.skip_optional_semicolon()? && precedence < self.peek_precedence()? {
            let next_token = self.next_required_token("an infix operator")?;
            expr = self.parse_expr_infix(next_token, expr)?;
        }
        Ok(expr)
    }

    fn parse_expr_prefix(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        match token._type {
            TokenType::Ident => self.parse_ident(token),
            TokenType::Integer => self.parse_int(token),
            TokenType::Floating => self.parse_float(token),
            TokenType::String => self.parse_string(token),
            TokenType::True | TokenType::False => Ok(ast::Expression::Bool {
                value: token._type == TokenType::True,
            }),
            TokenType::Null => Ok(ast::Expression::Null),
            TokenType::Char => self.parse_char(token),
            TokenType::Minus => self.parse_minus(token),
            TokenType::Bang => self.parse_negated(token),
            TokenType::Function => self.parse_function(token),
            TokenType::Lparen => self.parse_group(token),
            TokenType::If => self.parse_if(token),
            TokenType::Lsquare => self.parse_array(token),
            TokenType::Lsquirly => self.parse_dict(token),
            _ => Err(ParserError::ExpectedExpression {
                found: token._type,
                span: token_span(&token),
            }),
        }
    }

    fn parse_ident(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Ident);
        Ok(ast::Expression::Identifier {
            name: token.slice.to_string(),
        })
    }

    fn parse_int(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Integer);

        let value = token
            .slice
            .parse::<i64>()
            .map_err(|_| ParserError::InvalidIntegerLiteral {
                span: token_span(&token),
            })?;

        Ok(ast::Expression::Integer { value })
    }

    fn parse_float(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Floating);

        let value = token
            .slice
            .parse::<f64>()
            .map_err(|_| ParserError::InvalidFloatLiteral {
                span: token_span(&token),
            })?;

        Ok(ast::Expression::Float { value })
    }

    fn parse_string(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::String);

        let literal =
            token
                .slice
                .strip_suffix('"')
                .ok_or_else(|| ParserError::InvalidStringLiteral {
                    span: token_span(&token),
                })?;

        let value = unescape_str(literal).ok_or_else(|| ParserError::InvalidStringLiteral {
            span: token_span(&token),
        })?;

        Ok(ast::Expression::String { value })
    }

    fn parse_char(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Char);

        let literal = token
            .slice
            .strip_prefix('\'')
            .and_then(|slice| slice.strip_suffix('\''))
            .ok_or_else(|| ParserError::InvalidCharacterLiteral {
                span: token_span(&token),
            })?;

        let value = unescape_char(literal).ok_or_else(|| ParserError::InvalidCharacterLiteral {
            span: token_span(&token),
        })?;

        Ok(ast::Expression::Char { value })
    }

    fn parse_minus(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Minus);

        let next_token = self.next_required_token("an expression after `-`")?;
        Ok(ast::Expression::Minus {
            value: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        })
    }

    fn parse_negated(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Bang);

        let next_token = self.next_required_token("an expression after `!`")?;
        Ok(ast::Expression::Negated {
            value: Box::new(self.parse_expr(next_token, Precedence::Prefix)?),
        })
    }

    fn parse_group(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Lparen);

        let next_token = self.next_required_token("an expression after `(`")?;
        let result = self.parse_expr(next_token, Precedence::Min)?;
        self.expect_next_token(TokenType::Rparen, "`)` to close this grouped expression")?;
        Ok(result)
    }

    fn parse_function(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Function);

        let name = if self.peek_is(TokenType::Ident)? {
            let ident_token = self.next_required_token("a function name after fn")?;
            Some(ident_token.slice.to_string())
        } else {
            None
        };
        self.expect_next_token(TokenType::Lparen, "`(` after `fn`")?;
        let parameters = self.parse_function_params()?;
        let body_token = self.next_required_token("a function body after the parameter list")?;

        Ok(ast::Expression::Function {
            name,
            parameters,
            body: self.parse_block(body_token)?,
        })
    }

    fn parse_function_decl(&mut self, token: Token<'a>) -> ParseResult<ast::Node> {
        debug_assert_eq!(token._type, TokenType::Function);
        let function = self.parse_function(token)?;

        match function {
            ast::Expression::Function {
                name: Some(name),
                parameters,
                body,
            } => Ok(ast::Node::Let {
                name: name.clone(),
                value: ast::Expression::Function {
                    name: Some(name),
                    parameters,
                    body,
                },
            }),
            function => Ok(ast::Node::Expression(function)),
        }
    }

    fn parse_if(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::If);

        let condition_token = self.next_required_token("`(` after `if`")?;
        let condition = self.parse_group(condition_token)?;
        let consequence_token = self.next_required_token("an if-body after the condition")?;
        let consequence = if consequence_token._type == TokenType::Lsquirly {
            self.parse_block(consequence_token)?
        } else {
            ast::Block {
                statements: vec![self.parse(consequence_token)?],
            }
        };

        if self.peek_is(TokenType::Else)? {
            self.next_token()?;
            let alternative_token = self.next_required_token("an else-branch after `else`")?;
            let alternative = if alternative_token._type == TokenType::Lsquirly {
                self.parse_block(alternative_token)?
            } else {
                ast::Block {
                    statements: vec![self.parse(alternative_token)?],
                }
            };

            Ok(ast::Expression::If {
                condition: Box::new(condition),
                consequence,
                alternative: Some(alternative),
            })
        } else {
            Ok(ast::Expression::If {
                condition: Box::new(condition),
                consequence,
                alternative: None,
            })
        }
    }

    fn parse_array(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Lsquare);

        let mut values = Vec::new();
        if self.peek_is(TokenType::Rsquare)? {
            self.next_token()?;
            return Ok(ast::Expression::Array { values });
        }

        loop {
            let next_token = self.next_required_token("an array element")?;
            values.push(self.parse_expr(next_token, Precedence::Min)?);
            if !self.skip_optional_comma()? {
                break;
            }
        }

        self.expect_next_token(TokenType::Rsquare, "`]` to close this array")?;
        Ok(ast::Expression::Array { values })
    }

    fn parse_dict(&mut self, token: Token<'a>) -> ParseResult<ast::Expression> {
        debug_assert_eq!(token._type, TokenType::Lsquirly);

        let mut values = Vec::new();
        if self.peek_is(TokenType::Rsquirly)? {
            self.next_token()?;
            return Ok(ast::Expression::Dict { values });
        }

        loop {
            let key_token = self.next_required_token("a dictionary key")?;
            let key = self.parse_expr(key_token, Precedence::Min)?;
            self.expect_next_token(TokenType::Colon, "`:` between a dictionary key and value")?;
            let value_token = self.next_required_token("a dictionary value")?;
            let value = self.parse_expr(value_token, Precedence::Min)?;
            values.push((key, value));
            if !self.skip_optional_comma()? {
                break;
            }
        }

        self.expect_next_token(TokenType::Rsquirly, "`}` to close this dictionary")?;
        Ok(ast::Expression::Dict { values })
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<String>> {
        let mut result = Vec::new();
        if self.peek_is(TokenType::Rparen)? {
            self.next_token()?;
            return Ok(result);
        }

        loop {
            let token = self.next_required_token("a function parameter")?;
            if token._type == TokenType::Ident {
                result.push(token.slice.to_string());
            } else {
                return self.unexpected_token(token, "a parameter name");
            }

            if !self.skip_optional_comma()? {
                break;
            }
        }

        self.expect_next_token(TokenType::Rparen, "`)` after the parameter list")?;
        Ok(result)
    }

    fn parse_block(&mut self, token: Token<'a>) -> ParseResult<ast::Block> {
        if token._type != TokenType::Lsquirly {
            return self.unexpected_token(token, "`{` to start a block");
        }

        let mut statements = Vec::new();
        loop {
            let Some(next_token) = self.next_token()? else {
                return self.unexpected_eof("`}` to close this block");
            };

            if next_token._type == TokenType::Rsquirly {
                break;
            }

            statements.push(self.parse(next_token)?);
        }

        Ok(ast::Block { statements })
    }

    fn parse_expr_infix(
        &mut self,
        token: Token<'a>,
        left: ast::Expression,
    ) -> ParseResult<ast::Expression> {
        match token._type {
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
                let precedence = get_token_precedence(token._type);
                let next_token =
                    self.next_required_token("the right-hand side of this expression")?;
                Ok(ast::Expression::Infix {
                    left: Box::new(left),
                    op: token._type,
                    right: Box::new(self.parse_expr(next_token, precedence)?),
                })
            }
            TokenType::Lparen => self.parse_call(token, left),
            TokenType::Lsquare => self.parse_index_square(token, left),
            TokenType::Dot => {
                if self.peek_is(TokenType::Lparen)? {
                    let call_token = self.next_required_token("`(` after `.`")?;
                    self.parse_call(call_token, left)
                } else {
                    self.parse_index_dot(token, left)
                }
            }
            _ => self.unexpected_token(token, "a valid infix operator"),
        }
    }

    fn parse_index_square(
        &mut self,
        token: Token<'a>,
        left: ast::Expression,
    ) -> ParseResult<ast::Expression> {
        if token._type != TokenType::Lsquare {
            return self.unexpected_token(token, "`[` to start an index expression");
        }

        let index_token = self.next_required_token("an index expression")?;
        let index = self.parse_expr(index_token, Precedence::Min)?;
        self.expect_next_token(TokenType::Rsquare, "`]` to close this index expression")?;

        Ok(ast::Expression::Index {
            value: left.into(),
            index: index.into(),
        })
    }

    fn parse_index_dot(
        &mut self,
        token: Token<'a>,
        left: ast::Expression,
    ) -> ParseResult<ast::Expression> {
        if token._type != TokenType::Dot {
            return self.unexpected_token(token, "`.` to access a field");
        }

        let index_token = self.next_required_token("a field name or index after `.`")?;
        let index = self.parse_expr(index_token, Precedence::Index)?;
        Ok(ast::Expression::Index {
            value: left.into(),
            index: index.into(),
        })
    }

    fn parse_call(
        &mut self,
        token: Token<'a>,
        left: ast::Expression,
    ) -> ParseResult<ast::Expression> {
        if token._type != TokenType::Lparen {
            return self.unexpected_token(token, "`(` to start a call expression");
        }

        let function = Box::new(left);
        let mut parameters = Vec::new();
        if self.peek_is(TokenType::Rparen)? {
            self.next_token()?;
            return Ok(ast::Expression::Call {
                function,
                parameters,
            });
        }

        loop {
            let next_token = self.next_required_token("a call argument")?;
            parameters.push(self.parse_expr(next_token, Precedence::Min)?);
            if !self.skip_optional_comma()? {
                break;
            }
        }

        self.expect_next_token(TokenType::Rparen, "`)` to close this call")?;
        Ok(ast::Expression::Call {
            function,
            parameters,
        })
    }

    fn expect_next_token(
        &mut self,
        expected_type: TokenType,
        expected_description: &'static str,
    ) -> ParseResult<Token<'a>> {
        let token = self.next_required_token(expected_description)?;
        if token._type == expected_type {
            Ok(token)
        } else {
            self.unexpected_token(token, expected_description)
        }
    }

    fn next_required_token(&mut self, expected: &'static str) -> ParseResult<Token<'a>> {
        self.next_token()?
            .ok_or_else(|| ParserError::UnexpectedEof {
                expected,
                span: eof_span(self.source),
            })
    }

    fn unexpected_token<T>(&self, token: Token<'a>, expected: &'static str) -> ParseResult<T> {
        Err(ParserError::UnexpectedToken {
            expected,
            found: token._type,
            span: token_span(&token),
        })
    }

    fn unexpected_eof<T>(&self, expected: &'static str) -> ParseResult<T> {
        Err(ParserError::UnexpectedEof {
            expected,
            span: eof_span(self.source),
        })
    }

    fn skip_optional_comma(&mut self) -> ParseResult<bool> {
        if self.peek_is(TokenType::Comma)? {
            self.next_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn skip_optional_semicolon(&mut self) -> ParseResult<bool> {
        if self.peek_is(TokenType::Semicolon)? {
            self.next_token()?;
            Ok(true)
        } else {
            Ok(self.peek_token_type()?.is_none())
        }
    }

    fn next_token(&mut self) -> ParseResult<Option<Token<'a>>> {
        match self.lexer.next_token().map_err(ParserError::from)? {
            Token {
                _type: TokenType::Eof,
                ..
            } => Ok(None),
            token => Ok(Some(token)),
        }
    }

    fn peek_token_type(&mut self) -> ParseResult<Option<TokenType>> {
        match self.lexer.peek_token() {
            Ok(token) if token._type == TokenType::Eof => Ok(None),
            Ok(token) => Ok(Some(token._type)),
            Err(_) => match self.lexer.next_token() {
                Ok(_) => unreachable!("lexer peeked error should still be buffered"),
                Err(error) => Err(error.into()),
            },
        }
    }

    fn peek_is(&mut self, token_type: TokenType) -> ParseResult<bool> {
        Ok(matches!(self.peek_token_type()?, Some(peeked_type) if peeked_type == token_type))
    }

    fn peek_precedence(&mut self) -> ParseResult<Precedence> {
        Ok(self
            .peek_token_type()?
            .map(get_token_precedence)
            .unwrap_or(Precedence::Min))
    }
}

fn token_span(token: &Token<'_>) -> SourceSpan {
    (
        token.start_index,
        token.end_index.saturating_sub(token.start_index) + 1,
    )
        .into()
}

fn eof_span(input: &str) -> SourceSpan {
    (input.len(), 0).into()
}

#[cfg(test)]
mod test {
    use crate::{ast, lexer, token::TokenType};

    use super::{Parser, ParserError, escape_str, unescape_str};

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
                        name: Some("some".into()),
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
                        name: Some("some".into()),
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
                .unwrap_or_else(|error| panic!("couldn't parse let stmt: {source}\n{error:?}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_return() {
        let source = "return \"hello\";";
        let lex = lexer::Lexer::new(source);
        let mut parser = Parser::new(lex);
        let program = parser
            .parse_program()
            .unwrap_or_else(|error| panic!("couldn't parse let stmt\n{error:?}"));
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

    #[test]
    fn parse_function_declaration() {
        let source = "fn add(a, b) { return a + b; }";
        let lex = lexer::Lexer::new(source);
        let mut parser = Parser::new(lex);
        let program = parser
            .parse_program()
            .unwrap_or_else(|error| panic!("couldn't parse function declaration\n{error:?}"));

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            ast::Node::Let {
                name: "add".into(),
                value: ast::Expression::Function {
                    name: Some("add".into()),
                    parameters: vec!["a".into(), "b".into()],
                    body: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Infix {
                                left: ast::Expression::Identifier { name: "a".into() }.into(),
                                op: token(TokenType::Plus, "+"),
                                right: ast::Expression::Identifier { name: "b".into() }.into(),
                            },
                        }],
                    },
                },
            }
        );
    }

    #[test]
    fn parse_let_defined_function() {
        let source = "let add = fn(a, b) { return a + b; };";
        let lex = lexer::Lexer::new(source);
        let mut parser = Parser::new(lex);
        let program = parser
            .parse_program()
            .unwrap_or_else(|error| panic!("couldn't parse let-defined function\n{error:?}"));

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            ast::Node::Let {
                name: "add".into(),
                value: ast::Expression::Function {
                    name: Some("add".into()),
                    parameters: vec!["a".into(), "b".into()],
                    body: ast::Block {
                        statements: vec![ast::Node::Return {
                            value: ast::Expression::Infix {
                                left: ast::Expression::Identifier { name: "a".into() }.into(),
                                op: token(TokenType::Plus, "+"),
                                right: ast::Expression::Identifier { name: "b".into() }.into(),
                            },
                        }],
                    },
                },
            }
        );
    }

    #[test]
    fn parse_rejects_let_defined_function_with_name() {
        let error = Parser::new(lexer::Lexer::new(
            "let add = fn named(a, b) { return a + b; };",
        ))
        .parse_program()
        .expect_err("let-defined function with a name should be rejected");

        assert!(matches!(error, ParserError::UnexpectedToken { .. }));
        assert_eq!(
            error.to_string(),
            "expected an anonymous function in a let binding, found ident"
        );
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
                .unwrap_or_else(|error| panic!("couldn't parse let stmt: {source}\n{error:?}"));
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
                .unwrap_or_else(|error| panic!("couldn't parse let stmt: {source}\n{error:?}"));
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
                        name: None,
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
                .unwrap_or_else(|error| panic!("couldn't parse let stmt: {source}\n{error:?}"));
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
                            name: None,
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
                .unwrap_or_else(|error| panic!("couldn't parse let stmt: {source}\n{error:?}"));
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
                .unwrap_or_else(|error| panic!("couldn't parse let stmt: {source}\n{error:?}"));
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node)
        }
    }

    #[test]
    fn parse_string_and_char_escapes() {
        let tests: [(&str, ast::Node); 6] = [
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
            (
                r#"'\t'"#,
                ast::Node::Expression(ast::Expression::Char { value: '\t' }),
            ),
            (
                r#"'\''"#,
                ast::Node::Expression(ast::Expression::Char { value: '\'' }),
            ),
            (
                r#"'\0'"#,
                ast::Node::Expression(ast::Expression::Char { value: '\0' }),
            ),
            (
                r#"'\\'"#,
                ast::Node::Expression(ast::Expression::Char { value: '\\' }),
            ),
        ];

        for (source, node) in tests {
            let lex = lexer::Lexer::new(source);
            let mut parser = Parser::new(lex);
            let program = parser.parse_program().unwrap_or_else(|error| {
                panic!("couldn't parse escaped literal: {source}\n{error:?}")
            });
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], node);
        }
    }

    #[test]
    fn parse_reports_missing_assignment_value() {
        let error = Parser::new(lexer::Lexer::new("let value = ;"))
            .parse_program()
            .expect_err("missing value should be reported");

        assert!(matches!(error, ParserError::ExpectedExpression { .. }));
        assert_eq!(error.to_string(), "expected an expression, found ;");
    }

    #[test]
    fn escape_roundtrip() {
        let original = "line\nnext\rrow\tcol\\quote\"tick'\0";
        let escaped = escape_str(original);

        assert_eq!(unescape_str(&escaped), Some(original.to_string()));
    }
}
