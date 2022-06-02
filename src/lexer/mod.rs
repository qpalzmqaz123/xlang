use std::rc::Rc;

use crate::error::{self, Result};

const KEYWORDS: &[(&str, TokenValue)] = &[
    ("let", TokenValue::Let),
    ("fn", TokenValue::Fn),
    ("return", TokenValue::Return),
    ("if", TokenValue::If),
    ("else", TokenValue::Else),
    ("true", TokenValue::Bool(true)),
    ("false", TokenValue::Bool(false)),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    // Binary
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Rem,    // %
    Eq,     // ==
    Ne,     // !=
    Gt,     // >
    Ge,     // >=
    Lt,     // <
    Le,     // <=
    Shl,    // <<
    Shr,    // >>
    And,    // &&
    Or,     // ||
    BitAnd, // &
    BitOr,  // |

    // Unary
    Minus, // -
    Not,   // !
}

impl Operator {
    pub fn precedence(&self) -> u32 {
        use Operator::*;

        // Higger precedence means that the operator will be evaluated first.
        match self {
            Or => 6,
            And => 7,
            BitOr => 8,
            BitAnd => 9,
            Eq | Ne => 10,
            Gt | Ge | Lt | Le => 18,
            Shl | Shr => 19,
            Add | Sub => 20,
            Mul | Div | Rem => 30,
            Not | Minus => 40,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    // Literals
    Integer(i64),
    Float(f64),
    Bool(bool),

    // Symbols
    Operator(Operator), // '+', '-', '*', '/', '=='
    LeftParen,          // '('
    RightParen,         // ')'
    Colon,              // ':'
    Comma,              // ','
    LeftBrace,          // '{'
    RightBrace,         // '}'
    LeftBracket,        // '['
    RightBracket,       // ']'
    Assign,             // '='
    Semicolon,          // ';'

    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,

    // Other
    Ident(String),
}

#[derive(Debug, Clone)]
pub struct Position {
    pub module: Rc<String>,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub position: Position,
}

#[derive(Debug)]
pub struct Lexer {
    toks: Vec<Token>,
    current_index: usize,
}

impl Lexer {
    pub fn new(module: &str, source: &str) -> Result<Self> {
        let mut internal_lexer = InternalLexer::new(module, source);
        let mut toks = vec![];

        while let Some(tok) = internal_lexer.next()? {
            toks.push(tok);
        }

        Ok(Self {
            toks,
            current_index: 0,
        })
    }

    pub fn next(&self) -> Option<Token> {
        self.toks.get(self.current_index).cloned()
    }

    pub fn next2(&self) -> Option<Token> {
        self.toks.get(self.current_index + 1).cloned()
    }

    pub fn prev(&self) -> Option<Token> {
        if self.current_index > 0 {
            self.toks.get(self.current_index - 1).cloned()
        } else {
            None
        }
    }

    pub fn advance(&mut self) {
        self.current_index += 1;
    }
}

#[derive(Debug)]
struct InternalLexer<'s> {
    source: &'s str,
    module: Rc<String>,
    index: usize,
    line: usize,
    col: usize,
}

impl<'s> InternalLexer<'s> {
    pub fn new(module: &'s str, source: &'s str) -> Self {
        let instance = Self {
            module: Rc::new(module.to_string()),
            source,
            index: 0,
            line: 1,
            col: 1,
        };

        instance
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        let mut line;
        let mut col;

        let tok_val = loop {
            line = self.line;
            col = self.col;

            let cur_ch = match self.current_char() {
                Some(c) => c,
                None => return Ok(None),
            };

            match cur_ch {
                ' ' | '\t' | '\r' => {
                    self.advance();
                    continue;
                }
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    self.advance();
                }
                '+' => {
                    self.advance();
                    break TokenValue::Operator(Operator::Add);
                }
                '-' => {
                    self.advance();
                    break TokenValue::Operator(Operator::Sub);
                }
                '*' => {
                    self.advance();
                    break TokenValue::Operator(Operator::Mul);
                }
                '/' => {
                    self.advance();
                    break TokenValue::Operator(Operator::Div);
                }
                '%' => {
                    self.advance();
                    break TokenValue::Operator(Operator::Rem);
                }
                ':' => {
                    self.advance();
                    break TokenValue::Colon;
                }
                ',' => {
                    self.advance();
                    break TokenValue::Comma;
                }
                '0'..='9' => {
                    let mut s = String::new();
                    let mut is_float = false;
                    while let Some(ch) = self.current_char() {
                        match ch {
                            '_' => self.advance(),
                            '.' => {
                                is_float = true;
                                s.push(ch);
                                self.advance();
                            }
                            '0'..='9' => {
                                s.push(ch);
                                self.advance();
                            }
                            _ => break,
                        }
                    }

                    if is_float {
                        let num = s.parse::<f64>().map_err(|e| {
                            error::lexer!(self.module, line, col, "Parse float error: {}", e)
                        })?;

                        break TokenValue::Float(num);
                    } else {
                        let num = s.parse::<i64>().map_err(|e| {
                            error::lexer!(self.module, line, col, "Parse integer error: {}", e)
                        })?;

                        break TokenValue::Integer(num);
                    }
                }
                '(' => {
                    self.advance();
                    break TokenValue::LeftParen;
                }
                ')' => {
                    self.advance();
                    break TokenValue::RightParen;
                }
                '{' => {
                    self.advance();
                    break TokenValue::LeftBrace;
                }
                '}' => {
                    self.advance();
                    break TokenValue::RightBrace;
                }
                '[' => {
                    self.advance();
                    break TokenValue::LeftBracket;
                }
                ']' => {
                    self.advance();
                    break TokenValue::RightBracket;
                }
                '=' => {
                    self.advance();

                    if let Some('=') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Eq);
                    } else {
                        break TokenValue::Assign;
                    }
                }
                '!' => {
                    self.advance();

                    if let Some('=') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Ne);
                    } else {
                        break TokenValue::Operator(Operator::Not);
                    }
                }
                '>' => {
                    self.advance();

                    if let Some('=') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Ge);
                    } else if let Some('>') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Shr);
                    } else {
                        break TokenValue::Operator(Operator::Gt);
                    }
                }
                '<' => {
                    self.advance();

                    if let Some('=') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Le);
                    } else if let Some('<') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Shl);
                    } else {
                        break TokenValue::Operator(Operator::Lt);
                    }
                }
                '&' => {
                    self.advance();

                    if let Some('&') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::And);
                    } else {
                        break TokenValue::Operator(Operator::BitAnd);
                    }
                }
                '|' => {
                    self.advance();

                    if let Some('|') = self.current_char() {
                        self.advance();
                        break TokenValue::Operator(Operator::Or);
                    } else {
                        break TokenValue::Operator(Operator::BitOr);
                    }
                }
                ';' => {
                    self.advance();
                    break TokenValue::Semicolon;
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    break self.parse_ident();
                }
                _ => {
                    return Err(error::lexer!(
                        self.module,
                        line,
                        col,
                        "unexpected character: '{}'",
                        cur_ch
                    ))
                }
            };
        };

        Ok(Some(Token {
            value: tok_val,
            position: Position {
                module: self.module.clone(),
                line,
                col,
            },
        }))
    }

    fn current_char(&self) -> Option<char> {
        self.source
            .as_bytes()
            .get(self.index)
            .cloned()
            .map(|c| c as char)
    }

    fn advance(&mut self) {
        self.col += 1;
        self.index += 1;
    }

    fn parse_ident(&mut self) -> TokenValue {
        let mut id = String::new();
        while let Some(ch) = self.current_char() {
            match ch {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    id.push(ch);
                    self.advance();
                }
                _ => break,
            }
        }

        if let Some(value) = KEYWORDS.iter().find(|v| v.0 == id).map(|v| v.1.clone()) {
            // Keywords
            value
        } else {
            // Identifier
            TokenValue::Ident(id)
        }
    }
}
