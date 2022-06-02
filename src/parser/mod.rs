//! Parser bnf
//!
//! start: stmt_list
//!
//! stmt_list: stmt*
//!
//! stmt: fn_stmt
//!
//! fn_stmt: 'fn' IDENT fn_args ':' type block
//!
//! fn_args: '(' (id_type ',')* ')'
//!
//! id_type: IDENT ':' type
//!
//! type: IDENT
//!     | '&' IDENT
//!
//! block: '{' (block_stmt)* '}'
//!
//! block_stmt: let_assign ';'
//!     | return ';'
//!     | assign ';'
//!     | branch
//!
//! let_assign: 'let' id_type '=' expr
//!
//! return: 'return' expr?
//!
//! assign: IDENT '=' expr
//!
//! branch: 'if' '(' expr ')' block ('else' block)?
//!
//! expr: IDENT ('(' call_args ')')?
//!     | INTEGER
//!     | FLOAT
//!     | BOOL
//!     | expr '+' expr
//!     | expr '-' expr
//!     | expr '*' expr
//!     | expr '/' expr
//!     | expr '==' expr
//!     | '(' expr ')'
//!     | '-' expr
//!
//! call_args: (expr ',')*

use std::collections::LinkedList;

use crate::{
    ast::{
        AssignNode, BinaryNode, BlockNode, BlockStmtNode, BooleanNode, BranchNode, ExprNode,
        FloatNode, FnArgsNode, FnCallNode, FnStmtNode, IdTypeNode, IdentNode, IntegerNode,
        LetAssignNode, LitNode, ReturnNode, StmtListNode, StmtNode, TypeNode, UnaryNode,
    },
    error::{self, Result},
    lexer::{Lexer, Operator, Token, TokenValue},
};

macro_rules! require_next {
    ($self:ident) => {{
        let next = $self
            .lexer
            .next()
            .ok_or(error::internal!("Missing token"))?;
        next
    }};
}

macro_rules! require_next_token {
    ($self:ident, $tok_val:pat, $($arg:tt)*) => {{
        require_next_token!($self, $tok_val => (), $($arg)*)
    }};
    ($self:ident, $tok_val:pat => $v:expr, $($arg:tt)*) => {{
        let next = require_next!($self);
        if let $tok_val = next.value {
            $v
        } else {
            return Err(error::syntax!(
                next.position.module,
                next.position.line,
                next.position.col,
                $($arg)*
            ));
        }
    }};
}

#[derive(Debug)]
enum ExprItem {
    Expr(ExprNode, Token),
    Operator(Operator, Token),
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<StmtListNode> {
        let node = self.parse_stmt_list()?;

        Ok(node)
    }

    fn parse_stmt_list(&mut self) -> Result<StmtListNode> {
        let mut nodes = vec![];
        loop {
            // End of file
            if self.lexer.next().is_none() {
                break;
            }

            let node = self.parse_stmt()?;
            nodes.push(node);
        }

        Ok(StmtListNode { list: nodes })
    }

    fn parse_stmt(&mut self) -> Result<StmtNode> {
        let next = require_next!(self);

        let node = match next.value {
            TokenValue::Fn => StmtNode::Fn(self.parse_fn_stmt()?),
            _ => {
                return Err(error::syntax!(
                    next.position.module,
                    next.position.line,
                    next.position.col,
                    "Expect 'fn'"
                ))
            }
        };

        Ok(node)
    }

    fn parse_fn_stmt(&mut self) -> Result<FnStmtNode> {
        // 'fn'
        require_next_token!(self, TokenValue::Fn, "Expect 'fn'");
        self.lexer.advance();

        // Function name
        let name = self.parse_ident()?;

        // Function args
        let args = self.parse_fn_args()?;

        // ':'
        require_next_token!(self, TokenValue::Colon, "Expect ':'");
        self.lexer.advance();

        // Return type
        let ret_ty = self.parse_type()?;

        // Block
        let body = self.parse_block()?;

        Ok(FnStmtNode {
            name,
            args,
            ret_ty,
            body,
        })
    }

    fn parse_fn_args(&mut self) -> Result<FnArgsNode> {
        // '('
        require_next_token!(self, TokenValue::LeftParen, "Expect '('");
        self.lexer.advance();

        let mut id_tys = vec![];
        loop {
            let next = require_next!(self);
            match next.value {
                // ')'
                TokenValue::RightParen => {
                    break;
                }
                // ident: ty,
                TokenValue::Ident(_) => {
                    id_tys.push(self.parse_id_type()?);

                    // Skip ',' or break if ')'
                    let next = require_next!(self);
                    match next.value {
                        TokenValue::Comma => self.lexer.advance(),
                        TokenValue::RightParen => break,
                        _ => {
                            return Err(error::syntax!(
                                next.position.module,
                                next.position.line,
                                next.position.col,
                                "Expect ')' or ','"
                            ))
                        }
                    }
                }
                _ => {
                    return Err(error::syntax!(
                        next.position.module,
                        next.position.line,
                        next.position.col,
                        "Expect ')' or ',' or identifier"
                    ));
                }
            }
        }

        // ')'
        require_next_token!(self, TokenValue::RightParen, "Expect ')'");
        self.lexer.advance();

        Ok(FnArgsNode { args: id_tys })
    }

    fn parse_block(&mut self) -> Result<BlockNode> {
        // '{'
        require_next_token!(self, TokenValue::LeftBrace, "Expect '{{'");
        self.lexer.advance();

        let mut list = vec![];
        loop {
            let next = require_next!(self);
            if let TokenValue::RightBrace = next.value {
                break;
            }

            list.push(self.parse_block_stmt()?);
        }

        // '}'
        require_next_token!(self, TokenValue::RightBrace, "Expect '}}'");
        self.lexer.advance();

        Ok(BlockNode { list })
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmtNode> {
        let next = require_next!(self);
        let node = match next.value {
            TokenValue::Let => {
                let node = BlockStmtNode::LetAssign(self.parse_let_assign()?);

                // ';'
                require_next_token!(self, TokenValue::Semicolon, "Expect ';'");
                self.lexer.advance();

                node
            }
            TokenValue::Return => {
                let node = BlockStmtNode::Return(self.parse_return()?);

                // ';'
                require_next_token!(self, TokenValue::Semicolon, "Expect ';'");
                self.lexer.advance();

                node
            }
            TokenValue::Ident(_) => {
                let node = BlockStmtNode::Assign(self.parse_assign()?);

                // ';'
                require_next_token!(self, TokenValue::Semicolon, "Expect ';'");
                self.lexer.advance();

                node
            }
            TokenValue::If => BlockStmtNode::Branch(self.parse_branch()?),
            _ => {
                return Err(error::syntax!(
                    next.position.module,
                    next.position.line,
                    next.position.col,
                    "Expect 'let' or 'return'"
                ))
            }
        };

        Ok(node)
    }

    fn parse_let_assign(&mut self) -> Result<LetAssignNode> {
        // 'let'
        require_next_token!(self, TokenValue::Let, "Expect 'let'");
        self.lexer.advance();

        let id_ty = self.parse_id_type()?;

        // '='
        require_next_token!(self, TokenValue::Assign, "Expect '='");
        self.lexer.advance();

        // Expr
        let expr = self.parse_expr()?;

        Ok(LetAssignNode {
            ty: id_ty.ty,
            assign: AssignNode {
                ident: id_ty.ident,
                expr,
            },
        })
    }

    fn parse_return(&mut self) -> Result<ReturnNode> {
        // 'return'
        require_next_token!(self, TokenValue::Return, "Expect 'return'");
        let ret_tok = require_next!(self);
        self.lexer.advance();

        let next = require_next!(self);
        let expr = match next.value {
            // Return void
            TokenValue::Semicolon => None,

            // Return expr
            _ => Some(self.parse_expr()?),
        };

        Ok(ReturnNode {
            ret: expr,
            position: ret_tok.position.clone(),
        })
    }

    fn parse_assign(&mut self) -> Result<AssignNode> {
        // Ident
        let ident = self.parse_ident()?;

        // '='
        require_next_token!(self, TokenValue::Assign, "Expect '='");
        self.lexer.advance();

        // Expr
        let expr = self.parse_expr()?;

        Ok(AssignNode { ident, expr })
    }

    fn parse_branch(&mut self) -> Result<BranchNode> {
        // 'if'
        require_next_token!(self, TokenValue::If, "Expect 'if'");
        let if_tok = require_next!(self);
        self.lexer.advance();

        // Condition
        let cond = self.parse_expr()?;

        // 'then'
        let then = self.parse_block()?;

        // 'else'
        let mut otherwise = None;
        if let Some(next) = self.lexer.next() {
            if let TokenValue::Else = next.value {
                self.lexer.advance();
                otherwise = Some(self.parse_block()?);
            }
        }

        Ok(BranchNode {
            cond,
            then,
            otherwise,
            position: if_tok.position.clone(),
        })
    }

    fn parse_id_type(&mut self) -> Result<IdTypeNode> {
        // Ident
        let id = self.parse_ident()?;

        // ':'
        require_next_token!(self, TokenValue::Colon, "Expect ':'");
        self.lexer.advance();

        // Type
        let ty = self.parse_type()?;

        Ok(IdTypeNode { ident: id, ty })
    }

    fn parse_ident(&mut self) -> Result<IdentNode> {
        let id = require_next_token!(self, TokenValue::Ident(v) => v, "Expect identifier");
        let id_tok = require_next!(self);
        self.lexer.advance();

        Ok(IdentNode {
            ident: id,
            position: id_tok.position.clone(),
        })
    }

    fn parse_type(&mut self) -> Result<TypeNode> {
        Ok(TypeNode::Ident(self.parse_ident()?))
    }

    fn parse_expr(&mut self) -> Result<ExprNode> {
        let next = require_next!(self);

        // Use Shunting-yard algorithm: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
        let mut output_queue = LinkedList::<ExprItem>::new();
        let mut op_stack = LinkedList::<(Operator, Token)>::new();
        loop {
            match self.parse_primary_expr_or_op()? {
                Some(ExprItem::Expr(node, token)) => {
                    output_queue.push_back(ExprItem::Expr(node, token))
                }
                Some(ExprItem::Operator(op, token)) => {
                    loop {
                        if let Some((top, _)) = op_stack.back() {
                            // If the top operator has higher precedence than the current operator,
                            // pop the top operator off the stack and push it to the output queue
                            if top.precedence() >= op.precedence() {
                                let (op, token) = op_stack
                                    .pop_back()
                                    .ok_or(error::internal!("OpStack is empty"))?;
                                output_queue.push_back(ExprItem::Operator(op, token));
                                continue;
                            }
                        }

                        break;
                    }

                    // Push operator to stack
                    op_stack.push_back((op, token));
                }
                None => break,
            }

            if self.lexer.next().is_none() {
                break;
            }
        }

        // Push remaining operators to output queue
        while let Some((op, token)) = op_stack.pop_back() {
            output_queue.push_back(ExprItem::Operator(op, token));
        }

        // Generate expr
        let mut expr_stack = LinkedList::<ExprNode>::new();
        while let Some(item) = output_queue.pop_front() {
            match item {
                ExprItem::Expr(node, _) => expr_stack.push_back(node),
                ExprItem::Operator(op, token) => {
                    use Operator::*;

                    let node = match op {
                        // Unary
                        Minus | Not => {
                            let node = expr_stack.pop_back().ok_or(error::syntax!(
                                token.position.module,
                                token.position.line,
                                token.position.col,
                                "Missing operand"
                            ))?;

                            ExprNode::Unary(UnaryNode {
                                expr: Box::new(node),
                                op,
                                op_position: token.position,
                            })
                        }

                        // Binary
                        Add | Sub | Mul | Div | Rem | Eq | Ne | Gt | Ge | Lt | Le | Shl | Shr
                        | And | Or | BitAnd | BitOr => {
                            let right = expr_stack.pop_back().ok_or(error::syntax!(
                                token.position.module,
                                token.position.line,
                                token.position.col,
                                "Missing operand"
                            ))?;
                            let left = expr_stack.pop_back().ok_or(error::syntax!(
                                token.position.module,
                                token.position.line,
                                token.position.col,
                                "Missing operand"
                            ))?;

                            ExprNode::Binary(BinaryNode {
                                left: Box::new(left),
                                right: Box::new(right),
                                op,
                                op_position: token.position,
                            })
                        }
                    };

                    expr_stack.push_back(node);
                }
            }
        }

        if expr_stack.len() != 1 {
            return Err(error::syntax!(
                next.position.module,
                next.position.line,
                next.position.col,
                "Invalid expression"
            ));
        }

        Ok(expr_stack.pop_front().ok_or(error::syntax!(
            next.position.module,
            next.position.line,
            next.position.col,
            "Expr is empty"
        ))?)
    }

    fn parse_primary_expr_or_op(&mut self) -> Result<Option<ExprItem>> {
        let next = require_next!(self);

        let item = match next.value {
            TokenValue::Integer(n) => {
                self.lexer.advance();
                Some(ExprItem::Expr(
                    ExprNode::Lit(LitNode::Integer(IntegerNode {
                        value: n,
                        position: next.position.clone(),
                    })),
                    next,
                ))
            }
            TokenValue::Float(n) => {
                self.lexer.advance();
                Some(ExprItem::Expr(
                    ExprNode::Lit(LitNode::Float(FloatNode {
                        value: n,
                        position: next.position.clone(),
                    })),
                    next,
                ))
            }
            TokenValue::Bool(v) => {
                self.lexer.advance();
                Some(ExprItem::Expr(
                    ExprNode::Lit(LitNode::Bool(BooleanNode {
                        value: v,
                        position: next.position.clone(),
                    })),
                    next,
                ))
            }
            TokenValue::Ident(_) => {
                let ident = self.parse_ident()?;

                let next = require_next!(self);
                if let TokenValue::LeftParen = next.value {
                    // Function call
                    Some(ExprItem::Expr(
                        ExprNode::FnCall(self.parse_function_call_expr(ident)?),
                        next,
                    ))
                } else {
                    // Variable
                    Some(ExprItem::Expr(ExprNode::Ident(ident), next))
                }
            }
            TokenValue::Operator(op) => {
                (|| -> Result<Option<ExprItem>> {
                    if let Operator::Sub = op {
                        if let Some(prev) = self.lexer.prev() {
                            match prev.value {
                                // Is subtraction
                                TokenValue::Ident(..)
                                | TokenValue::Integer(..)
                                | TokenValue::RightParen => {}
                                // Is unary minus operator
                                _ => {
                                    self.lexer.advance();
                                    return Ok(Some(ExprItem::Operator(Operator::Minus, next)));
                                }
                            }
                        }
                    }

                    self.lexer.advance();
                    Ok(Some(ExprItem::Operator(op, next)))
                })()?
            }
            TokenValue::LeftParen => Some(ExprItem::Expr(self.parse_paren_expr()?, next)),
            _ => None,
        };

        Ok(item)
    }

    fn parse_function_call_expr(&mut self, name: IdentNode) -> Result<FnCallNode> {
        // '('
        require_next_token!(self, TokenValue::LeftParen, "Expect '('");
        self.lexer.advance();

        let mut args = vec![];
        loop {
            let next = require_next!(self);
            match next.value {
                // ')'
                TokenValue::RightParen => {
                    break;
                }
                // expr
                _ => {
                    // Expr
                    args.push(self.parse_expr()?);

                    // Skip ',' or break if ')'
                    let next = require_next!(self);
                    match next.value {
                        TokenValue::Comma => self.lexer.advance(),
                        TokenValue::RightParen => break,
                        _ => {
                            return Err(error::syntax!(
                                next.position.module,
                                next.position.line,
                                next.position.col,
                                "Expect ')' or ','"
                            ))
                        }
                    }
                }
            }
        }

        // ')'
        require_next_token!(self, TokenValue::RightParen, "Expect ')'");
        self.lexer.advance();

        Ok(FnCallNode {
            fn_name: name,
            args,
        })
    }

    fn parse_paren_expr(&mut self) -> Result<ExprNode> {
        // '('
        require_next_token!(self, TokenValue::LeftParen, "Expect '('");
        self.lexer.advance();

        // Expr
        let node = self.parse_expr()?;

        // ')'
        require_next_token!(self, TokenValue::RightParen, "Expect ')'");
        self.lexer.advance();

        Ok(node)
    }
}
