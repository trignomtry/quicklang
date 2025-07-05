use crate::TokenKind::*;
use actix_web::{App, HttpRequest, HttpResponse, HttpServer, Responder};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_native;
use dashmap::DashMap;
use fastrand::f64 as random;
use std::collections::HashMap;
use std::env;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::sync::{Arc, RwLock};

#[derive(Debug, Clone)]
struct Token {
    value: std::string::String,
    kind: TokenKind,
    line: usize,
}

impl Token {
    fn print(&self) {
        let token = self;
        if let Error(_, _) = token.kind {
            eprintln!("{}{}", token.kind, token.value);
        } else if let Str(_) = token.kind {
            println!("{} \"{}\" {}", token.kind, token.value, token.value);
        } else if let Eof = token.kind {
            println!("EOF  null");
        } else if let Num(_) = token.kind {
            println!(
                "{} {} {}",
                token.kind,
                token.value,
                format_float(&token.value)
            );
        } else {
            println!("{} {} null", token.kind, token.value);
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    Star,
    Dot,
    Comma,
    Plus,
    Minus,
    Colon,
    Semicolon,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,
    Str(String),
    Identifier(String),
    And,
    Object,
    Maybe,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Reprint,
    Return,
    Super,
    This,
    True,
    Let,
    While,
    Eof,
    Num(f64),
    Error(u64, std::string::String),
}

#[derive(Debug, Clone)]
enum Unary {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    NotEq,
    EqEq,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone)]
enum Expr {
    Literal(Value),
    Variable(String),
    Unary(Unary, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Grouping(Box<Expr>),
    Get(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    List(Vec<Expr>),
    Object(DashMap<String, Expr>),
    Block(Vec<Instruction>),
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "Function block")
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Function((*self.0).clone_box())
    }
}

#[derive(Debug, Clone)]
enum Instruction {
    Let {
        name: String,
        value: Expr,
        type_hint: Option<Type>, // optional, inferred if None
    },
    Assign(String, Expr, Option<Type>),
    Println(Expr),
    Return(Expr),
    Expr(Expr), // For function calls or side-effect exprs
    If {
        condition: Expr,
        then_block: Vec<Instruction>,
        elses: Option<Box<Instruction>>,
    },
    While {
        condition: Expr,
        body: Vec<Instruction>,
    },
    For {
        init: Box<Instruction>,
        condition: Expr,
        step: Box<Instruction>,
        body: Vec<Instruction>,
    },
    Block(Vec<Instruction>),
    FunctionDef {
        name: String,
        params: Vec<(String, Type)>,
        return_type: Type,
        body: Vec<Instruction>,
    },
    CallFn {
        dest: Option<String>,
        name: String,
        args: Vec<Expr>,
    },
    Maybe(Expr, Box<Instruction>, Option<Box<Instruction>>),
    Nothing,
}

#[derive(Clone)]
enum Value {
    Num(f64),
    Str(String),
    /// Special return value used to signal early exit from functions
    Return(Box<Value>),
    Object(DashMap<String, Value>),
    Bool(bool),
    Function(Vec<(String, String)>, Function),
    List(Vec<Value>),
    Nil,
}

#[derive(Default, Clone, Debug)]
enum Reqs {
    #[default]
    Get,
    Post,
}

#[derive(Default, Clone, Debug)]
enum PathPart {
    #[default]
    Dynamic,
    Static(String),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(l), Value::Num(r)) => l == r,
            (Value::Str(l), Value::Str(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Function(l, _), Value::Function(r, _)) => l == r,
            (Value::List(l), Value::List(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "Num({n})"),
            Self::Str(s) => write!(f, "Str({s})"),
            Self::Return(v) => write!(f, "{:?}", *v),
            Self::Bool(b) => write!(f, "Bool({b})"),
            Self::Function(p, _) => write!(f, "Function({})", p.len()),
            Self::List(l) => write!(
                f,
                "[{}]",
                l.iter()
                    .map(move |f| format!("{f:?}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Object(o) => write!(f, "{o:?}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Str(s) => write!(f, "{s}"),
            Self::Return(v) => write!(f, "{:?}", *v),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Function(p, _) => write!(f, "{}", p.len()),
            Self::List(l) => write!(
                f,
                "[{}]",
                l.iter()
                    .map(move |f| format!("{f:?}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Object(o) => write!(f, "{o:?}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    fn value(&self) -> String {
        match self {
            Value::Num(n) => n.to_string(),
            Value::Str(s) => s.to_string(),
            Value::Return(inner) => inner.value(),
            Value::Function(_, _) => "function block".to_string(),
            Value::List(vals) => {
                let inner = vals
                    .iter()
                    .map(|v| v.value())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{inner}]")
            }
            Value::Object(o) => format!("{o:?}"),
            Value::Nil => "nil".to_string(),
            Value::Bool(b) => b.to_string(),
        }
    }
}

#[derive(Default, Clone)]
struct PreCtx {
    var_types: DashMap<String, Type>,
    types: DashMap<String, DashMap<String, Type>>,
}

#[derive(Default, Clone)]
struct Context {
    variables: DashMap<String, Value>,
    routes: Arc<RwLock<Vec<(Reqs, Vec<PathPart>, Function)>>>,
    parent: Option<Arc<Context>>,
}

impl Context {
    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.variables.get(name) {
            Some(val.value().clone())
        } else if let Some(parent) = self.parent.as_ref() {
            parent.get(name)
        } else {
            None
        }
    }
    fn change(&self, name: &str, val: Value) -> Option<Value> {
        // If the variable exists in the current context, update it here
        if self.variables.contains_key(name) {
            return self.insert(name, val);
        }
        // Otherwise, if there's a parent context, recurse into it
        if let Some(parent_ctx) = &self.parent {
            return parent_ctx.change(name, val);
        }
        // If no existing declaration in any context, insert into current
        self.insert(name, val)
    }

    fn route(&self, method: Reqs, path: Vec<PathPart>, func: Function) {
        // Walk to the root context before pushing a route
        let mut current: Arc<Context> = Arc::new(self.clone());
        while let Some(parent) = &current.parent {
            current = parent.clone();
        }
        current.routes.write().unwrap().push((method, path, func));
    }

    fn insert(&self, name: &str, val: Value) -> Option<Value> {
        self.variables.insert(name.to_string(), val)
    }
}

struct Parser {
    tokens: Vec<Token>,
    current: usize, // index into `tokens`
    pctx: PreCtx,
    /// Inferred return type for the current function being parsed
    current_return_type: Option<Type>,
    /// Are we currently inside a `maybe` block?
    inside_maybe: bool,
    /// Tracks if any non-nil return has been seen in current function
    saw_non_nil_return: bool,
    /// Tracks if any explicit nil return has been seen in current function
    saw_nil_return: bool,
}

#[derive(Debug, Clone)]
enum Type {
    Num,
    Str,
    Bool,
    Nil,
    Io,
    List(Box<Type>),
    Option(Box<Type>),
    Custom(DashMap<String, Type>),
    Function(Vec<(String, String)>, Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Num, Type::Num)
            | (Type::Str, Type::Str)
            | (Type::Bool, Type::Bool)
            | (Type::Nil, Type::Nil)
            | (Type::Io, Type::Io) => true,

            (Type::List(left), Type::List(right)) | (Type::Option(left), Type::Option(right)) => {
                left == right
            }

            (Type::Function(params_l, ret_l), Type::Function(params_r, ret_r)) => {
                params_l == params_r && ret_l == ret_r
            }

            (Type::Custom(map_l), Type::Custom(map_r)) => {
                // Compare as maps: same length and all corresponding entries equal
                if map_l.len() != map_r.len() {
                    return false;
                }
                for entry in map_l.iter() {
                    let key = entry.key();
                    let val_l = entry.value();
                    match map_r.get(key) {
                        Some(val_r) if *val_l == *val_r => continue,
                        _ => return false,
                    }
                }
                true
            }

            _ => false,
        }
    }
}

trait CloneableFnMut: Fn(Arc<Context>) -> Value + Send + Sync {
    fn clone_box(&self) -> Box<dyn CloneableFnMut>;
}

impl<T> CloneableFnMut for T
where
    T: Fn(Arc<Context>) -> Value + Clone + Send + Sync + 'static,
{
    fn clone_box(&self) -> Box<dyn CloneableFnMut> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CloneableFnMut> {
    fn clone(&self) -> Box<dyn CloneableFnMut> {
        // Force vtable dispatch on the underlying closure
        let f: &dyn CloneableFnMut = &**self;
        f.clone_box()
    }
}

type MetalFunction = Box<dyn CloneableFnMut>;
struct Function(MetalFunction);
impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            pctx: PreCtx::default(),
            current_return_type: None,
            inside_maybe: false,
            saw_non_nil_return: false,
            saw_nil_return: false,
        }
    }

    // ───── entry point ─────

    fn parse_program(&mut self) -> Result<Vec<Instruction>, String> {
        let mut prgm = Vec::new();
        self.pctx.var_types.insert("io".to_string(), Type::Io);
        while !self.is_at_end() {
            prgm.push(self.parse_statement()?);
        }
        Ok(prgm)
    }

    fn parse_statement(&mut self) -> Result<Instruction, String> {
        // Handle return statements with type inference and consistency checking
        if self.match_kind(TokenKind::Return) {
            let expr = self.expression()?;
            let ret_type = get_type_of_expr(&expr, &self.pctx)?;
            // Track explicit nil/non-nil returns for this function
            if ret_type == Type::Nil {
                self.saw_nil_return = true;
            } else {
                self.saw_non_nil_return = true;
            }
            // Unify return types: allow Nil and uniform type => Option(inner)
            let new_ret_type = if let Some(old) = &self.current_return_type {
                if *old == ret_type {
                    old
                } else if *old == Type::Nil {
                    &Type::Option(Box::new(ret_type))
                } else if ret_type == Type::Nil {
                    &Type::Option(Box::new(old.clone()))
                } else {
                    return Err(format!(
                        "Mismatched return types in function: {old:?} vs {ret_type:?}",
                    ));
                }
            } else {
                &ret_type
            };
            self.current_return_type = Some(new_ret_type.clone());
            self.match_kind(TokenKind::Semicolon);
            Ok(Instruction::Return(expr))
        } else if self.match_kind(TokenKind::For) {
            let start = self.parse_statement()?;
            let cond = self.expression()?;
            self.consume(Semicolon, "Expected semicolon after condition")?;
            let step = self.parse_statement()?;
            let block = self.parse_statement()?;
            Ok(Instruction::For {
                init: Box::new(start),
                condition: cond,
                step: Box::new(step),
                body: vec![block],
            })
        } else if self.match_kind(TokenKind::Maybe) {
            let prev_inside = self.inside_maybe;
            self.inside_maybe = true;
            let maybe = self.expression()?;
            let block = self.parse_statement()?;
            let mut once_else = None;
            if self.match_kind(TokenKind::Else) {
                once_else = Some(Box::new(self.parse_statement()?));
            }
            let peekd = self.peek().line;

            self.inside_maybe = prev_inside;
            Ok(Instruction::Maybe(maybe, Box::new(block), once_else))
        } else if self.match_kind(TokenKind::LBrace) {
            // Static type scope for block: save outer types
            let saved_types = self.pctx.var_types.clone();
            let mut stmts = Vec::new();
            while !self.check(&TokenKind::RBrace) && !self.is_at_end() {
                stmts.push(self.parse_statement()?);
            }
            self.consume(TokenKind::RBrace, "Expected '}' after block")?;
            // Restore outer static types after block
            self.pctx.var_types = saved_types;

            Ok(Instruction::Block(stmts))
        } else if self.match_kind(TokenKind::Print) || self.match_kind(TokenKind::Reprint) {
            let prkind = self.previous().clone();
            let expr = self.expression()?;
            // Static type checking for print expression
            let _expr_type = get_type_of_expr(&expr, &self.pctx)?;

            if !self.inside_maybe {
                match _expr_type {
                    Type::Nil | Type::Option(_) => {
                        return Err(
                            "Cannot use a value that might be nil outside of a `maybe` block."
                                .into(),
                        );
                    }
                    _ => {}
                }
            }
            self.match_kind(TokenKind::Semicolon);
            Ok(Instruction::Println(expr))
        } else if self.match_kind(TokenKind::Object) {
            let Identifier(obj_name) = self.peek().kind else {
                return Err(format!(
                    "Expected Identifier after object, found {}",
                    self.peek().kind
                ));
            };
            self.advance();
            self.consume(LBrace, "Expected a { after object name")?;
            let fields = DashMap::new();
            while !self.match_kind(TokenKind::RBrace) {
                let Identifier(field) = self.peek().kind else {
                    return Err(format!(
                        "[line {}] Expected identifier, found {}",
                        self.peek().line,
                        self.peek().kind
                    ));
                };
                self.advance();
                self.consume(
                    Colon,
                    format!(
                        "Expected : after field name, found {} and {}",
                        self.previous().value,
                        self.peek().value
                    )
                    .as_str(),
                )?;
                let act_typ = match self.peek().kind {
                    Identifier(r) => match r.as_str() {
                        "Str" => Type::Str,
                        "Num" => Type::Num,
                        "Bool" => Type::Bool,
                        l => {
                            let Some(a) = self.pctx.types.get(l) else {
                                return Err(format!("Couldn't find type {l}"));
                            };
                            Type::Custom(a.clone())
                        }
                    },
                    LBrack => {
                        self.advance();
                        let ret = match self.peek().kind {
                            Identifier(r) => Type::List(Box::new(match r.as_str() {
                                "Str" => Type::Str,
                                "Num" => Type::Num,
                                "Bool" => Type::Bool,
                                l => {
                                    let Some(a) = self.pctx.types.get(l) else {
                                        return Err(format!("Couldn't find type {l}"));
                                    };
                                    Type::Custom(a.clone())
                                }
                            })),
                            _ => Type::Nil,
                        };
                        self.advance();
                        self.consume(TokenKind::RBrack, "Expected ] after type for list type")?;
                        ret
                    }
                    l => return Err(format!("Expected valid type, found {l}")),
                };

                fields.insert(field, act_typ);
                self.advance();
                let _ = self.consume(Comma, "msg");
            }

            self.pctx.types.insert(obj_name, fields);

            Ok(Instruction::Nothing)
        } else if self.match_kind(TokenKind::Fun) {
            if self.is_at_end() {
                return Err(format!(
                    "[line {}] Unexpected keyword: fun",
                    self.previous().line
                ));
            }
            let Identifier(fun_name) = self.peek().kind else {
                return Err(format!(
                    "[line {}] Expected function name after fun, got {}",
                    self.peek().line,
                    self.peek().value
                ));
            };
            self.advance();
            self.consume(LParen, "Expected '(' after function name")?;
            let mut params = vec![];
            while !self.is_at_end() && self.peek().kind != RParen {
                let param_name = if let Identifier(i) = self.peek().kind {
                    i
                } else {
                    return Err(format!(
                        "[line {}] Expected parameter name after '(', got {}",
                        self.peek().line,
                        self.peek().value
                    ));
                };
                self.advance();
                self.consume(Colon, {
                    if self.peek().kind == Comma {
                        return Err(format!(
                            "Function parameters must have types, try: {param_name}: Str or {param_name}: Num",
                        ));
                    } else {
                        "Expected ':' after param name"
                    }
                })?;
                let Identifier(typ) = self.peek().kind else {
                    return Err(format!(
                        "Expected tpe after ':', found {}",
                        self.peek().kind
                    ));
                };
                // consume the type token so we don't re-enter the loop on it
                self.advance();
                if self.peek().kind == Comma {
                    self.advance();
                }
                params.push((param_name, typ));
            }
            self.advance();

            // Insert parameter types into context for static type checking within function body
            let mut new_params = vec![];
            for (param_name, param_type_str) in &params {
                let param_type = match param_type_str.as_str() {
                    "Num" => Type::Num,
                    "Str" => Type::Str,
                    "Bool" => Type::Bool,
                    _ => Type::Nil,
                };
                new_params.push((param_name.clone(), param_type.clone()));
                self.pctx
                    .var_types
                    .insert(param_name.clone(), param_type.clone());
            }

            // Reset return type inference and flags for this new function
            self.current_return_type = None;
            self.saw_non_nil_return = false;
            self.saw_nil_return = false;

            let block = self.parse_statement()?;

            // Compute function return type:
            // - Mixed nil and non-nil ⇒ Option(inner)
            // - Only non-nil ⇒ inner
            // - No non-nil ⇒ Nil
            let fn_ret_type = if self.saw_non_nil_return && self.saw_nil_return {
                // Mixed returns: Option of inner non-nil type
                match self.current_return_type.clone() {
                    Some(Type::Option(inner)) => Type::Option(inner),
                    Some(inner) if inner != Type::Nil => Type::Option(Box::new(inner)),
                    _ => Type::Nil,
                }
            } else if self.saw_non_nil_return {
                // Only non-nil returns: return that type
                match self.current_return_type.clone() {
                    Some(inner) if inner != Type::Nil => inner,
                    _ => Type::Nil,
                }
            } else {
                // No non-nil returns ⇒ always nil
                Type::Nil
            };
            // Store the function's type signature for strong typing on calls
            self.pctx.var_types.insert(
                fun_name.clone(),
                Type::Function(params.clone(), Box::new(fn_ret_type.clone())),
            );
            Ok(Instruction::FunctionDef {
                body: vec![block],
                name: fun_name,
                params: new_params,
                return_type: fn_ret_type,
            })
        } else if self.match_kind(TokenKind::If) {
            // Parse the primary `if`
            let condition = self.expression()?;
            let then_block_stmt = self.parse_statement()?;
            // Build an else-chain for any number of else-if or else clauses
            let mut else_node: Option<Box<Instruction>> = None;
            // A mutable pointer to the current nested else slot
            let mut current_else = &mut else_node;
            // Keep consuming `else` clauses
            while self.match_kind(TokenKind::Else) {
                if self.match_kind(TokenKind::If) {
                    // else-if clause
                    let else_condition = self.expression()?;
                    let else_stmt = self.parse_statement()?;
                    // Create a new nested If node
                    let new_if = Instruction::If {
                        condition: else_condition,
                        then_block: vec![else_stmt],
                        elses: None,
                    };
                    // Insert it into the current slot
                    *current_else = Some(Box::new(new_if));
                    // Descend into its `elses` field for further chaining
                    {
                        // Descend into the nested `If` instruction's `elses` field
                        let boxed_if = current_else.as_mut().unwrap();
                        if let Instruction::If { ref mut elses, .. } = **boxed_if {
                            current_else = elses;
                        } else {
                            unreachable!("Chained else must be an Instruction::If");
                        }
                    }
                } else {
                    // plain else: treat as an If with a true condition
                    let else_stmt = self.parse_statement()?;
                    let new_if = Instruction::If {
                        condition: Expr::Literal(Value::Bool(true)),
                        then_block: vec![else_stmt],
                        elses: None,
                    };
                    *current_else = Some(Box::new(new_if));
                    break; // no more chaining after a plain else
                }
            }
            Ok(Instruction::If {
                condition,
                then_block: vec![then_block_stmt],
                elses: else_node,
            })
        } else if self.match_kind(TokenKind::While) {
            // Parse while loop condition
            let expr = self.expression()?;
            // Static type checking: ensure condition is boolean
            let cond_type = get_type_of_expr(&expr, &self.pctx)?;
            if cond_type != Type::Bool {
                return Err(format!(
                    "Condition in 'while' statement must be a boolean, found {cond_type:?}",
                ));
            }
            // Parse the loop body (a statement, e.g., a block)
            let body = self.parse_statement()?;
            // Generate a function for the while loop
            Ok(Instruction::While {
                body: vec![body],
                condition: expr,
            })
        } else if self.match_kind(TokenKind::Let) {
            let var_name = if let TokenKind::Identifier(n) = self.peek().kind.clone() {
                self.advance();
                n
            } else {
                return Err("Expected a variable name after 'var'".into());
            };
            self.consume(TokenKind::Equal, "Expected = after variable name")?;
            let expr = self.expression()?;
            // Special-case list literal assignment
            if let Expr::List(items) = expr.clone() {
                self.consume(
                    TokenKind::Semicolon,
                    "Expected ';' after variable declaration",
                )?;
                let name = var_name.clone();
                // Register the inferred list type for static checking
                let inner_type = if let Some(first) = items.first() {
                    get_type_of_expr(first, &self.pctx)?
                } else {
                    Type::Nil
                };
                self.pctx
                    .var_types
                    .insert(name.clone(), Type::List(Box::new(inner_type)));
            }
            // Static type checking: infer expression type and enforce consistency
            let expr_type = get_type_of_expr(&expr, &self.pctx)?;
            if let Some(existing) = self.pctx.var_types.get(&var_name) {
                if *existing != expr_type {
                    return Err(format!(
                            "Cannot redeclare variable '{var_name}' with different type. Previous: {existing:?}, New: {expr_type:?}",
                        ));
                }
            } else {
                self.pctx
                    .var_types
                    .insert(var_name.clone(), expr_type.clone());
            }
            self.match_kind(TokenKind::Semicolon);
            Ok(Instruction::Let {
                name: var_name,
                value: expr,
                type_hint: Some(expr_type),
            })
        } else if matches!(self.peek().kind, TokenKind::Identifier(_))
            && self
                .tokens
                .get(self.current + 1)
                .map(|t| matches!(t.kind, TokenKind::Equal))
                .unwrap_or(false)
        {
            // Assignment statement: <name> = <expr>;
            let name = if let TokenKind::Identifier(n) = self.peek().kind.clone() {
                n
            } else {
                unreachable!();
            };
            self.advance(); // consume identifier
            self.advance(); // consume '='
            let expr = self.expression()?;
            // Static type checking
            let expr_type = get_type_of_expr(&expr, &self.pctx)?;
            if let Some(existing) = self.pctx.var_types.get(&name) {
                if *existing != expr_type && *existing != Type::Nil {
                    return Err(format!(
                        "Cannot assign to variable '{name}' with different type. Previous: {existing:?}, New: {expr_type:?}",
                    ));
                }
            } else {
                return Err(format!("Variable '{name}' used before declaration"));
            }
            self.match_kind(TokenKind::Semicolon);
            Ok(Instruction::Assign(name, expr, Some(expr_type)))
        } else {
            // expression statement
            let expr: Expr = self.expression()?;
            let expr_type = get_type_of_expr(&expr, &self.pctx)?;
            if !self.inside_maybe {
                if let Type::Option(_) = expr_type {
                    return Err(
                        "Cannot use a value that might be nil outside of a `maybe` block.".into(),
                    );
                }
            }
            self.match_kind(TokenKind::Semicolon);
            Ok(Instruction::Return(expr))
        }
    }

    fn parse(&mut self) -> Result<Expr, String> {
        self.expression()
    }

    // ───── recursive-descent grammar ─────
    fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.match_any(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = match self.previous().clone().kind {
                BangEqual => BinOp::NotEq,
                EqualEqual => BinOp::EqEq,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.match_any(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let op = match self.previous().clone().kind {
                Greater => BinOp::Greater,
                GreaterEqual => BinOp::GreaterEqual,
                Less => BinOp::Less,
                LessEqual => BinOp::LessEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_any(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match self.previous().clone().kind {
                Plus => BinOp::Plus,
                Minus => BinOp::Minus,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_any(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match self.previous().clone().kind {
                Star => BinOp::Mult,
                Slash => BinOp::Div,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_any(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = match self.previous().clone().kind {
                Bang => Unary::Not,
                Minus => Unary::Neg,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            return Ok(Expr::Unary(op, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_kind(TokenKind::True) {
            return Ok(Expr::Literal(Value::Bool(true)));
        }
        if self.match_kind(TokenKind::False) {
            return Ok(Expr::Literal(Value::Bool(false)));
        }
        if self.match_kind(TokenKind::Nil) {
            return Ok(Expr::Literal(Value::Nil));
        }

        let pekd = self.peek().clone();

        if let TokenKind::Num(n) = pekd.kind {
            self.advance();
            return Ok(Expr::Literal(Value::Num(n)));
        } else if let TokenKind::Str(o) = &pekd.kind {
            self.advance();
            return Ok(Expr::Literal(Value::Str(o.into())));
        } else if let TokenKind::Identifier(i) = &pekd.kind {
            self.advance();

            if i.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
                && self.match_kind(TokenKind::LBrace)
            {
                let vals = DashMap::new();
                while !self.check(&TokenKind::RBrace) && !self.is_at_end() {
                    let Identifier(key) = self.peek().kind else {
                        return Err(format!("Expected identifier, found {}", self.peek().kind));
                    };
                    self.advance();
                    self.consume(Colon, "Expected ':' after field name")?;
                    let expr = self.expression()?;
                    if !self.match_any(&[TokenKind::Comma, TokenKind::RBrace]) {
                        return Err(format!(
                            "Expected ',' or '}}' after field declaration, found {}",
                            self.peek().kind
                        ));
                    }
                    vals.insert(key, expr);
                }
                self.consume(TokenKind::RBrace, "Expected '}' after object literal")?;
                if let Some(r) = self.pctx.types.get(i) {
                    for entry in r.iter() {
                        let name = entry.key();
                        let typ = entry.value();
                        if let Some(r) = vals.get(name) {
                            let real_type = get_type_of_expr(&r, &self.pctx)?;
                            if real_type != *typ {
                                return Err(format!(
                                    "Expected {name} to be type {typ:?}, got {real_type:?}",
                                ));
                            }
                        } else {
                            return Err(format!("{i} object requires the field {name}"));
                        }
                    }
                }
                return Ok(Expr::Object(vals));
            }
            // start with a variable reference
            let mut expr = Expr::Variable(i.clone());
            // handle chained field access: foo.bar.baz
            while self.match_kind(TokenKind::Dot) {
                if let TokenKind::Identifier(name) = &self.peek().kind {
                    let prop = name.clone();
                    self.advance();
                    expr = Expr::Get(Box::new(expr), prop);
                } else {
                    return Err(format!(
                        "Expected identifier after '.', found {}",
                        self.peek().kind
                    ));
                }
            }
            if self.match_kind(TokenKind::LBrack) {
                let indexpr = self.expression()?;
                self.consume(RBrack, "Expected ']' after list index")?;
                return Ok(Expr::Index(Box::new(expr), Box::new(indexpr)));
            }

            if self.match_kind(TokenKind::LParen) {
                let mut args = Vec::new();
                while !self.check(&TokenKind::RParen) {
                    args.push(self.expression()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.consume(TokenKind::RParen, "Expected ')' after arguments.")?;
                if let Some(Type::Function(f, _)) =
                    self.pctx.var_types.get(i).map(move |o| o.value().clone())
                {
                    if f.len() != args.len() {
                        return Err(format!(
                            "Function parameters incorrect, expected {} found {}",
                            f.len(),
                            args.len()
                        ));
                    }
                }
                for arg in &args {
                    let ty = get_type_of_expr(arg, &self.pctx)?;
                    if let Some(Type::Function(f, _)) =
                        self.pctx.var_types.get(i).map(move |o| o.value().clone())
                    {
                        for (_, t) in f {
                            let te = match t.as_str() {
                                "Str" => Type::Str,
                                "Num" => Type::Num,
                                "Bool" => Type::Bool,
                                _ => Type::Nil,
                            };
                            if te != ty {
                                return Err(format!(
                                    "Function parameters incorrect, expected {t} found {ty:?}",
                                ));
                            }
                        }
                    }
                }
                expr = Expr::Call(Box::new(expr), args);
            }
            return Ok(expr);
        }

        if self.match_kind(TokenKind::LParen) {
            let expr = self.expression()?;
            self.consume(TokenKind::RParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if self.match_kind(TokenKind::LBrace) {
            let mut inner = Vec::new();
            while !self.check(&TokenKind::RBrace) && !self.is_at_end() {
                inner.push(self.parse_statement()?);
            }
            self.consume(TokenKind::RBrace, "Expected '}' after block")?;
            return Ok(Expr::Block(inner));
        }

        if self.match_kind(TokenKind::LBrack) {
            let mut items = Vec::new();
            // handle empty list
            if self.check(&TokenKind::RBrack) {
                self.advance();
            } else {
                loop {
                    items.push(self.expression()?);
                    if self.match_kind(TokenKind::Comma) {
                        continue;
                    }
                    self.consume(TokenKind::RBrack, "Expected ']' after list")?;
                    break;
                }
            }
            // allow property access on lists, e.g., list.len
            let mut expr = Expr::List(items);
            while self.match_kind(TokenKind::Dot) {
                if let TokenKind::Identifier(name) = &self.peek().kind {
                    self.advance();
                    expr = Expr::Get(Box::new(expr), name.clone());
                } else {
                    return Err(format!(
                        "Expected property name after '.', found {}",
                        self.peek().kind
                    ));
                }
            }
            if self.match_kind(TokenKind::LParen) {
                let mut args = Vec::new();
                while !self.check(&TokenKind::RParen) {
                    args.push(self.expression()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.consume(TokenKind::RParen, "Expected ')' after arguments.")?;
                expr = Expr::Call(Box::new(expr), args);
            }
            return Ok(expr);
        }

        Err(format!(
            "[line {}] Error at '{}': Expect expression.",
            pekd.line, pekd.value
        ))
    }

    // ───── helpers ─────
    fn match_kind(&mut self, kind: TokenKind) -> bool {
        if self.check(&kind) {
            self.advance();
            true
        } else {
            false
        }
    }
    fn match_any(&mut self, kinds: &[TokenKind]) -> bool {
        for k in kinds {
            if self.check(k) {
                self.advance();
                return true;
            }
        }
        false
    }
    fn consume(&mut self, kind: TokenKind, msg: &str) -> Result<(), String> {
        if self.check(&kind) {
            self.advance();
            Ok(())
        } else {
            Err(msg.into())
        }
    }
    fn check(&self, kind: &TokenKind) -> bool {
        !self.is_at_end()
            && std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }
    fn peek(&self) -> Token {
        if self.current < self.tokens.len() {
            self.tokens[self.current].clone()
        } else {
            let wow = Token {
                kind: Eof,
                value: "".into(),
                line: self.previous().line,
            };
            wow
        }
    }
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::LParen => "LEFT_PAREN",
            Self::RParen => "RIGHT_PAREN",
            Self::LBrace => "LEFT_BRACE",
            Self::RBrace => "RIGHT_BRACE",
            Self::LBrack => "LEFT_BRACK",
            Self::RBrack => "RIGHT_BRACK",
            Self::Star => "STAR",
            Self::Dot => "DOT",
            Self::Comma => "COMMA",
            Self::Plus => "PLUS",
            Self::Minus => "MINUS",
            Self::Colon => "COLON",
            Self::Semicolon => "SEMICOLON",
            Self::Equal => "EQUAL",
            Self::EqualEqual => "EQUAL_EQUAL",
            Self::Bang => "BANG",
            Self::BangEqual => "BANG_EQUAL",
            Self::Less => "LESS",
            Self::LessEqual => "LESS_EQUAL",
            Self::Greater => "GREATER",
            Self::GreaterEqual => "GREATER_EQUAL",
            Self::Slash => "SLASH",
            Self::Str(_) => "STRING",
            Self::Num(_) => "NUMBER",
            Self::Identifier(_) => "IDENTIFIER",
            Self::And => "AND",
            Self::Object => "OBJECT",
            Self::Maybe => "MAYBE",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::For => "FOR",
            Self::Fun => "FUN",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Print => "PRINT",
            Self::Reprint => "REPRINT",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Let => "LET",
            Self::While => "WHILE",
            Self::Eof => "EOF",
            Self::Error(line, error) => &format!("[line {line}] Error: {error}"),
        };
        write!(f, "{s}")
    }
}

fn is_single_char_token(c: char) -> Option<TokenKind> {
    match c {
        '(' => Some(LParen),
        ')' => Some(RParen),
        '{' => Some(LBrace),
        '}' => Some(RBrace),
        '*' => Some(Star),
        '.' => Some(Dot),
        ',' => Some(Comma),
        '+' => Some(Plus),
        '-' => Some(Minus),
        ':' => Some(Colon),
        ';' => Some(Semicolon),
        _ => None,
    }
}

fn get_special_ident(val: String) -> TokenKind {
    match val.as_str() {
        "and" => And,
        "object" => Object,
        "maybe" => Maybe,
        "else" => Else,
        "false" => False,
        "for" => For,
        "fun" => Fun,
        "if" => If,
        "nil" => Nil,
        "or" => Or,
        "print" => Print,
        "reprint" => Reprint,
        "return" => Return,
        "super" => Super,
        "this" => This,
        "true" => True,
        "let" => Let,
        "while" => While,
        _ => Identifier(val),
    }
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn main() {
    let args: Vec<std::string::String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {filename}");
                std::string::String::new()
            });
            let tokens = tokenize(file_contents.chars().collect());
            let has_error = tokens.iter().any(|t| matches!(t.kind, Error(_, _)));
            if has_error {
                for token in tokens {
                    token.print();
                }
                std::process::exit(65);
            }
            for token in tokens {
                token.print();
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {filename}");
                std::string::String::new()
            });
            let tokens = tokenize(file_contents.chars().collect());
            if tokens.iter().any(|t| matches!(t.kind, Error(_, _))) {
                for t in tokens {
                    if let Error(_, _) = t.kind {
                        t.print();
                    }
                }
                std::process::exit(65);
            }
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(p) => {
                    println!("{}", eval(p, Arc::new(Context::default())).value());
                }
                Err(e) => {
                    eprintln!("{e}");
                    std::process::exit(65);
                }
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {filename}");
                std::string::String::new()
            });
            let tokens = tokenize(file_contents.chars().collect());
            // Lexical error check
            if tokens.iter().any(|t| matches!(t.kind, Error(_, _))) {
                for t in tokens {
                    if let Error(_, _) = t.kind {
                        t.print();
                    }
                }
                std::process::exit(65);
            }
            let mut parser = Parser::new(tokens);
            //let ctx = Arc::new(Context::default());
            match parser.parse_program() {
                Ok(p) => {
                    let mut flag_builder = settings::builder();
                    flag_builder.set("is_pic", "false").unwrap();
                    let isa_builder = cranelift_native::builder().unwrap();
                    let isa = isa_builder
                        .finish(settings::Flags::new(flag_builder))
                        .unwrap();
                    let mut builder =
                        JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();
                    let mut module = JITModule::new(builder);

                    let mut ctx = module.make_context();
                    ctx.func.signature.returns.push(AbiParam::new(types::I32));
                    let mut func_ctx = FunctionBuilderContext::new();
                    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

                    let block = builder.create_block();
                    builder.switch_to_block(block);

                    let mut vars: HashMap<String, Variable> = HashMap::new();
                    let mut var_counter = 0;

                    compile(&mut builder, p, &vars);
                    builder.seal_all_blocks();

                    builder.finalize();

                    // 1. Declare the function
                    let func_id = module
                        .declare_function(
                            "main",
                            cranelift_module::Linkage::Export,
                            &ctx.func.signature,
                        )
                        .unwrap();

                    // 2. Define the function body in the module
                    module.define_function(func_id, &mut ctx).unwrap();

                    // 3. Finalize to get executable code
                    module.clear_context(&mut ctx);
                    module.finalize_definitions().unwrap();

                    // 4. Get the raw function pointer
                    let func_ptr = module.get_finalized_function(func_id);

                    // 5. SAFETY: You're telling Rust that you know this function takes no args and returns i32
                    let run: fn() -> i32 = unsafe { std::mem::transmute(func_ptr) };

                    // 6. Call it!
                    let result = run();
                    println!("Program returned: {}", result);
                }
                Err(e) => {
                    eprintln!("{e}");
                    std::process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {command}");
        }
    }
}

fn compile(
    builder: &mut FunctionBuilder,
    prgm: Vec<Instruction>,
    vars: &HashMap<String, Variable>,
) {
    for stmt in prgm {
        match stmt {
            Instruction::If {
                condition,
                then_block,
                elses,
            } => {
                let then_block_k = builder.create_block();
                let else_block_k = builder.create_block();
                let merge_block = builder.create_block();

                let cond_val = codegen_expr(builder, vars, &condition);
                builder
                    .ins()
                    .brif(cond_val, then_block_k, &[], else_block_k, &[]);

                builder.switch_to_block(then_block_k);
                builder.seal_block(then_block_k);
                compile(builder, then_block, vars);
                if builder
                    .func
                    .layout
                    .block_insts(builder.current_block().unwrap())
                    .last()
                    .is_none()
                {
                    builder.ins().jump(merge_block, &[]);
                }

                builder.switch_to_block(else_block_k);
                builder.seal_block(else_block_k);
                if let Some(else_inst) = elses {
                    compile(builder, vec![*else_inst], vars);
                }
                if builder
                    .func
                    .layout
                    .block_insts(builder.current_block().unwrap())
                    .last()
                    .is_none()
                {
                    builder.ins().jump(merge_block, &[]);
                }

                builder.switch_to_block(merge_block);
            }
            Instruction::Return(r) => {
                let exp = codegen_expr(builder, vars, &r);
                builder.ins().return_(&[exp]);
            }
            Instruction::Block(b) => {
                compile(builder, b, vars);
            }
            l => todo!("{l:?}"),
        }
    }
}

fn codegen_expr(
    builder: &mut FunctionBuilder,
    vars: &HashMap<String, Variable>,
    expr: &Expr,
) -> cranelift::prelude::Value {
    match expr {
        Expr::Literal(Value::Num(n)) => builder.ins().iconst(types::I32, *n as i64),
        Expr::Literal(val) => match val {
            Value::Num(n) => builder.ins().iconst(types::I32, *n as i64),
            Value::Str(s) => {
                let leaked = Box::leak(s.clone().into_boxed_str());
                builder.ins().iconst(types::I64, leaked.as_ptr() as i64)
            }
            Value::Bool(b) => builder.ins().iconst(types::I8, if *b { 1 } else { 0 }),
            _ => unimplemented!("We don't support the {val} value type yet"),
        },
        Expr::Variable(name) => {
            let var = vars.get(name).expect("Undefined variable");
            builder.use_var(*var)
        }

        Expr::Unary(d, thing) => {
            let resolved = codegen_expr(builder, vars, thing);
            match d {
                Unary::Neg => builder.ins().ineg(resolved),
                Unary::Not => builder.ins().bnot(resolved),
            }
        }

        Expr::Binary(lhs, op, rhs) => {
            let left = codegen_expr(builder, vars, lhs);
            let right = codegen_expr(builder, vars, rhs);

            match op {
                BinOp::Plus => builder.ins().iadd(left, right),
                BinOp::Minus => builder.ins().isub(left, right),
                BinOp::Mult => builder.ins().imul(left, right),
                BinOp::Div => builder.ins().sdiv(left, right),
                BinOp::NotEq => builder.ins().icmp(IntCC::NotEqual, left, right),
                BinOp::EqEq => builder.ins().icmp(IntCC::Equal, left, right),
                BinOp::Greater => builder.ins().icmp(IntCC::SignedGreaterThan, left, right),
                BinOp::GreaterEqual => {
                    builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, left, right)
                }
                BinOp::Less => builder.ins().icmp(IntCC::SignedLessThan, left, right),
                BinOp::LessEqual => builder
                    .ins()
                    .icmp(IntCC::SignedLessThanOrEqual, left, right),
            }
        }

        _ => unimplemented!("Expr variant not handled"),
    }
}

fn format_float(lexeme: &str) -> String {
    if lexeme.contains('.') {
        let mut s = lexeme.trim_end_matches('0').to_string();
        if s.ends_with('.') {
            s.push('0');
        }
        s
    } else {
        format!("{lexeme}.0")
    }
}

fn tokenize(chars: Vec<char>) -> Vec<Token> {
    let mut is_commented = false;
    let mut in_string: Option<std::string::String> = None;

    let mut tokens = vec![];
    let mut line = 1;
    let mut index = 0;

    while index < chars.len() {
        let current_char = chars[index];

        // Handle comments
        if is_commented {
            if current_char == '\n' {
                is_commented = false;
                line += 1;
            }
            index += 1;
            continue;
        }

        // Handle strings
        if current_char == '"' {
            if in_string.is_none() {
                in_string = Some("".to_string());
            } else {
                tokens.push(Token {
                    value: in_string.clone().unwrap(),
                    kind: Str(in_string.clone().unwrap()),
                    line,
                });
                in_string = None;
            }
            index += 1;
            continue;
        } else if let Some(ref mut s) = in_string {
            s.push(current_char);
            index += 1;
            continue;
        }

        // Handle newlines
        if current_char == '\n' {
            line += 1;
            index += 1;
            continue;
        }
        // Skip whitespace
        if current_char == ' ' || current_char == '\t' {
            index += 1;
            continue;
        }

        // Handle numbers
        if current_char.is_ascii_digit()
            || (current_char == '.' && index + 1 < chars.len() && chars[index + 1].is_ascii_digit())
        {
            let mut number_str = String::new();
            let mut has_dot = false;
            let mut j = index;

            while j < chars.len() {
                let c = chars[j];
                if c.is_ascii_digit() {
                    number_str.push(c);
                } else if c == '.' && !has_dot {
                    number_str.push(c);
                    has_dot = true;
                } else {
                    break;
                }
                j += 1;
            }

            if let Ok(num_val) = number_str.parse::<f64>() {
                tokens.push(Token {
                    value: number_str,
                    kind: Num(num_val),
                    line,
                });
                index = j;
                continue;
            }
        }

        // Handle identifiers
        if current_char.is_alphabetic() || current_char == '_' {
            let mut identifier = String::new();
            let mut j = index;

            while j < chars.len() && is_identifier_char(chars[j]) {
                identifier.push(chars[j]);
                j += 1;
            }

            tokens.push(Token {
                value: identifier.clone(),
                kind: get_special_ident(identifier),
                line,
            });
            index = j;
            continue;
        }

        // Handle two-character operators
        if index + 1 < chars.len() {
            let next_char = chars[index + 1];
            let two_char = format!("{current_char}{next_char}");

            match two_char.as_str() {
                "==" => {
                    tokens.push(Token {
                        value: "==".to_string(),
                        kind: EqualEqual,
                        line,
                    });
                    index += 2;
                    continue;
                }
                "!=" => {
                    tokens.push(Token {
                        value: "!=".to_string(),
                        kind: BangEqual,
                        line,
                    });
                    index += 2;
                    continue;
                }
                "<=" => {
                    tokens.push(Token {
                        value: "<=".to_string(),
                        kind: LessEqual,
                        line,
                    });
                    index += 2;
                    continue;
                }
                ">=" => {
                    tokens.push(Token {
                        value: ">=".to_string(),
                        kind: GreaterEqual,
                        line,
                    });
                    index += 2;
                    continue;
                }
                "//" => {
                    is_commented = true;
                    index += 2;
                    continue;
                }
                _ => {}
            }
        }

        // Handle single-character tokens
        if let Some(token_kind) = is_single_char_token(current_char) {
            tokens.push(Token {
                value: current_char.to_string(),
                kind: token_kind,
                line,
            });
            index += 1;
            continue;
        }

        // Handle single-character operators
        match current_char {
            '=' => {
                tokens.push(Token {
                    value: "=".to_string(),
                    kind: Equal,
                    line,
                });
            }
            '!' => {
                tokens.push(Token {
                    value: "!".to_string(),
                    kind: Bang,
                    line,
                });
            }
            '<' => {
                tokens.push(Token {
                    value: "<".to_string(),
                    kind: Less,
                    line,
                });
            }
            '>' => {
                tokens.push(Token {
                    value: ">".to_string(),
                    kind: Greater,
                    line,
                });
            }
            '/' => {
                tokens.push(Token {
                    value: "/".to_string(),
                    kind: Slash,
                    line,
                });
            }
            '[' => {
                tokens.push(Token {
                    value: "[".to_string(),
                    kind: TokenKind::LBrack,
                    line,
                });
            }
            ']' => {
                tokens.push(Token {
                    value: "]".to_string(),
                    kind: TokenKind::RBrack,
                    line,
                });
            }
            _ => {
                tokens.push(Token {
                    value: "".to_string(),
                    kind: Error(line as u64, format!("Unexpected character: {current_char}")),
                    line,
                });
            }
        }
        index += 1;
    }

    if in_string.is_some() {
        tokens.push(Token {
            value: "".to_string(),
            kind: Error(line as u64, "Unterminated string.".to_string()),
            line,
        });
    }
    // if tokens.len() > 1 || !has_error {
    tokens.push(Token {
        value: "EOF".to_string(),
        kind: Eof,
        line,
    });
    // }

    tokens
}

async fn quick_get_handler(
    context: actix_web::web::Data<Arc<Context>>,
    req: HttpRequest,
) -> impl Responder {
    let path = req.path();

    let path_split = path.split("/").collect::<Vec<&str>>();
    'outer: for (_, pat, fun) in context
        .routes
        .read()
        .unwrap()
        .iter()
        .filter(move |o| matches!(o.0, Reqs::Get))
    {
        if pat.len() != path_split.len() {
            continue;
        }
        let mut dynamic_parts = vec![];
        for (i, part) in pat.iter().enumerate() {
            match part {
                PathPart::Static(s) => {
                    if path_split[i] != s {
                        dynamic_parts.clear();
                        continue 'outer;
                    }
                }
                PathPart::Dynamic => {
                    dynamic_parts.push(path_split[i]);
                }
            }
        }
        return match fun.0(Arc::new(Context {
            parent: Some(context.as_ref().clone()),
            ..Default::default()
        })) {
            Value::Str(s) => HttpResponse::Ok().body(s),
            Value::Bool(b) => HttpResponse::Ok().body(b.to_string()),
            Value::Num(n) => HttpResponse::Ok().body(n.to_string()),
            Value::Object(o) => HttpResponse::Ok().body(format!("{o:?}")),
            Value::Nil => HttpResponse::NoContent().body(""),
            _ => todo!(),
        };
    }
    HttpResponse::NotFound().body("Not found (404)")
}

fn eval(ex: Expr, ctx: Arc<Context>) -> Value {
    match ex {
        Expr::Variable(name) => {
            if name == "io" {
                let mut io_object = DashMap::new();
                io_object.insert(
                    "random".to_string(),
                    Value::Function(vec![], Function(Box::new(move |_| Value::Num(random())))),
                );
                io_object.insert(
                    "listen".to_string(),
                    Value::Function(
                        vec![("port".to_string(), "Num".to_string())],
                        Function(Box::new(move |ctx| {
                            //Todo: implement http listener
                            let port = match ctx.get("port").unwrap_or(Value::Num(8080.0)) {
                                Value::Num(p) => p,
                                _ => {
                                    eprintln!("Port was not a number");
                                    std::process::exit(70);
                                }
                            };
                            // Only move the port (which is Copy) into the closure
                            let sys = actix_web::rt::System::new();
                            sys.block_on(async move {
                                match HttpServer::new(move || {
                                    App::new()
                                        .app_data(actix_web::web::Data::new(ctx.clone()))
                                        .route(
                                            "/{tail:.*}",
                                            actix_web::web::get().to(quick_get_handler),
                                        )
                                })
                                .bind(("127.0.0.1", port as u16))
                                {
                                    Ok(server) => {
                                        if let Err(e) = server.run().await {
                                            eprintln!("Server error: {e}");
                                            std::process::exit(70);
                                        }
                                    }
                                    Err(_) => {
                                        eprintln!("Port {port} was not available. Exiting...");
                                        std::process::exit(70);
                                    }
                                }
                            });
                            Value::Nil
                        })),
                    ),
                );
                io_object.insert(
                    "route".to_string(),
                    Value::Function(
                        vec![
                            ("method".to_string(), "Str".to_string()),
                            ("route".to_string(), "Str".to_string()),
                            ("func".to_string(), "Function".to_string()),
                        ],
                        Function(Box::new(move |ctx| {
                            let method =
                                match ctx.variables.get("method").map(move |o| o.value().clone()) {
                                    Some(o) => match o {
                                        Value::Str(ref s) if s == "GET" => Reqs::Get,
                                        Value::Str(ref s) if s == "POST" => Reqs::Post,
                                        l => {
                                            eprintln!(
                                                "Request method must be GET or POST, found {l}"
                                            );
                                            std::process::exit(70);
                                        }
                                    },
                                    None => {
                                        eprintln!("Request method parameter not found.");
                                        std::process::exit(70);
                                    }
                                };

                            let path =
                                match ctx.variables.get("route").map(move |o| o.value().clone()) {
                                    Some(o) => match o {
                                        Value::Str(ref s) => s
                                            .split("/")
                                            .map(move |o| {
                                                if o.starts_with(":") {
                                                    PathPart::Dynamic
                                                } else {
                                                    PathPart::Static(o.to_string())
                                                }
                                            })
                                            .collect::<Vec<PathPart>>(),
                                        l => {
                                            eprintln!("Request route must be a string, found {l}");
                                            std::process::exit(70);
                                        }
                                    },
                                    None => {
                                        eprintln!("Request route parameter not found.");
                                        std::process::exit(70);
                                    }
                                };

                            let func =
                                match ctx.variables.get("func").map(move |o| o.value().clone()) {
                                    Some(o) => match o {
                                        Value::Function(_params, runs) => runs,
                                        l => {
                                            eprintln!(
                                                "Request handler must be a function, found {l}"
                                            );
                                            std::process::exit(70);
                                        }
                                    },
                                    None => {
                                        eprintln!("Request function parameter not found.");
                                        std::process::exit(70);
                                    }
                                };

                            ctx.route(method, path, func);
                            Value::Nil
                        })),
                    ),
                );
                Value::Object(io_object)
            } else if let Some(v) = ctx.get(&name) {
                v
            } else {
                eprintln!("Variable \"{name}\" not found");
                std::process::exit(70);
            }
        }
        Expr::Index(li, index) => {
            let Value::List(actl) = eval(*li, ctx.clone()) else {
                eprintln!("You can't index into a non-list object");
                std::process::exit(70);
            };
            let Value::Num(actindex) = eval(*index, ctx.clone()) else {
                eprintln!("Index was not a number");
                std::process::exit(70);
            };

            if let Some(r) = actl.get(actindex as usize) {
                r.clone()
            } else {
                Value::Nil
            }
        }
        Expr::Object(o) => {
            let new_map = DashMap::new();

            for (name, exp) in o {
                new_map.insert(name, eval(exp, ctx.clone()));
            }
            Value::Object(new_map)
        }
        Expr::List(exprs) => {
            // evaluate each element into a Value
            let values: Vec<Value> = exprs.into_iter().map(|e| eval(e, ctx.clone())).collect();
            // wrap into a temporary identifier and insert into context?
            // For now, represent the list as a global literal name
            // We can return Nil or error until full runtime support is added.
            Value::List(values)
        }
        Expr::Literal(l) => l,
        Expr::Binary(l, o, r) => {
            let left = eval(*l, ctx.clone());
            let right = eval(*r, ctx.clone());
            match o {
                BinOp::Plus => match left {
                    Value::Str(ref left_str) => match right {
                        Value::Str(right_str) => Value::Str(format!("{left_str}{right_str}")),
                        _ => {
                            eprintln!("Adding must be two numbers or two strings");
                            std::process::exit(70);
                        }
                    },
                    Value::Num(left_num) => {
                        if let Value::Num(right_num) = right {
                            Value::Num(left_num + right_num)
                        } else {
                            eprintln!("Type Error: Cannot add a number to anything but a number");
                            std::process::exit(70);
                        }
                    }
                    l => {
                        eprintln!("We haven't supported adding {l} and {right} yet");
                        std::process::exit(70);
                    }
                },
                BinOp::Minus => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            Value::Num(left_num - right_num)
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {right:?} from {left_num} yet",
                            );
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported subtracting {right} from {left} yet",);
                        std::process::exit(70);
                    }
                }
                BinOp::Mult => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            Value::Num(left_num * right_num)
                        } else {
                            eprintln!(
                                "We haven't supported multiplying {right} and {left_num} yet",
                            );
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported multiplying {right} and {left} yet",);
                        std::process::exit(70);
                    }
                }
                BinOp::Div => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            Value::Num(left_num / right_num)
                        } else {
                            eprintln!("We haven't supported dividing {right} by {left_num} yet",);
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported dividing {right} from {left} yet");
                        std::process::exit(70);
                    }
                }
                BinOp::Greater => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            if left_num > right_num {
                                Value::Bool(true)
                            } else {
                                Value::Bool(false)
                            }
                        } else {
                            eprintln!(
                                "We haven't supported coprarisons of {right} and {left_num} yet",
                            );
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported coprarisons of {right} and {left} yet",);
                        std::process::exit(70);
                    }
                }
                BinOp::Less => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            if left_num < right_num {
                                Value::Bool(true)
                            } else {
                                Value::Bool(false)
                            }
                        } else {
                            eprintln!(
                                "We haven't supported coprarisons of {right} and {left_num} yet",
                            );
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported coprarisons of {right} and {left} yet",);
                        std::process::exit(70);
                    }
                }
                BinOp::GreaterEqual => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            if left_num >= right_num {
                                Value::Bool(true)
                            } else {
                                Value::Bool(false)
                            }
                        } else {
                            eprintln!(
                                "We haven't supported coprarisons of {right} and {left_num} yet",
                            );
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported coprarisons of {right} and {left} yet",);
                        std::process::exit(70);
                    }
                }
                BinOp::LessEqual => {
                    if let Value::Num(left_num) = left {
                        if let Value::Num(right_num) = right {
                            if left_num <= right_num {
                                Value::Bool(true)
                            } else {
                                Value::Bool(false)
                            }
                        } else {
                            eprintln!(
                                "We haven't supported coprarisons of {right} and {left_num} yet",
                            );
                            std::process::exit(70);
                        }
                    } else {
                        eprintln!("We haven't supported coprarisons of {right} and {left} yet",);
                        std::process::exit(70);
                    }
                }
                BinOp::EqEq => {
                    // Resolve identifiers to actual values before comparison

                    if left == right {
                        Value::Bool(true)
                    } else {
                        Value::Bool(false)
                    }
                }
                BinOp::NotEq => {
                    // Resolve identifiers to actual values before comparison

                    if left != right {
                        Value::Bool(true)
                    } else {
                        Value::Bool(false)
                    }
                }
            }
        }
        Expr::Grouping(val) => eval(*val, ctx),
        Expr::Unary(tolk, val) => match tolk {
            Unary::Neg => {
                let other_val = *val;
                if let Value::Num(num) = eval(other_val, ctx) {
                    Value::Num(-num)
                } else {
                    eprintln!("Operand must be a number.");

                    std::process::exit(70);
                }
            }
            Unary::Not => {
                let evald = eval(*val, ctx);
                if let Value::Bool(true) = evald {
                    Value::Bool(false)
                } else if let Value::Nil = evald {
                    Value::Bool(true)
                } else if let Value::Num(_) = evald {
                    Value::Bool(false)
                } else {
                    Value::Bool(false)
                }
            }
        },
        Expr::Call(callee, args) => {
            // Fallback for user-defined functions
            let l_val = eval(*callee, ctx.clone());
            if let Value::Function(params, body_fn) = l_val {
                if args.len() != params.len() {
                    eprintln!("Expected {} arguments but got {}", params.len(), args.len());
                    std::process::exit(70);
                }
                // Create a nested context that shares routes and variables via clone
                let new_ctx = Arc::new(Context {
                    variables: ctx.variables.clone(),
                    routes: ctx.routes.clone(),
                    parent: Some(ctx.clone()),
                });
                for (i, expr) in args.into_iter().enumerate() {
                    let val = eval(expr, ctx.clone());
                    let name = &params[i].0;
                    new_ctx.insert(name, val);
                }
                body_fn.0(new_ctx)
            } else {
                eprintln!("Identifier is not a function");
                std::process::exit(70);
            }
        }
        Expr::Get(obj, prop) => {
            // Evaluate the object expression into a Value
            let val = eval(*obj, ctx);
            // // Runtime support for list.len on variables
            if prop == "len" {
                if let Value::List(items) = val {
                    // return Value::Num(items.len() as f64);
                    return Value::Function(
                        vec![],
                        Function(Box::new(move |_| Value::Num(items.len() as f64))),
                    );
                } else if let Value::Str(string) = val {
                    return Value::Function(
                        vec![],
                        Function(Box::new(move |_| Value::Num(string.len() as f64))),
                    );
                }
            }

            if let Value::Object(ref o) = val {
                if let Some(p) = o.get(&prop) {
                    return p.clone();
                } else {
                    eprintln!("Couldn't find property {prop} on object {val}");
                    std::process::exit(70);
                }
            }
            if let Value::Num(n) = val {
                if prop == "str" {
                    return Value::Function(
                        vec![],
                        Function(Box::new(move |_| Value::Str(n.to_string()))),
                    );
                }
            }
            todo!("{}", val);
        }
        Expr::Block(fns) => {
            let inner_ctx = Arc::new(Context {
                variables: ctx.variables.clone(),
                routes: ctx.routes.clone(),
                parent: Some(ctx.clone()),
            });
            let mut last = Value::Nil;
            todo!();
            // for f in fns {
            //     last = f.0(inner_ctx.clone());
            // }
            last
        }
    }
}

fn get_type_of_expr(expr: &Expr, ctx: &PreCtx) -> Result<Type, String> {
    match expr {
        Expr::Variable(v) => match ctx.var_types.get(v) {
            Some(t) => Ok(t.clone()),
            None => {
                eprintln!("Variable \"{v}\" not found");
                std::process::exit(70);
            }
        },
        Expr::Index(list, _) => get_type_of_expr(list, ctx),
        Expr::Object(o) => {
            let fields = DashMap::new();
            for entry in o.iter() {
                let name = entry.key();
                let expr = entry.value();
                let field_type = get_type_of_expr(expr, ctx)?;
                fields.insert(name.clone(), field_type);
            }
            Ok(Type::Custom(fields))
        }
        Expr::Literal(tk) => match tk {
            Value::Num(_) => Ok(Type::Num),
            Value::Str(_) => Ok(Type::Str),
            Value::Bool(_) => Ok(Type::Bool),
            Value::Nil => Ok(Type::Nil),
            _ => Err("Unsupported literal type".into()),
        },
        Expr::List(l) => Ok(Type::List(if let Some(r) = l.first() {
            Box::new(get_type_of_expr(r, ctx)?)
        } else {
            Box::new(Type::Nil)
        })),
        Expr::Unary(_, e) => get_type_of_expr(e, ctx),
        Expr::Grouping(e) => get_type_of_expr(e, ctx),
        Expr::Binary(l, op, r) => {
            let lt = get_type_of_expr(l, ctx)?;
            let rt = get_type_of_expr(r, ctx)?;
            match op {
                BinOp::Plus => match (&lt, &rt) {
                    (Type::Num, Type::Num) => Ok(Type::Num),
                    (Type::Str, Type::Str) => Ok(Type::Str),
                    _ => Err("Operands of '+' must be both numbers or both strings".into()),
                },
                BinOp::Minus | BinOp::Mult | BinOp::Div => {
                    if lt == Type::Num && rt == Type::Num {
                        Ok(Type::Num)
                    } else {
                        Err(format!("Operator {op:?} requires two numbers"))
                    }
                }
                BinOp::EqEq | BinOp::NotEq => Ok(Type::Bool),
                BinOp::Greater | BinOp::Less | BinOp::GreaterEqual | BinOp::LessEqual => {
                    if lt == Type::Num && rt == Type::Num {
                        Ok(Type::Bool)
                    } else {
                        Err(format!("Operator {op:?} requires two numbers"))
                    }
                }
            }
        }
        Expr::Get(obj, prop) => {
            // Type-checking for field access
            let obj_type = get_type_of_expr(obj, ctx)?;
            match obj_type {
                Type::List(_) => {
                    if prop == "len" {
                        Ok(Type::Function(vec![], Box::new(Type::Num)))
                    } else {
                        Err(format!("Property '{prop}' not found on list type"))
                    }
                }
                Type::Io => {
                    // Special-case io properties

                    match prop.as_str() {
                        "random" => Ok(Type::Function(vec![], Box::new(Type::Num))),
                        "route" => Ok(Type::Function(
                            vec![
                                ("method".to_string(), "Str".to_string()),
                                ("route".to_string(), "Str".to_string()),
                                ("func".to_string(), "Function".to_string()),
                            ],
                            Box::new(Type::Nil),
                        )),
                        "listen" => Ok(Type::Function(
                            vec![("port".to_string(), "Num".to_string())],
                            Box::new(Type::Nil),
                        )),
                        other => Err(format!("Unknown property '{other}' on io")),
                    }
                }
                Type::Num => match prop.as_str() {
                    "str" => Ok(Type::Function(vec![], Box::new(Type::Str))),
                    other => Err(format!("Unknown property '{other}' on type Num")),
                },
                Type::Custom(fields) => {
                    // Look up property in custom type
                    if let Some(t) = fields.get(prop) {
                        Ok(t.clone())
                    } else {
                        Err(format!("Property '{prop}' not found on type {fields:?}",))
                    }
                }
                other => Err(format!("Cannot access property '{prop}' on type {other:?}",)),
            }
        }
        Expr::Block(_) => Ok(Type::Nil),
        Expr::Call(callee, args) => {
            // Special-case: io.random() always returns a number
            if let Expr::Get(inner, prop) = &**callee {
                if let Expr::Variable(obj) = &**inner {
                    if obj == "io" && prop == "random" {
                        if args.is_empty() {
                            return Ok(Type::Num);
                        } else {
                            return Err(format!(
                                "io.random() expects no arguments, got {}",
                                args.len()
                            ));
                        }
                    }
                }
            }
            // Existing function call type-checking
            let callee_type = get_type_of_expr(callee, ctx)?;
            if let Type::Function(params, ret_type) = callee_type {
                if args.len() != params.len() {
                    return Err(format!(
                        "Expected {} arguments but got {}",
                        params.len(),
                        args.len()
                    ));
                }
                // verify each arg’s inferred type against the declared parameter type
                for (i, (_, param_tstr)) in params.iter().enumerate() {
                    let arg_t = get_type_of_expr(&args[i], ctx)?;
                    if param_tstr == "Function" {
                        // Accept any function value
                        if let Type::Function(_, _) = arg_t {
                            // OK
                        } else {
                            return Err(format!(
                                "Parameter #{i} type mismatch: expected a function, got {arg_t:?}",
                            ));
                        }
                    } else {
                        let expected = match param_tstr.as_str() {
                            "Num" => Type::Num,
                            "Str" => Type::Str,
                            "Bool" => Type::Bool,
                            _ => Type::Nil,
                        };
                        if expected != arg_t {
                            return Err(format!(
                                "Parameter #{i} type mismatch: expected {expected:?}, got {arg_t:?}",
                            ));
                        }
                    }
                }
                Ok(*ret_type)
            } else {
                Err(format!("Can only call functions, found {callee_type:?}"))
            }
        }
    }
}
