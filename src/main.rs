use crate::TokenKind::*;
use std::env;
use std::fmt::Display;
use std::fs;

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
        } else if let Number(_) = token.kind {
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
    Star,
    Dot,
    Comma,
    Plus,
    Minus,
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
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
    Number(f64),
    Error(u64, std::string::String),
}

#[derive(Debug, Clone)]
enum Expr {
    Literal(TokenKind),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
}

impl Expr {
    fn print(&self) -> String {
        match self {
            Expr::Literal(k) => match k {
                TokenKind::Number(n) => format!("{:?}", n),
                TokenKind::Str(o) => o.into(),
                TokenKind::Identifier(i) => i.into(),
                TokenKind::True => "true".into(),
                TokenKind::False => "false".into(),
                TokenKind::Nil => "nil".into(),
                _ => "<bad lit>".into(),
            },
            Expr::Unary(op, right) => parenthesize(&op.value, &[right]),
            Expr::Binary(left, op, right) => parenthesize(&op.value, &[left, right]),
            Expr::Grouping(expr) => parenthesize("group", &[expr]),
        }
    }
}
fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
    let inner = exprs
        .iter()
        .map(|e| e.print())
        .collect::<Vec<_>>()
        .join(" ");
    format!("({} {})", name, inner)
}

struct Parser {
    tokens: Vec<Token>,
    current: usize, // index into `tokens`
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    // ───── entry point ─────
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
            let op = self.previous().clone();
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
            let op = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_any(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_any(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_any(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(op, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_kind(TokenKind::True) {
            return Ok(Expr::Literal(TokenKind::True));
        }
        if self.match_kind(TokenKind::False) {
            return Ok(Expr::Literal(TokenKind::False));
        }
        if self.match_kind(TokenKind::Nil) {
            return Ok(Expr::Literal(TokenKind::Nil));
        }

        let pekd = self.peek().clone();

        if let TokenKind::Number(n) = pekd.kind {
            self.advance();
            return Ok(Expr::Literal(TokenKind::Number(n)));
        } else if let TokenKind::Str(o) = &pekd.kind {
            self.advance();
            return Ok(Expr::Literal(TokenKind::Str(o.into())));
        } else if let TokenKind::Identifier(i) = &pekd.kind {
            self.advance();
            return Ok(Expr::Literal(TokenKind::Identifier(i.into())));
        }

        if self.match_kind(TokenKind::LParen) {
            let expr = self.expression()?;
            self.consume(TokenKind::RParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
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

impl TokenKind {
    fn type_name(&self) -> String {
        let s = match self {
            Self::LParen => "LEFT_PAREN",
            Self::RParen => "RIGHT_PAREN",
            Self::LBrace => "LEFT_BRACE",
            Self::RBrace => "RIGHT_BRACE",
            Self::Star => "STAR",
            Self::Dot => "DOT",
            Self::Comma => "COMMA",
            Self::Plus => "PLUS",
            Self::Minus => "MINUS",
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
            Self::Number(_) => "NUMBER",
            Self::Identifier(_) => "IDENTIFIER",
            Self::And => "AND",
            Self::Class => "CLASS",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::For => "FOR",
            Self::Fun => "FUN",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Print => "PRINT",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Var => "VAR",
            Self::While => "WHILE",
            Self::Eof => "EOF",
            Self::Error(line, error) => &format!("[line {}] Error: {}", line, error),
        };
        s.to_string()
    }
    fn value(&self) -> String {
        match self {
            Self::LParen => "(".to_string(),
            Self::RParen => ")".to_string(),
            Self::LBrace => "[".to_string(),
            Self::RBrace => "]".to_string(),
            Self::Star => "*".to_string(),
            Self::Dot => ".".to_string(),
            Self::Comma => ",".to_string(),
            Self::Plus => "+".to_string(),
            Self::Minus => "-".to_string(),
            Self::Semicolon => ";".to_string(),
            Self::Equal => "=".to_string(),
            Self::EqualEqual => "==".to_string(),
            Self::Bang => "!".to_string(),
            Self::BangEqual => "!=".to_string(),
            Self::Less => "<".to_string(),
            Self::LessEqual => "<=".to_string(),
            Self::Greater => ">".to_string(),
            Self::GreaterEqual => ">=".to_string(),
            Self::Slash => "/".to_string(),
            Self::Str(s) => s.clone(),
            Self::Number(num) => num.to_string(),
            Self::Identifier(i) => i.clone(),
            Self::And => "&".to_string(),
            Self::Class => "class".to_string(),
            Self::Else => "else".to_string(),
            Self::False => "false".to_string(),
            Self::For => "for".to_string(),
            Self::Fun => "fun".to_string(),
            Self::If => "if".to_string(),
            Self::Nil => "nil".to_string(),
            Self::Or => "|".to_string(),
            Self::Print => "print".to_string(),
            Self::Return => "return".to_string(),
            Self::Super => "super".to_string(),
            Self::This => "this".to_string(),
            Self::True => "true".to_string(),
            Self::Var => "var".to_string(),
            Self::While => "while".to_string(),
            Self::Eof => "".to_string(),
            Self::Error(line, error) => {
                format!("[line {}] Error: {}", line, error)
            }
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::LParen => "LEFT_PAREN",
            Self::RParen => "RIGHT_PAREN",
            Self::LBrace => "LEFT_BRACE",
            Self::RBrace => "RIGHT_BRACE",
            Self::Star => "STAR",
            Self::Dot => "DOT",
            Self::Comma => "COMMA",
            Self::Plus => "PLUS",
            Self::Minus => "MINUS",
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
            Self::Number(_) => "NUMBER",
            Self::Identifier(_) => "IDENTIFIER",
            Self::And => "AND",
            Self::Class => "CLASS",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::For => "FOR",
            Self::Fun => "FUN",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Print => "PRINT",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Var => "VAR",
            Self::While => "WHILE",
            Self::Eof => "EOF",
            Self::Error(line, error) => &format!("[line {}] Error: {}", line, error),
        };
        write!(f, "{}", s)
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
        ';' => Some(Semicolon),
        _ => None,
    }
}

fn get_special_ident(val: String) -> TokenKind {
    match val.as_str() {
        "and" => And,
        "class" => Class,
        "else" => Else,
        "false" => False,
        "for" => For,
        "fun" => Fun,
        "if" => If,
        "nil" => Nil,
        "or" => Or,
        "print" => Print,
        "return" => Return,
        "super" => Super,
        "this" => This,
        "true" => True,
        "var" => Var,
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
                eprintln!("Failed to read file {}", filename);
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
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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
            match parser.parse() {
                Ok(p) => {
                    println!("{}", p.print());
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(65);
                }
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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
            match parser.parse() {
                Ok(p) => {
                    println!("{}", eval(p).value());
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
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
        format!("{}.0", lexeme)
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

        // Handle newlines
        if current_char == '\n' {
            line += 1;
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
                    kind: Number(num_val),
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
            let two_char = format!("{}{}", current_char, next_char);

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
            _ => {
                tokens.push(Token {
                    value: "".to_string(),
                    kind: Error(
                        line as u64,
                        format!("Unexpected character: {}", current_char),
                    ),
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

fn eval(ex: Expr) -> TokenKind {
    match ex {
        Expr::Literal(l) => l,

        Expr::Binary(l, o, r) => {
            let left = eval(*l);
            let right = eval(*r);
            match o.kind {
                Plus => match left {
                    Str(left_str) => match right {
                        Str(right_str) => Str(format!("{}{}", left_str, right_str)),
                        Number(right_num) => Str(format!("{}{}", left_str, right_num)),
                        Identifier(_) => todo!("to implement adding identifiers"),
                        l => todo!("This is not supported yet: {}", l),
                    },
                    Number(left_num) => {
                        if let Number(right_num) = right {
                            Number(left_num + right_num)
                        } else {
                            eprintln!("Type Error: Cannot add a number to anything but a number");
                            std::process::exit(65);
                        }
                    }
                    Identifier(_left_ident) => {
                        eprintln!("We havent supported adding variables yet...");
                        std::process::exit(65);
                    }
                    l => {
                        eprintln!("We haven't supported adding {} and {} yet", l, right);
                        std::process::exit(65);
                    }
                },
                Minus => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            Number(left_num - right_num)
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                Star => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            Number(left_num * right_num)
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                Slash => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            Number(left_num / right_num)
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                Greater => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            if left_num > right_num {
                                True
                            } else {
                                False
                            }
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                Less => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            if left_num < right_num {
                                True
                            } else {
                                False
                            }
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                GreaterEqual => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            if left_num >= right_num {
                                True
                            } else {
                                False
                            }
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                LessEqual => {
                    if let Number(left_num) = left {
                        if let Number(right_num) = right {
                            if left_num <= right_num {
                                True
                            } else {
                                False
                            }
                        } else {
                            eprintln!(
                                "We haven't supported subtracting {} from {} yet",
                                right, left_num
                            );
                            std::process::exit(65);
                        }
                    } else {
                        eprintln!(
                            "We haven't supported subtracting {} from {} yet",
                            right, left
                        );
                        std::process::exit(65);
                    }
                }
                EqualEqual => {
                    if left == right {
                        True
                    } else {
                        False
                    }
                }
                BangEqual => {
                    if left != right {
                        True
                    } else {
                        False
                    }
                }
                l => todo!("{l}"),
            }
        }
        Expr::Grouping(val) => eval(*val),
        Expr::Unary(tolk, val) => {
            if let Minus = tolk.kind {
                let other_val = *val;
                if let Number(num) = eval(other_val.clone()) {
                    Number(num * -1.0)
                } else {
                    eprintln!("Operand must be a number.");
                    std::process::exit(70);
                }
            } else if let Bang = tolk.kind {
                let evald = eval(*val);
                if let True = evald {
                    False
                } else if let False = evald {
                    True
                } else if let Nil = evald {
                    True
                } else if let Number(_) = evald {
                    False
                } else {
                    todo!("Idk what this is  {:?}{:?}", tolk, evald);
                }
            } else {
                todo!("Idk wth this is {:?}{:?}", tolk, val);
            }
        }
        l => todo!("{:?}", l),
    }
}
