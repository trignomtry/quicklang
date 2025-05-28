use crate::TokenKind::*;
use std::env;
use std::fmt::Display;
use std::fs;

struct Token {
    value: std::string::String,
    kind: TokenKind,
}

#[derive(PartialEq)]
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
    Str,
    Identifier,
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
    Number(f64),
    Error(u64, std::string::String),
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
            Self::Str => "STRING",
            Self::Number(_) => "NUMBER",
            Self::Identifier => "IDENTIFIER",
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
        _ => Identifier,
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
            let mut has_error = false;
            let mut is_commented = false;
            let mut in_string: Option<std::string::String> = None;

            eprintln!("Logs from your program will appear here!");

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                std::string::String::new()
            });

            let mut tokens = vec![];
            let mut line = 1;
            let chars: Vec<char> = file_contents.chars().collect();
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
                            kind: Str,
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
                    || (current_char == '.'
                        && index + 1 < chars.len()
                        && chars[index + 1].is_ascii_digit())
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
                            });
                            index += 2;
                            continue;
                        }
                        "!=" => {
                            tokens.push(Token {
                                value: "!=".to_string(),
                                kind: BangEqual,
                            });
                            index += 2;
                            continue;
                        }
                        "<=" => {
                            tokens.push(Token {
                                value: "<=".to_string(),
                                kind: LessEqual,
                            });
                            index += 2;
                            continue;
                        }
                        ">=" => {
                            tokens.push(Token {
                                value: ">=".to_string(),
                                kind: GreaterEqual,
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
                        });
                    }
                    '!' => {
                        tokens.push(Token {
                            value: "!".to_string(),
                            kind: Bang,
                        });
                    }
                    '<' => {
                        tokens.push(Token {
                            value: "<".to_string(),
                            kind: Less,
                        });
                    }
                    '>' => {
                        tokens.push(Token {
                            value: ">".to_string(),
                            kind: Greater,
                        });
                    }
                    '/' => {
                        tokens.push(Token {
                            value: "/".to_string(),
                            kind: Slash,
                        });
                    }
                    _ => {
                        has_error = true;
                        tokens.push(Token {
                            value: "".to_string(),
                            kind: Error(line, format!("Unexpected character: {}", current_char)),
                        });
                    }
                }
                index += 1;
            }

            // Handle unterminated string
            if in_string.is_some() {
                has_error = true;
                tokens.push(Token {
                    value: "".to_string(),
                    kind: Error(line, "Unterminated string.".to_string()),
                });
            }

            // Output tokens
            for token in tokens {
                if let Error(_, _) = token.kind {
                    eprintln!("{}{}", token.kind, token.value);
                } else if token.kind == Str {
                    println!("{} \"{}\" {}", token.kind, token.value, token.value);
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

            println!("EOF  null");

            if has_error {
                std::process::exit(65);
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
