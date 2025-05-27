use crate::TokenKind::*;
use std::env;
use std::fmt::Display;
use std::fs;

struct Token {
    value: String,
    kind: TokenKind,
}

#[derive(PartialEq, Eq)]
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
    Error(u64),
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
            Self::Error(line) => &format!("[line {}] Error: Unexpected character:", line),
        };
        write!(f, "{}", s)
    }
}

fn get_kind(token: char) -> Result<TokenKind, Result<char, ()>> {
    Ok(match token {
        '(' => LParen,
        ')' => RParen,
        '{' => LBrace,
        '}' => RBrace,
        '*' => Star,
        '.' => Dot,
        ',' => Comma,
        '+' => Plus,
        '-' => Minus,
        ';' => Semicolon,
        '=' | '!' | '>' | '<' | '/' | ' ' => {
            return Err(Ok(token));
        }
        '\n' => {
            return Err(Ok(token));
        }
        _ => {
            return Err(Err(()));
        }
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let mut has_error = false;
            let mut last: char = '\n';
            let mut is_commented = false;

            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let mut file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let mut tokens = vec![];
            let mut line = 1;
            for token in file_contents.chars() {
                if is_commented && token != '\n' {
                    continue;
                } else {
                    is_commented = false;
                }
                if token == '\n' {
                    line += 1;
                    continue;
                }
                if last != '\n' {
                    match last {
                        '!' => {
                            if token == '=' {
                                tokens.push(Token {
                                    value: "!=".to_string(),
                                    kind: BangEqual,
                                });
                                last = '\n';
                                continue;
                            } else {
                                tokens.push(Token {
                                    value: "!".to_string(),
                                    kind: Bang,
                                });
                                last = '\n';
                            }
                        }
                        '=' => {
                            if token == '=' {
                                tokens.push(Token {
                                    value: "==".to_string(),
                                    kind: EqualEqual,
                                });
                                last = '\n';
                                continue;
                            } else {
                                tokens.push(Token {
                                    value: "=".to_string(),
                                    kind: Equal,
                                });
                            }
                        }
                        '>' | '<' => {
                            if token == '=' {
                                tokens.push(Token {
                                    value: format!("{}{}", last, token),
                                    kind: match last {
                                        '>' => GreaterEqual,
                                        '<' => LessEqual,
                                        _ => {
                                            eprintln!("This should never happen");
                                            std::process::exit(1);
                                        }
                                    },
                                });
                                last = '\n';
                                continue;
                            } else {
                                tokens.push(Token {
                                    value: last.to_string(),
                                    kind: match last {
                                        '>' => Greater,
                                        '<' => Less,
                                        _ => {
                                            eprintln!("This should never happen");
                                            std::process::exit(1);
                                        }
                                    },
                                });
                            }
                        }
                        '/' => {
                            if token == '/' {
                                is_commented = true;
                                continue;
                            } else {
                                tokens.push(Token {
                                    value: "/".to_string(),
                                    kind: Slash,
                                });
                                last = '\n';
                            }
                        }
                        _ => {}
                    };
                }
                tokens.push(Token {
                    value: token.to_string(),
                    kind: match get_kind(token) {
                        Ok(t) => t,
                        Err(e) => match e {
                            Ok(c) => {
                                last = c;
                                continue;
                            }
                            Err(()) => {
                                has_error = true;
                                Error(line)
                            }
                        },
                    },
                });
                last = token;
            }
            if !is_commented && last != '\n' && ['!', '=', '>', '<', '/'].contains(&last) {
                tokens.push(Token {
                    value: last.to_string(),
                    kind: match get_kind(last) {
                        Ok(r) => r,
                        Err(e) => match e {
                            Ok(r) => match r {
                                '=' => Equal,
                                '!' => Bang,
                                '/' => Slash,
                                '<' => Less,
                                '>' => Greater,
                                _ => {
                                    println!("If you're seeing this just give up");
                                    std::process::exit(1);
                                }
                            },
                            Err(()) => {
                                has_error = true;
                                Error(line)
                            }
                        },
                    },
                });
            }

            file_contents.clear();
            for token in tokens {
                if let Error(_) = token.kind {
                    eprintln!("{} {}", token.kind, token.value);
                } else {
                    println!("{} {} null", token.kind, token.value,);
                }
            }

            if !file_contents.is_empty() {
                panic!("Scanner not implemented");
            } else {
                println!("EOF  null"); // Placeholder, replace this line when implementing the scanner
            }
            if has_error {
                std::process::exit(65);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
