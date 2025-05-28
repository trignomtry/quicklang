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
            Self::Error(line, error) => &format!("[line {}] Error: {}", line, error),
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
        '=' | '!' | '>' | '<' | '/' | ' ' | '\t' | '\n' => {
            return Err(Ok(token));
        }

        _ => {
            return Err(Err(()));
        }
    })
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
            let mut last: char = '\n';
            let mut is_commented = false;
            let mut in_string: Option<std::string::String> = None;
            let mut curr_ident: Option<String> = None;

            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let mut file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                std::string::String::new()
            });

            let mut tokens = vec![];
            let mut line = 1;
            let mut iterable: Vec<char> = file_contents.chars().collect();
            let mut index = 0;
            while index < iterable.len() {
                //println!("{} - {:?}", index, iterable.get(index));
                let token = *iterable.get(index).unwrap();
                if is_commented && token != '\n' {
                    index += 1;
                    continue;
                } else {
                    is_commented = false;
                }
                if token == '"' {
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
                    s.push(token);
                    index += 1;
                    continue;
                }
                if let Some(i) = curr_ident.as_mut() {
                    if ![
                        '>', '<', '=', ' ', '=', '!', '>', '<', '/', ' ', '\t', '\n', '(', ')',
                        '{', '}', '+', '-', '*', '/', '.',
                    ]
                    .contains(&token)
                    {
                        i.push(token);
                        index += 1;
                        continue;
                    } else {
                        // We hit a delimiter, so flush the current identifier
                        tokens.push(Token {
                            value: curr_ident.take().unwrap(),
                            kind: Identifier,
                        });
                        // Don't increment index here - we need to process this delimiter token
                    }
                }
                let mut is_pointed = false;
                let mut our_num = 0.0;
                let mut j = index;
                let mut act_num = String::new();
                let mut factor = 0.1;
                while let Some(num) = iterable.get(j) {
                    if let Ok(n) = num.to_string().parse::<f64>() {
                        act_num.push(*num);
                        if is_pointed {
                            our_num += n * factor;
                            factor *= 0.1;
                        } else {
                            our_num *= 10.0;
                            our_num += n;
                        }
                    } else if *num == '.' {
                        if act_num == String::new() {
                            break;
                        }
                        act_num.push(*num);
                        is_pointed = true;
                    } else {
                        break;
                    }
                    j += 1;
                }
                if index != j {
                    for _ in index..j {
                        index += 1;
                    }
                    tokens.push(Token {
                        value: act_num,
                        kind: Number(our_num),
                    });
                    continue;
                }

                if token == '\n' {
                    line += 1;
                    index += 1;
                    if let Some(r) = curr_ident.clone() {
                        tokens.push(Token {
                            value: r,
                            kind: Identifier,
                        });
                        curr_ident = None;
                    }
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
                                index += 1;
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
                                index += 1;
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
                                index += 1;
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
                                last = '\n';
                                index += 1;
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
                                index += 1;
                                if let Some(r) = curr_ident.clone() {
                                    tokens.push(Token {
                                        value: r,
                                        kind: Identifier,
                                    });
                                    curr_ident = None;
                                }
                                continue;
                            }
                            Err(()) => {
                                //has_error = true;
                                let mut a = curr_ident.unwrap_or_default();
                                a.push(token);
                                curr_ident = Some(a);
                                index += 1;
                                continue;
                                //Error(line, "Unexpected character: ".to_string())
                            }
                        },
                    },
                });
                last = token;
                index += 1;
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
                                Error(line, "Unexpected character: ".to_string())
                            }
                        },
                    },
                });
            }

            if in_string.is_some() {
                has_error = true;
                tokens.push(Token {
                    value: "".to_string(),
                    kind: Error(line, "Unterminated string.".to_string()),
                });
            }

            if let Some(r) = curr_ident {
                tokens.push(Token {
                    value: r,
                    kind: Identifier,
                });
            }

            file_contents.clear();
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
