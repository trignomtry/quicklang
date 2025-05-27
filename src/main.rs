use crate::TokenKind::*;
use std::env;
use std::fmt::Display;
use std::fs;

struct Token {
    value: String,
    kind: TokenKind,
}

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
        };
        write!(f, "{}", s)
    }
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
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let mut file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let mut tokens = vec![];
            for token in file_contents.chars() {
                tokens.push(Token {
                    value: token.to_string(),
                    kind: match token {
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
                        _ => todo!("Anything but parens not implemented"),
                    },
                });
            }
            file_contents.clear();
            for token in tokens {
                println!("{} {} null", token.kind, token.value);
            }

            if !file_contents.is_empty() {
                panic!("Scanner not implemented");
            } else {
                println!("EOF  null"); // Placeholder, replace this line when implementing the scanner
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
