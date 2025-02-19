use std::{env, fs};

mod common;
mod lexer;
mod parser;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let tokens = lexer::lex(&src);

    //println!("{:#?}", tokens.clone().unwrap());
    println!("{:#?}", parser::parse(src.clone(), &tokens.unwrap()));
}
