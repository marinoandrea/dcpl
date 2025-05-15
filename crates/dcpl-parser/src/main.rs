use std::{env, fs};

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::error::Rich;
use dcpl_parser::*;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    match parser::lexer::lex(&src) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(directives) => println!("{:#?}", directives),
            Err(parse_errors) => {
                handle_errors(filename, &src, parse_errors);
            }
        },
        Err(lex_errors) => {
            handle_errors(filename, &src, lex_errors);
        }
    }
}

fn handle_errors<'i, T: Clone + std::string::ToString>(
    filename: String,
    src: &'i str,
    errors: Vec<Rich<'i, T>>,
) {
    errors
        .into_iter()
        .map(|e| e.map_token(|tok| tok.to_string()))
        .for_each(|e| {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([(filename.clone(), src)]))
                .unwrap();
        });
}
