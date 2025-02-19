use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;

use std::fmt;

use crate::common::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Undefined,
    Number(f64),
    Boolean(bool),
    String(&'a str),
    ExternalExpr(&'a str),

    Column,
    Period,
    Comma,
    CurlyOpen,
    CurlyClose,
    ParenOpen,
    ParenClose,
    NewLine,
    As,
    Arrow,

    Identifier(&'a str),

    // events
    Plus,
    Minus,
    Hash,

    // deontic frames
    Duty,
    Prohibition,
    Liberty,
    Claim,
    Protection,
    NoClaim,

    // power frames
    Power,
    Liability,
    Disability,
    Immunity,

    // descriptors
    Becomes,
    Loses,
    Is,
    Union,
    Intersection,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Undefined => write!(f, "undefined"),
            Token::Boolean(x) => write!(f, "{}", x),
            Token::Number(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "{}", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::ExternalExpr(s) => write!(f, "{}", s),
            Token::Duty => write!(f, "duty"),
            Token::Prohibition => write!(f, "prohibition"),
            Token::Liberty => write!(f, "liberty"),
            Token::Claim => write!(f, "claim"),
            Token::Protection => write!(f, "protection"),
            Token::NoClaim => write!(f, "noclaim"),
            Token::Power => write!(f, "power"),
            Token::Liability => write!(f, "liability"),
            Token::Disability => write!(f, "disability"),
            Token::Immunity => write!(f, "immunity"),
            Token::Becomes => write!(f, "becomes"),
            Token::Loses => write!(f, "loses"),
            Token::Is => write!(f, "is"),
            Token::Hash => write!(f, "#"),
            Token::CurlyOpen => write!(f, "{{"),
            Token::CurlyClose => write!(f, "}}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Period => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::Column => write!(f, ":"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Union => write!(f, "|"),
            Token::Intersection => write!(f, "&"),
            Token::As => write!(f, "as"),
            Token::Arrow => write!(f, "=>"),
            Token::NewLine => write!(f, "\\n"),
        }
    }
}

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<Token<'a>>>, extra::Err<Rich<'a, char, Span>>>
{
    let number = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Number);

    let string = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(Token::String);

    let external_expr = just('`')
        .ignore_then(none_of('`').repeated().to_slice())
        .then_ignore(just('`'))
        .map(Token::ExternalExpr);

    let curly_open = just("{").map(|_| Token::CurlyOpen);
    let curly_close = just("}").map(|_| Token::CurlyClose);
    let paren_open = just("(").map(|_| Token::ParenOpen);
    let paren_close = just(")").map(|_| Token::ParenClose);
    let column = just(":").map(|_| Token::Column);
    let period = just(".").map(|_| Token::Period);
    let comma = just(",").map(|_| Token::Comma);
    let hash = just("#").map(|_| Token::Hash);
    let plus = just("+").map(|_| Token::Plus);
    let minus = just("-").map(|_| Token::Minus);
    let union = just("|").map(|_| Token::Union);
    let arrow = just("=>").map(|_| Token::Arrow);
    let intersection = just("&").map(|_| Token::Intersection);
    let newline = just("\n").map(|_| Token::NewLine);

    let ident = text::ident().map(|ident: &str| match ident {
        "duty" => Token::Duty,
        "prohibition" => Token::Prohibition,
        "liberty" => Token::Liberty,
        "claim" => Token::Claim,
        "protection" => Token::Protection,
        "noclaim" => Token::NoClaim,
        "power" => Token::Power,
        "liability" => Token::Liability,
        "disability" => Token::Disability,
        "immunity" => Token::Immunity,
        "becomes" => Token::Becomes,
        "loses" => Token::Loses,
        "is" => Token::Is,
        "as" => Token::As,
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "undefined" => Token::Undefined,
        _ => Token::Identifier(ident),
    });

    let token = choice((
        number,
        string,
        external_expr,
        curly_open,
        curly_close,
        paren_open,
        paren_close,
        column,
        period,
        comma,
        hash,
        plus,
        minus,
        union,
        intersection,
        newline,
        arrow,
        ident,
    ));

    let comment = just("//").then(just('\n').not().repeated()).padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded_by(one_of(" \r\t").repeated())
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub fn lex(filename: String, input: &'_ str) -> Option<Vec<Spanned<Token<'_>>>> {
    let (tokens, parse_errs) = lexer().parse(input).into_output_errors();

    if parse_errs.is_empty() {
        tokens
    } else {
        parse_errs
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
                    .print(sources([(filename.clone(), input)]))
                    .unwrap()
            });
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn power_frame() {
        match lex(
            "test_power_frame".into(),
            "
            power {
                holder: student | staff
                action: #register { instrument: holder.id_card }
                consequence: holder becomes member
            } as p1
        ",
        ) {
            Some(mut tokens) => assert_eq!(
                tokens
                    .iter_mut()
                    .map(|(t, _)| t.clone())
                    .collect::<Vec<_>>(),
                vec![
                    Token::NewLine,
                    Token::Power,
                    Token::CurlyOpen,
                    Token::Identifier("holder"),
                    Token::Column,
                    Token::Identifier("student"),
                    Token::Union,
                    Token::Identifier("staff"),
                    Token::NewLine,
                    Token::Identifier("action"),
                    Token::Column,
                    Token::Hash,
                    Token::Identifier("register"),
                    Token::CurlyOpen,
                    Token::Identifier("instrument"),
                    Token::Column,
                    Token::Identifier("holder"),
                    Token::Period,
                    Token::Identifier("id_card"),
                    Token::CurlyClose,
                    Token::NewLine,
                    Token::Identifier("consequence"),
                    Token::Column,
                    Token::Identifier("holder"),
                    Token::Becomes,
                    Token::Identifier("member"),
                    Token::NewLine,
                    Token::CurlyClose,
                    Token::As,
                    Token::Identifier("p1"),
                    Token::NewLine
                ]
            ),
            None => panic!(),
        }
    }
}
