mod common;
mod directive;

pub mod ast;
pub mod lexer;

use chumsky::{extra, input::ValueInput, prelude::*};

use crate::{
    parser::ast::*,
    parser::common::{Span, Spanned},
    parser::lexer::Token,
};

/// Parse a `Vec` of [`Token`](crate::parser::lexer::Token) into a `Vec` of [`Directive`](crate::parser::ast::Directive).
pub fn parse<'i>(
    tokens: &'i Vec<Spanned<Token<'i>>>,
) -> Result<Vec<Spanned<Directive<'i>>>, Vec<Rich<'i, Token<'i>>>> {
    parser()
        .parse(
            tokens
                .as_slice()
                .map((tokens.len()..tokens.len()).into(), |(t, s)| (t, s)),
        )
        .into_result()
}

fn parser<'i, I>(
) -> impl Parser<'i, I, Vec<Spanned<Directive<'i>>>, extra::Err<Rich<'i, Token<'i>, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token<'i>, Span = Span>,
{
    directive::directive_parser()
        .padded_by(just(Token::NewLine).repeated())
        .separated_by(just(Token::NewLine).repeated())
        .allow_trailing()
        .allow_leading()
        .collect::<Vec<_>>()
        .then_ignore(end().or_not())
}
