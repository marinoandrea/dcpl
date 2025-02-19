use std::collections::HashMap;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{extra, input::ValueInput, prelude::*};

use crate::{
    common::{Span, Spanned},
    lexer::Token,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'i> {
    Undefined,
    Number(f64),
    Boolean(bool),
    String(&'i str),
}

type Identifier<'i> = &'i str;
type ExternalExpr<'i> = &'i str;
type Refinement<'i> = HashMap<Spanned<Identifier<'i>>, Spanned<Descriptor<'i>>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Directive<'i> {
    TransformationalRule {
        condition: Option<Spanned<TransitionEvent<'i>>>,
        conclusion: Spanned<TransitionEvent<'i>>,
    },
    ReactiveRule {
        event: Option<Spanned<ActionEvent<'i>>>,
        reaction: Spanned<ActionEvent<'i>>,
    },
    DeonticFrame(DeonticFrame<'i>),
    PowerFrame(PowerFrame<'i>),
    CompositeFrame(CompositeFrame<'i>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object<'i> {
    name: Spanned<Identifier<'i>>,
    refinement: Option<Spanned<Refinement<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeonticFrame<'i> {
    position: Spanned<DeonticPosition>,
    holder: Option<Spanned<Descriptor<'i>>>,
    counterparty: Option<Spanned<Descriptor<'i>>>,
    action: Spanned<ActionEvent<'i>>,
    violation: Option<Spanned<ExternalExpr<'i>>>,
    termination: Option<Spanned<ExternalExpr<'i>>>,
    alias: Option<Spanned<Identifier<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PowerFrame<'i> {
    position: Spanned<PowerPosition>,
    holder: Option<Spanned<Descriptor<'i>>>,
    action: Spanned<ActionEvent<'i>>,
    consequence: Option<Spanned<TransitionEvent<'i>>>,
    alias: Option<Spanned<Identifier<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeFrame<'i> {
    identifier: Spanned<Identifier<'i>>,
    params: Vec<Spanned<CompositeFrameParam<'i>>>,
    content: Vec<Spanned<Directive<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Descriptor<'i> {
    Error,
    Literal(Literal<'i>),
    Identifier(Identifier<'i>),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
    },
    Projection {
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Union,
    Intersection,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeonticPosition {
    Duty,
    Prohibition,
    Liberty,
    Claim,
    Protection,
    NoClaim,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PowerPosition {
    Power,
    Liability,
    Disability,
    Immunity,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionEvent<'i> {
    name: Spanned<Identifier<'i>>,
    refinement: Option<Spanned<Refinement<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TransitionEvent<'i> {
    Production(ProductionEvent<'i>),
    Naming(NamingEvent<'i>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamingEvent<'i> {
    input: Box<Spanned<Descriptor<'i>>>,
    output: Box<Spanned<Descriptor<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProductionEvent<'i> {
    Plus(Box<ProductionEventTarget<'i>>),
    Minus(Box<ProductionEventTarget<'i>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeFrameParam<'i> {
    name: Option<Spanned<Identifier<'i>>>,
    descriptor: Spanned<Descriptor<'i>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProductionEventTarget<'i> {
    Descriptor(Spanned<Descriptor<'i>>),
    PowerFrame(Spanned<PowerFrame<'i>>),
    DeonticFrame(Spanned<DeonticFrame<'i>>),
    CompositeFrameCall {
        name: Spanned<Identifier<'i>>,
        args: Vec<Spanned<Identifier<'i>>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
enum FrameProperty<'i> {
    Holder(Option<Descriptor<'i>>),
    Counterparty(Option<Descriptor<'i>>),
    Action(ActionEvent<'i>),
    Consequence(Option<TransitionEvent<'i>>),
    Violation(Option<ExternalExpr<'i>>),
    Termination(Option<ExternalExpr<'i>>),
}

fn directive_parser<'i, I>(
) -> impl Parser<'i, I, Spanned<Directive<'i>>, extra::Err<Rich<'i, Token<'i>, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token<'i>, Span = Span>,
{
    recursive(|directive| {
        let identifier_parser = select! { Token::Identifier(ident) => ident }
            .map_with(|ident, e| (ident, e.span()))
            .labelled("identifier");

        let descriptor_parser = recursive(|desc| {
            let literal = select! {
                Token::Undefined => Descriptor::Literal(Literal::Undefined),
                Token::Boolean(x) => Descriptor::Literal(Literal::Boolean(x)),
                Token::Number(n) => Descriptor::Literal(Literal::Number(n)),
                Token::String(s) => Descriptor::Literal(Literal::String(s)),
            }
            .labelled("literal");

            let ident = select! { Token::Identifier(ident) => Descriptor::Identifier(ident) }
                .labelled("identifier");

            let atom = literal
                .or(ident)
                .map_with(|desc, e| (desc, e.span()))
                .or(desc
                    .clone()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
                .recover_with(via_parser(nested_delimiters(
                    Token::ParenOpen,
                    Token::ParenClose,
                    [(Token::CurlyOpen, Token::CurlyClose)],
                    |span| (Descriptor::Error, span),
                )))
                .boxed();

            // projection (object-access) has priority over everything
            let projection = atom.clone().foldl_with(
                just(Token::Period).ignore_then(atom).repeated(),
                |lhs, rhs, e| {
                    (
                        Descriptor::Projection {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        e.span(),
                    )
                },
            );

            // & has precedence over |
            let op = just(Token::Intersection)
                .to(BinaryOp::Intersection)
                .or(just(Token::Union).to(BinaryOp::Union));

            let binary_op = projection.clone().foldl_with(
                op.then(projection).repeated(),
                |lhs, (op, rhs), e| {
                    (
                        Descriptor::BinaryOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        e.span(),
                    )
                },
            );

            binary_op.labelled("descriptor").as_context()
        });

        let holder_parser = just(Token::Identifier("holder"))
            .then_ignore(just(Token::Column))
            .ignore_then(descriptor_parser.clone())
            .map_with(|(start, _), e| (FrameProperty::Holder(Some(start)), e.span()))
            .labelled("holder");

        let counterparty_parser = just(Token::Identifier("counterparty"))
            .then_ignore(just(Token::Column))
            .ignore_then(descriptor_parser.clone())
            .map_with(|(start, _), e| (FrameProperty::Counterparty(Some(start)), e.span()))
            .labelled("counterparty");

        let alias_parser = just(Token::As)
            .ignore_then(select! { Token::Identifier(ident) => ident as Identifier })
            .map_with(|start, e| (start, e.span()))
            .labelled("alias");

        let refinement_parser = identifier_parser
            .then_ignore(just(Token::Column))
            .then(descriptor_parser.clone())
            .separated_by(just(Token::NewLine).recover_with(skip_then_retry_until(
                any().ignored(),
                just(Token::CurlyClose).ignored(),
            )))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::CurlyOpen), just(Token::CurlyClose))
            .map_with(|start, e| (start, e.span()))
            .labelled("refinement");

        let naming_event_parser = descriptor_parser
            .clone()
            .then(just(Token::Becomes))
            .then(descriptor_parser.clone())
            .map_with(|((input, _), output), e| {
                (
                    TransitionEvent::Naming(NamingEvent {
                        input: Box::new(input),
                        output: Box::new(output),
                    }),
                    e.span(),
                )
            });

        let production_event_descriptor_parser = just(Token::Plus)
            .or(just(Token::Minus))
            .then(descriptor_parser.clone())
            .map_with(|(token, desc), e| {
                (
                    TransitionEvent::Production(match token {
                        Token::Plus => {
                            ProductionEvent::Plus(Box::new(ProductionEventTarget::Descriptor(desc)))
                        }
                        Token::Minus => ProductionEvent::Minus(Box::new(
                            ProductionEventTarget::Descriptor(desc),
                        )),
                        _ => unreachable!(),
                    }),
                    e.span(),
                )
            });

        let production_event_composite_parser = just(Token::Plus)
            .or(just(Token::Minus))
            .then(identifier_parser)
            .then(
                identifier_parser
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
            )
            .map(|((token, name), args)| match token {
                Token::Plus => TransitionEvent::Production(ProductionEvent::Plus(Box::new(
                    ProductionEventTarget::CompositeFrameCall { name, args },
                ))),
                Token::Minus => TransitionEvent::Production(ProductionEvent::Minus(Box::new(
                    ProductionEventTarget::CompositeFrameCall { name, args },
                ))),
                _ => unreachable!(),
            })
            .map_with(|start, e| (start, e.span()));

        let production_event_frame_parser = just(Token::Plus)
            .or(just(Token::Minus))
            .then(directive.clone())
            .map_with(|(token, (directive, dspan)), e| {
                (
                    match directive {
                        Directive::DeonticFrame(df) => TransitionEvent::Production(match token {
                            Token::Plus => ProductionEvent::Plus(Box::new(
                                ProductionEventTarget::DeonticFrame((df, dspan)),
                            )),
                            Token::Minus => ProductionEvent::Plus(Box::new(
                                ProductionEventTarget::DeonticFrame((df, dspan)),
                            )),
                            _ => unreachable!(),
                        }),
                        Directive::PowerFrame(pf) => TransitionEvent::Production(match token {
                            Token::Plus => ProductionEvent::Plus(Box::new(
                                ProductionEventTarget::PowerFrame((pf, dspan)),
                            )),
                            Token::Minus => ProductionEvent::Plus(Box::new(
                                ProductionEventTarget::PowerFrame((pf, dspan)),
                            )),
                            _ => unreachable!(),
                        }),
                        _ => panic!(), // FIXME: we should return an error here
                    },
                    e.span(),
                )
            });

        let production_event_parser = choice((
            production_event_composite_parser.clone(),
            production_event_descriptor_parser.clone(),
            production_event_frame_parser.clone(),
        ));

        let action_object_parser = just(Token::Hash)
            .ignore_then(identifier_parser)
            .then(refinement_parser.or_not())
            .map_with(|start, e| (start, e.span()));

        let action_parser = just(Token::Identifier("action"))
            .ignore_then(just(Token::Column))
            .ignore_then(action_object_parser.clone())
            .map(|((name, refinement), e)| {
                (FrameProperty::Action(ActionEvent { name, refinement }), e)
            })
            .labelled("action");

        let consequence_parser = just(Token::Identifier("consequence"))
            .ignore_then(just(Token::Column))
            .ignore_then(
                naming_event_parser
                    .clone()
                    .or(production_event_parser.clone()),
            )
            .map_with(|(ne, _), e| (FrameProperty::Consequence(Some(ne)), e.span()))
            .labelled("consequence");

        let termination_parser = just(Token::Identifier("termination"))
            .ignore_then(just(Token::Column))
            .ignore_then(select! { Token::ExternalExpr(expr) => expr })
            .map_with(|expr, e| (FrameProperty::Termination(Some(expr)), e.span()))
            .labelled("termination");

        let violation_parser = just(Token::Identifier("violation"))
            .ignore_then(just(Token::Column))
            .ignore_then(select! { Token::ExternalExpr(expr) => expr })
            .map_with(|expr, e| (FrameProperty::Violation(Some(expr)), e.span()))
            .labelled("violation");

        let power_position_parser = select! {
            Token::Power => PowerPosition::Power,
            Token::Liability => PowerPosition::Liability,
            Token::Disability => PowerPosition::Disability,
            Token::Immunity => PowerPosition::Immunity,
        }
        .map_with(|start, e| (start, e.span()))
        .labelled("position");

        let power_properties = choice((
            holder_parser.clone(),
            action_parser.clone(),
            consequence_parser.clone(),
        ))
        .separated_by(just(Token::NewLine))
        .allow_trailing()
        .allow_leading()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::CurlyOpen), just(Token::CurlyClose));

        let power_frame_parser = power_position_parser
            .then(power_properties)
            .then(alias_parser.clone().or_not())
            .map(|((position, properties), alias)| {
                Directive::PowerFrame(PowerFrame {
                    position,
                    alias,
                    action: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Action(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Action(ae) => Some((ae.clone(), *s)),
                            _ => unreachable!(),
                        })
                        .unwrap(), // FIXME(andrea): we should return an error if action was not provided
                    holder: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Holder(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Holder(h) => Some((h.clone().unwrap(), *s)),
                            _ => unreachable!(),
                        }),
                    consequence: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Consequence(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Consequence(c) => Some((c.clone().unwrap(), *s)),
                            _ => unreachable!(),
                        }),
                })
            })
            .map_with(|start, e| (start, e.span()));

        let deontic_position_parser = select! {
            Token::Duty => DeonticPosition::Duty,
            Token::Prohibition => DeonticPosition::Prohibition,
            Token::Liberty => DeonticPosition::Liberty,
            Token::Claim => DeonticPosition::Claim,
            Token::Protection => DeonticPosition::Protection,
            Token::NoClaim => DeonticPosition::NoClaim,
        }
        .map_with(|start, e| (start, e.span()))
        .labelled("position");

        let deontic_properties = choice((
            holder_parser.clone(),
            action_parser.clone(),
            counterparty_parser.clone(),
            violation_parser.clone(),
            termination_parser.clone(),
        ))
        .separated_by(just(Token::NewLine))
        .allow_trailing()
        .allow_leading()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::CurlyOpen), just(Token::CurlyClose));

        let deontic_frame_parser = deontic_position_parser
            .then(deontic_properties)
            .then(alias_parser.or_not())
            .map(|((position, properties), alias)| {
                Directive::DeonticFrame(DeonticFrame {
                    position,
                    alias,
                    action: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Action(_)))
                        .map(|(p, s)| match p {
                            FrameProperty::Action(ae) => (ae.clone(), *s),
                            _ => unreachable!(),
                        })
                        .unwrap(), // FIXME(andrea): we should return an error if action was not provided
                    holder: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Holder(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Holder(h) => Some((h.clone().unwrap(), *s)),
                            _ => unreachable!(),
                        }),
                    counterparty: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Counterparty(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Counterparty(h) => Some((h.clone().unwrap(), *s)),
                            _ => unreachable!(),
                        }),
                    violation: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Violation(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Violation(h) => Some((h.unwrap(), *s)),
                            _ => unreachable!(),
                        }),
                    termination: properties
                        .iter()
                        .find(|(p, _)| matches!(p, FrameProperty::Termination(_)))
                        .and_then(|(p, s)| match p {
                            FrameProperty::Termination(h) => Some((h.unwrap(), *s)),
                            _ => unreachable!(),
                        }),
                })
            })
            .map_with(|start, e| (start, e.span()));

        let transformational_rule_parser = production_event_parser
            .clone()
            .or_not()
            .then_ignore(just(Token::Arrow))
            .then(production_event_parser.clone())
            .map(|(condition, conclusion)| Directive::TransformationalRule {
                condition,
                conclusion,
            })
            .map_with(|start, e| (start, e.span()));

        let reactive_rule_parser = action_object_parser
            .clone()
            .then_ignore(just(Token::Arrow))
            .then(action_object_parser.clone())
            .map(
                |(((e_name, e_refinement), e_span), ((r_name, r_refinement), r_span))| {
                    Directive::ReactiveRule {
                        event: Some((
                            ActionEvent {
                                name: e_name,
                                refinement: e_refinement,
                            },
                            e_span,
                        )),
                        reaction: (
                            ActionEvent {
                                name: r_name,
                                refinement: r_refinement,
                            },
                            r_span,
                        ),
                    }
                },
            )
            .map_with(|start, e| (start, e.span()));

        let composite_frame_arg_parser = identifier_parser
            .then(just(Token::Column).ignore_then(descriptor_parser.clone()))
            .map(|(first, second)| CompositeFrameParam {
                name: Some(first),
                descriptor: second,
            })
            .or(identifier_parser.map(|(name, e)| CompositeFrameParam {
                descriptor: (Descriptor::Identifier(name), e),
                name: Some((name, e)),
            }))
            .map_with(|start, e| (start, e.span()));

        let composite_frame_parser = identifier_parser
            .then(
                composite_frame_arg_parser
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
            )
            .then(
                directive
                    .clone()
                    .padded_by(just(Token::NewLine).repeated())
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::CurlyOpen), just(Token::CurlyClose)),
            )
            .map(|((identifier, params), content)| {
                Directive::CompositeFrame(CompositeFrame {
                    identifier,
                    params,
                    content,
                })
            })
            .map_with(|start, e| (start, e.span()));

        choice((
            transformational_rule_parser,
            reactive_rule_parser,
            composite_frame_parser,
            power_frame_parser,
            deontic_frame_parser,
        ))
    })
}

fn parser<'i, I>(
) -> impl Parser<'i, I, Vec<Spanned<Directive<'i>>>, extra::Err<Rich<'i, Token<'i>, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token<'i>, Span = Span>,
{
    directive_parser()
        .padded_by(just(Token::NewLine).repeated())
        .separated_by(just(Token::NewLine).repeated())
        .allow_trailing()
        .allow_leading()
        .collect::<Vec<_>>()
        .then_ignore(end().or_not())
}

pub fn parse<'i>(
    filename: String,
    src: &'i str,
    input: &'i Vec<Spanned<Token<'i>>>,
) -> Option<Vec<Spanned<Directive<'i>>>> {
    let (ast, parse_errs) = parser()
        .parse(
            input
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_output_errors();

    if parse_errs.is_empty() {
        ast
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
                    .print(sources([(filename.clone(), src)]))
                    .unwrap()
            });
        None
    }
}
