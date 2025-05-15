use std::collections::HashMap;

use crate::parser::common::Spanned;

pub type Identifier<'i> = &'i str;
pub type ExternalExpr<'i> = &'i str;
pub type Refinement<'i> = HashMap<Spanned<Identifier<'i>>, Spanned<Descriptor<'i>>>;

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
pub enum Literal<'i> {
    Undefined,
    Number(f64),
    Boolean(bool),
    String(&'i str),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object<'i> {
    pub name: Spanned<Identifier<'i>>,
    pub refinement: Option<Spanned<Refinement<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeonticFrame<'i> {
    pub position: Spanned<DeonticPosition>,
    pub holder: Option<Spanned<Descriptor<'i>>>,
    pub counterparty: Option<Spanned<Descriptor<'i>>>,
    pub action: Spanned<ActionEvent<'i>>,
    pub violation: Option<Spanned<ExternalExpr<'i>>>,
    pub termination: Option<Spanned<ExternalExpr<'i>>>,
    pub alias: Option<Spanned<Identifier<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PowerFrame<'i> {
    pub position: Spanned<PowerPosition>,
    pub holder: Option<Spanned<Descriptor<'i>>>,
    pub action: Spanned<ActionEvent<'i>>,
    pub consequence: Option<Spanned<TransitionEvent<'i>>>,
    pub alias: Option<Spanned<Identifier<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeFrame<'i> {
    pub identifier: Spanned<Identifier<'i>>,
    pub params: Vec<Spanned<CompositeFrameParam<'i>>>,
    pub content: Vec<Spanned<Directive<'i>>>,
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
    pub name: Spanned<Identifier<'i>>,
    pub refinement: Option<Spanned<Refinement<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TransitionEvent<'i> {
    Production(ProductionEvent<'i>),
    Qualification(QualificationEvent<'i>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct QualificationEvent<'i> {
    pub input: Box<Spanned<Descriptor<'i>>>,
    pub output: Box<Spanned<Descriptor<'i>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProductionEvent<'i> {
    Plus(Box<ProductionEventTarget<'i>>),
    Minus(Box<ProductionEventTarget<'i>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeFrameParam<'i> {
    pub name: Option<Spanned<Identifier<'i>>>,
    pub descriptor: Spanned<Descriptor<'i>>,
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
pub enum FrameProperty<'i> {
    Holder(Option<Descriptor<'i>>),
    Counterparty(Option<Descriptor<'i>>),
    Action(ActionEvent<'i>),
    Consequence(Option<TransitionEvent<'i>>),
    Violation(Option<ExternalExpr<'i>>),
    Termination(Option<ExternalExpr<'i>>),
}
