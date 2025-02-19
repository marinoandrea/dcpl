use chumsky::span::SimpleSpan;

pub(crate) type Span = SimpleSpan;
pub(crate) type Spanned<T> = (T, Span);
