use chumsky::span::SimpleSpan;

/// Common span definition for character range in source code
pub type Span = SimpleSpan;
/// Utility to wrap parsing artifacts with their source character range
pub type Spanned<T> = (T, Span);
