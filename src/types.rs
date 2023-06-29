// project related type utilities
// not used in type checker

use std::ops::Range;

pub type Spanned<T> = (T, Range<usize>);
