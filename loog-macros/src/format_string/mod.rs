mod parser;

use std::collections::HashMap;

use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr};

use crate::Dialect;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct FormatString {
    raw: LitStr,
    segments: Vec<Segment>,
    positional_arguments: usize,
    named_arguments: Vec<Ident>,
}

impl Parse for FormatString {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let raw = input.parse::<LitStr>()?;
        let raw_span = raw.span();
        let mut raw_segments = parser::parse(&raw.value()).unwrap();

        let next_index = {
            let mut next_unused = 0;
            let mut next_implicit = 0;

            for segment in raw_segments.iter_mut() {
                if let RawSegment::Parameter { argument, .. } = segment {
                    match argument {
                        ArgumentSpec::Named(_) => {}
                        ArgumentSpec::Position(i) => {
                            next_unused = next_unused.max(*i);
                        }
                        ArgumentSpec::None => {
                            let i = next_implicit;
                            next_implicit += 1;
                            *argument = ArgumentSpec::Position(i);
                        }
                    }
                }
            }

            next_unused.max(next_implicit)
        };

        let mut positions = Vec::new();
        let mut named_arguments = HashMap::new();
        let mut named_indices = next_index..;
        let segments = raw_segments
            .into_iter()
            .map(|raw| match raw {
                RawSegment::Literal(literal) => Segment::Literal(literal),
                RawSegment::Parameter {
                    argument,
                    type_hint,
                    format,
                } => {
                    let index = match argument {
                        ArgumentSpec::Named(name) => {
                            let i = named_arguments
                                .entry(name)
                                .or_insert_with(|| named_indices.next().unwrap());
                            *i
                        }
                        ArgumentSpec::Position(i) => {
                            positions.push(i);
                            i
                        }
                        ArgumentSpec::None => unreachable!(),
                    };

                    Segment::Parameter {
                        index,
                        type_hint,
                        format,
                    }
                }
            })
            .collect();

        let argument_not_used = |x| {
            Err(syn::Error::new(
                raw_span,
                format!("argument {x} is not used in this format string"),
            ))
        };

        // Check that there aren't any gaps in the positional argument indices
        positions.sort_unstable();
        positions.dedup();
        match positions.len() {
            0 => {}
            1 if positions[0] == 0 => {}
            1 => return argument_not_used(0),
            _ => {
                for pair in positions.windows(2) {
                    if pair[1].checked_sub(pair[0]) != Some(1) {
                        return argument_not_used(pair[0] + 1);
                    }
                }
            }
        }

        let mut named_arguments = named_arguments.into_iter().collect::<Vec<_>>();
        named_arguments.sort_unstable_by_key(|(_, index)| *index);
        let named_arguments = named_arguments
            .into_iter()
            .map(|(name, _)| Ident::new(&name, raw_span))
            .collect();

        Ok(Self {
            raw,
            segments,
            positional_arguments: positions.len(),
            named_arguments,
        })
    }
}

impl FormatString {
    pub(crate) fn raw_span(&self) -> proc_macro2::Span {
        self.raw.span()
    }

    pub(crate) fn positional_arguments(&self) -> usize {
        self.positional_arguments
    }

    pub(crate) fn translate(self, dialect: Dialect) -> (LitStr, Vec<Ident>) {
        let span = self.raw.span();

        let mut format_string = String::new();

        for segment in self.segments {
            match segment {
                Segment::Literal(s) => format_string.push_str(&s),
                Segment::Parameter {
                    index,
                    type_hint,
                    format,
                } => {
                    format_string.push('{');
                    format_string.push_str(&index.to_string());

                    if dialect == Dialect::Defmt {
                        if let Some(TypeHint { raw, .. }) = type_hint {
                            format_string.push('=');
                            format_string.push_str(&raw);
                        }
                    }

                    format.translate(dialect, &mut format_string);

                    format_string.push('}');
                }
            }
        }

        (LitStr::new(&format_string, span), self.named_arguments)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Segment {
    Literal(String),
    Parameter {
        index: usize,
        type_hint: Option<TypeHint>,
        format: FormatSpec,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RawSegment {
    Literal(String),
    Parameter {
        argument: ArgumentSpec,
        type_hint: Option<TypeHint>,
        format: FormatSpec,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ArgumentSpec {
    Named(String),
    Position(usize),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeHint {
    raw: String,
    kind: TypeHintKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeHintKind {
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F32,
    F64,
    Bitfield { start: usize, end: usize },
    ByteArray { len: usize },
    ByteSlice,
    StringSlice,
    InternedString,
    Format,
    FormatArray { len: usize },
    FormatSlice,
}

// TODO: support width + precision as passed in parameters?
#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct FormatSpec {
    fill: Option<char>,
    align: Option<Alignment>,
    sign: Option<Sign>,
    alternate: bool,
    number_fill: bool,
    width: Option<usize>,
    precision: Option<usize>,
    format: FormatTrait,
}

impl FormatSpec {
    fn translate(&self, dialect: Dialect, format_string: &mut String) {
        format_string.push(':');
        let len = format_string.len();

        match dialect {
            Dialect::Std => {
                if let Some(fill) = self.fill {
                    format_string.push(fill);
                }

                if let Some(align) = self.align {
                    format_string.push(match align {
                        Alignment::Left => '<',
                        Alignment::Center => '^',
                        Alignment::Right => '>',
                    });
                }

                if let Some(sign) = self.sign {
                    format_string.push(match sign {
                        Sign::Plus => '+',
                        Sign::Minus => '-',
                    });
                }

                if self.alternate {
                    format_string.push('#');
                }

                if self.number_fill {
                    format_string.push('0');
                }

                if let Some(width) = self.width {
                    format_string.push_str(&width.to_string());
                }

                if let Some(precision) = self.precision {
                    format_string.push('.');
                    format_string.push_str(&precision.to_string());
                }
            }
            Dialect::Defmt => {
                if self.alternate {
                    format_string.push('#');
                }
            }
        }

        format_string.push_str(self.format.translate(dialect));
        if len == format_string.len() {
            format_string.pop();
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Alignment {
    Left,
    Center,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Sign {
    Plus,
    Minus,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum FormatTrait {
    #[default]
    Display,
    Debug,
    DebugLowerHex,
    DebugUpperHex,
    Binary,
    Octal,
    LowerHex,
    UpperHex,
    Pointer,
    LowerExponent,
    UpperExponent,
    Ascii,
    Timestamp(TimestampFormat),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TimestampFormat {
    Milliseconds,
    Microseconds,
    HumanSeconds,
    HumanMilliseconds,
    HumanMicroseconds,
}

impl FormatTrait {
    fn translate(self, dialect: Dialect) -> &'static str {
        use Dialect::*;
        use FormatTrait::*;
        use TimestampFormat::*;

        match (self, dialect) {
            (Display, _) => "",
            (Debug, _) => "?",
            (Binary, _) => "b",
            (Octal, _) => "o",
            (LowerHex, _) => "x",
            (UpperHex, _) => "X",

            (DebugLowerHex, Std) => "x?",
            (DebugUpperHex, Std) => "X?",
            (Pointer, Std) => "p",
            (LowerExponent, Std) => "e",
            (UpperExponent, Std) => "E",

            (Ascii, Defmt) => "a",
            (Timestamp(Milliseconds), Defmt) => "ms",
            (Timestamp(Microseconds), Defmt) => "us",
            (Timestamp(HumanSeconds), Defmt) => "ts",
            (Timestamp(HumanMilliseconds), Defmt) => "tms",
            (Timestamp(HumanMicroseconds), Defmt) => "tus",

            // Unsupported
            _ => "",
        }
    }
}
