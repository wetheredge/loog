use winnow::ascii::{digit1, space0};
use winnow::combinator::{alt, delimited, dispatch, empty, fail, opt, preceded, repeat, seq};
use winnow::token::{any, one_of, take_while};
use winnow::{PResult, Parser};

use super::{
    Alignment, ArgumentSpec, FormatSpec, FormatTrait, RawSegment, Sign, TimestampFormat, TypeHint,
    TypeHintKind,
};

pub(super) fn parse(input: &str) -> Result<Vec<RawSegment>, String> {
    repeat(.., alt((parse_literal, parse_parameter)))
        .parse(input)
        .map_err(|err| err.to_string())
}

fn parse_literal(input: &mut &str) -> PResult<RawSegment> {
    let literal = take_while(1.., |c| c != '{' && c != '}');

    repeat::<_, _, (), _, _>(1.., alt(("{{", "}}", literal)))
        .take()
        .map(|s: &str| RawSegment::Literal(s.to_owned()))
        .parse_next(input)
}

fn parse_parameter(input: &mut &str) -> PResult<RawSegment> {
    seq!(RawSegment::Parameter {
        _: "{",
        _: space0,
        argument: parse_parameter_name,
        _: space0,
        type_hint: opt(preceded("=", parse_type_hint)),
        _: space0,
        format: opt(preceded(":", parse_format_spec)).map(|f| f.unwrap_or_default()),
        _: space0,
        _: "}",
    })
    .parse_next(input)
}

fn parse_parameter_name(input: &mut &str) -> PResult<ArgumentSpec> {
    alt((
        identifier.map(ArgumentSpec::Named),
        integer.map(ArgumentSpec::Position),
        empty.value(ArgumentSpec::None),
    ))
    .parse_next(input)
}

fn parse_type_hint(input: &mut &str) -> PResult<TypeHint> {
    alt((
        "bool".value(TypeHintKind::Bool),
        "i8".value(TypeHintKind::I8),
        "u8".value(TypeHintKind::U8),
        "i16".value(TypeHintKind::I16),
        "u16".value(TypeHintKind::U16),
        "i32".value(TypeHintKind::I32),
        "u32".value(TypeHintKind::U32),
        "i64".value(TypeHintKind::I64),
        "u64".value(TypeHintKind::U64),
        "i128".value(TypeHintKind::I128),
        "u128".value(TypeHintKind::U128),
        "f32".value(TypeHintKind::F32),
        "f64".value(TypeHintKind::F64),
        "str".value(TypeHintKind::StringSlice),
        "istr".value(TypeHintKind::InternedString),
        "?".value(TypeHintKind::Format),
        (integer, "..", integer).map(|(start, _, end)| TypeHintKind::Bitfield { start, end }),
        delimited(
            ("[", space0),
            alt((
                ("u8", space0, ";", space0, integer)
                    .map(|(.., len)| TypeHintKind::ByteArray { len }),
                "u8".value(TypeHintKind::ByteSlice),
                ("?", space0, ";", space0, integer)
                    .map(|(.., len)| TypeHintKind::FormatArray { len }),
                "?".value(TypeHintKind::FormatSlice),
            )),
            (space0, "]"),
        ),
    ))
    .with_taken()
    .map(|(kind, raw): (_, &str)| TypeHint {
        raw: raw.to_owned(),
        kind,
    })
    .parse_next(input)
}

fn parse_format_spec(input: &mut &str) -> PResult<FormatSpec> {
    fn is_some<T>(x: Option<T>) -> bool {
        x.is_some()
    }

    let align = || {
        alt((
            "<".value(Alignment::Left),
            "^".value(Alignment::Center),
            ">".value(Alignment::Right),
        ))
    };
    let fill_align = alt((
        (any, align()).map(|(f, a): (char, Alignment)| (Some(f), Some(a))),
        align().map(|a: Alignment| (None, Some(a))),
        empty.value((None, None)),
    ));

    let sign = opt(alt(("+".value(Sign::Plus), "-".value(Sign::Minus))));
    let alternate = opt("#").map(is_some);
    let number_fill = opt("0").map(is_some);
    let width = opt(integer);
    let precision = opt(preceded(".", integer));

    let format = opt(dispatch! {any;
        '?' => empty.value(FormatTrait::Debug),
        'b' => empty.value(FormatTrait::Binary),
        'o' => empty.value(FormatTrait::Octal),
        'x' => alt(("?".value(FormatTrait::DebugLowerHex), empty.value(FormatTrait::LowerHex))),
        'X' => alt(("?".value(FormatTrait::DebugUpperHex), empty.value(FormatTrait::UpperHex))),
        'p' => empty.value(FormatTrait::Pointer),
        'e' => empty.value(FormatTrait::LowerExponent),
        'E' => empty.value(FormatTrait::UpperExponent),
        'a' => empty.value(FormatTrait::Ascii),
        'm' => "s".value(FormatTrait::Timestamp(TimestampFormat::Milliseconds)),
        'u' => "s".value(FormatTrait::Timestamp(TimestampFormat::Microseconds)),
        't' => alt((
            "s".value(TimestampFormat::HumanSeconds),
            "ms".value(TimestampFormat::HumanMilliseconds),
            "us".value(TimestampFormat::HumanMicroseconds),
        )).map(FormatTrait::Timestamp),
        _ => fail::<_, FormatTrait, _>,
    })
    .map(|f| f.unwrap_or(FormatTrait::Display));

    (
        fill_align,
        sign,
        alternate,
        number_fill,
        width,
        precision,
        format,
    )
        .map(
            |((fill, align), sign, alternate, number_fill, width, precision, format)| FormatSpec {
                fill,
                align,
                sign,
                alternate,
                number_fill,
                width,
                precision,
                format,
            },
        )
        .parse_next(input)
}

fn integer(input: &mut &str) -> PResult<usize> {
    digit1.parse_to().parse_next(input)
}

fn identifier(input: &mut &str) -> PResult<String> {
    (
        one_of(('_', unicode_ident::is_xid_start)),
        take_while(.., unicode_ident::is_xid_continue),
    )
        .take()
        .verify_map(|i: &str| (i != "_").then(|| i.to_owned()))
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::RawSegment::{self, *};
    use super::TypeHintKind::*;
    use super::*;

    impl From<&str> for ArgumentSpec {
        fn from(ident: &str) -> Self {
            Self::Named(ident.to_owned())
        }
    }

    impl From<usize> for ArgumentSpec {
        fn from(index: usize) -> Self {
            Self::Position(index)
        }
    }

    macro_rules! tests {
        (
            $f:ident;
            $( $name:ident : $input:literal => ($($expected:tt)+) ; )+ $(;)?
        ) => {
            mod $f {
                #[allow(unused_imports)]
                use super::*;
                $(
                    #[test]
                    fn $name() {
                        tests!(_ $f, $input, $($expected)+);
                    }
                )+
            }
        };
        (_ $f:ident, $input:expr, fail) => {
            super::super::$f(&mut $input).unwrap_err();
        };
        (_ $f:ident, $input:expr, partial $($expected:tt)+) => {
            assert_eq!(super::super::$f(&mut $input).unwrap(), $($expected)+);
        };
        (_ $f:ident, $input:expr, $($expected:tt)+) => {
            assert_eq!(::winnow::Parser::parse(&mut super::super::$f, $input).unwrap(), $($expected)+);
        };
    }

    fn default_parameter() -> RawSegment {
        RawSegment::Parameter {
            argument: ArgumentSpec::None,
            type_hint: None,
            format: FormatSpec::default(),
        }
    }

    macro_rules! struct_default {
        ($struct:path, $default:expr, $($f:ident : $v:expr),*) => {{
            #[allow(unused_mut)]
            let mut x = $default;
            #[allow(unreachable_patterns)]
            match x {
                $struct { $(ref mut $f,)* .. } => {
                    $(*$f = $v.into();)*
                }
                _ => unreachable!(),
            }
            x
        }};
    }

    macro_rules! Parameter {
        ($($f:ident : $v:expr),* $(,)?) => {
            struct_default!(RawSegment::Parameter, default_parameter(), $($f : $v),*)
        }
    }

    macro_rules! FormatSpec {
        ($($f:ident : $v:expr),* $(,)?) => {
            struct_default!(FormatSpec, FormatSpec::default(), $($f : $v),*)
        }
    }

    #[test]
    fn parse_complete() {
        let input = "{}: foo{bar} {{";
        let got = super::parse(input).unwrap();
        let expected = vec![
            Parameter! {},
            Literal(": foo".to_owned()),
            Parameter! { argument: "bar" },
            Literal(" {{".to_owned()),
        ];
        assert_eq!(got, expected);
    }

    tests! {
        parse_literal;
        empty: "" => (fail);
        simple: "foo bar" => (Literal("foo bar".to_owned()));
        newline: "foo\nbar" => (Literal("foo\nbar".to_owned()));
        escaped_open_curly: "foo {{ end" => (Literal("foo {{ end".to_owned()));
        escaped_close_curly: "foo }} end" => (Literal("foo }} end".to_owned()));
        unescaped_curly: "foo {" => (partial Literal("foo ".to_owned()));
        empty_then_unescaped_curly: "{" => (fail);
    }

    tests! {
        parse_parameter;
        simple: "{}" => (Parameter! {});
        named: "{foo}" => (Parameter! { argument: "foo" });
        positional: "{0}" => (Parameter! { argument: 0 });
        type_hint: "{=u8}" => (Parameter! { type_hint: TypeHint { raw: "u8".into(), kind: U8 } });
        named_with_type_hint: "{foo=str}" => (Parameter! { argument: "foo", type_hint: TypeHint { raw: "str".into(), kind: StringSlice } });
        debug: "{:?}" => (Parameter! { format: FormatSpec! { format: FormatTrait::Debug } });
        named_format: "{foo:x}" => (Parameter! { argument: "foo", format: FormatSpec! { format: FormatTrait::LowerHex } });
        typed_format: "{=istr:#b}" => (Parameter! {
            type_hint: TypeHint { raw: "istr".into(), kind: InternedString },
            format: FormatSpec! { alternate: true, format: FormatTrait::Binary },
        });
        everything: "{ foo_bar =[?; 2] :!^-#010.20X? }" => (Parameter {
            argument: ArgumentSpec::Named("foo_bar".to_owned()),
            type_hint: Some(TypeHint { raw: "[?; 2]".into(), kind: FormatArray { len: 2 } }),
            format: FormatSpec {
                fill: Some('!'),
                align: Some(Alignment::Center),
                sign: Some(Sign::Minus),
                alternate: true,
                number_fill: true,
                width: Some(10),
                precision: Some(20),
                format: FormatTrait::DebugUpperHex,
            },
        });
    }

    tests! {
        parse_type_hint;
        bitfield:                "1..3"       => (TypeHint { raw: "1..3".into(),       kind: Bitfield { start: 1, end: 3 } });
        byte_slice:              "[u8]"       => (TypeHint { raw: "[u8]".into(),       kind: ByteSlice });
        byte_slice_whitespace:   "[ u8\t]"    => (TypeHint { raw: "[ u8\t]".into(),    kind: ByteSlice });
        byte_array:              "[u8;1]"     => (TypeHint { raw: "[u8;1]".into(),     kind: ByteArray { len: 1 } });
        byte_array_whitespace:   "[ u8 ; 1 ]" => (TypeHint { raw: "[ u8 ; 1 ]".into(), kind: ByteArray { len: 1 } });
        format_slice:            "[?]"        => (TypeHint { raw: "[?]".into(),        kind: FormatSlice });
        format_slice_whitespace: "[ ? ]"      => (TypeHint { raw: "[ ? ]".into(),      kind: FormatSlice });
        format_array:            "[?;1]"      => (TypeHint { raw: "[?;1]".into(),      kind: FormatArray { len: 1 } });
        format_array_whitespace: "[ ? ; 1 ]"  => (TypeHint { raw: "[ ? ; 1 ]".into(),  kind: FormatArray { len: 1 } });
    }

    tests! {
        parse_format_spec;
        defaults: "" => (FormatSpec! {});
        fill: "0^" => (FormatSpec! { fill: '0', align: Alignment::Center });
        weird_fill: ">>" => (FormatSpec! { fill: '>', align: Alignment::Right });
        align_right: ">" => (FormatSpec! { align: Alignment::Right });
        aligned_sign: "+<-" => (FormatSpec! { fill: '+', align: Alignment::Left, sign: Sign::Minus });
        precision: ".1" => (FormatSpec! { precision: 1 });
        precision_leading_zero: ".02" => (FormatSpec! { precision: 2 });
        sign: "-" => (FormatSpec! { sign: Sign::Minus });
        debug: "?" => (FormatSpec! { format: FormatTrait::Debug });
        pretty_debug: "#?" => (FormatSpec! { alternate: true, format: FormatTrait::Debug });
        debug_hex: "x?" => (FormatSpec! { format: FormatTrait::DebugLowerHex });
        milliseconds: "ms" => (FormatSpec! { format: FormatTrait::Timestamp(TimestampFormat::Milliseconds) });
        microseconds: "us" => (FormatSpec! { format: FormatTrait::Timestamp(TimestampFormat::Microseconds) });
        human_seconds: "ts" => (FormatSpec! { format: FormatTrait::Timestamp(TimestampFormat::HumanSeconds) });
        human_milliseconds: "tms" => (FormatSpec! { format: FormatTrait::Timestamp(TimestampFormat::HumanMilliseconds) });
        human_microseconds: "tus" => (FormatSpec! { format: FormatTrait::Timestamp(TimestampFormat::HumanMicroseconds) });
        both_fills: "0>0" => (FormatSpec! { fill: '0', align: Alignment::Right, number_fill: true });
        double_zero: "00" => (FormatSpec! { number_fill: true, width: 0 });
        everything: ".>+#01.2X" => (FormatSpec {
            fill: Some('.'),
            align: Some(Alignment::Right),
            sign: Some(Sign::Plus),
            alternate: true,
            number_fill: true,
            width: Some(1),
            precision: Some(2),
            format: FormatTrait::UpperHex,
        });
    }

    tests! {
        integer;
        zero: "0" => (0);
        bond: "007" => (7);
    }

    tests! {
        identifier;
        basic: "foo_bar" => ("foo_bar");
        constant: "FOO" => ("FOO");
        underscore: "_foo" => ("_foo");
        underscore_only: "_" => (fail);
        whitespace: " " => (fail);
    }
}
