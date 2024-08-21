use std::cmp::Ordering;
use std::collections::HashMap;

use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::{parse_quote, token};
use syn::{Expr, ExprPath, Ident, Path, PathArguments};

use crate::format_string::FormatString;
use crate::Dialect;

#[derive(Debug)]
pub(crate) struct Translate {
    dialect: Dialect,
    target_macro: Path,
    pre_args: Vec<Expr>,
    format_string: FormatString,
    arguments: Arguments,
}

impl Translate {
    pub(crate) fn expand(self) -> proc_macro2::TokenStream {
        let Self {
            dialect,
            target_macro,
            pre_args,
            format_string,
            arguments,
        } = self;

        let (format_string, named_arguments) = format_string.translate(dialect);
        let post_args = arguments.expand(&named_arguments);

        quote!(#target_macro!(#(#pre_args,)* #format_string #(, #post_args)*))
    }
}

impl Parse for Translate {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let dialect = input.parse()?;
        input.parse::<token::Comma>()?;
        let target_macro = input.parse()?;
        input.parse::<token::Comma>()?;

        let pre_args;
        syn::bracketed!(pre_args in input);
        let pre_args = pre_args.call(Punctuated::<Expr, token::Comma>::parse_terminated)?;
        let pre_args = pre_args.into_iter().collect();

        input.parse::<token::Comma>()?;
        let format_string = input.parse::<FormatString>()?;

        let arguments = if input.parse::<Option<token::Comma>>()?.is_some() {
            input.parse()?
        } else {
            Arguments::default()
        };

        let positional_count = arguments.positional.len();
        match format_string.positional_arguments().cmp(&positional_count) {
            Ordering::Equal => {}
            Ordering::Less => {
                return Err(syn::Error::new(
                    arguments.positional[format_string.positional_arguments()].span(),
                    "argument never used",
                ))
            }
            Ordering::Greater => {
                return Err(syn::Error::new(
                    format_string.raw_span(),
                    format!(
                        "format string requires {} arguments but only {} were provided",
                        format_string.positional_arguments(),
                        positional_count,
                    ),
                ))
            }
        }

        Ok(Self {
            dialect,
            target_macro,
            pre_args,
            format_string,
            arguments,
        })
    }
}

#[derive(Debug, Default)]
struct Arguments {
    positional: Vec<Expr>,
    named: HashMap<Ident, Expr>,
}
impl Arguments {
    fn expand(self, named_arguments: &[Ident]) -> Vec<Expr> {
        let mut expansion = self.positional;
        expansion.extend(named_arguments.iter().map(|name| {
            if let Some(value) = self.named.get(name) {
                parse_quote!(#value)
            } else {
                parse_quote!(#name)
            }
        }));

        expansion
    }
}

impl Parse for Arguments {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // TODO: error on unused named args

        let mut positional = Vec::new();
        let mut named = HashMap::new();

        let arguments = Punctuated::<Expr, token::Comma>::parse_terminated(input)?;

        for arg in arguments {
            match arg {
                Expr::Assign(arg) => {
                    if let Some(attr) = arg.attrs.first() {
                        return Err(syn::Error::new(
                            attr.span(),
                            "attributes on arguments are not supported",
                        ));
                    }

                    let name = match *arg.left {
                        Expr::Path(ExprPath {
                            attrs,
                            qself,
                            path:
                                Path {
                                    leading_colon,
                                    segments,
                                },
                        }) if attrs.is_empty()
                            && qself.is_none()
                            && leading_colon.is_none()
                            && segments.len() == 1
                            && segments.first().unwrap().arguments == PathArguments::None =>
                        {
                            segments.first().unwrap().ident.clone()
                        }
                        name => return Err(syn::Error::new(name.span(), "expected identifier")),
                    };

                    named.insert(name, *arg.right);
                }
                arg if named.is_empty() => positional.push(arg),
                arg => {
                    return Err(syn::Error::new(
                        arg.span(),
                        "all positional arguments must come before named arguments",
                    ));
                }
            }
        }

        Ok(Self { positional, named })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        let input = r#"std, println, [], "{{ {}, {0}, {named}, {implicit} }}", 0, named = 1"#;
        let parsed: Translate = syn::parse_str(&input).unwrap();
        dbg!(&parsed);

        let expanded = parsed.expand().to_string();
        dbg!(&expanded);
        let expected: syn::Expr =
            syn::parse_quote!(println!("{{ {0}, {0}, {1}, {2} }}", 0, 1, implicit));
        assert_eq!(expected, syn::parse_str::<syn::Expr>(&expanded).unwrap());
    }

    #[test]
    fn defmt_implicit_named() {
        let input = r#"defmt, println, [], "{implicit}""#;
        let parsed: Translate = syn::parse_str(&input).unwrap();
        dbg!(&parsed);

        let expanded = parsed.expand().to_string();
        dbg!(&expanded);
        let expected: syn::Expr = syn::parse_quote!(println!("{0}", implicit));
        assert_eq!(expected, syn::parse_str::<syn::Expr>(&expanded).unwrap());
    }
}
