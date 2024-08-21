mod format_string;
mod translate;

use proc_macro::TokenStream;
use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
};
use translate::Translate;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Dialect {
    Std,
    Defmt,
}

impl Parse for Dialect {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.call(syn::Ident::parse_any)?;
        match ident.to_string().as_str() {
            "std" => Ok(Self::Std),
            "defmt" => Ok(Self::Defmt),
            _ => Err(syn::Error::new_spanned(ident, "expected std or defmt")),
        }
    }
}

#[proc_macro]
pub fn translate(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as Translate);
    input.expand().into()
}
