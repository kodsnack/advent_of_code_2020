use proc_macro::TokenStream;
use syn::{Ident, Token};

#[macro_use]
extern crate lazy_static;

#[proc_macro_attribute]
pub fn aoc(attr: TokenStream, item: TokenStream) -> TokenStream {
    let parsed: Aoc = syn::parse::<Aoc>(attr).unwrap();
    item
}

struct Aoc {
    pub day: u8,
    pub part: Part,
}

impl syn::parse::Parse for Aoc {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let day: syn::LitInt = input.parse()?;
        let _: syn::token::Comma = input.parse()?;
        let part: syn::LitStr = input.parse()?;
        Ok(Aoc {
            day: day.base10_digits().parse().unwrap(),
            part: match part.value().as_str() {
                "First" | "1" => Part::First,
                "Second" | "2" => Part::Second,
                _ => return Err(input.lookahead1().error()),
            },
        })
    }
}

enum Part {
    First,
    Second,
}
