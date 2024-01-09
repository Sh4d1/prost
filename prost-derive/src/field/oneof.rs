use anyhow::{bail, Error};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_str, Expr, ExprLit, Ident, Lit, Meta, MetaNameValue, Path};

use crate::field::{set_option, tags_attr};

#[derive(Clone)]
pub struct Field {
    pub ty: Path,
    pub tags: Vec<u32>,
}

impl Field {
    pub fn new(attrs: &[Meta]) -> Result<Option<Field>, Error> {
        let mut ty = None;
        let mut tags = None;
        let mut unknown_attrs = Vec::new();

        for attr in attrs {
            if attr.path().is_ident("oneof") {
                let t = match *attr {
                    Meta::NameValue(MetaNameValue {
                        value:
                            Expr::Lit(ExprLit {
                                lit: Lit::Str(ref lit),
                                ..
                            }),
                        ..
                    }) => parse_str::<Path>(&lit.value())?,
                    Meta::List(ref list) => list.parse_args::<Ident>()?.into(),
                    _ => bail!("invalid oneof attribute: {:?}", attr),
                };
                set_option(&mut ty, t, "duplicate oneof attribute")?;
            } else if let Some(t) = tags_attr(attr)? {
                set_option(&mut tags, t, "duplicate tags attributes")?;
            } else {
                unknown_attrs.push(attr);
            }
        }

        let ty = match ty {
            Some(ty) => ty,
            None => return Ok(None),
        };

        if !unknown_attrs.is_empty() {
            bail!(
                "unknown attribute(s) for message field: #[prost({})]",
                quote!(#(#unknown_attrs),*)
            );
        }

        let tags = match tags {
            Some(tags) => tags,
            None => bail!("oneof field is missing a tags attribute"),
        };

        Ok(Some(Field { ty, tags }))
    }

    /// Returns a statement which encodes the oneof field.
    pub fn encode(&self, ident: TokenStream) -> TokenStream {
        quote! {
            if let Some(::scalar::OpenVariant::Known(ref oneof)) = #ident {
                oneof.encode(buf)
            }
        }
    }

    /// Returns an expression which evaluates to the result of decoding the oneof field.
    pub fn merge(&self, ident: TokenStream) -> TokenStream {
        let ty = &self.ty;
        quote! {
            {
                let (result, is_some) = match std::mem::take(#ident) {
                    None => {
                        // So far, for that oneof we've seen:
                        // - 0 unknown
                        // - 0 known
                        // We are currently seeing the first known
                        let mut option = None;
                        let result = #ty::merge(&mut option, tag, wire_type, buf, ctx);
                        let is_some = option.is_some();
                        if let Some(variant) = option {
                            *#ident = Some(::scalar::OpenVariant::Known(variant))
                        };
                        (result, is_some)
                    }
                    Some(::scalar::OpenVariant::Known(variant)) => {
                        // So far, for that oneof we've seen:
                        // - 0+ unknown
                        // - 1+ known
                        // We are currently seeing a new known. (last one wins)
                        let mut option = Some(variant);
                        let result = #ty::merge(&mut option, tag, wire_type, buf, ctx);
                        let is_some = option.is_some();
                        if let Some(variant) = option {
                            *#ident = Some(::scalar::OpenVariant::Known(variant))
                        };
                        (result, is_some)
                    }
                    Some(::scalar::OpenVariant::Unknown) => {
                        // So far, for that oneof we've seen:
                        // - 1+ unknown
                        // - 0 known
                        // We are currently seeing the first known
                        let mut option = None;
                        let result = #ty::merge(&mut option, tag, wire_type, buf, ctx);
                        let is_some = option.is_some();
                        if let Some(variant) = option {
                            *#ident = Some(::scalar::OpenVariant::Known(variant))
                        } else {
                            *#ident = Some(::scalar::OpenVariant::Unknown)
                        };
                        (result, is_some)
                    }
                };
                match (result, is_some) {
                    (Err(err), _) => Err(err),
                    (Ok(()), true) => Ok(()),
                    (Ok(()), false) => Err(::prost::DecodeError::new("merge was expected to set option")),
                }
            }
        }
    }

    /// Returns an expression which evaluates to the encoded length of the oneof field.
    pub fn encoded_len(&self, ident: TokenStream) -> TokenStream {
        let ty = &self.ty;
        quote! {
            match #ident.as_ref() {
                None => 0,
                Some(scalar::OpenVariant::Unknown) => {
                    // We don't encode unknown variants. We could if the undecoded bytes were a
                    // payload of `Unknown`.
                    0
                },
                Some(scalar::OpenVariant::Known(value)) => #ty::encoded_len(value),
            }
        }
    }

    pub fn clear(&self, ident: TokenStream) -> TokenStream {
        quote!(#ident = ::core::option::Option::None)
    }
}
