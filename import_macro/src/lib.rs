use proc_macro::TokenStream;

mod emitter;
use quote::quote;

impl emitter::InlineType {
    pub fn emit_tokens(&self, ctx: &emitter::CodeEmissionContext) -> proc_macro2::TokenStream {
        match self {
            Self::Bool => quote!{ bool },
            Self::Nat => quote!{ u64 },
            Self::Int => quote!{ i64 },
            Self::Dbl => quote!{ f64 },
            Self::Txt => quote!{ String },
            Self::Opt(x) => { let inner = x.emit_tokens(ctx); quote!{ Option<#inner> } },
            Self::List(x) => { let inner = x.emit_tokens(ctx); quote!{ Vec<#inner> } },
            &Self::TypeRef(tyid) => {
                let ident = syn::Ident::new(&ctx.type_name(tyid), proc_macro2::Span::call_site());
                quote!{ #ident }
            }
        }
    }
}
impl emitter::ItemType<'_> {
    pub fn emit_tokens(&self, name: &str, ctx: &emitter::CodeEmissionContext) -> proc_macro2::TokenStream {
        match self {
            Self::Struct(es) => {
                let fields = es.iter().map(|(k, v)| {
                    let name_ident = syn::Ident::new(k, proc_macro2::Span::call_site());
                    let ty_token = v.emit_tokens(ctx);
                    quote!{ pub #name_ident: #ty_token }
                });
                let name_ident = syn::Ident::new(name, proc_macro2::Span::call_site());
                quote!{
                    #[derive(serde::Deserialize, serde::Serialize, Debug, Clone, PartialEq, Eq)]
                    pub struct #name_ident {
                        #(#fields),*
                    }
                }
            },
            Self::Enum(es) => {
                let variants = es.iter().map(|(k, v)| {
                    let name_ident = syn::Ident::new(k, proc_macro2::Span::call_site());
                    let ty_tokens = v.as_ref().map(|t| t.emit_tokens(ctx));
                    match ty_tokens {
                        Some(t) => quote!{ #name_ident(#t) },
                        None => quote!{ #name_ident }
                    }
                });
                let name_ident = syn::Ident::new(name, proc_macro2::Span::call_site());
                quote!{
                    #[derive(serde::Deserialize, serde::Serialize, Debug, Clone, PartialEq, Eq)]
                    pub enum #name_ident {
                        #(#variants),*
                    }
                }
            }
        }
    }
}

#[proc_macro]
pub fn import_dhall_schema(t: TokenStream) -> TokenStream {
    let file_path = syn::parse_macro_input!(t as syn::LitStr);
    println!("input: {}", file_path.value());

    let parsed = dhall::Parsed::parse_file(file_path.value().as_ref()).expect("Failed to parse schema file");
    let resolved = parsed.resolve().expect("Failed to resolve parsed schema");
    let typed = resolved.typecheck().expect("Failed to check type in schema");
    let normalized = typed.normalize();
    let tree = emitter::ElementTree::from_nir(normalized.as_nir());

    let mut ctx = emitter::CodeEmissionContext::new();
    let tyast = match tree {
        emitter::ElementTree::RecordLiteral(ref es) => {
            // toplevel is record
            es.iter().map(|(k, v)| {
                let ty = v.emit_item_type(&mut ctx);
                let tyid = ctx.query_type_index(ty.clone());
                ctx.update_type_name(tyid, k);
                (tyid, ty)
            }).collect()
        },
        _ => {
            let ty = tree.emit_item_type(&mut ctx);
            let tyid = ctx.query_type_index(ty.clone());
            vec![(tyid, ty)]
        }
    };

    let quoted = tyast.into_iter().map(|(tx, t)| t.emit_tokens(&ctx.type_name(tx), &ctx));
    TokenStream::from(quote!{ #(#quoted)* })
}
