extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    //turn TokenStream into abstract syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name); //generate name for builder struct
    let bident = syn::Ident::new(&bname, name.span()); //identifier for the name

    // Extract fields of input struct into iterator.
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        // Must match the type of struct we can build, so a struct with named fields.
        unimplemented!();
    };

    // The fields of the builder version of the struct.
    // An instance of this will be used to generate the actual target struct.
    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        //If the type is already option, or the field has builder attribute, leave type as is.
        if get_inner_ty(ty, "Option").is_some() || builder_of(f).is_some() {
            quote! {#name: #ty}
        } else {
            // Otherwise surround with option.
            quote! {#name: std::option::Option<#ty>}
        }
    });

    //The fields used to build a new #nameBuilder
    let empty_builder = fields.iter().map(|f| {
        let name = &f.ident;
        // If the field is buildable, it is assumed vec.
        if builder_of(f).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            //None suffices.
            quote! { #name: std::option::Option::None }
        }
    });

    // All of the methods for setting and building fields.
    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let (arg_type, value) = if let Some(inner_ty) = get_inner_ty(ty, "Option") {
            // If the field is an Option<T>, setting should take just a T,
            // but then we need to store it within a Some.
            (inner_ty, quote! { std::option::Option::Some(#name) })
        } else if builder_of(&f).is_some() {
            // If the field is a builder, we can assume it is a Vec<T>,
            // and the value in the builder is _not_ wrapped in an Option (Vec<Option<...>>),
            // we shouldn't wrap the value in Some.
            (ty, quote! { #name })
        } else {
            // In all other cases, we take the type used by the target,
            // and store it in an Option.
            (ty, quote! { std::option::Option::Some(#name) })
        };
        // The set method for field, might be discarded.
        let set_method = quote! {
            pub fn #name(&mut self, #name: #arg_type) -> &mut Self {
                self.#name = #value;
                self
            }
        };
        // Either generate the extend method, set method, or both
        match extend_method(&f) {
            None => set_method.into(),
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => {
                // safe to generate both
                let expr = quote! {
                    #set_method
                    #extend_method
                };
                expr.into()
            }
        }
    });

    // The values for the fields for builder.build().
    let builder_to_built = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        //Fine if not set.
        if get_inner_ty(ty, "Option").is_some() || builder_of(f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            // Insist not None.
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let doc = format!("\
            Implements the [builder pattern] for [`{}`].\n\
            \n\
            [builder pattern]: https://rust-lang-nursery.github.io/api-guidelines/type-safety.html#c-builder", name);
    // The TokenStream that is generated.
    let generated = quote! {
        #[doc = #doc]
        pub struct #bident {
            #(#builder_fields,)*
        }

        impl #bident {
            #(#methods)*

            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#builder_to_built,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#empty_builder,)*
                }
            }
        }
    };
    //proc_macro2::TokenStream to proc_macro::TokenStream
    generated.into()
}

// Takes the Type of a field and retrieves the actual inner type, ty *-> ident.
// Checks to make sure that the type inside is the expected type and that it is not a compound type.
// Also assures that the structure of the ast traversal from ty to ident is what is expected.
fn get_inner_ty<'a>(ty: &'a syn::Type, expected_type: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != expected_type {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

// Takes a field and checks if there should be a builder for that field.
fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
            return Some(attr);
        }
    }
    None
}

// Parses the #[builder(each = "...")] and produces the extend method.
fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;

    // Helper function for errors with span.
    fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
        Some((
            false,
            syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
        ))
    }

    let meta = match g.parse_meta() {
        Ok(syn::Meta::List(mut nvs)) => {
            // list here is '.. in #[builder(..)]'.
            assert_eq!(nvs.path.segments[0].ident, "builder"); //looking at buidler attribute
            if nvs.nested.len() != 1 {
                return mk_err(nvs);
            }

            // nvs.nested[0] here is (hopefully): each = "foo"
            match nvs.nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    if nv.path.segments[0].ident != "each" {
                        return mk_err(nvs);
                    }
                    nv
                }
                meta => {
                    // nvs.nested[0] was not k = v
                    return mk_err(meta);
                }
            }
        }
        Ok(meta) => {
            // Inside of #[] there was either just an identifier (`#[builder]`) or a key-value
            // pair (`#[builder = "foo"]`), neither of which are valid.
            return mk_err(meta);
        }
        Err(e) => {
            return Some((false, e.to_compile_error()));
        }
    };

    // At this point we have a meta, which contains the name of the each for the builder method.
    match meta.lit {
        syn::Lit::Str(s) => {
            // Ex: #[builder(each = "thing")], arg would be thing.
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = get_inner_ty(&f.ty, "Vec").unwrap();
            let method = quote! {
                pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                    self.#name.push(#arg);
                    self
                }
            };
            Some((&arg == name, method))
        }
        lit => panic!("expected string, found {:?}", lit),
    }
}
