use proc_macro2::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::DeriveInput;
use syn::{parse_macro_input, Data, Field, Fields};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let builder_name = &Ident::new(&(struct_name.to_string() + "Builder"), Span::call_site());

    let struct_fields = match input.data {
        Data::Struct(ref data_struct) => match data_struct.fields {
            Fields::Named(ref fields) => &fields.named,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let builder = builder(builder_name, struct_fields);
    let builder_constructor = builder_constructor(struct_name, builder_name, struct_fields);
    let builder_setters = builder_setters(struct_fields);
    let builder_build = builder_build(struct_name, struct_fields);

    let builder_impl: TokenStream = quote! {
        impl #builder_name {
            #builder_setters
            #builder_build
        }
    };

    let out = quote! {
        #builder
        #builder_constructor
        #builder_impl
    };
    out.into()
}

fn is_type(field: &Field, ident: String) -> bool {
    match field.ty {
        syn::Type::Path(ref type_path) => type_path.path.segments.first().unwrap().ident == ident,
        _ => unimplemented!(),
    }
}

fn is_optional(field: &Field) -> bool {
    is_type(field, "Option".to_string())
}

fn is_vector(field: &Field) -> bool {
    is_type(field, "Vec".to_string())
}

fn first_generic_type_argument(field: &Field) -> &syn::Type {
    match field.ty {
        syn::Type::Path(ref type_path) => {
            match type_path.path.segments.first().unwrap().arguments {
                syn::PathArguments::AngleBracketed(ref generic_arguments) => {
                    match generic_arguments.args.first().unwrap() {
                        syn::GenericArgument::Type(ref ty) => ty,
                        _ => unimplemented!(),
                    }
                }
                _ => unimplemented!(),
            }
        }
        _ => unimplemented!(),
    }
}

// std::option::Option
fn builder_build(struct_name: &Ident, struct_fields: &Punctuated<Field, Comma>) -> TokenStream {
    let checks = struct_fields
        .iter()
        .filter(|it| !is_optional(it))
        .map(|field| {
            let name = &field.ident;
            quote! {
                if self.#name.is_none() {
                    return std::result::Result::Err(std::boxed::Box::new(std::fmt::Error))
                }
            }
        });
    let constructor_params = struct_fields.iter().map(|field| {
        let name = &field.ident;
        let value = if is_optional(field) {
            quote!(if self.#name.is_none() { None } else { self.#name.take().unwrap() })
        } else {
            quote!(self.#name.take().unwrap())
        };
        quote! {
            #name: #value,
        }
    });
    quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
            #(#checks)*
            std::result::Result::Ok(#struct_name {
                #(#constructor_params)*
            })
        }
    }
}

fn builder_setters(struct_fields: &Punctuated<Field, Comma>) -> TokenStream {
    let setters = struct_fields.iter().map(|field| {
        let setter_name = field.ident.as_ref().unwrap();
        let ty;
        let value;
        if is_optional(field) {
            ty = first_generic_type_argument(field);
            value = quote!(std::option::Option::Some(std::option::Option::Some(#setter_name)));
        } else {
            ty = &field.ty;
            value = quote!(std::option::Option::Some(#setter_name));
        };
        let mut vec_setter = quote!();
        let mut names_collision = false;
        if is_vector(field) {
            let attrs = &field.attrs;
            if attrs.len() > 1 {
                unimplemented!()
            }
            if !attrs.is_empty() {
                let tmp = extract_vec_setter_name(attrs.first().unwrap());
                let vec_setter_name = match tmp {
                    Err(e) => return e.to_compile_error(),
                    Ok(name) => Ident::new(&name, Span::call_site()),
                };
                names_collision = *setter_name == vec_setter_name;
                let builder_field_name = &field.ident;
                let ty = first_generic_type_argument(field);
                vec_setter = quote! {
                    fn #vec_setter_name(&mut self, #vec_setter_name: #ty) -> &mut Self {
                        self.#builder_field_name.as_mut().unwrap().push(#vec_setter_name);
                        self
                    }
                };
            }
        }
        if !names_collision {
            quote! {
                #vec_setter
                fn #setter_name(&mut self, #setter_name: #ty) -> &mut Self {
                    self.#setter_name = #value;
                    self
                }
            }
        } else {
            vec_setter
        }
    });
    quote!(#(#setters)*)
}

fn extract_vec_setter_name(attr: &syn::Attribute) -> syn::Result<String> {
    match attr.parse_meta().unwrap() {
        syn::Meta::List(ref meta_list) => match meta_list.nested.first().unwrap() {
            syn::NestedMeta::Meta(syn::Meta::NameValue(ref name_value)) => {
                let name = name_value.path.segments.first().unwrap().ident.to_string();
                if name != "each" {
                    return Err(syn::Error::new(
                        name_value.span(),
                        "expected `builder(each = \"...\")`",
                    ));
                }
                let value = match name_value.lit {
                    syn::Lit::Str(ref str) => str.value(),
                    _ => unimplemented!(),
                };
                Ok(value)
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn builder_constructor(
    struct_name: &Ident,
    builder_name: &Ident,
    struct_fields: &Punctuated<Field, Comma>,
) -> TokenStream {
    let builder_constructor_fields = struct_fields.iter().map(|field| {
        let name = &field.ident;
        let value = if is_vector(field) {
            quote!(std::option::Option::Some(std::vec::Vec::new()))
        } else {
            quote!(std::option::Option::None)
        };
        quote! {
            #name: #value
        }
    });

    quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_constructor_fields),*
                }
            }
        }
    }
}

fn builder(builder_name: &Ident, struct_fields: &Punctuated<Field, Comma>) -> TokenStream {
    let builder_fields = struct_fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        quote! {
            #name: std::option::Option<#ty>
        }
    });

    quote! {
        pub struct #builder_name {
            #(#builder_fields),*
        }
    }
}
