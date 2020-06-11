///! My implementation of the `builder` example
///!
///! I felt that as the successive requirements were addressed, there
///! developed an increased opportunity for refactoring the code which
///! I didn't do fully.

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse_macro_input;
use syn::DeriveInput;
use syn::{
    visit::Visit, Attribute, Data, Error, Field, GenericArgument, MetaNameValue, PathArguments,
    Type, Meta,
};

type GeneratedTokenStream = proc_macro2::TokenStream;

// TODO : maybe create a struct with all generated vecs to avoid re-iteration for every function

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    // dbg!(&input);

    // validate the inert attributes using a syn visitor
    let mut visitor = InertAttributeValidator {
        invalid_attr: None,
        found_invalid: false,
    };
    visitor.visit_data(&input.data);
    if visitor.found_invalid {
        return Error::new_spanned(visitor.invalid_attr, "expected `builder(each = \"...\")`")
            .to_compile_error()
            .into();
    }

    let name = input.ident;
    let builder_name = format_ident!("{}Builder", &name);
    let builder_expr_initial = gen_from_fields(&input.data, gen_builder_field_init);
    let builder_field_decls = gen_from_fields(&input.data, gen_builder_field);
    let builder_fns = gen_from_fields(&input.data, gen_setter_fn);
    let validate_fields = gen_from_fields(&input.data, gen_validation);
    let struct_expr_fields = gen_from_fields(&input.data, gen_struct_expr_field);

    (quote! {
    pub struct #builder_name {
        // Each field will be an Option<T> where T is the field type
        // TODO : I have "*," by mistake and `cargo expand` showed the same output, but got errors when compiling
        #(#builder_field_decls),*
    }

    impl #builder_name {
        // Each function will set the member to Some(val) in the builder
        #(#builder_fns)*

        // TODO : let this consume self
        pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
            // Check that all fields are set
            #(#validate_fields)*
            // Instantiate the struct
            std::result::Result::Ok(#name {
                #(#struct_expr_fields),*
            })
        }
    }

    // Add builder() method to inherent impl of the struct containing the derive attr
    impl #name {
        fn builder() -> #builder_name {
            #builder_name {
                #(#builder_expr_initial),*
            }
        }
    }
    })
    .into()
}

struct InertAttributeValidator {
    /// Current attribute we're visiting
    invalid_attr: Option<Box<dyn quote::ToTokens>>,
    /// Whether we found an invalid key
    found_invalid: bool,
}

impl<'ast> Visit<'ast> for InertAttributeValidator {
    fn visit_meta_name_value(&mut self, i: &'ast MetaNameValue) {
        if !i.path.is_ident("each") {
            self.found_invalid = true;
        }
    }

    // need to implement this to get the exact span the error message expects.
    // otherwise, visit_attribute() or visit_meta_name_value() spans would be fine imo
    fn visit_meta(&mut self, i: &'ast Meta) {
        syn::visit::visit_meta(self, i);
        if self.found_invalid {
            self.invalid_attr = Some(Box::new(i.clone()));
        }
    }

    // we have to parse the attribute as a Meta
    fn visit_attribute(&mut self, i: &'ast Attribute) {
        if let Ok(meta) = i.parse_meta() {
            self.visit_meta(&meta);
        }
    }
}

/// Generate a vector of token streams by executing the `handle_field` function
/// over head field in the input data.
// (returning proc_macro2's token stream here because otherwise using
//  it above complains that proc_macro's token stream doesn't implement
//  ToTokens)
fn gen_from_fields<F>(input_data: &syn::Data, handle_field: F) -> Vec<GeneratedTokenStream>
where
    F: Fn(&Field) -> GeneratedTokenStream,
{
    match input_data {
        Data::Struct(data) => data.fields.iter().map(handle_field).collect(),
        _ => vec![],
    }
}

/// Generate a field in the builder
fn gen_builder_field(f: &Field) -> GeneratedTokenStream {
    let field_name = f.ident.as_ref().unwrap();
    let field_type = &f.ty;
    if f.is_option() ||
    // "each" is already a vec in the struct
    f.each_name().is_some()
    {
        quote! { #field_name: #field_type }
    } else {
        quote! { #field_name: std::option::Option<#field_type> }
    }
}

/// Generate an assignment for builder field initial values
fn gen_builder_field_init(f: &Field) -> GeneratedTokenStream {
    let field_name = f.ident.as_ref().unwrap();
    if f.each_name().is_some() {
        quote! { #field_name: vec![] }
    } else {
        quote! { #field_name: std::option::Option::None }
    }
}

/// Generate a setter function for a struct field
fn gen_setter_fn(f: &Field) -> GeneratedTokenStream {
    let field_name = f.ident.as_ref().unwrap().clone();
    let mut field_type = &f.ty;
    if let Some(each_name) = f.each_name() {
        // push a single entry to the vec for "each"
        let method_name = format_ident!("{}", each_name);
        field_type = get_type_with_single_argument(&f.ty).unwrap().1;
        quote! {
            fn #method_name(&mut self, #method_name: #field_type) -> &mut Self {
                self.#field_name.push(#method_name);
                self
            }
        }
    } else {
        if f.is_option() {
            // we accept a "raw" value and wrap it
            field_type = f.type_argument().unwrap();
        }
        quote! {
            fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                self.#field_name = std::option::Option::Some(#field_name);
                self
            }
        }
    }
}

/// Generate a validation call that a field was set
fn gen_validation(f: &Field) -> GeneratedTokenStream {
    let field_name = f.ident.as_ref().unwrap();
    let message = format!("Field `{}` not provided to builder", field_name);
    if f.is_option() || f.each_name().is_some() {
        // no validation for optional fields
        quote! {}
    } else {
        // TODO : any other way we should be doing this?
        quote! { self.#field_name.as_ref().ok_or(#message)?; }
    }
}

/// Generate a field "assignment" in the struct expression. Using "self." to
/// reference the fields in builder.
fn gen_struct_expr_field(f: &Field) -> GeneratedTokenStream {
    let field_name = f.ident.as_ref().unwrap();
    if f.is_option() || f.each_name().is_some() {
        quote! { #field_name: self.#field_name.clone() }
    } else {
        // TODO : this clone() feels wrong. What else should we be doing here?
        quote! { #field_name: self.#field_name.as_ref().unwrap().clone() }
    }
}

// TODO : doc me
fn get_type_with_single_argument(ty: &Type) -> Option<(String, &Type)> {
    if let Type::Path(path) = ty {
        if let Some(first_segment) = path.path.segments.first() {
            if let PathArguments::AngleBracketed(type_args) = &first_segment.arguments {
                if let Some(GenericArgument::Type(ty)) = type_args.args.first() {
                    return Some((first_segment.ident.to_string(), ty));
                }
            }
        }
    }
    None
}

// TODO : this could be a struct and store these values
trait FieldExt {
    fn is_option(&self) -> bool;

    /// Get the type argument of the Option if, in fact, it is an Option
    fn type_argument(&self) -> Option<&Type>;

    fn each_name(&self) -> Option<String>;
}

impl FieldExt for Field {
    fn is_option(&self) -> bool {
        self.type_argument().is_some()
    }

    fn type_argument(&self) -> Option<&Type> {
        get_type_with_single_argument(&self.ty)
            .filter(|pair| pair.0 == "Option")
            .map(|pair| pair.1)
    }

    fn each_name(&self) -> Option<String> {
        self.attrs.iter().find_map(|attr| {
            //dbg!(attr.parse_meta());
            // TODO : is it possible to use ? here and return None from a Result?
            if let Ok(syn::Meta::List(meta_list)) = attr.parse_meta() {
                if meta_list.path.is_ident("builder") {
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) =
                        meta_list.nested.first()?
                    {
                        // dbg!(name_value);
                        // if !name_value.path.is_ident("each") {
                        //     // compile_error!("Expected abc");
                        // }
                        if let syn::Lit::Str(ref lit_str) = name_value.lit {
                            return Some(lit_str.value());
                        }
                    }
                }
            }
            None
        })
    }
}
