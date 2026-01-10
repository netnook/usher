use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, GenericArgument, Ident, PathArguments, Type, spanned::Spanned};

#[proc_macro_derive(UsherArgs, attributes(usher))]
pub fn derive_usher_args(input: TokenStream) -> TokenStream {
    match do_derive_usher_args(input.into()) {
        Ok(r) => r.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(usher))]
struct Attributes {
    #[deluxe(default = false)]
    internal: bool,
}

struct Arg {
    name: Ident,
    item_type: ArgType,
    option: bool,
    arg_idx: bool,
    nillable: bool,
    vec: bool,
    positional: bool,
}

fn do_derive_usher_args(
    item: proc_macro2::TokenStream,
) -> deluxe::Result<proc_macro2::TokenStream> {
    let mut input = syn::parse2::<syn::DeriveInput>(item)?;

    let Attributes { internal } = deluxe::extract_attributes(&mut input)?;

    let extern_stmt = if internal {
        quote! { extern crate self as __private_usher; }
    } else {
        quote! { extern crate usher as __private_usher; }
    };

    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let syn::Data::Struct(mut s) = input.data else {
        panic!("[UsherArgs] Only struct supported by UsherArgs");
    };

    let mut args = Vec::new();
    let mut found_star_args = false;
    for field in s.fields.iter_mut() {
        let mut arg = build_arg(field)?;
        if arg.vec {
            if found_star_args {
                return Err(Error::new(
                    field.span(),
                    "[UsherArgs] At most one star-args argument (Vec) may be defined.",
                ));
            }
            found_star_args = true;
        }
        arg.positional = !found_star_args;
        args.push(arg);
    }

    let field_init = args.iter().map(|arg| {
        let name = &arg.name;
        if arg.vec {
            quote! { let mut #name = ::std::vec::Vec::new(); }
        } else {
            quote! { let mut #name = ::std::option::Option::None; }
        }
    });

    fn converter(arg: &Arg) -> proc_macro2::TokenStream {
        let name_str = arg.name.to_string();

        let convert_type = match arg.item_type {
            ArgType::String => {
                quote! { let val = __private_usher::lang::ArgUtils::to_string(val, arg.span(), #name_str)?; }
            }
            ArgType::Integer => {
                quote! { let val = __private_usher::lang::ArgUtils::to_int(val, arg.span(), #name_str)?; }
            }
            ArgType::Float => {
                quote! { let val = __private_usher::lang::ArgUtils::to_float(val, arg.span(), #name_str)?; }
            }
            ArgType::Bool => {
                quote! { let val = __private_usher::lang::ArgUtils::to_bool(val, arg.span(), #name_str)?; }
            }
            ArgType::List => {
                quote! { let val = __private_usher::lang::ArgUtils::to_list(val, arg.span(), #name_str)?; }
            }
            ArgType::Dict => {
                quote! { let val = __private_usher::lang::ArgUtils::to_dict(val, arg.span(), #name_str)?; }
            }
            ArgType::Value => {
                quote! {}
            }
        };

        let convert_nillable = if arg.nillable {
            quote! {
                let val = if let __private_usher::lang::Value::Nil = val {
                    __private_usher::lang::Nillable::Nil
                } else {
                    #convert_type
                    __private_usher::lang::Nillable::Some(val)
                };
            }
        } else {
            convert_type
        };

        if arg.arg_idx {
            quote! {
                #convert_nillable
                let val = (idx, val);
            }
        } else {
            convert_nillable
        }
    }

    fn assigner(arg: &Arg) -> proc_macro2::TokenStream {
        let name = &arg.name;
        if arg.vec {
            quote! { #name.push(val); }
        } else {
            quote! { #name = ::std::option::Option::Some(val); }
        }
    }

    let field_pos = args
        .iter()
        .filter(|arg| arg.positional || arg.vec)
        .enumerate()
        .map(|(idx, arg)| {
            let convert = converter(arg);
            let assign = assigner(arg);

            if arg.vec {
                quote! {
                    let val = __private_usher::lang::Eval::eval(arg, ctxt)?;
                    #convert
                    #assign
                    continue;
                }
            } else {
                quote! {
                    if idx == #idx {
                        let val = __private_usher::lang::Eval::eval(arg, ctxt)?;
                        #convert
                        #assign
                        continue;
                    }
                }
            }
        });

    let field_named = args.iter().filter(|arg| !arg.vec).map(|arg| {
        let name = &arg.name;
        let name_str = name.to_string();
        let convert = converter(arg);
        let assign = assigner(arg);
        quote! {
            if arg.name.key.as_str() == #name_str {
                if #name.is_some() {
                    return Err(__private_usher::lang::ArgUtils::param_already_set_error(#name_str, arg.span()));
                }
                let val = __private_usher::lang::Eval::eval(arg, ctxt)?;
                #convert
                #assign
                continue;
            }
        }
    });

    let check_required = args.iter().map(|arg| {
        let name = &arg.name;
        let name_str = name.to_string();
        if !arg.option && !arg.vec {
            Some(quote! {
                let ::std::option::Option::Some(#name) = #name else {
                    return Err(__private_usher::lang::ArgUtils::missing_argument_error(#name_str, call.span));
                };
            })
        } else {
            None
        }
    });

    let struct_fields = args.iter().map(|arg| {
        let name = &arg.name;
        quote! { #name, }
    });

    let star_args_exists = args.iter().any(|arg| arg.vec);
    let unexpected_argument = if !star_args_exists {
        Some(quote! {
            return ::std::result::Result::Err(
                __private_usher::lang::InternalProgramError::FunctionCallUnexpectedArgument {
                    span: arg.span(),
                }.into_stop(),
            );
        })
    } else {
        None
    };

    Ok(quote::quote! {
        #extern_stmt
        impl #impl_generics #ident #type_generics #where_clause {
            fn eval(call: &__private_usher::lang::FunctionCall, ctxt: &mut __private_usher::lang::Context) -> ::std::result::Result<Self,EvalStop> {
                #(#field_init)*

                let mut mode_positional = true;
                for (idx, outer_arg) in call.args.iter().enumerate() {
                    if mode_positional {
                        match outer_arg {
                            __private_usher::lang::Arg::Positional(arg) => {
                                #(#field_pos)*
                                #unexpected_argument
                            }
                            __private_usher::lang::Arg::Named(_) => {
                                mode_positional = false;
                            }
                        };
                    }
                    if !mode_positional {
                        match outer_arg {
                            __private_usher::lang::Arg::Positional(arg) => {
                                return ::std::result::Result::Err(
                                    __private_usher::lang::InternalProgramError::FunctionCallPositionalArgAfterNamedArg {
                                        span: arg.span(),
                                    }.into(),
                                );
                            }
                            __private_usher::lang::Arg::Named(arg) => {
                                #(#field_named)*
                                return ::std::result::Result::Err(
                                    __private_usher::lang::InternalProgramError::FunctionCallNoSuchParameter {
                                        name: arg.name.as_string(),
                                        span: arg.name.span,
                                    }.into_stop(),
                                );
                            }
                        }
                    }
                }


                #(#check_required)*

                ::std::result::Result::Ok(Self {
                    #(#struct_fields)*
                })
            }
        }
    })
}

#[derive(Clone, PartialEq)]
enum ArgType {
    String,
    Integer,
    Float,
    Bool,
    List,
    Dict,
    Value,
}

fn type_error(ty: &Type) -> Error {
    Error::new(ty.span(), "[UsherArgs] Could not determine field type.")
}

fn build_arg(field: &syn::Field) -> Result<Arg, Error> {
    let field_name = field.ident.as_ref().unwrap().clone();
    let mut arg = Arg {
        name: field_name,
        item_type: ArgType::Value,
        option: false,
        arg_idx: false,
        nillable: false,
        vec: false,
        positional: false,
    };
    field_type_top(&mut arg, &field.ty)?;
    Ok(arg)
}

fn field_type_top(arg: &mut Arg, ty: &Type) -> Result<(), Error> {
    if let Some((container_type_name, contained_type)) = simple_generic_container(ty) {
        match container_type_name.as_str() {
            "Vec" => {
                arg.vec = true;
                return field_type_arg(arg, contained_type);
            }
            "Option" => {
                arg.option = true;
                return field_type_arg(arg, contained_type);
            }
            _ => {}
        }
    };
    field_type_arg(arg, ty)
}

fn field_type_arg(arg: &mut Arg, ty: &Type) -> Result<(), Error> {
    if let Some(contained_type) = arg_idx_container(ty) {
        arg.arg_idx = true;
        return field_type_nillable(arg, contained_type);
    };
    field_type_nillable(arg, ty)
}

fn field_type_nillable(arg: &mut Arg, ty: &Type) -> Result<(), Error> {
    if let Some((container_type_name, contained_type)) = simple_generic_container(ty)
        && container_type_name.as_str() == "Nillable"
    {
        arg.nillable = true;
        return field_type_item(arg, contained_type);
    };
    field_type_item(arg, ty)
}

fn field_type_item(arg: &mut Arg, ty: &Type) -> Result<(), Error> {
    let Type::Path(path_type) = ty else {
        return Err(type_error(ty));
    };
    let Some(seg) = path_type.path.segments.last() else {
        return Err(type_error(ty));
    };

    match &seg.arguments {
        PathArguments::None => {
            let Some(ty) = ident_type(&seg.ident) else {
                return Err(type_error(ty));
            };
            arg.item_type = ty;
        }
        _ => return Err(type_error(ty)),
    };
    Ok(())
}

fn simple_generic_container(ty: &Type) -> Option<(String, &Type)> {
    let Type::Path(path_type) = ty else {
        return None;
    };
    let seg = path_type.path.segments.last()?;

    match &seg.arguments {
        PathArguments::None => None,
        PathArguments::AngleBracketed(generic_arguments) => {
            if generic_arguments.args.len() != 1 {
                return None;
            }
            let GenericArgument::Type(gty) = generic_arguments.args.last().unwrap() else {
                return None;
            };
            let wstr = seg.ident.to_string();
            Some((wstr, gty))
        }
        _ => None,
    }
}

fn arg_idx_container(ty: &Type) -> Option<&Type> {
    let Type::Tuple(tuple) = ty else {
        return None;
    };
    if tuple.elems.len() != 2 {
        return None;
    }
    if !is_ident_type(&tuple.elems[0], "usize") {
        return None;
    };

    Some(&tuple.elems[1])
}

fn is_ident_type(ty: &Type, name: &str) -> bool {
    if let Type::Path(syn::TypePath { path, .. }) = ty {
        path.segments.last().is_some_and(|seg| seg.ident == name)
    } else {
        false
    }
}

fn ident_type(ident: &Ident) -> Option<ArgType> {
    if ident == "StringCell" {
        Some(ArgType::String)
    } else if ident == "isize" {
        Some(ArgType::Integer)
    } else if ident == "f64" {
        Some(ArgType::Float)
    } else if ident == "bool" {
        Some(ArgType::Bool)
    } else if ident == "List" {
        Some(ArgType::List)
    } else if ident == "Dict" {
        Some(ArgType::Dict)
    } else if ident == "Value" {
        Some(ArgType::Value)
    } else {
        None
    }
}
