use syn::*;

pub fn type_to_string(ty: &Type) -> String {
    "".to_string();
    use Type::*;
    match ty {
        Slice(slice) => "[".to_string() + &type_to_string(&*slice.elem) + "]",
        Reference(reference) => {
            let mut ret = "&".to_string();
            if let Some(lifetime) = reference.lifetime.clone() {
                ret = ret + "'" + &lifetime.ident.to_string() + " ";
            }
            if reference.mutability != None {
                ret += "mut ";
            }
            ret + &type_to_string(&*reference.elem)
        }
        Tuple(tuple) => {
            let tys: Vec<String> = tuple.elems.iter().map(|ty| type_to_string(ty)).collect();
            "(".to_string() + &tys.join(", ") + ")"
        }
        Path(TypePath { qself, path }) => {
            NeverConsidered!(
                type_to_string,
                Type::Path,
                [qself, |x: &Option<QSelf>| *x == None]
            );
            path_to_string(path)
        }
        _ => {
            error!("type_to_string NeverConsidered! {:?}", ty);
            "_type".to_string()
        }
    }
}

pub fn path_to_string(path: &Path) -> String {
    fn path_segment_to_string(pathseg: &PathSegment) -> String {
        fn generic_argument_to_string(gen: &GenericArgument) -> String {
            use GenericArgument::*;
            match gen {
                Lifetime(lifetime) => "'".to_string() + &lifetime.ident.to_string(),
                Type(ty) => type_to_string(ty),
                Binding(binding) => binding.ident.to_string() + "=" + &type_to_string(&binding.ty),
                Constraint(constraint) => "Constraint(constraint)NeverConsidered!".to_string(),
                Const(expr) => "Const(Expr)NeverConsidered!".to_string(),
            }
        }

        let mut ident = pathseg.ident.to_string();
        match &pathseg.arguments {
            PathArguments::None => ident,
            PathArguments::AngleBracketed(anglebracketed) => {
                ident += if anglebracketed.colon2_token == None {
                    ""
                } else {
                    "::"
                };
                let args: Vec<String> = anglebracketed
                    .args
                    .iter()
                    .map(|x| generic_argument_to_string(x))
                    .collect();
                ident + "<" + &args.join(",") + ">"
            }
            PathArguments::Parenthesized(parenthesized) => {
                "PathArguments::ParenthesizedNeverConsidered!".to_string()
            }
        }
    }

    let i: Vec<String> = path
        .segments
        .iter()
        .map(|x| path_segment_to_string(x))
        .collect();

    i.join("::")
}

pub fn fn_arg_to_string(fn_arg: &FnArg) -> String {
    // info!("fn_arg in &decl.inputs  {:?}", fn_arg);
    use FnArg::*;
    match fn_arg {
        SelfRef(arg_self_ref) => "SelfRefNeverConsidered!".to_string(),
        SelfValue(arg_self) => "SelfValueNeverConsidered!".to_string(),
        Captured(arg_captured) => {
            // error!("Captured ty {:?}", type_to_string(&arg_captured.ty));
            let ty = type_to_string(&arg_captured.ty);
            match &arg_captured.pat {
                Pat::Wild(_) => "_".to_string() + &ty,
                Pat::Ident(ident) => {
                    let mut ret = "".to_string();
                    if ident.by_ref != None {
                        ret += "ref "
                    }
                    if ident.mutability != None {
                        ret += "mut "
                    }
                    ret + &ty
                }
                _ => {
                    error!("Captured pat {:?}", arg_captured.pat);
                    "CapturedNeverConsidered!".to_string()
                }
            }
        }
        Inferred(pat) => "InferredNeverConsidered!".to_string(),
        Ignored(_) => "IgnoredNeverConsidered!".to_string(),
    }
}

pub fn generic_to_string(generic: &Generics) -> String {
    fn generic_param_to_string(param: &GenericParam) -> String {
        fn type_param_bound_to_string(param_bound: &TypeParamBound) -> String {
            fn trait_bound_to_string(trait_bound: &TraitBound) -> String {
                // fn bound_lifetimes_to_string(bound_lifetimes:&BoundLifetimes)->String{
                // }
                let modifier = if trait_bound.modifier == TraitBoundModifier::None {
                    ""
                } else {
                    "?"
                };
                let mut ret = modifier.to_string();
                if let Some(_) = trait_bound.lifetimes.clone() {
                    ret += "BoundLifetimesNeverConsidered!"; // BoundLifetimes to string
                }

                ret + &path_to_string(&trait_bound.path)
            }
            use TypeParamBound::*;
            match param_bound {
                Trait(trait_bound) => trait_bound_to_string(trait_bound),
                Lifetime(lifetime) => "'".to_string() + &lifetime.ident.to_string(),
            }
        }

        use GenericParam::*;
        match param {
            Type(type_param) => {
                NeverConsidered!(
                    generic_param_to_string,
                    Type,
                    [type_param.attrs, |x: &Vec<Attribute>| x.len() == 0]
                );
                trace!(
                    "generic_param_to_string, type  id {:?}",
                    type_param.ident.to_string()
                );

                let mut ret = type_param.ident.to_string();
                if type_param.colon_token != None {
                    ret += ":";

                    let bounds: Vec<String> = type_param
                        .bounds
                        .iter()
                        .map(|x| type_param_bound_to_string(x))
                        .collect();
                    ret += &bounds.join("+");
                }

                ret
            }
            _ => {
                error!("GenericParam {:?}", param);
                "param".to_string()
            }
        }
    }

    if generic.params.len() == 0 {
        "".to_string()
    } else {
        let params: Vec<String> = generic
            .params
            .iter()
            .map(|param| generic_param_to_string(param))
            .collect();
        "<".to_string() + &params.join(", ") + ">"
    }
}
