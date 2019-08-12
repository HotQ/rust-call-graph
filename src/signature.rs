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

fn path_segment_to_string(pathseg: &PathSegment) -> String {
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
    // ident
}

pub fn path_to_string(path: &Path) -> String {
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
