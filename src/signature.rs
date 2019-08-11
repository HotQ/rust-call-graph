use crate::utils::*;
use syn::*;

pub fn type_to_string(ty: &Type) -> String {
    "".to_string();
    match ty {
        Type::Path(TypePath { qself, path }) => {
            NeverConsidered!(
                type_to_string,
                Type::Path,
                [qself, |x: &Option<QSelf>| *x == None]
            );
            // utils::type_names!();
            trace!("segments : {:?}",path.segments.iter().next());
            "Type::Path".to_string()
        }
        _ => "_type".to_string(),
    }
}
