pub fn type_name<T>(_arg: &T) -> String {
    unsafe { String::from(std::intrinsics::type_name::<T>()) }
}

#[macro_export]
macro_rules! type_names{
    ($($var:expr),*)=>{
        {
            let mut s =String::from("");
            let mut is_1st = true;
            $(
                if is_1st == false {s.push('\n');}
                else{is_1st = false;}
                s += &type_name(&$var);
            )*
            s
        }
    }
}
