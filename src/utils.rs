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

#[macro_export]
macro_rules! is_exist{
    ( $src:expr, $( $x:expr ),+ ) => {
        {
            let mut ret = false;
            $(if $src==$x {ret = true;})*
            ret
        }
    };
}

#[macro_export]
macro_rules! html_escape {
    ( $src:expr) => {
        $src.replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
    };
}
