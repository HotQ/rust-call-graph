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
macro_rules! NeverConsidered{
    ($($str:path),* , $([ $in1:expr,$in2:expr ]),*) => {
        {
            let info = brk!("Never considered!  ",$($str),*).to_string();
            $(
                if !$in2(& $in1){
                    error!("{} {}:{} | {} => {:#?}",info, file!(), line!(), stringify!($in1), $in1);
                }
            )*
        }
    };
}

#[macro_export]
macro_rules! brk{ // i already forget why i name this macro like this.....
    ($type:expr, $($str:expr),*) => {
        concat!($type,$(concat!("[",stringify!($str)),"]"),*)
    };
}

#[macro_export]
macro_rules! debug_all {
    ($info:expr,$($arg:expr),+) => {
        trace!( concat!(stringify!($info), concat!($(concat!("\n\t\t\t\t\t",stringify!($arg) ," {:?}"),)+ )), $($arg),+)
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
