use std::ffi::{c_char, CStr};

use libloading::Symbol;

#[repr(C)]
pub enum Literal {
    String(*const i8),
    Int(i64),
    UInt(u64),
}

#[repr(C)]
pub enum Token {
    Literal(Literal),
}

#[repr(C)]
#[derive(Debug)]
pub struct Slice<T> {
    pub ptr: *const T,
    pub len: usize,
}

pub type MacroFn = extern "C" fn(Slice<Token>) -> Slice<Token>;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Macro {
    name: *const c_char,
    func: MacroFn,
}

impl Macro {
    pub fn name(&self) -> &str {
        unsafe {
            CStr::from_ptr(self.name)
                .to_str()
                .expect("Failed to convert CStr to str")
        }
    }

    pub fn func(&self) -> &MacroFn {
        &self.func
    }
}

pub fn symbol_to_macros<'lib>(symbol: Symbol<'lib, *const Slice<Macro>>) -> &'lib [Macro] {
    unsafe {
        let slice = symbol.read();
        std::slice::from_raw_parts(slice.ptr, slice.len)
    }
}
