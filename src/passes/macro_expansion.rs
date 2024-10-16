use super::Pass;
use crate::{
    macros::{symbol_to_macros, Macro, MacroFn, Slice},
    parser::Stmt,
    scope::Scope,
};
use libloading::Library;
use std::ffi::OsStr;

pub struct MacroExpansion {
    libs: Vec<Library>,
}

impl Pass for MacroExpansion {
    type State = Vec<String>;
    type Output = ();

    fn new(paths: Self::State) -> Self {
        Self {
            libs: paths
                .into_iter()
                .map(|path| unsafe { libloading::Library::new(OsStr::new(&path)).unwrap() })
                .collect(),
        }
    }

    fn run_pass(self, stmts: &mut Vec<Stmt>, scope: &mut Scope) -> Self::Output {
        ()
    }
}

impl MacroExpansion {
    const MACROS_SYMBOL_NAME: &'static str = "macros\0";

    fn get_macro(&self, name: &str) -> Option<&MacroFn> {
        for lib in &self.libs {
            let symbol = unsafe {
                lib.get::<*const Slice<Macro>>(Self::MACROS_SYMBOL_NAME.as_bytes())
                    .unwrap()
            };

            if let Some(mr) = symbol_to_macros(symbol).into_iter().find_map(|mr| {
                if mr.name() == name {
                    Some(mr)
                } else {
                    None
                }
            }) {
                return Some(mr.func());
            }
        }

        None
    }
}
