#[derive(Default)]
pub enum Scope {
    #[default]
    Global,
    Local(String),
}
