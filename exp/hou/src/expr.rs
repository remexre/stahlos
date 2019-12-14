use std::rc::Rc;

#[derive(Debug)]
pub enum Expr {
    App(Rc<Expr>, Rc<Expr>),
    Arr(Rc<Expr>, Rc<Expr>),
    Con(Rc<str>),
    Hle,
    Lam(Rc<Expr>),
    Set,
    Var(usize),
}
