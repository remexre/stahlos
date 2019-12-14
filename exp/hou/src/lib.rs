pub mod expr;
pub mod metavar;

use crate::metavar::MetaVar;
use anyhow::Result;
use derive_more::From;
use std::rc::Rc;

#[derive(Debug, From)]
pub enum Type {
    #[from(ignore)]
    Set,
    Term(Rc<Term>),
    #[from(ignore)]
    Arr(Rc<Type>, Rc<Type>),
}

#[derive(Debug, From)]
pub enum Term {
    #[from(ignore)]
    Var(usize),
    Con(Rc<str>),
    #[from(ignore)]
    App(Rc<Term>, Rc<Term>),
    #[from(ignore)]
    Lam(Rc<Term>),
}

#[derive(Debug, From)]
pub enum Ctrt {
    TyConv(Rc<Ctx>, Rc<Type>, Rc<Type>),
    TmConv(Rc<Ctx>, Rc<Type>, Rc<Term>, Rc<Term>),
    CtxConv(Rc<Ctx>, Vec<Rc<Term>>, Vec<Rc<Term>>, Rc<Ctx>),
}

#[derive(Debug, From)]
pub struct Ctx(pub Vec<Type>);

#[derive(Debug)]
pub enum Sig1 {
    Dec(Rc<str>, Rc<Type>),
    Def(Rc<str>, Rc<Type>, Rc<Term>),
    Grd(Rc<str>, Rc<Type>, Rc<Term>, Rc<Ctrt>),
}

#[derive(Debug, From)]
pub struct Sig(pub Vec<Sig1>);

impl Sig {
    pub fn lookup(&self, con: &str) -> Result<Rc<Type>> {
        unimplemented!()
    }

    pub fn add_meta(&mut self, ty: Rc<Type>) -> Result<MetaVar> {
        unimplemented!()
    }

    pub fn set(&mut self, var: MetaVar, tm: Rc<Term>) -> Result<()> {
        unimplemented!()
    }

    pub fn add_const(
        &mut self,
        name: Rc<str>,
        ty: Rc<Type>,
        tm: Rc<Term>,
        ctrt: Rc<Ctrt>,
    ) -> Result<()> {
        unimplemented!()
    }

    // TODO: Fix arg type
    pub fn in_scope(&self, set: ()) -> Result<()> {
        unimplemented!()
    }
}

pub fn sig_valid(sig: &Sig) -> Result<()> {
    unimplemented!()
}

pub fn ctx_valid(sig: &Sig, ctx: &Ctx) -> Result<()> {
    unimplemented!()
}

pub fn ty_valid(sig: &Sig, ctx: &Ctx, ty: &Type) -> Result<()> {
    unimplemented!()
}

pub fn tm_has_ty(sig: &Sig, ctx: &Ctx, ty: &Type, tm: &Term) -> Result<()> {
    unimplemented!()
}

pub fn ty_convertible(sig: &Sig, ctx: &Ctx, a: &Type, b: &Type) -> Result<()> {
    unimplemented!()
}

pub fn tm_convertible(sig: &Sig, ctx: &Ctx, ty: &Type, s: &Term, t: &Term) -> Result<()> {
    unimplemented!()
}

pub fn conv_whnf(ctx: &Ctx, s: &Term, t: &Term, ty: &Type) -> Result<Ctrt> {
    unimplemented!()
}

pub fn conv_whnfs(ctx: &Ctx, s: &[Rc<Term>], t: &[Rc<Term>], ty: &Ctx) -> Result<Ctrt> {
    unimplemented!()
}
