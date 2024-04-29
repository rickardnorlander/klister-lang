
#![allow(unused_imports)]
use gc::Gc;
use gc::Trace;

use crate::ast::KlisterStatement;
use crate::except::KlisterRTE;
use dyn_clone::DynClone;
use std::fmt::Debug;
use crate::interpret::Context;
use crate::interpret::handle_statement;
use crate::ccall::ffi_call;
use libffi::middle::Arg;

pub type ValWrap = Gc<Box<dyn KlisterValueV2>>;

pub fn valwrap(v: impl KlisterValueV2 + 'static) -> ValWrap {
    return Gc::new(Box::new(v))
}

fn invalid_member_exception(type_s: &str, index: &str) -> KlisterRTE {
    KlisterRTE::new(&format!("{} has no member {}", type_s, index), false)
}

pub trait KlisterValueV2: Trace + DynClone + Debug {
    fn dot(&self, gcself: &ValWrap, s: &str) -> Result<ValWrap, KlisterRTE>  {
        return self.dot_impl(gcself, s).ok_or_else(||invalid_member_exception(self.get_type_name(), s));
    }

    fn dot_impl(&self, _gcself: &ValWrap, _s: &str) -> Option<ValWrap> {
        return None
    }

    fn get_type_name(&self) -> &'static str {
        return std::any::type_name::<Self>();
    }

    fn call(&self, _context: &mut Context, _arguments: Vec<ValWrap>) -> Result<ValWrap, KlisterRTE>  {
        return Err(KlisterRTE::new("Object is not callable", false));
    }

    fn bool_val(&self) -> Result<bool, KlisterRTE>  {
        return Err(KlisterRTE::new("Not boolean", false));
    }

    fn str_val(&self) -> Result<String, KlisterRTE>  {
        return Err(KlisterRTE::new("Not string", false));
    }

    fn cast_to_klisterstr(&self) -> Option<&KlisterStr>  {
        return None;
    }

    fn cast_to_klisterbytes(&self) -> Option<&KlisterBytes>  {
        return None;
    }

    fn cast_to_klisterinteger(&self) -> Option<&KlisterInteger>  {
        return None;
    }
}


dyn_clone::clone_trait_object!(KlisterValueV2);


#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub enum KlisterResult {
    ResOk(ValWrap),
    ResErr(Box<KlisterRTE>),
}

impl KlisterResult {
    pub fn ok_wrapped(v: ValWrap) -> ValWrap {
        return valwrap(KlisterResult::ResOk(v));
    }
}

impl KlisterValueV2 for KlisterResult {
    fn dot(&self, _gcself: &ValWrap, subscript: &str) -> Result<ValWrap, KlisterRTE> {
        match subscript {
            "is_ok" => {
                return Ok(KlisterBool::v(matches!(self, KlisterResult::ResOk(_))))
            }
            "ok_variant" => {
                let KlisterResult::ResOk(ok) = self else {
                    // todo: reconsider whether this should be catchable
                    return Err(KlisterRTE::new("Accessed inactive variant", true));
                };
                return Ok(ok.clone());
            }
            "err_variant" => {
                let KlisterResult::ResErr(err) = self else {
                    return Err(KlisterRTE::new("Accessed inactive variant", false));
                };
                return Ok(valwrap(KlisterException{e: (**err).clone()}));
            }
            _ => {Err(invalid_member_exception(self.get_type_name(), subscript))}
        }
    }
}


#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterFunction {
    #[unsafe_ignore_trace] 
    pub body: Box<KlisterStatement>,
}

impl KlisterValueV2 for KlisterFunction {
    fn call(&self, context: &mut Context, arguments: Vec<ValWrap>) -> Result<ValWrap, KlisterRTE> {
        if !arguments.is_empty() {
            return Err(KlisterRTE::new("Wrong number of arguments", false));
        }
        return handle_statement(context, self.body.as_ref()).map(|_| valwrap(KlisterNothing{}));
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterBool {
    pub val: bool,
}

impl KlisterValueV2 for KlisterBool {
    fn bool_val(&self) -> Result<bool, KlisterRTE> {
        return Ok(self.val);
    }
}

impl KlisterBool {
    pub fn v(x: bool) -> ValWrap {
        valwrap(KlisterBool{val: x})
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterNothing {
}

impl KlisterValueV2 for KlisterNothing {
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterException {
    pub e: KlisterRTE,
}

impl KlisterValueV2 for KlisterException {
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterCFunction {
    pub name: String,
}

impl KlisterValueV2 for KlisterCFunction {
    fn call(&self, context: &mut Context, arguments: Vec<ValWrap>) -> Result<ValWrap, KlisterRTE> {
        let arguments2 = arguments.into_iter().map(|x| (*x).clone()).collect::<Vec<_>>();
        ffi_call(&mut context.libs, &self.name, arguments2)
    }
}

impl KlisterCFunction {
    pub fn wrapped(s: &str) -> ValWrap {
        return valwrap(KlisterCFunction{name: s.to_string()});
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterStr {
    pub val: String,
}

use std::ffi::CStr;
use libffi::high::arg;
use std::ffi::c_void;

impl KlisterValueV2 for KlisterStr {
    fn str_val(&self) -> Result<String, KlisterRTE> {
        return Ok(self.val.clone());
    }

    fn cast_to_klisterstr(&self) -> Option<&KlisterStr> {
        return Some(&self)
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterBytes {
    pub val: Vec<u8>,
}

impl KlisterValueV2 for KlisterBytes {
    fn dot_impl(&self, _gcself: &ValWrap, subscript: &str) -> Option<ValWrap> {
        match subscript {
            "len" => Some(KlisterInteger::wrap(self.val.len().into())),
            _ => None,
        }
    }

    fn cast_to_klisterbytes(&self) -> Option<&KlisterBytes> {
        return Some(&self)
    }
}

use num_bigint::BigInt;

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterInteger {
    #[unsafe_ignore_trace]
    pub val: BigInt,
}

impl KlisterInteger {
    pub fn wrap(v: BigInt) -> ValWrap {
        return valwrap(KlisterInteger{val: v});
    }
    pub fn wrapn(v: BigInt) -> Box<dyn KlisterValueV2> {
        return Box::new(KlisterInteger{val: v});
    }
}


impl KlisterValueV2 for KlisterInteger {
    fn dot_impl(&self, gcself: &ValWrap, subscript: &str) -> Option<ValWrap> {
        match subscript {
            "to_string" => Some(KlisterMemberFunction::new(gcself, subscript)),
            _ => None,
        }
    }

    fn cast_to_klisterinteger(&self) -> Option<&KlisterInteger> {
        return Some(&self)
    }
}


#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterMemberFunction {
    pub obj: ValWrap,
    pub name: String,
}

impl KlisterValueV2 for KlisterMemberFunction {
    fn call(&self, _context: &mut Context, arguments: Vec<ValWrap>) -> Result<ValWrap, KlisterRTE> {
        if let Some(ref x) = self.obj.cast_to_klisterinteger() {
            match self.name.as_str() {
                "to_string" => {
                    if arguments.len() != 0 {
                        return Err(KlisterRTE::new("Wrong number of arguments", false));
                    }
                    return Ok(valwrap(KlisterStr{val:x.val.to_string()}));
                }
                _ => {}
            }
        }
        return Err(KlisterRTE::new(&format!("Type {} has no member function {}", self.get_type_name(), self.name), false));
    }
}

impl KlisterMemberFunction {
    pub fn new(obj: &ValWrap, subscript: &str) -> ValWrap {
        return valwrap(KlisterMemberFunction{obj: obj.clone(), name: subscript.to_string()});
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub enum KlisterShellRes {
    SResOk(Vec<u8>),
    SResErr(#[unsafe_ignore_trace] KlisterRTE, Vec<u8>, Option<i32>),
}

impl KlisterValueV2 for KlisterShellRes {
    fn dot_impl(&self, _gcself: &ValWrap, subscript: &str) -> Option<ValWrap> {
        match subscript {
            "is_ok" => Some(KlisterBool::v(matches!(self, KlisterShellRes::SResOk(_)))),
            _ => None,
        }
    }
}