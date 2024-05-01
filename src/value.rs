
#![allow(unused_imports)]

use std::collections::HashMap;
use std::fmt::Debug;

use as_any::{AsAny, Downcast};
use dyn_clone::DynClone;
use gc::Gc;
use gc::Trace;
use libffi::middle::Arg;
use num_traits::cast::ToPrimitive;

use crate::ast::KlisterStatement;
use crate::ast::Operation;
use crate::ccall::ffi_call;
use crate::except::KlisterRTE;
use crate::interpret::Context;
use crate::interpret::handle_statement;

pub type ValWrap = Gc<Box<dyn KlisterValueV2>>;

pub fn valwrap(v: impl KlisterValueV2 + 'static) -> ValWrap {
    return Gc::new(Box::new(v))
}

pub enum Oppi<T, E> {
    Ok(T),
    Err(E),
    NotSupported,
}

fn invalid_member_exception(type_s: &str, index: &str) -> KlisterRTE {
    KlisterRTE::new(&format!("{} has no member {}", type_s, index), false)
}

pub fn bin_op(op: Operation, lhs: ValWrap, rhs: ValWrap) -> Result<ValWrap, KlisterRTE> {
    match lhs.bin_op_forward(op, (*rhs).as_ref()) {
        Oppi::Ok(a) => {return Result::Ok(a)}
        Oppi::Err(a) => {return Result::Err(a)}
        Oppi::NotSupported => {}
    }
    match rhs.bin_op_backward(op, (*lhs).as_ref()) {
        Oppi::Ok(a) => {return Result::Ok(a)}
        Oppi::Err(a) => {return Result::Err(a)}
        Oppi::NotSupported => {}
    }
    return Err(KlisterRTE::new("Operation not supported", false));
}

pub trait KlisterValueV2: Trace + DynClone + Debug + AsAny {
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

    fn bin_op_forward(&self, _op: Operation, _other: &dyn KlisterValueV2) -> Oppi<ValWrap, KlisterRTE> {
        Oppi::NotSupported
    }

    fn bin_op_backward(&self, _op: Operation, _other: &dyn KlisterValueV2) -> Oppi<ValWrap, KlisterRTE> {
        Oppi::NotSupported
    }

    fn un_op(&self, _op: &str) -> Result<ValWrap, KlisterRTE> {
        Err(KlisterRTE::new("Type error", false))
    }

    fn subscript(&self, _context: &mut Context, _subscript: &ValWrap) -> Result<ValWrap, KlisterRTE>  {
        return Err(KlisterRTE::new("Object is not subscriptable", false));
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

use std::iter::zip;

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterFunction {
    #[unsafe_ignore_trace] 
    pub body: Box<KlisterStatement>,
    pub arg_names: Vec<String>,
}

use crate::interpret::StatementE;

impl KlisterValueV2 for KlisterFunction {
    fn call(&self, context: &mut Context, arguments: Vec<ValWrap>) -> Result<ValWrap, KlisterRTE> {
        if arguments.len() != self.arg_names.len() {
            return Err(KlisterRTE::new("Wrong number of arguments", false));
        }
        context.enter_function();
        for (ref n, v) in zip(&self.arg_names, arguments) {
            context.put_var(n, v)
        }
        let res = match handle_statement(context, self.body.as_ref()) {
            StatementE::AllGood => Ok(valwrap(KlisterNothing{})),
            StatementE::Err(e) => Err(e),
            StatementE::Return(r) => Ok(r),
        };
        context.exit_function();
        return res;
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

    fn bin_op_forward(&self, op: Operation, other_ref: &dyn KlisterValueV2) -> Oppi<ValWrap, KlisterRTE> {
        let other_opt = other_ref.as_any().downcast_ref::<KlisterBool>();
        if let Some(ref other) = other_opt {
            let selfval = self.val.clone();
            let otherval = other.val.clone();
            match op {
                Operation::And => {return Oppi::Ok(KlisterBool::v(selfval && otherval));}
                Operation::Or => {return Oppi::Ok(KlisterBool::v(selfval || otherval));}
                _ => {
                    return Oppi::NotSupported
                }
            }
        }
        return Oppi::NotSupported
    }

    fn un_op(&self, op: &str) -> Result<ValWrap, KlisterRTE> {
        match op {
            "!" => {return Ok(KlisterBool::v(!self.val));}
            _ => {
                return Err(KlisterRTE::new(&format!("Unknown un_op {}", op), false))
            }
        }
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

    fn subscript(&self, _context: &mut Context, subscript: &ValWrap) -> Result<ValWrap, KlisterRTE>  {
        let xx: &dyn KlisterValueV2 = (**subscript).as_ref();
        let Some(index) = xx.as_any().downcast_ref::<KlisterInteger>() else {
            return Err(KlisterRTE::new("Index is not integer", false));
        };
        let Ok(ind) = index.val.clone().try_into() else {
            return Err(KlisterRTE::new("Index is not valid usize", false));
        };
        let Some(nthchar) = self.val.chars().nth(ind) else {
            return Err(KlisterRTE::new("Index out of bounds", false));
        };

        let cu32 = nthchar as u32;
        let bi:BigInt = cu32.into();
        Ok(KlisterInteger::wrap(bi))
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterBytes {
    pub val: Vec<u8>,
}

impl KlisterValueV2 for KlisterBytes {
    fn dot_impl(&self, gcself: &ValWrap, subscript: &str) -> Option<ValWrap> {
        match subscript {
            "len" => Some(KlisterInteger::wrap(self.val.len().into())),
            "read0_strs" => Some(KlisterMemberFunction::new(gcself, subscript)),
            "read0" => Some(KlisterMemberFunction::new(gcself, subscript)),
            "parse_string" => Some(KlisterMemberFunction::new(gcself, subscript)),
            _ => None,
        }
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

    fn bin_op_forward(&self, op: Operation, other_ref: &dyn KlisterValueV2) -> Oppi<ValWrap, KlisterRTE> {
        let other_opt = other_ref.as_any().downcast_ref::<KlisterInteger>();
        if let Some(ref other) = other_opt {
            let selfval = self.val.clone();
            let otherval = other.val.clone();
            match op {
                Operation::Add => {return Oppi::Ok(KlisterInteger::wrap(selfval + otherval));}
                Operation::Sub => {return Oppi::Ok(KlisterInteger::wrap(selfval - otherval));}
                Operation::Mul => {return Oppi::Ok(KlisterInteger::wrap(selfval * otherval));}
                Operation::Div => {                    
                    let lv = match bint_to_double(&self.val) {
                        Ok(x) => x,
                        Err(x) => {return Oppi::Err(x);},
                    };
                    let rv = match bint_to_double(&other.val) {
                        Ok(x) => x,
                        Err(x) => {return Oppi::Err(x);},
                    };
                    return Oppi::Ok(KlisterDouble::wrap(lv / rv));
                }
                Operation::Eq => {return Oppi::Ok(KlisterBool::v(selfval == otherval));}
                Operation::Ne => {return Oppi::Ok(KlisterBool::v(selfval != otherval));}
                Operation::Lt => {return Oppi::Ok(KlisterBool::v(selfval < otherval));}
                Operation::Gt => {return Oppi::Ok(KlisterBool::v(selfval > otherval));}
                Operation::Lte => {return Oppi::Ok(KlisterBool::v(selfval <= otherval));}
                Operation::Gte => {return Oppi::Ok(KlisterBool::v(selfval >= otherval));}
                _ => {
                    return Oppi::NotSupported
                }
            }
        }
        Oppi::NotSupported
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
        // Todo: this architecture is kind of awful but I haven't figured out how to do it better yet.
        // Probably something like having a member Fn(x, y) -> z
        let xx: &dyn KlisterValueV2 = (*self.obj).as_ref();
        if let Some(ref x) = xx.as_any().downcast_ref::<KlisterInteger>() {
            match self.name.as_str() {
                "to_string" => {
                    if arguments.len() != 0 {
                        return Err(KlisterRTE::new("Wrong number of arguments", false));
                    }
                    return Ok(valwrap(KlisterStr{val:x.val.to_string()}));
                }
                _ => {}
            }
        } else if let Some(ref x) = xx.as_any().downcast_ref::<KlisterBytes>() {
            match self.name.as_str() {
                "read0_strs" => {
                    if arguments.len() != 0 {
                        return Err(KlisterRTE::new("Wrong number of arguments", false));
                    }
                    let mut result = Vec::<ValWrap>::new();
                    let mut cur = Vec::<u8>::new();
                    for c in &x.val {
                        if *c == 0 {
                            let thestring = match String::from_utf8(cur) {
                                Ok(s) => s,
                                Err(_) => {return Err(KlisterRTE::new("Invalid utf8", true));}
                            };
                            result.push(valwrap(KlisterStr{val: thestring}));
                            cur = Vec::<u8>::new()
                        } else {
                            cur.push(*c);
                        }
                    }
                    if !cur.is_empty() {
                        return Err(KlisterRTE::new("Trailing garbage for read0_str", true));
                    }
                    return Ok(valwrap(KlisterArray{val: result}));
                }
                "read0" => {
                    if arguments.len() != 0 {
                        return Err(KlisterRTE::new("Wrong number of arguments", false));
                    }
                    let mut result = Vec::<ValWrap>::new();
                    let mut cur = Vec::<u8>::new();
                    for c in &x.val {
                        if *c == 0 {
                            result.push(valwrap(KlisterBytes{val: cur}));
                            cur = Vec::<u8>::new()
                        } else {
                            cur.push(*c);
                        }
                    }
                    if !cur.is_empty() {
                        return Err(KlisterRTE::new("Trailing garbage for read0", true));
                    }
                    return Ok(valwrap(KlisterArray{val: result}));
                }
                "parse_string" => {
                    if arguments.len() != 0 {
                        return Err(KlisterRTE::new("Wrong number of arguments", false));
                    }
                    let Ok(s) = String::from_utf8(x.val.clone()) else {
                        return Err(KlisterRTE::new("Not valid string", false));
                    };
                    return Ok(valwrap(KlisterStr{val: s}));
                }
                _ => {}
            }
        }
        return Err(KlisterRTE::new(&format!("Type {} has no member function {}", self.obj.get_type_name(), self.name), false));
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

fn bint_to_double(bi: &BigInt) -> Result<f64, KlisterRTE> {
    let Some(x) = bi.to_f64() else {
        return Err(KlisterRTE::new("Unknown error when converting integer to double", false));
    };
    if !x.is_finite() {
        return Err(KlisterRTE::new("Overflow when converting integer to double", true));
    }
    Ok(x)
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterDouble {
    pub val: f64,
}

impl KlisterDouble {
    pub fn wrap(v: f64) -> ValWrap {
        return valwrap(KlisterDouble{val: v});
    }
    pub fn wrapn(v: f64) -> Box<dyn KlisterValueV2> {
        return Box::new(KlisterDouble{val: v});
    }

    fn do_op(op: Operation, selfval: f64, otherval: f64) -> Oppi<ValWrap, KlisterRTE> {
        return match op {
            Operation::Add => Oppi::Ok(KlisterDouble::wrap(selfval + otherval)),
            Operation::Sub => Oppi::Ok(KlisterDouble::wrap(selfval - otherval)),
            Operation::Mul => Oppi::Ok(KlisterDouble::wrap(selfval * otherval)),
            Operation::Div => Oppi::Ok(KlisterDouble::wrap(selfval / otherval)),
            Operation::Eq => Oppi::Ok(KlisterBool::v(selfval == otherval)),
            Operation::Ne => Oppi::Ok(KlisterBool::v(selfval != otherval)),
            Operation::Lt => Oppi::Ok(KlisterBool::v(selfval < otherval)),
            Operation::Gt => Oppi::Ok(KlisterBool::v(selfval > otherval)),
            Operation::Lte => Oppi::Ok(KlisterBool::v(selfval <= otherval)),
            Operation::Gte => Oppi::Ok(KlisterBool::v(selfval >= otherval)),
            _ => {
                Oppi::NotSupported
            }
        }
    }

    fn get_other(other_ref: &dyn KlisterValueV2) -> Oppi<f64, KlisterRTE> {
        if let Some(other) = other_ref.as_any().downcast_ref::<KlisterDouble>() {
            Oppi::Ok(other.val)
        } else if let Some(other) = other_ref.as_any().downcast_ref::<KlisterInteger>() {
            match bint_to_double(&other.val) {
                Ok(x) => Oppi::Ok(x),
                Err(x) => {return Oppi::Err(x);},
            }
        } else {
            return Oppi::NotSupported
        }
    }
}

impl KlisterValueV2 for KlisterDouble {
    fn dot_impl(&self, gcself: &ValWrap, subscript: &str) -> Option<ValWrap> {
        match subscript {
            "to_string" => Some(KlisterMemberFunction::new(gcself, subscript)),
            _ => None,
        }
    }

    fn bin_op_backward(&self, op: Operation, other_ref: &dyn KlisterValueV2) -> Oppi<ValWrap, KlisterRTE> {
        let selfval = self.val.clone();
        let otheroppi = KlisterDouble::get_other(other_ref);
        let otherval = match otheroppi {
            Oppi::Ok(v) => v,
            Oppi::Err(e) => {return Oppi::Err(e)}
            Oppi::NotSupported => {return Oppi::NotSupported}
        };
        return KlisterDouble::do_op(op, otherval, selfval);
    }

    fn bin_op_forward(&self, op: Operation, other_ref: &dyn KlisterValueV2) -> Oppi<ValWrap, KlisterRTE> {
        let selfval = self.val.clone();
        let otheroppi = KlisterDouble::get_other(other_ref);
        let otherval = match otheroppi {
            Oppi::Ok(v) => v,
            Oppi::Err(e) => {return Oppi::Err(e)}
            Oppi::NotSupported => {return Oppi::NotSupported}
        };
        return KlisterDouble::do_op(op, selfval, otherval);
    }
}


#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterArray {
    pub val: Vec<ValWrap>,
}

impl KlisterValueV2 for KlisterArray {
    fn subscript(&self, _context: &mut Context, subscript: &ValWrap) -> Result<ValWrap, KlisterRTE>  {
        let xx: &dyn KlisterValueV2 = (**subscript).as_ref();
        let Some(index) = xx.as_any().downcast_ref::<KlisterInteger>() else {
            return Err(KlisterRTE::new("Index is not integer", false));
        };
        let Ok(ind): Result<usize, _> = index.val.clone().try_into() else {
            return Err(KlisterRTE::new("Index is not valid usize", false));
        };
        let Some(ret): Option<&ValWrap> = self.val.get(ind) else {
            return Err(KlisterRTE::new("Index out of bounds", false));
        };
        Ok(ret.clone())
    }
    fn dot_impl(&self, _gcself: &ValWrap, subscript: &str) -> Option<ValWrap> {
        match subscript {
            "len" => Some(KlisterInteger::wrap(self.val.len().into())),
            _ => None,
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterDict {
    pub val: HashMap<String, ValWrap>,
}

impl KlisterValueV2 for KlisterDict {
    fn subscript(&self, _context: &mut Context, subscript: &ValWrap) -> Result<ValWrap, KlisterRTE>  {
        let xx: &dyn KlisterValueV2 = (**subscript).as_ref();
        let Some(index) = xx.as_any().downcast_ref::<KlisterStr>() else {
            return Err(KlisterRTE::new("Index is not string", false));
        };
        let Some(ret): Option<&ValWrap> = self.val.get(&index.val) else {
            return Err(KlisterRTE::new("Index not present", true));
        };
        Ok(ret.clone())
    }
}