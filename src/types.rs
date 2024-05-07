#![allow(dead_code)]
use std::ffi::c_double;
use std::ffi::c_int;
use std::ffi::c_void;
use std::mem;

use libffi::middle::Type;

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum OwnershipTag {
    Borrowed, OwnedMalloced, Owned
}
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Mutability {
    Mutable, Const
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum TypeTag {
    KlisterInt, KlisterCStr(OwnershipTag, Mutability), KlisterPtr(Box<TypeTag>, OwnershipTag, Mutability), KlisterBytes, KlisterDouble, OpaquePointer, KlU64
}

impl TypeTag {
    pub fn from_string(s: &str) -> Option<TypeTag> {
        match s {
            "int" => Some(TypeTag::KlisterInt),
            "cbc" => Some(TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const)),
            "cbb" => Some(TypeTag::KlisterBytes),
            "double" => Some(TypeTag::KlisterDouble),
            "ptr" => Some(TypeTag::OpaquePointer),
            "u64" => Some(TypeTag::KlU64),
            //"mmb" -> TypeTag::KlisterPtr(OwnershipTag::OwnedMalloced, Mutability::Mutable)
            _ => None
        }
    }

    pub fn get_ffi_type(&self) -> Option<Type> {
        match self {
            TypeTag::KlisterInt => Some(Type::c_int()),
            TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const) => Some(Type::pointer()),
            TypeTag::KlisterBytes => Some(Type::pointer()),
            TypeTag::KlisterDouble => Some(Type::f64()),
            TypeTag::OpaquePointer => Some(Type::pointer()),
            TypeTag::KlU64=> Some(Type::u64()),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            TypeTag::KlisterInt => Some(mem::size_of::<c_int>()),
            TypeTag::OpaquePointer => Some(mem::size_of::<*const c_void>()),
            TypeTag::KlisterDouble => Some(mem::size_of::<c_double>()),
            TypeTag::KlU64 => Some(mem::size_of::<u64>()),
            _ => None
        }
    }
}