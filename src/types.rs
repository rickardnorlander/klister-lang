#![allow(dead_code)]
use std::ffi::c_int;
use std::mem;

use libffi::middle::Type;

#[derive(Clone)]
#[derive(Debug)]
pub enum OwnershipTag {
    Borrowed, OwnedMalloced, Owned
}
#[derive(Clone)]
#[derive(Debug)]
pub enum Mutability {
    Mutable, Const
}

#[derive(Clone)]
#[derive(Debug)]
pub enum TypeTag {
    KlisterInt, KlisterCStr(OwnershipTag, Mutability), KlisterPtr(Box<TypeTag>, OwnershipTag, Mutability), KlisterBytes
}

impl TypeTag {
    pub fn from_string(s: &str) -> Option<TypeTag> {
        match s {
            "int" => Some(TypeTag::KlisterInt),
            "cbc" => Some(TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const)),
            "cbb" => Some(TypeTag::KlisterBytes),
            //"mmb" -> TypeTag::KlisterPtr(OwnershipTag::OwnedMalloced, Mutability::Mutable)
            _ => None
        }
    }

    pub fn get_ffi_type(&self) -> Option<Type> {
        match self {
            TypeTag::KlisterInt => Some(Type::c_int()),
            TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const) => Some(Type::pointer()),
            TypeTag::KlisterBytes => Some(Type::pointer()),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            TypeTag::KlisterInt => Some(mem::size_of::<c_int>()),
            _ => None
        }
    }
}