#![allow(dead_code)]
use std::ffi::c_int;
use std::mem;

use libffi::middle::Type;

#[derive(Debug)]
pub enum OwnershipTag {
    Borrowed, OwnedMalloced, Owned
}
#[derive(Debug)]
pub enum Mutability {
    Mutable, Const
}

#[derive(Debug)]
pub enum TypeTag {
    KlisterInt, KlisterCStr(OwnershipTag, Mutability), KlisterPtr(Box<TypeTag>, OwnershipTag, Mutability), KlisterBytes
}

impl TypeTag {
    pub fn from_string(s: &str) -> TypeTag {
        match s {
            "int" => TypeTag::KlisterInt,
            "cbc" => TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const),
            "cbb" => TypeTag::KlisterBytes,
            //"mmb" -> TypeTag::KlisterPtr(OwnershipTag::OwnedMalloced, Mutability::Mutable)
            _ => panic!("Invalid type {}", s),
        }
    }

    pub fn get_ffi_type(&self) -> Type {
        match self {
            TypeTag::KlisterInt => Type::c_int(),
            TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const) => Type::pointer(),
            TypeTag::KlisterBytes => Type::pointer(),
            _ => panic!(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            TypeTag::KlisterInt => mem::size_of::<c_int>(),
            _ => panic!(),
        }
    }
}