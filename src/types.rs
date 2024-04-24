#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(private_interfaces)]

use std::ffi::c_int;
use std::mem;

use libffi::middle::Type;

#[derive(Debug)]
enum OwnershipTag {
    Borrowed, OwnedMalloced, Owned
}
#[derive(Debug)]
enum Mutability {
    Mutable, Const
}

#[derive(Debug)]
pub enum TypeTag {
    klister_int, klister_cstr(OwnershipTag, Mutability), klister_ptr(Box<TypeTag>, OwnershipTag, Mutability), klister_dict, klister_bytes
}

impl TypeTag {
    pub fn from_string(s: &str) -> TypeTag {
        match s {
            "int" => TypeTag::klister_int,
            "cbc" => TypeTag::klister_cstr(OwnershipTag::Borrowed, Mutability::Const),
            "cbb" => TypeTag::klister_bytes,
            //"mmb" -> TypeTag::klister_ptr(OwnershipTag::OwnedMalloced, Mutability::Mutable)
            _ => panic!("Invalid type {}", s),
        }
    }

    pub fn get_ffi_type(&self) -> Type {
        match self {
            TypeTag::klister_int => Type::c_int(),
            TypeTag::klister_cstr(OwnershipTag::Borrowed, Mutability::Const) => Type::pointer(),
            TypeTag::klister_bytes => Type::pointer(),
            _ => panic!(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            TypeTag::klister_int => mem::size_of::<c_int>(),
            _ => panic!(),
        }
    }
}