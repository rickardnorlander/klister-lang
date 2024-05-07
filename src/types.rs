#![allow(dead_code)]
use std::ffi::c_double;
use std::ffi::c_float;
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
    KlisterInt, KlisterCStr(OwnershipTag, Mutability), KlisterPtr(Box<TypeTag>, OwnershipTag, Mutability), KlisterBytes, KlisterDouble, KlisterFloat, OpaquePointer, KlU64, KlU32, KlU16, KlU8, KlI64, KlI32, KlI16, KlI8
}

impl TypeTag {
    pub fn from_string(s: &str) -> Option<TypeTag> {
        match s {
            "int" => Some(TypeTag::KlisterInt),
            "cbc" => Some(TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const)),
            "cbb" => Some(TypeTag::KlisterBytes),
            "double" => Some(TypeTag::KlisterDouble),
            "float" => Some(TypeTag::KlisterFloat),
            "ptr" => Some(TypeTag::OpaquePointer),
            "u64" => Some(TypeTag::KlU64),
            "u32" => Some(TypeTag::KlU32),
            "u16" => Some(TypeTag::KlU16),
            "u8" => Some(TypeTag::KlU8),
            "i64" => Some(TypeTag::KlI64),
            "i32" => Some(TypeTag::KlI32),
            "i16" => Some(TypeTag::KlI16),
            "i8" => Some(TypeTag::KlI8),
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
            TypeTag::KlisterFloat => Some(Type::f32()),
            TypeTag::OpaquePointer => Some(Type::pointer()),
            TypeTag::KlU64=> Some(Type::u64()),
            TypeTag::KlU32=> Some(Type::u32()),
            TypeTag::KlU16=> Some(Type::u16()),
            TypeTag::KlU8=> Some(Type::u8()),
            TypeTag::KlI64=> Some(Type::i64()),
            TypeTag::KlI32=> Some(Type::i32()),
            TypeTag::KlI16=> Some(Type::i16()),
            TypeTag::KlI8=> Some(Type::i8()),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            TypeTag::KlisterInt => Some(mem::size_of::<c_int>()),
            TypeTag::OpaquePointer => Some(mem::size_of::<*const c_void>()),
            TypeTag::KlisterDouble => Some(mem::size_of::<c_double>()),
            TypeTag::KlisterFloat => Some(mem::size_of::<c_float>()),
            TypeTag::KlU64 => Some(mem::size_of::<u64>()),
            TypeTag::KlU32 => Some(mem::size_of::<u32>()),
            TypeTag::KlU16 => Some(mem::size_of::<u16>()),
            TypeTag::KlU8 => Some(mem::size_of::<u8>()),
            TypeTag::KlI64 => Some(mem::size_of::<i64>()),
            TypeTag::KlI32 => Some(mem::size_of::<i32>()),
            TypeTag::KlI16 => Some(mem::size_of::<i16>()),
            TypeTag::KlI8 => Some(mem::size_of::<i8>()),
            _ => None
        }
    }
}