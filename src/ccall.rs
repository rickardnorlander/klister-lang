use std::collections::HashMap;
use std::ffi::c_char;
use std::ffi::c_double;
use std::ffi::c_int;
use std::ffi::c_long;
use std::ffi::c_void;
use std::ffi::CString;
use std::iter::zip;

use libffi::low;
use libffi::middle::arg;
use libffi::middle::Arg;
use libffi::middle::Cif;
use libffi::middle::Type;
use num_bigint::BigInt;

use crate::except::KlisterRTE;
use crate::types::Mutability;
use crate::types::TypeTag;
use crate::types::OwnershipTag;
use crate::value::KlisterBytes;
use crate::value::KlisterDouble;
use crate::value::KlisterInteger;
use crate::value::KlisterValueV2;
use crate::value::KlisterStr;
use crate::value::ValWrap;
use crate::value::valwrap;

type LmidT = c_long;

pub const LM_ID_NEWLM:i64 = -1;

const RTLD_NOW:i32 = 0x00002;
const RTLD_NODELETE:i32 = 0x01000;
const RTLD_DI_LMID:c_int = 1;

extern "C" {
    fn dlmopen(mid: LmidT, filename: *const c_char, flags: c_int) -> *mut c_void;
    pub fn dlsym(handle: *const c_void, symbol: *const c_char) -> *const c_void;
    fn dlinfo(handle: *const c_void, request: c_int, info: *mut c_void) -> c_int;
}


pub struct FunctionData {
    pub ptr: *const c_void, 
    pub cif: Cif,
    pub retstr: String,
    pub args: Vec<TypeTag>,
}

pub struct Libraries {
    pub lib_handles: HashMap<String, *mut c_void>,
    pub functions: HashMap<String, FunctionData>,
    lmid: LmidT,
}


impl Libraries {
    pub fn new() -> Libraries {
        Libraries{lib_handles: HashMap::new(), functions: HashMap::new(), lmid: LM_ID_NEWLM}
    }

    fn get_mid(handle: *mut c_void) -> LmidT {
        unsafe {
            let mut mid_out: LmidT = 0;
            let mid_out_ptr = &mut mid_out as *mut LmidT;
            let dlinforet = dlinfo(handle, RTLD_DI_LMID, mid_out_ptr as *mut c_void);
            if dlinforet != 0 {
                panic!("Failed to get mid");
            }
            mid_out
        }
    }

    pub fn load_lib(&mut self, libname: &str) -> Result<*mut c_void, KlisterRTE> {
        let lib_opt = self.lib_handles.get(libname);
        if let Some(lib) = lib_opt {
            return Ok(*lib);
        }
        let Ok(c_libname) = CString::new(&libname as &str) else {
            return Err(KlisterRTE::new("Invalid library name", false));
        };
        let lib = unsafe {
            dlmopen(self.lmid, c_libname.as_c_str().as_ptr(), RTLD_NOW | RTLD_NODELETE)
        };
        if lib.is_null() {
            return Err(KlisterRTE::new("Failed to load library", true));
        };
        if self.lmid == LM_ID_NEWLM {
            self.lmid = Self::get_mid(lib);
        }
        self.lib_handles.insert(libname.to_string(), lib);
        return Ok(lib);
    }

    pub fn load_fn(&mut self, libname: &str, fname: &str, rettypename: &str, argnames: &[&str]) -> Result<(), KlisterRTE> {
        let library = self.load_lib(&libname)?;
        let Ok(c_fnname) = CString::new(fname) else {
            return Err(KlisterRTE::new("Import name is not valid c-string", false))?;
        };
        let function = unsafe {dlsym(library, c_fnname.as_c_str().as_ptr())};
        if function.is_null() {
            return Err(KlisterRTE::new("Failed to load symbol", true));
        };
        let retinfo = TypeTag::from_string(rettypename).ok_or(KlisterRTE::new("Unknown type tag", false))?.get_ffi_type().ok_or(KlisterRTE::new("Unsupported type tag", false))?;
        let mut argtypes = Vec::<Type>::new();
        let mut argtags = Vec::<TypeTag>::new();
        for argname in argnames {
            let tag = TypeTag::from_string(argname).ok_or(KlisterRTE::new("Unknown type tag", false))?;
            argtags.push(tag.clone());
            let argtype = tag.get_ffi_type().ok_or(KlisterRTE::new("Unsupported type tag", false))?;
            argtypes.push(argtype);
        }
        let cif = Cif::new(argtypes, retinfo);

        self.functions.insert(fname.to_string(), FunctionData{ptr: function, cif, retstr: rettypename.to_string(), args: argtags});
        Ok(())
    }
}

struct ArgumentStorage {
    vec: Vec::<*mut [u8]>
}

impl ArgumentStorage {
    fn new() -> ArgumentStorage {
        ArgumentStorage{vec: Vec::new()}
    }

    fn push_ptr(&mut self, ptr: *mut u8) -> *mut u8 {
        let bytes = (ptr as usize).to_ne_bytes();
        return self.gift_vec(bytes.to_vec());
    }

    fn gift_vec(&mut self, v: Vec<u8>) -> *mut u8 {
        let b = v.into_boxed_slice();
        return self.gift_box(b);
    }

    fn gift_box(&mut self, b: Box<[u8]>) -> *mut u8  {
        let ptr = Box::<[u8]>::into_raw(b);
        self.vec.push(ptr);
        return ptr as *mut u8;
    }
}

impl Drop for ArgumentStorage {
    fn drop(&mut self) {
        for v in &mut self.vec {
            unsafe {
                drop(Box::<[u8]>::from_raw(*v));
            }
        }
    }
}

pub fn ffi_call(libs: &mut Libraries, fn_name: &str, argument_values: Vec<Box<dyn KlisterValueV2>>) -> Result<ValWrap, KlisterRTE> {
    let mut args = Vec::<Arg>::new();
    let mut arg_storage = ArgumentStorage::new();

    let fn_data = libs.functions.get(&fn_name as &str).expect("Internal interpreter error: Function not found");
    let FunctionData{ptr: fn_ptr, cif, retstr: rettypename, args: argtypes} = fn_data;

    if argument_values.len() != argtypes.len() {
        return Err(KlisterRTE::new(&format!("Wrong number of arguments {} {} {}", fn_name, argument_values.len(), argtypes.len()), false))
    }

    for (z, argtype) in zip(&argument_values, argtypes) {
        let xx: &dyn KlisterValueV2 = (*z).as_ref();
        if let Some(x) = xx.as_any().downcast_ref::<KlisterStr>() {
            if *argtype != TypeTag::KlisterCStr(OwnershipTag::Borrowed, Mutability::Const) {
                return Err(KlisterRTE::new("Invalid argument type", false));
            }
            let Ok(cs) = CString::new(x.val.clone()) else {
                return Err(KlisterRTE::new("Validation as c-string failed", true));
            };
            let ptr = arg_storage.gift_vec(cs.into_bytes());
            let ptrptr = arg_storage.push_ptr(ptr);
            unsafe {
                args.push(arg(&*ptrptr));
            }
        } else if let Some(x) = xx.as_any().downcast_ref::<KlisterBytes>() {
            if *argtype != TypeTag::KlisterBytes {
                return Err(KlisterRTE::new("Invalid argument type", false));
            }
            let ptr = arg_storage.gift_vec(x.val.clone());
            let ptrptr = arg_storage.push_ptr(ptr);
            unsafe {
                args.push(arg(&*ptrptr));
            }
        } else if let Some(x) = xx.as_any().downcast_ref::<KlisterInteger>() {
            if *argtype != TypeTag::KlisterInt {
                return Err(KlisterRTE::new("Invalid argument type", false));
            }
            let downcast_res = x.val.clone().try_into();
            let Ok(downcast_x) = downcast_res else {return Err(KlisterRTE::new("Number too big to pass", true));};
            let downcast: c_int = downcast_x;
            let ptr = arg_storage.gift_vec(downcast.to_ne_bytes().to_vec());
            unsafe {
                args.push(arg(&*ptr));
            }
        } else if let Some(x) = xx.as_any().downcast_ref::<KlisterDouble>() {
            if *argtype != TypeTag::KlisterDouble {
                return Err(KlisterRTE::new("Invalid argument type", false));
            }
            let ptr = arg_storage.gift_vec(x.val.to_ne_bytes().to_vec());
            unsafe {
                args.push(arg(&*ptr));
            }
        }
    }

    let codeptr = low::CodePtr::from_ptr(*fn_ptr);

    let tt = TypeTag::from_string(&rettypename).ok_or(KlisterRTE::new("Unknown type tag", false))?;
    let tt_size = tt.size().ok_or(KlisterRTE::new("Return type not suppoted", false))?;
    let mut result = vec![0u8; tt_size];

    unsafe {
        libffi::raw::ffi_call(cif.as_raw_ptr(), Some(*codeptr.as_fun()), result.as_mut_ptr() as *mut c_void, (&args).as_ptr() as *mut *mut c_void);
    }

    let ret = match rettypename.as_str() {
        "int" => {
            let cint = c_int::from_ne_bytes(result.try_into().expect("Internal interpreter error: Failed to convert int"));

            let bint:BigInt = cint.into();
            Ok(KlisterInteger::wrap(bint))
        }
        "double" => {
            let d = c_double::from_ne_bytes(result.try_into().expect("Internal interpreter error: Failed to convert int"));
            Ok(valwrap(KlisterDouble{val: d}))
        }
        _ => {
            return Err(KlisterRTE::new("Invalid return type", false));
        }
    };

    // There are pointers into these structures.
    // So use explicit drops to make sure the values live until this point.
    drop(arg_storage);
    drop(argument_values);
    return ret;
}