#![allow(dead_code)]

use std::collections::HashMap;
use std::ffi::c_void;
use std::ffi::c_char;
use std::ffi::c_int;
use std::ffi::c_long;
use std::ffi::CString;

use libffi::middle::Cif;

use crate::types::TypeTag;


type LmidT = c_long;

pub const LM_ID_NEWLM:i64 = -1;

const RTLD_NOW:i32 = 0x00002;
const RTLD_GLOBAL:i32 = 0x00100;
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

/*pub fn get_type_and_size(t: &str) -> (Type, usize) {
    match t {
        "cbc" => (Type::pointer(), mem::size_of::<*const c_void>()),
        "int" => (Type::c_int(), mem::size_of::<c_int>()),
        _ => panic!("Unknown type"),
    }
}*/

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

    pub fn load_lib(&mut self, libname: &str) -> *mut c_void {
        let lib = self.lib_handles.entry(libname.to_string()).or_insert_with(|| {
    	    let c_libname = CString::new(&libname as &str).unwrap();
            unsafe {
                let handle = dlmopen(self.lmid, c_libname.as_c_str().as_ptr(), RTLD_NOW | RTLD_NODELETE);
		if handle.is_null() {
		    panic!("Failed to load library");
		}
		if self.lmid == LM_ID_NEWLM {
		    self.lmid = Self::get_mid(handle);
		}
		handle
      	    }
        });
	return *lib;
    }

    pub fn load_fn(&mut self, libname: &str, fname: &str, rettypename: &str, argnames: &[&str]) {
        let library = self.load_lib(&libname);
        let c_fnname = CString::new(fname).unwrap();
        let function = unsafe {dlsym(library, c_fnname.as_c_str().as_ptr())};
        let retinfo = TypeTag::from_string(rettypename).get_ffi_type();
        let cif = Cif::new(argnames.iter().map(|x| TypeTag::from_string(x).get_ffi_type()), retinfo);

        let argcopy: Vec<TypeTag> = argnames.iter().map(|x| TypeTag::from_string(x)).collect();

        self.functions.insert(fname.to_string(), FunctionData{ptr: function, cif, retstr: rettypename.to_string(), args: argcopy});
    }
}