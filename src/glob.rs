#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use std::ffi::c_char;
use std::ffi::c_int;
use std::ffi::c_void;
use std::ffi::CStr;
use std::ffi::CString;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::ffi::OsStringExt;
use std::ptr;

use crate::except::KlisterRTE;

type size_t = usize;

const GLOB_ERR: i32 = 1 << 0;
const GLOB_BRACE: i32 = 1 << 10;
const GLOB_NOMAGIC: i32 = 1 << 11;
const GLOB_TILDE_CHECK: i32 = 1 << 14;

#[repr(C)]
struct glob_t {
    gl_pathc: size_t,
    gl_pathv: *mut *mut c_char,
    gl_offsL: size_t,
}

extern "C" {
    fn glob(pattern: *const c_char, flags: c_int, errfunc: *const c_void, pglob: *mut glob_t) -> c_int;
    fn globfree(pglob: *mut glob_t) -> c_void;
}

pub fn globwalk(pattern: OsString) -> Result<Vec<OsString>, KlisterRTE> {
    let mut pattern_u8 = pattern.into_vec();
    pattern_u8.push(0);
    let pattern_cstring = CString::from_vec_with_nul(pattern_u8).or_else(|_| Err(KlisterRTE::new("Internal nul", true)))?;
    let mut ret = Vec::new();
    let is_ok;
    unsafe {
        let mut theglob = glob_t{gl_pathc: 0, gl_pathv: ptr::null_mut(), gl_offsL: 0};
        let flags = GLOB_ERR | GLOB_BRACE | GLOB_NOMAGIC | GLOB_TILDE_CHECK;
        is_ok = 0 == glob(pattern_cstring.as_ptr(), flags, ptr::null(), &mut theglob as *mut glob_t);
        if is_ok {
            ret.reserve(theglob.gl_pathc);
            for i in 0..theglob.gl_pathc {
                let raw_cstr = *theglob.gl_pathv.offset(i.try_into().unwrap());
                let cstr = CStr::from_ptr(raw_cstr);
                let osstr = OsStr::from_bytes(cstr.to_bytes());
                ret.push(osstr.to_os_string());
            }
        }
        globfree(&mut theglob);
    }
    return if is_ok {
        Ok(ret)
    } else {
        Err(KlisterRTE::new("Glob failed", true))
    }
}
