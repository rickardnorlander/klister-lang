#![allow(dead_code)]

#[derive(Clone)]
#[derive(Debug)]
#[derive(gc::Trace, gc::Finalize)]
pub struct KlisterRTE {
    pub s: String,
    pub catchable: bool,
}

impl KlisterRTE {
    pub fn new(s: &str, catchable: bool) -> KlisterRTE {
        KlisterRTE{s: s.to_string(), catchable}
    }
}
