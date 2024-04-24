
#[derive(Clone)]
#[derive(Debug)]
pub struct KlisterRTE {
    s: String
}

impl KlisterRTE {

    pub fn new() -> KlisterRTE {
        KlisterRTE{s: "".to_string()}
    }
    pub fn from_str(s: &str) -> KlisterRTE {
        KlisterRTE{s: s.to_string()}
    }
}
