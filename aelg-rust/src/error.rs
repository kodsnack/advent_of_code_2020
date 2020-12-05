use std::convert;
use std::fmt;

#[derive(Debug, Clone)]
pub struct AOCError {
    msg: String,
}

impl fmt::Display for AOCError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl convert::From<&str> for AOCError {
    fn from(s: &str) -> Self {
        AOCError {
            msg: String::from(s),
        }
    }
}
