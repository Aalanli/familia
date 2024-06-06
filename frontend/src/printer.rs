use std::fmt;

#[derive(Clone)]
pub enum Doc {
    Text(String),
    Lines(Vec<Doc>),
    Indent(Box<Doc>),
    Concat(Box<Doc>, Box<Doc>),
}

pub fn text<S: AsRef<str>>(s: S) -> Doc {
    let s = s.as_ref();
    Doc::Text(s.to_string())
}

pub fn lines(ds: &[Doc]) -> Doc {
    Doc::Lines(ds.to_vec())
}

impl Doc {
    pub fn indent(self) -> Doc {
        Doc::Indent(Box::new(self))
    }

    pub fn concat(self, d2: Doc) -> Doc {
        Doc::Concat(Box::new(self), Box::new(d2))
    }

    pub fn render(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        self.render_to(buf, 0)
    }

    fn render_to(&self, buf: &mut impl fmt::Write, indent: usize) -> fmt::Result {
        match self {
            Doc::Text(s) => {
                for _ in 0..indent {
                    buf.write_char(' ')?;
                }
                buf.write_str(s)?;
            }
            Doc::Indent(d) => {
                d.render_to(buf, indent + 2)?;
            }
            Doc::Concat(d1, d2) => {
                d1.render_to(buf, indent)?;
                d2.render_to(buf, indent)?;
            }
            Doc::Lines(ds) => {
                for d in ds {
                    buf.write_char('\n')?;
                    d.render_to(buf, indent)?;
                }
            }
        }
        Ok(())
    }
}


impl fmt::Display for Doc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.render(f)
    }
}

use std::ops::{Add, Mul};

impl Add for Doc {
    type Output = Doc;

    fn add(self, rhs: Doc) -> Doc {
        Doc::Lines(vec![self, rhs])
    }
}

impl Mul for Doc {
    type Output = Doc;

    fn mul(self, rhs: Doc) -> Doc {
        Doc::Concat(Box::new(self), Box::new(rhs))
    }
}
