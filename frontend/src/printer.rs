pub enum Doc {
    Text(String),
    Lines(Vec<Doc>),
    Indent(Box<Doc>),
    Concat(Box<Doc>, Box<Doc>),
}

impl Doc {
    pub fn text(s: &str) -> Doc {
        Doc::Text(s.to_string())
    }

    pub fn indent(self) -> Doc {
        Doc::Indent(Box::new(self))
    }

    pub fn concat(self, d2: Doc) -> Doc {
        Doc::Concat(Box::new(self), Box::new(d2))
    }

    pub fn render(&self) -> String {
        let mut buf = String::new();
        self.render_to(&mut buf, 0);
        buf
    }

    fn render_to(&self, buf: &mut String, indent: usize) {
        match self {
            Doc::Text(s) => {
                for _ in 0..indent {
                    buf.push(' ');
                }
                buf.push_str(s)
            }
            Doc::Indent(d) => {
                d.render_to(buf, indent + 2);
            }
            Doc::Concat(d1, d2) => {
                d1.render_to(buf, indent);
                d2.render_to(buf, indent);
            }
            Doc::Lines(ds) => {
                for d in ds {
                    buf.push('\n');
                    d.render_to(buf, indent);
                }
            }
        }
    }
}

use std::fmt;

impl fmt::Display for Doc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.render())
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
