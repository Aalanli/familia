use std::{collections::VecDeque, fmt::Display, rc::Rc};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub lhs: Loc,
    pub rhs: Loc,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Loc {
    pub global: usize,
    pub line: u64,
    pub start: u32,
}

impl Default for Loc {
    fn default() -> Self {
        Loc {
            global: 0,
            line: 0,
            start: 0,
        }
    }
}

impl Loc {
    pub fn span(&self, end: Loc) -> Span {
        Span {
            lhs: *self,
            rhs: end,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Symbol {
    name: Box<str>,
}

impl Default for Symbol {
    fn default() -> Self {
        Symbol {
            name: Box::from(""),
        }
    }
}

impl Symbol {
    pub fn from_str(s: &str) -> Self {
        Symbol { name: Box::from(s) }
    }

    pub fn view(&self) -> &str {
        &*self.name
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn get_str(&self) -> &str {
        self.name.view()
    }

    pub fn new(name: &str, span: Span) -> Self {
        Ident {
            name: Symbol::from_str(name),
            span,
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.view())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tok {
    Interface,
    Class,
    Type,
    Fn,
    For,
    Let,
    Return,
    This, // this
    Plus,
    Equal,
    Dot,
    LParen,
    RParen,
    DoubleParen, // ()
    LBrace,
    RBrace,
    Colon,
    SemiColon,
    ColonAcc, // ::
    Comma,
    Ident(Ident),
    Int(i32),
    String(String),
}

#[derive(Debug)]
pub enum LexError {
    UnrecognizedToken(String, Span),
    ParseIntError(String, Span),
    ParseStringError(String, Span),
}

impl LexError {
    pub fn get_span(&self) -> &Span {
        match self {
            LexError::UnrecognizedToken(_, span) => span,
            LexError::ParseIntError(_, span) => span,
            LexError::ParseStringError(_, span) => span,
        }
    }

    pub fn get_message(&self) -> &str {
        match self {
            LexError::UnrecognizedToken(s, _) => s,
            LexError::ParseIntError(s, _) => s,
            LexError::ParseStringError(s, _) => s,
        }
    }
}

type Lex = Result<(Loc, Tok, Loc), LexError>;

pub struct Lexer<T: Iterator<Item = char>> {
    input: T,
    window: Option<char>,
    buf: VecDeque<char>,
    global_pos: usize,
    line_num: u64,
    line_pos: u32,
}

impl<T: Iterator<Item = char>> Lexer<T> {
    pub fn new(input: T) -> Self {
        Lexer {
            input,
            window: None,
            buf: VecDeque::new(),
            global_pos: 0,
            line_num: 1,
            line_pos: 1,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.input.next();
        if let Some(wc) = self.window {
            self.window = c;
            return Some(wc);
        }
        if let Some(c) = c {
            self.window = self.input.next();
            return Some(c);
        }
        None
    }

    fn peek_char(&mut self) -> Option<char> {
        if self.window.is_none() {
            self.window = self.input.next();
        }
        self.window
    }

    /// Fill the buffer while f return true
    fn fill_buf(&mut self, mut f: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek_char() {
            if !f(c) {
                break;
            }
            self.buf.push_back(c);
            self.next_char();
        }
    }

    /// Clear the buffer while f return true
    fn clear_buf(&mut self, mut f: impl FnMut(char) -> bool) {
        while self.buf.len() > 0 {
            let c = self.buf[0];
            if !f(self.buf[0]) {
                break;
            }
            if c == '\n' {
                self.line_num += 1;
                self.line_pos = 1;
            } else {
                self.line_pos += 1;
            }
            self.global_pos += 1;
            self.buf.pop_front();
        }
    }

    fn clear_buf_by(&mut self, count: usize) {
        let mut i = count as isize;
        self.clear_buf(|_| {
            i -= 1;
            i >= 0
        });
    }

    fn match_buf(&self, s: &str) -> bool {
        if self.buf.len() != s.len() {
            return false;
        }
        for (i, c) in s.chars().enumerate() {
            if self.buf[i] != c {
                return false;
            }
        }
        true
    }

    fn match_and_eat(&mut self, s: &str) -> bool {
        if self.match_buf(s) {
            self.clear_buf_by(s.len());
            true
        } else {
            false
        }
    }

    fn fill_buf_by(&mut self, count: usize) {
        let mut i = 0;
        self.fill_buf(|_| {
            i += 1;
            i <= count
        });
    }

    fn fill_ident(&mut self) {
        self.fill_buf(|c| c.is_alphanumeric() || c == '_');
    }

    fn ignore_line(&mut self) {
        self.fill_buf(|c| c != '\n'); // stop when first '\n' is found
        self.fill_buf_by(1); // eat '\n'
        self.clear_buf(|_| true); // clear all characters
    }

    fn ignore_whitespace(&mut self) {
        self.clear_buf(|_| true);
        while let Some(c) = self.peek_char() {
            if c == ' ' {
                self.next_char();
                self.global_pos += 1;
                self.line_pos += 1;
            } else if c == '\n' {
                self.next_char();
                self.global_pos += 1;
                self.line_num += 1;
                self.line_pos = 0;
            } else {
                break;
            }
        }
    }

    fn is_ident(&self) -> bool {
        self.buf[0].is_alphabetic() && self.buf.iter().all(|c| c.is_alphanumeric() || *c == '_')
    }

    fn get_loc(&self) -> Loc {
        Loc {
            global: self.global_pos,
            line: self.line_num,
            start: self.line_pos,
        }
    }

    fn next(&mut self) -> Option<Lex> {
        loop {
            self.ignore_whitespace();
            if self.peek_char().is_none() {
                return None;
            }
            self.fill_buf_by(1);
            let lpos = self.get_loc();
            if self.match_and_eat("\"") {
                let mut s = String::new();
                loop {
                    self.fill_buf(|c| c != '"' && c != '\\');

                    for c in self.buf.iter() {
                        s.push(*c);
                    }
                    self.clear_buf(|_| true);
                    if let Some('"') = self.peek_char() {
                        self.fill_buf_by(1);
                        self.clear_buf_by(1);
                        break;
                    }
                    if let Some('\\') = self.peek_char() {
                        self.fill_buf_by(1);
                        self.clear_buf_by(1);
                        if let Some('"') = self.peek_char() {
                            self.fill_buf_by(1);
                            continue;
                        }
                        if let Some('\\') = self.peek_char() {
                            self.fill_buf_by(1);
                            continue;
                        }
                        if let Some('n') = self.peek_char() {
                            self.fill_buf_by(1);
                            self.clear_buf_by(1);
                            s.push('\n');
                            continue;
                        }
                        let span = lpos.span(self.get_loc());
                        return Some(Err(LexError::ParseStringError(
                            format!("Invalid escape sequence"),
                            span,
                        )));
                    }

                    if self.peek_char().is_none() {
                        let span = lpos.span(self.get_loc());
                        return Some(Err(LexError::ParseStringError(
                            "Unterminated string".to_string(),
                            span,
                        )));
                    }
                }
                return Some(Ok((lpos, Tok::String(s), self.get_loc())));
            } else if self.match_and_eat("{") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::LBrace, rpos)));
            } else if self.match_and_eat("}") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::RBrace, rpos)));
            } else if self.match_and_eat("(") {
                if self.match_and_eat(")") {
                    let rpos = self.get_loc();
                    return Some(Ok((lpos, Tok::DoubleParen, rpos)));
                }
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::LParen, rpos)));
            } else if self.match_and_eat(")") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::RParen, rpos)));
            } else if self.match_and_eat(":") {
                if self.peek_char() == Some(':') {
                    self.fill_buf_by(1);
                    self.clear_buf_by(1);
                    let rpos = self.get_loc();
                    return Some(Ok((lpos, Tok::ColonAcc, rpos)));
                }
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Colon, rpos)));
            } else if self.match_and_eat("+") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Plus, rpos)));
            } else if self.match_and_eat(";") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::SemiColon, rpos)));
            } else if self.match_and_eat(",") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Comma, rpos)));
            } else if self.match_and_eat(".") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Dot, rpos)));
            } else if self.match_and_eat("=") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Equal, rpos)));
            }

            if self.buf[0] == '/' && self.peek_char() == Some('/') {
                self.ignore_line();
                continue;
            }

            self.fill_ident();
            // println!("{:?}", self.buf.iter().collect::<String>());
            if self.match_and_eat("interface") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Interface, rpos)));
            } else if self.match_and_eat("class") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Class, rpos)));
            } else if self.match_and_eat("type") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Type, rpos)));
            } else if self.match_and_eat("fn") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Fn, rpos)));
            } else if self.match_and_eat("let") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Let, rpos)));
            } else if self.match_and_eat("return") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Return, rpos)));
            } else if self.match_and_eat("this") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::This, rpos)));
            } else if self.match_and_eat("for") {
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::For, rpos)));
            } else if self.buf.iter().all(|x| x.is_numeric()) {
                // todo: should change to regex
                let span = lpos.span(self.get_loc());
                let num = self
                    .buf
                    .iter()
                    .collect::<String>()
                    .parse()
                    .map_err(|x| LexError::ParseIntError(format!("{x}"), span));
                self.clear_buf(|_| true);
                if let Err(e) = num {
                    return Some(Err(e));
                }
                let rpos = self.get_loc();
                return Some(Ok((lpos, Tok::Int(num.unwrap()), rpos)));
            } else if self.is_ident() {
                let ident = self.buf.iter().collect::<String>();
                self.clear_buf(|_| true);
                let rpos = self.get_loc();
                return Some(Ok((
                    lpos,
                    Tok::Ident(Ident {
                        name: Symbol::from_str(&ident),
                        span: lpos.span(rpos),
                    }),
                    self.get_loc(),
                )));
            }
            self.clear_buf(|_| true);
            let span = lpos.span(self.get_loc());
            let res = Some(Err(LexError::UnrecognizedToken(
                format!("Unrecognized token"),
                span,
            )));
            self.clear_buf(|_| true);
            return res;
        }
    }
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Lex;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

#[cfg(test)]
mod test_lex {
    #![allow(unused_variables)]

    use super::*;
    #[test]
    fn test_lex1() {
        let input = "fn main() { return 1 + 2; }";

        let lexer = Lexer::new(input.chars());
        for i in lexer {
            i.unwrap();
        }
    }

    #[test]
    fn test_lex2() {
        let input = "\
            type T = {
                a: i32,
                b: i32
            }

            class c1 {
                fn foo(a: T, b: i32): i32 {
                    return a.a + b;
                }
            }

            fn main() {
                c1::foo({a: 1, b: 2}, 3);
            }
        ";

        let lexer = Lexer::new(input.chars());
        for i in lexer {
            i.unwrap();
        }
    }

    #[test]
    fn test_lex3() {
        let input1 = "f124";
        let input2 = "124";
        let input3 = "fn";
        let input4 = "fn1";
        let input5 = "a::b::c3::2";

        let mut lexer1 = Lexer::new(input1.chars());
        if let Tok::Ident(..) = lexer1.next().unwrap().unwrap().1 {
        } else {
            panic!();
        }

        let mut lexer2 = Lexer::new(input2.chars());
        assert!(lexer2.next().unwrap().unwrap().1 == Tok::Int(124));
        let mut lexer3 = Lexer::new(input3.chars());
        assert!(lexer3.next().unwrap().unwrap().1 == Tok::Fn);
        let mut lexer4 = Lexer::new(input4.chars());
        if let Tok::Ident(..) = lexer4.next().unwrap().unwrap().1 {
        } else {
            panic!();
        }

        let mut lexer5 = Lexer::new(input5.chars());
        if let Tok::Ident(..) = lexer5.next().unwrap().unwrap().1 {
        } else {
            panic!();
        }
        assert!(lexer5.next().unwrap().unwrap().1 == Tok::ColonAcc);
    }

    #[test]
    fn test_lex_string() {
        let input = "\"hello world\"";
        let mut lexer = Lexer::new(input.chars());
        let (l, tok, r) = lexer.next().unwrap().unwrap();
        assert!(matches!(tok, Tok::String(s) if s == "hello world"));

        let input = "\"hello \\\"world\\\" \n \"";
        let mut lexer = Lexer::new(input.chars());
        let (l, tok, r) = lexer.next().unwrap().unwrap();
        assert!(matches!(tok, Tok::String(s) if &s == "hello \"world\" \n "));

        let input = "\"test";
        let mut lexer = Lexer::new(input.chars());
        let res = lexer.next().unwrap();
        assert!(matches!(res, Err(LexError::ParseStringError(_, _))));

        let input = "\"test\\\\\"";
        let mut lexer = Lexer::new(input.chars());
        let (l, tok, r) = lexer.next().unwrap().unwrap();
        assert!(matches!(tok, Tok::String(s) if &s == "test\\"));

        let input = "\"test\\n\"";
        let mut lexer = Lexer::new(input.chars());
        let (l, tok, r) = lexer.next().unwrap().unwrap();
        assert!(matches!(tok, Tok::String(s) if &s == "test\n"));

        let input = "\"test\\\\\\ \"";
        let mut lexer = Lexer::new(input.chars());
        let res = lexer.next().unwrap();
        assert!(matches!(res, Err(LexError::ParseStringError(_, _))));

        let input = "\"🥲\"";
        let mut lexer = Lexer::new(input.chars());
        let (l, tok, r) = lexer.next().unwrap().unwrap();
        assert!(matches!(tok, Tok::String(s) if &s == "🥲"));
    }

    #[test]
    fn test_lex_call() {
        let input = "fn main() { to_str(1, 2); }";
        let lexer = Lexer::new(input.chars());
        for i in lexer {
            // println!("{:?}", i);
            i.unwrap();
        }
    }
}
