
// mutability error
fn foo(a: &str) -> () {
    a = "hello";
}

// lifetime error, local return
fn bar<'a>(a: &'a i32, b: &'a i32) -> &'a i32 {
    if *a > *b {
        a
    } else {
        &(*b + 1)
    }
}

// lifetime error, return value
fn baz<'a, 'b>(a: &'a i32, b: &'b i32) -> &'a i32 {
    if *a > *b {
        a
    } else {
        b
    }
}

// ownership error
fn blah() {
    let mut x = 4;
    let y = &x;
    x = 5;
    let z = *y;
}

fn main() {
    foo("world");
    let x = 5;
    let y = 6;
    let z = bar(&x, &y);
    let a = 7;
    let b = 8;
    let c = baz(&a, &b);
    blah();
}

