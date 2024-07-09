

int foo(int a) {
    return a + 1;
}

static int (*foov)(int) = 0;

int main() {
    int (*f)(int) = foo;
    f(1);
    foov = foo;
    foov(2);
}
