
struct T {
    int a;
    float b;
    int c;
    int d;
};

struct R {
    struct T t;
    struct T *r;
    int c;
};

struct T foo(struct T a) {
    a.a = 1;
    a.b = 2.0;
    return a;
}

int bar(struct R a) {
    return a.t.a + 1;
}

int main() {
    struct T t;
    t.a = 1;
    t.b = 2.0;
    foo(t);
}

