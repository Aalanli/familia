
int t[] = {1, 2, 3, 4, 5};

class Foo {
public:
    virtual void bar() = 0;
    virtual void baz() = 0;
};

class Foo1 : public Foo {
    void bar() override {
        for (int i = 0; i < 5; i++) {
            t[i] += 1;
        }
    }
    void baz() override {
        for (int i = 0; i < 5; i++) {
            t[i] = 0;
        }
    }
};

void foo(Foo *f) {
    f->bar();
    f->baz();
}