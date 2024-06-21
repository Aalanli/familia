
void foo(int a, int* b) {
    a = 1;
    b = 2;
}

void bar(T b) {}

int main() {
    float a;
    int b;
    foo(a, &b);
    bar(a);
}
