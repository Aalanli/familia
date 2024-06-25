#include <string>
#include <iostream>
#include <cstring>

using namespace std;

extern "C" {

struct rts_string {
    char* data;
    int length;
};

void __rts_print_string(const rts_string* str) {
    cout.write(str->data, str->length);
}

rts_string __rts_concat_string(const rts_string* a, const rts_string* b) {
    rts_string result;
    result.length = a->length + b->length;
    result.data = new char[result.length];
    memcpy(result.data, a->data, a->length);
    memcpy(result.data + a->length, b->data, b->length);
    return result;
}

void __rts_free_string(rts_string* str) {
    delete[] str->data;
}

rts_string __rts_int_to_string(int n) {
    string s = to_string(n);
    rts_string result;
    result.length = s.length();
    result.data = new char[result.length];
    memcpy(result.data, s.data(), result.length);
    return result;
}

}

