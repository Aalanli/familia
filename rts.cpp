#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <vector>
#include <iostream>
#include <cstring>
#include <unordered_set>

using namespace std;
extern "C" {

struct TraceConfig {
    int len; // length of subtraces
    // the offsets are relative to the start of the root object
    // the root object + offset should yield a pointer to an instance AllocPtr that was previously allocated
    // by __rts_gc_alloc
    int offsets[1]; // this is a flexible array member
};

// AllocPtr should always be behind a pointer
// a pointer of this type is managed by the garbage collector, and created via __rts_gc_alloc
// config contains a pointer to a TraceConfig, which contains the offsets to other AllocPtrs
struct AllocPtr {
    TraceConfig* config; // nullptr if does not contain subtraces
    char data[1]; // this is a flexible array member
};

}

struct GC {
    unordered_set<AllocPtr*> nodes;
    unordered_set<AllocPtr*> roots;
    ~GC() {
        for (auto node : nodes) {
            free(node);
        }
    }

};

static GC* gc = nullptr;

void add_trace(AllocPtr* node, vector<AllocPtr*> &to_visit) {
    if (node->config == nullptr) {
        return;
    }
    auto root = node->data;
    auto config = node->config;
    for (int i = 0; i < config->len; ++i) {
        auto offset = config->offsets[i];
        auto ref = (AllocPtr*)(root + offset);
        to_visit.push_back(ref);
    }
}


extern "C" {

void __rts_gc_init() {
    gc = new GC();
}

void __rts_gc_destroy() {
    delete gc;
}

AllocPtr* __rts_gc_alloc(TraceConfig* config, int bytes) {
    AllocPtr* node = (AllocPtr*) malloc(offsetof(AllocPtr, data[bytes]));
    node->config = config;
    gc->nodes.insert(node);
    return node;
}

void __rts_gc_add_root(AllocPtr* node) {
    gc->roots.insert(node);
}

void __rts_gc_remove_root(AllocPtr* node) {
    gc->roots.erase(node);
}

void __rts_gc_collect() {
    vector<AllocPtr*> to_visit;
    for (auto root : gc->roots) {
        to_visit.push_back(root);
    }
    unordered_set<AllocPtr*> visited;
    while (!to_visit.empty()) {
        auto node = to_visit.back();
        to_visit.pop_back();
        if (visited.find(node) != visited.end()) {
            continue;
        }
        visited.insert(node);
        add_trace(node, to_visit);
    }
    for (auto it = gc->nodes.begin(); it != gc->nodes.end();) {
        if (visited.find(*it) == visited.end()) {
            free(*it);
            it = gc->nodes.erase(it);
        } else {
            it++;
        }
    }
}

struct rts_string {
    AllocPtr ptr;
};

rts_string* __rts_new_string(int64_t length) {
    auto ptr = __rts_gc_alloc(nullptr, length + 8);
    ((int64_t*) ptr->data)[0] = length;
    return (rts_string*) ptr;
}

int64_t __rts_string_length(const rts_string* str) {
    return ((int64_t*) str->ptr.data)[0];
}

char* __rts_string_data(rts_string* str) {
    return str->ptr.data + sizeof(int64_t);
}

void __rts_print_string(rts_string* str) {
    cout.write(__rts_string_data(str), ((int64_t*) str->ptr.data)[0]);
}

rts_string* __rts_concat_string(rts_string* a, rts_string* b) {
    auto len_a = __rts_string_length(a);
    auto len_b = __rts_string_length(b);
    auto str = __rts_new_string(len_a + len_b);
    auto data = __rts_string_data(str);
    memcpy(data, __rts_string_data(a), len_a);
    memcpy(data + len_a, __rts_string_data(b), len_b);
    return str;
}

rts_string* __rts_int_to_string(int n) {
    string s = to_string(n);
    auto result = __rts_new_string(s.size());
    memcpy(__rts_string_data(result), s.data(), s.size());
    return result;
}

}

