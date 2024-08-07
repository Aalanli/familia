#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <sys/types.h>
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
    u_int64_t len; // length of the data
    char data[1]; // this is a flexible array member
};

}

struct GC {
    unordered_set<AllocPtr*> nodes;
    unordered_set<AllocPtr*> roots;
    u_int64_t allocated = 0;
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
    // todo find more elegant solution for exiting success
    exit(0);
}

AllocPtr* __rts_gc_alloc(TraceConfig* config, int bytes) {
    AllocPtr* node = (AllocPtr*) malloc(16 + bytes);
    node->config = config;
    node->len = (u_int64_t) bytes;
    gc->nodes.insert(node);
    gc->allocated += bytes;
    return node;
}

char* __rts_get_data(AllocPtr* node) {
    return node->data;
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
            gc->allocated -= (*it)->len;
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

rts_string* __rts_new_string(int length, const char* data) {
    auto ptr = __rts_gc_alloc(nullptr, length + sizeof(int));
    ((int*) ptr->data)[0] = length;
    if (data != nullptr) {
        memcpy(ptr->data + sizeof(int), data, length);
    }
    return (rts_string*) ptr;
}

int __rts_string_length(const rts_string* str) {
    return ((int*) str->ptr.data)[0];
}

char* __rts_string_data(rts_string* str) {
    return str->ptr.data + sizeof(int);
}

void __rts_string_print(rts_string* str) {
    cout.write(__rts_string_data(str), ((int*) str->ptr.data)[0]);
}

rts_string* __rts_string_concat(rts_string* a, rts_string* b) {
    auto len_a = __rts_string_length(a);
    auto len_b = __rts_string_length(b);
    auto str = __rts_new_string(len_a + len_b, nullptr);
    auto data = __rts_string_data(str);
    memcpy(data, __rts_string_data(a), len_a);
    memcpy(data + len_a, __rts_string_data(b), len_b);
    return str;
}

rts_string* __rts_int_to_string(int n) {
    string s = to_string(n);
    auto result = __rts_new_string(s.size(), s.data());
    return result;
}

}
