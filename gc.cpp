/*

*/

#include <assert.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;
class GC {
public:
    GC () {
        nodes = unordered_set<char*>();
        trace = unordered_map<char*, vector<char*>>();
    }

    ~GC() {
        for (auto node : nodes) {
            delete node;
        }
    }

    char* new_node(int bytes) {
        char* node = new char[bytes];
        nodes.insert(node);
        return node;
    }

    void delete_node(char* node) {
        // assert(nodes.find(node) != nodes.end());
        // should also make sure that no traces point to this node
        nodes.erase(node);
        delete node;
    }

    void add_trace(char* node, char* ref) {
        // assert(nodes.find(node) != nodes.end());
        if (trace.find(node) == trace.end()) {
            trace[node] = vector<char*>();
        }
        trace[node].push_back(ref);
    }

    void add_root(char* node) {
        assert(nodes.find(node) != nodes.end());
        roots.push_back(node);
    }

    void collect() {
        unordered_set<char*> visited;
        while (!roots.empty()) {
            char* node = roots.back();
            roots.pop_back();
            if (visited.find(node) == visited.end()) {
                continue;
            }
            visited.insert(node);

            if (trace.find(node) != trace.end()) {
                for (auto ref : trace[node]) {
                    visited.insert(ref);
                    roots.push_back(ref);
                }
            }
        }
        for (auto it = nodes.begin(); it != nodes.end();) {
            if (visited.find(*it) == visited.end()) {
                delete *it;
                it = nodes.erase(it);
            } else {
                it++;
            }
        }
        trace.clear();
    }

private:
    vector<char*> roots;
    unordered_set<char*> nodes;
    unordered_map<char*, vector<char*>> trace;

};

extern "C" {
    GC* gc_init() {
        return new GC();
    }

    void gc_destroy(GC* gc) {
        delete gc;
    }

    char* gc_new(GC* gc, int bytes) {
        return gc->new_node(bytes);
    }

    void gc_delete(GC* gc, char* ptr) {
        gc->delete_node(ptr);
    }

    void gc_add_trace(GC* gc, char* node, char* ref) {
        gc->add_trace(node, ref);
    }

    void gc_collect(GC* gc) {
        gc->collect();
    }
}
