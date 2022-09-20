#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>
#include <list>
#include <algorithm>
#include <assert.h>
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream& semant_error(tree_node *t);

  std::map<Symbol, Class_> map_symbol_to_class;
  std::map<Symbol, Symbol> map_to_parent;
  std::map<Symbol, std::vector<Symbol>> map_to_children;
  enum color { WHITE, GREY, BLACK };
  std::map<Symbol, color> map_to_color;

  void check_class_inheritance_graph_for_cycle();
  std::vector<Symbol>* get_graph_cycle();
  std::vector<Symbol>* get_graph_cycle(Symbol);

  void dfs_inheritance_tree_for_feature_type(Symbol);

  void check_main();

};

template<class T>
class TreeRelation {
private:
  T root;
  std::map<T, T> father;
  std::map<T, std::vector<T>> children;
  std::vector<T> all_nodes;

  std::map<T, int> depth;
  std::map<T, int> order; // first order

  int max_lg2_depth;

  std::map<T, std::map<int, T>> ancestor;
  bool initialized_flag = false;

  void dfs_set_depth_order(T current_node, const int current_depth, int& current_order) {
    all_nodes.push_back(current_node);
    depth[current_node] = current_depth;
    order[current_node] = current_order;
    ++current_order;
    for (auto child : children[current_node]) {
      dfs_set_depth_order(child, current_depth+1, current_order);
    }
  }

  int up_bound_lg2(int n) {
    int i = 0;
    while ((1<<i) < n) {
      ++i;
    }
    return i;
  }

  T get_ancestor(T node, int up_step) {
    T current_node = node;
    int i = 0;
    while(current_node != root && (1<<i) <= up_step) {
      if(up_step & (1<<i)) {
        current_node = ancestor[current_node][i];
      }
      i++;
    }
    return current_node;
  }

  void swap(T& node1, T& node2) {
    T node_tmp = node1;
    node1 = node2;
    node2 = node_tmp;
  }

public:
  TreeRelation(T other_root, std::map<T, T> other_father, std::map<T, std::vector<T>> other_children): 
    root(other_root), father(other_father), children(other_children) {}

  void init() {
    int init_order = 1;
    dfs_set_depth_order(root, 0, init_order);

    max_lg2_depth = up_bound_lg2(all_nodes.size()-1);

    ancestor[root][0] = root;
    for (std::vector<Symbol>::size_type i = 1; i < all_nodes.size(); i++) {
      ancestor[all_nodes[i]][0] = father[all_nodes[i]];
    }

    for (const auto& node : all_nodes) {
      for (int i = 1; i <= max_lg2_depth; ++i) {
        ancestor[node][i] = ancestor[ancestor[node][i-1]][i-1];
      }
    }
    initialized_flag = true;
  }

  T get_lca(std::vector<T> nodes) {
    if (!initialized_flag) {
      init();
    }
    assert(nodes.size()>=1 && "nodes.size() smaller than 1!" );
    T node_smallest_order = nodes[0];
    T node_largest_order = nodes[0];
    for (const auto& node : nodes) {
      if(order[node] < node_smallest_order) {
        node_smallest_order = order[node];
      } else if (order[node] > order[node_largest_order]) {
        node_largest_order = order[node];
      }
    }
    return get_lca(node_smallest_order, node_largest_order);
  }

  T get_lca(T node1, T node2) {
    if (!initialized_flag) {
      init();
    }
    if (depth[node1] > depth[node2]) {
      swap(node1, node2);
    }
    node2 = get_ancestor(node2, depth[node2]-depth[node1]);
    if (node1 == node2) {
      return node1;
    }
    for(int i = max_lg2_depth; i >= 0; --i) {
      if(ancestor[node1][i] != ancestor[node2][i]) {
        node1 = ancestor[node1][i];
        node2 = ancestor[node2][i];
      }
    }
    return ancestor[node1][0];
  }

  // This is a wrapper for get_lca.
  // If there exists no known node in the parameter nodes, the result is the last class name in nodes.
  // If there exist(s) known node(s) in the parameter nodes, the result is lca of these known nodes.
  T get_lca_wrapper(const std::vector<T>& nodes) {
      assert(nodes.size()>=1 && "nodes.size() is smaller than 1!");
      std::vector<T> known_nodes;
      for (const auto& node : nodes) {
          if (all_nodes.find(name) != all_nodes.end()) {
              known_nodes.push_back(node);
          }
      }
      if (known_nodes.empty()) {
        return nodes.back();
      }
      return get_lca(known_nodes);
  }
};

extern SymbolTable<Symbol, Symbol> object_name_to_type_table;
extern std::map<Symbol, std::map<Symbol, std::pair<Symbol, Formals>>> map_class_to_map_method_to_types;
extern std::map<Symbol, std::map<Symbol, Symbol>> map_class_to_map_attr_to_type;
extern Symbol current_class_name;

extern TreeRelation<Symbol>* class_tree_handler;

#endif

