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

#define TRUE 1
#define FALSE 0

typedef struct InheritGraphNode {
  Class_ current_class = NULL;
  InheritGraphNode* parent_node_ptr = NULL;
  std::list<InheritGraphNode*> children_node_ptr_list;
  enum { WHITE, GREY, BLACK } color = WHITE;
}InheritGraphNode;

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

  std::map<Symbol, Class_> map_symbol_to_class;
  std::list<InheritGraphNode> class_graph_node_list;

  void check_class_inheritance_graph_for_cycle();
  std::list<InheritGraphNode*>* get_graph_cycle();
  std::list<InheritGraphNode*>* get_graph_cycle(InheritGraphNode*);

  InheritGraphNode* class_inheritance_tree_node_root_ptr;
  bool dfs_inheritance_tree_for_feature_type(InheritGraphNode*);
};

extern SymbolTable<Symbol, Symbol> object_name_to_type_table;
extern std::map<Class_, std::map<Symbol, std::list<Symbol>>> map_method_to_types;
extern std::map<Class_, std::map<Symbol, Symbol>> map_attr_to_type;

#endif

