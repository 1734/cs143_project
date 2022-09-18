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

extern std::map<Symbol, Class_> map_symbol_to_class;
extern SymbolTable<Symbol, Symbol> object_name_to_type_table;
extern std::map<Symbol, std::map<Symbol, std::pair<Symbol, Formals>>> map_class_to_map_method_to_types;
extern std::map<Symbol, std::map<Symbol, Symbol>> map_class_to_map_attr_to_type;

#endif

