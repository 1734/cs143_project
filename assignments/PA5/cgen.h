#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <map>
#include <algorithm>
#include <string>
#include <set>
#include <execinfo.h>
#include <stdio.h>

extern std::map<Symbol, std::vector<Symbol>> class_method_order;
typedef std::pair<int, Symbol> T_index_class;
typedef std::map<Symbol, T_index_class> T_map_method;
extern std::map<Symbol, T_map_method> class_method_to_index_class;

extern std::map<Symbol, std::vector<Symbol>> class_attr_order;
extern std::map<Symbol, std::map<Symbol, std::pair<int, Symbol>>> class_attr_to_index_type;

extern SymbolTable<Symbol, Addressing> id_to_addr_table;
extern std::map<Symbol, std::pair<int, int>> class_tag_table;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class TempObjHandler;
extern TempObjHandler* temp_obj_handler_ptr;

extern int label_num;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes(Classes cs);
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void traverse_inheritance_tree_to_build();
   void visit1(CgenNodeP nd);
   void visit2(CgenNodeP nd);

   std::vector<Symbol> class_name_tag_order; // dfs pre-order
   std::vector<Symbol> class_name_pre_order; // dfs pre-order. The brother relations are reversed compared with class_name_tag_order.
   int current_class_tag = 0;
   void code_class_nameTab();
   void code_class_objTab();
   void code_class_dispTabs();
   void code_protObjs();
   void code_object_initializer();
   void code_class_methods();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children = NULL;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

class Addressing
{
public:
   virtual void code_ref(ostream& s) const = 0;
   virtual bool equal_addressing(Addressing* addr) const = 0;
};

// addressing directly using register, for example: $s1
class DirectAddressing : public Addressing
{
private:
   char* reg_name;
public:
   DirectAddressing(char* reg_name_);
   char* get_reg_name() const { return reg_name; }
   virtual void code_ref(ostream& s) const { s << reg_name; }
   virtual bool equal_addressing(Addressing* addr) const;
};

// addressing indirectly using register, for example: 0($fp)
class IndirectAddressing : public Addressing
{
private:
   char* reg_name;
   int offset;
public:
   IndirectAddressing(char* reg_name_, int offset_);
   char* get_reg_name() const { return reg_name; }
   int get_offset() const { return offset; }
   virtual void code_ref(ostream& s) const { s << offset << "(" << reg_name << ")"; }
   virtual bool equal_addressing(Addressing* addr) const;
};

class AddressingTable
{
private:
   std::vector<Addressing*> addressing_ptr_vec;
public:
   Addressing* add_addr(char* reg_name);
   Addressing* add_addr(char* reg_name, int offset);
};

extern AddressingTable addr_table;

class TempObjHandler
{
private:
   int max_temp_number;
   int current_used_temp_number;
   char* temp_register[6] = { "$s1", "$s2", "$s3", "$s4", "$s5", "$s6" };
public:
   TempObjHandler(int max_temp_number_, int current_used_temp_number_) :
      max_temp_number(max_temp_number_), current_used_temp_number(current_used_temp_number_) {}
   int get_max_temp_number() { return max_temp_number; }
   void emit_temp_prepare(ostream &s);
   void emit_temp_restore(ostream &s);
   Addressing* get_next_free_temp_addr() const;
   void occupy_next_free_temp_addr();
   void free_last_occupied_temp_addr();
};
