

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

SymbolTable<Symbol, Symbol> object_name_to_type_table;
std::map<Symbol, std::map<Symbol, std::list<Symbol>>> map_class_to_map_method_to_types;
std::map<Symbol, std::map<Symbol, Symbol>> map_class_to_map_attr_to_type;


ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */

    install_basic_classes();

    for (int i = classes->first(); classes->more(i); i = classes->next(i) ) {
        Symbol current_class_name = classes->nth(i)->get_name();
        if (current_class_name == SELF_TYPE) {
            semant_error(classes->nth(i)) << "Error! Class " << current_class_name << " redefined!" << std::endl;
        }
        if (map_symbol_to_class.find(current_class_name) != map_symbol_to_class.end()) {
            semant_error(classes->nth(i)) << "Error! Class " << current_class_name << " redefined!" << std::endl;
        } else {
            map_symbol_to_class[current_class_name] = classes->nth(i);
        }
    }

    // Create the class inheritance graph.
    for (auto const& e : map_symbol_to_class) {
        class_graph_node_list.push_back({e.second, NULL, {}, InheritGraphNode::WHITE});
    }
    for (auto it = class_graph_node_list.begin(); it != class_graph_node_list.end(); ++it) {
        Symbol parent_class_name = it->current_class->get_parent();
        if (it->current_class->get_name() == Object) {
            inheritance_tree_node_root_ptr = &(*it);
        } else {
            if (map_symbol_to_class.find(parent_class_name) == map_symbol_to_class.end()) {
                semant_error(it->current_class) << "Error! The parent class " << parent_class_name << " is not defined!" << std::endl;
            } else {
                Class_ parent_class = map_symbol_to_class[parent_class_name];
                it->parent_node_ptr = &(*find_if(class_graph_node_list.begin(), class_graph_node_list.end(), [&] (const InheritGraphNode& node) { return node.current_class == parent_class; }));
                (it->parent_node_ptr->children_node_ptr_list).push_back(&(*it));
            }
        }
    }

}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    map_symbol_to_class = { {Object, Object_class}, {IO, IO_class}, {Int, Int_class}, {Bool, Bool_class}, {Str, Str_class} };
}

void ClassTable::check_class_inheritance_graph_for_cycle() {
    std::list<InheritGraphNode*>* cycle_node_list_ptr = get_graph_cycle();
    if (cycle_node_list_ptr) {
        auto it = cycle_node_list_ptr->begin();
        semant_error((*it)->current_class) << "Class " << (*it)->current_class->get_name();
        ++it;
        for (; it != cycle_node_list_ptr->end(); ++it) {
            error_stream << " inherited by\n";
            semant_error((*it)->current_class) << "Class " << (*it)->current_class->get_name();
        }
        error_stream << "\nforms an inheritance cycle.\n";
        return;
    }
}

// Traverse class inheritance graph to check for cycle.
std::list<InheritGraphNode*>* ClassTable::get_graph_cycle() {
    for (auto it = class_graph_node_list.begin(); it != class_graph_node_list.end(); ++it) {
        it->color = InheritGraphNode::WHITE;
    }
    for (auto it = class_graph_node_list.begin(); it != class_graph_node_list.end(); ++it) {
        if (it->color == InheritGraphNode::WHITE) {
            std::list<InheritGraphNode*>* cycle_node_list_ptr = get_graph_cycle(&(*it));
            if (cycle_node_list_ptr) {
                return cycle_node_list_ptr;
            }
        }
    }
    return NULL;
}

std::list<InheritGraphNode*>* ClassTable::get_graph_cycle(InheritGraphNode* node_ptr) {
    node_ptr->color = InheritGraphNode::GREY;
    for (auto child_ptr : node_ptr->children_node_ptr_list) {
        if (child_ptr->color == InheritGraphNode::WHITE) {
            std::list<InheritGraphNode*>* cycle_node_list_ptr = get_graph_cycle(child_ptr);
            if (cycle_node_list_ptr) {
                if(cycle_node_list_ptr->front() != cycle_node_list_ptr->back()) {
                    cycle_node_list_ptr -> push_back(node_ptr);
                }
                return cycle_node_list_ptr;
            }
        } else if (child_ptr->color == InheritGraphNode::GREY) {
            std::list<InheritGraphNode*>* cycle_node_list_ptr = new std::list<InheritGraphNode*>;
            cycle_node_list_ptr -> push_back(child_ptr);
            cycle_node_list_ptr -> push_back(node_ptr);
            return cycle_node_list_ptr;
        }
    }
    node_ptr->color = InheritGraphNode::BLACK;
    return NULL;
}

// Traverse class inheritance tree to create type mapping for features.
void ClassTable::dfs_inheritance_tree_for_feature_type(InheritGraphNode* node_ptr) {
    if (!node_ptr || !node_ptr->current_class) {
        return;
    }
    Class_ current_class = node_ptr->current_class;
    assert(dynamic_cast<class__class*>(current_class) && "The object pointed by current_class is not of type class__class!");

    std::map<Symbol, std::list<Symbol>>* map_parent_method_to_types_ptr;
    std::map<Symbol, Symbol>* map_parent_attr_to_type_ptr;
    if (node_ptr->parent_node_ptr) {
        Class_ parent_class = node_ptr->parent_node_ptr->current_class;
        map_parent_method_to_types_ptr = &map_class_to_map_method_to_types[parent_class->get_name()];
        map_parent_attr_to_type_ptr = &map_class_to_map_attr_to_type[parent_class->get_name()];
    }
    std::map<Symbol, std::list<Symbol>>* map_current_method_to_types_ptr = new std::map<Symbol, std::list<Symbol>>;
    std::map<Symbol, Symbol>* map_current_attr_to_type_ptr = new std::map<Symbol, Symbol>;

    for (int i = current_class->get_features()->first(); current_class->get_features()->more(i); i = current_class->get_features()->next(i) ) {
        Feature current_feature = current_class->get_features()->nth(i);
        method_class* current_method = dynamic_cast<method_class*>(current_feature);
        attr_class* current_attr = dynamic_cast<attr_class*>(current_feature);
        if ((current_method && current_attr) || (!current_method && !current_attr)) {
            assert(0 && "Bad feature ptr!");
        }
        if (current_method) {
            Symbol method_name = current_method->get_name();
            if (map_parent_method_to_types_ptr && map_parent_method_to_types_ptr->find(method_name) != map_parent_method_to_types_ptr->end()) {
                
            }
        }
        if (current_attr) {
            Symbol attr_name = current_attr->get_name();
            if (map_parent_attr_to_type_ptr && map_parent_attr_to_type_ptr->find(attr_name) != map_parent_attr_to_type_ptr->end()) {
                semant_error(current_class->get_filename(), current_attr) << " Attribute " << attr_name << " is an attribute of an inherited class." << endl;
                continue;
            }
            map_current_attr_to_type_ptr->insert({attr_name, current_attr->get_type_decl()});
        }
    }

    map_class_to_map_attr_to_type[current_class->get_name()] = std::move(*map_current_attr_to_type_ptr);

    for (const auto child_node_ptr : node_ptr->children_node_ptr_list) {
        dfs_inheritance_tree_for_feature_type(child_node_ptr);
    }
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    classtable->check_class_inheritance_graph_for_cycle();
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    classtable->dfs_inheritance_tree_for_feature_type(classtable->inheritance_tree_node_root_ptr);
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}


