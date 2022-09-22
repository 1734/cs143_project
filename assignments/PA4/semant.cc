

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
std::map<Symbol, std::map<Symbol, std::pair<Symbol, Formals>>> map_class_to_map_method_to_types;
std::map<Symbol, std::map<Symbol, Symbol>> map_class_to_map_attr_to_type;
Symbol current_class_name;

TreeRelation<Symbol>* class_tree_handler = NULL;

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
            assert(dynamic_cast<class__class*>(classes->nth(i)) && "The object pointed is not of type class__class!");
            map_symbol_to_class[current_class_name] = classes->nth(i);
        }
    }

    // Create the class inheritance graph.
    for (auto const& e : map_symbol_to_class) {
        Symbol current_name = e.first;
        Symbol parent_name = e.second->get_parent();
        if (current_name != Object && map_symbol_to_class.find(parent_name) == map_symbol_to_class.end()) {
            semant_error(e.second) << "Error! The parent class " << parent_name << " is not defined!" << std::endl;
            continue;
        }
        if (parent_name == Int || parent_name == Str || parent_name == Bool) {
            semant_error(e.second) << "Class " << current_name << " cannot inherit class " << parent_name << "." << std::endl;
            continue;
        }
        map_to_parent[current_name] = parent_name;
        map_to_children[parent_name].push_back(current_name);
        map_to_color[current_name] = WHITE;
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
    std::vector<Symbol>* cycle_ptr = get_graph_cycle();
    if (cycle_ptr) {
        auto it = cycle_ptr->begin();
        semant_error(map_symbol_to_class[*it]) << "Class " << (*it);
        ++it;
        for (; it != cycle_ptr->end(); ++it) {
            error_stream << " inherits\n";
            semant_error(map_symbol_to_class[*it]) << "Class " << (*it);
        }
        error_stream << "\nforms an inheritance cycle.\n";
        delete cycle_ptr;
    }
}

// Traverse class inheritance graph to check for cycle.
std::vector<Symbol>* ClassTable::get_graph_cycle() {
    for (auto const& e : map_symbol_to_class) {
        map_to_color[e.first] = WHITE;
    }
    for (auto const& e : map_symbol_to_class) {
        if (map_to_color[e.first] == WHITE) {
            std::vector<Symbol>* cycle_ptr = get_graph_cycle(e.first);
            if (cycle_ptr) {
                return cycle_ptr;
            }
        }
    }
    return NULL;
}

std::vector<Symbol>* ClassTable::get_graph_cycle(Symbol current_name) {
    map_to_color[current_name] = GREY;
    for (const auto& child_name : map_to_children[current_name]) {
        if (map_to_color[child_name] == WHITE) {
            std::vector<Symbol>* cycle_ptr = get_graph_cycle(child_name);
            if (cycle_ptr) {
                if(cycle_ptr->front() != cycle_ptr->back()) {
                    cycle_ptr -> push_back(current_name);
                }
                return cycle_ptr;
            }
        } else if (map_to_color[child_name] == GREY) {
            std::vector<Symbol>* cycle_ptr = new std::vector<Symbol>;
            cycle_ptr -> push_back(child_name);
            cycle_ptr -> push_back(current_name);
            return cycle_ptr;
        }
    }
    map_to_color[current_name] = BLACK;
    return NULL;
}

// Traverse class inheritance tree to create type mapping for features.
void ClassTable::dfs_inheritance_tree_for_feature_type(Symbol current_class_name) {
    if (map_symbol_to_class.find(current_class_name) == map_symbol_to_class.end()) {
        assert(0 && "current_class_name unknown!");
    }
    Class_ current_class = map_symbol_to_class[current_class_name];
    assert(dynamic_cast<class__class*>(current_class) && "The object pointed by current_class is not of type class__class!");

    std::map<Symbol, std::pair<Symbol, Formals>>* map_parent_method_to_types_ptr = NULL;
    std::map<Symbol, Symbol>* map_parent_attr_to_type_ptr = NULL;
    Symbol parent_class_name = current_class->get_parent();
    if (current_class_name != Object) {
        map_parent_method_to_types_ptr = &map_class_to_map_method_to_types[parent_class_name];
        map_parent_attr_to_type_ptr = &map_class_to_map_attr_to_type[parent_class_name];
    }
    std::map<Symbol, std::pair<Symbol, Formals>>* map_current_method_to_types_ptr = new std::map<Symbol, std::pair<Symbol, Formals>>;
    std::map<Symbol, Symbol>* map_current_attr_to_type_ptr = new std::map<Symbol, Symbol>;

    for (int feature_index = current_class->get_features()->first(); current_class->get_features()->more(feature_index); feature_index = current_class->get_features()->next(feature_index)) {
        Feature current_feature = current_class->get_features()->nth(feature_index);
        method_class* current_method = dynamic_cast<method_class*>(current_feature);
        attr_class* current_attr = dynamic_cast<attr_class*>(current_feature);
        if ((current_method && current_attr) || (!current_method && !current_attr)) {
            assert(0 && "Bad feature ptr!");
        }
        if (current_method) {
            Symbol method_name = current_method->get_name();
            if (map_current_method_to_types_ptr && (map_current_method_to_types_ptr->find(method_name) != map_current_method_to_types_ptr->end())) {
                semant_error(current_class->get_filename(), current_method) << "Method " << method_name << " is multiply defined." << endl;
                continue;
            }
            if (map_parent_method_to_types_ptr && (map_parent_method_to_types_ptr->find(method_name) != map_parent_method_to_types_ptr->end())) {
                Symbol parent_method_return_type = (*map_parent_method_to_types_ptr)[method_name].first;
                Symbol current_method_return_type = current_method->get_return_type();
                if (current_method_return_type != parent_method_return_type) {
                    semant_error(current_class->get_filename(), current_method) << "In redefined method " << method_name << ", return type " << current_method_return_type << " is different from original return type " << parent_method_return_type << "." << endl;
                    continue;
                }
                Formals parent_method_formals = (*map_parent_method_to_types_ptr)[method_name].second;
                Formals current_method_formals = current_method->get_formals();
                if (current_method_formals->len() != parent_method_formals->len()) {
                    semant_error(current_class->get_filename(), current_method) << "Incompatible number of formal parameters in redefined method " << method_name << "." << endl;
                    continue;
                }
                bool continue_flag = false;
                for (int formal_index = current_method_formals->first(); current_method_formals->more(formal_index); formal_index = current_method_formals->next(formal_index)) {
                    Symbol parent_formal_type = parent_method_formals->nth(formal_index)->get_type_decl();
                    Symbol current_formal_type = current_method_formals->nth(formal_index)->get_type_decl();
                    if (current_formal_type != parent_formal_type) {
                        semant_error(current_class->get_filename(), current_method_formals->nth(formal_index)) << "In redefined method " << method_name << " , parameter type " << current_formal_type << " is different from original type " << parent_formal_type << endl;
                        continue_flag = true;
                        break;
                    }
                }
                if (continue_flag) {
                    continue;
                }
            }
            map_current_method_to_types_ptr->insert({method_name, {current_method->get_return_type(), current_method->get_formals()}});
        }
        if (current_attr) {
            Symbol attr_name = current_attr->get_name();
            if (attr_name == self) {
                semant_error(current_class->get_filename(), current_attr) << "'self' cannot be the name of an attribute." << endl;
                continue;
            }
            if (map_parent_attr_to_type_ptr && map_parent_attr_to_type_ptr->find(attr_name) != map_parent_attr_to_type_ptr->end()) {
                semant_error(current_class->get_filename(), current_attr) << "Attribute " << attr_name << " is an attribute of an inherited class." << endl;
                continue;
            }
            if (map_current_attr_to_type_ptr->find(attr_name) != map_current_attr_to_type_ptr->end()) {
                semant_error(current_class->get_filename(), current_attr) << "Attribute " << attr_name << " is multiply defined in class." << endl;
                continue;
            }
            map_current_attr_to_type_ptr->insert({attr_name, current_attr->get_type_decl()});
        }
    }

    map_class_to_map_method_to_types[current_class->get_name()] = std::move(*map_current_method_to_types_ptr);
    if(map_parent_method_to_types_ptr) {
        map_class_to_map_method_to_types[current_class->get_name()].insert(map_parent_method_to_types_ptr->begin(), map_parent_method_to_types_ptr->end());
    }

    map_class_to_map_attr_to_type[current_class->get_name()] = std::move(*map_current_attr_to_type_ptr);
    if(map_parent_attr_to_type_ptr) {
        map_class_to_map_attr_to_type[current_class->get_name()].insert(map_parent_attr_to_type_ptr->begin(), map_parent_attr_to_type_ptr->end());
    }

    for (const auto child_class_name : map_to_children[current_class_name]) {
        dfs_inheritance_tree_for_feature_type(child_class_name);
    }
}

void ClassTable::check_main() {
    if (map_symbol_to_class.find(Main) == map_symbol_to_class.end()) {
        semant_error() << "Class Main is not defined." << endl;
        return;
    }
    if (map_class_to_map_method_to_types[Main].find(main_meth) == map_class_to_map_method_to_types[Main].end()) {
        semant_error(map_symbol_to_class[Main]) << "No 'main' method in class Main." << endl;
        return;
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

ostream& ClassTable::semant_error(tree_node *t)
{
    error_stream << map_symbol_to_class[current_class_name]->get_filename() << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

Symbol& get_instan_self_type(Symbol& type_name) {
    if (type_name == SELF_TYPE) {
        return current_class_name;
    }
    return type_name;
}

bool ClassTable::no_conform_to(Symbol ancestor_class_name, Symbol child_class_name) {
    if (class_tree_handler
        && (map_symbol_to_class.find(get_instan_self_type(ancestor_class_name)) != map_symbol_to_class.end())
        && (map_symbol_to_class.find(get_instan_self_type(child_class_name)) != map_symbol_to_class.end())
        && (class_tree_handler->get_lca(get_instan_self_type(ancestor_class_name), get_instan_self_type(child_class_name)) != get_instan_self_type(ancestor_class_name))
    ) {
        return true;
    }
    return false;
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

    classtable->dfs_inheritance_tree_for_feature_type(Object);

    classtable->check_main();

    class_tree_handler = new TreeRelation<Symbol>(Object, classtable->map_to_parent, classtable->map_to_children);

    for (int index_class = classes->first(); classes->more(index_class); index_class = classes->next(index_class)) {
        Class_ current_class = classes->nth(index_class);
        current_class->type_check(classtable);
    }

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

}

void class__class::type_check(ClassTable* classtable) {
    object_name_to_type_table.enterscope();
    current_class_name = name;
    for (auto& e : map_class_to_map_attr_to_type[name]) {
        object_name_to_type_table.addid(e.first, &(e.second));
    }
    for (int index_feature = features->first(); features->more(index_feature); index_feature = features->next(index_feature)) {
        Feature current_feature = features->nth(index_feature);
        method_class* current_method = dynamic_cast<method_class*>(current_feature);
        attr_class* current_attr = dynamic_cast<attr_class*>(current_feature);
        if ((current_method && current_attr) || (!current_method && !current_attr)) {
            assert(0 && "Bad feature ptr!");
        }
        if (current_method) {
            current_method->type_check(classtable);
        } else if (current_attr) {
            current_attr->type_check(classtable);
        }
    }
    object_name_to_type_table.exitscope();
}

void method_class::type_check(ClassTable* classtable) {
    object_name_to_type_table.enterscope();
    object_name_to_type_table.addid(self, &SELF_TYPE);
    for (int index_formal = get_formals()->first(); get_formals()->more(index_formal); index_formal = get_formals()->next(index_formal)) {
        Formal current_formal = get_formals()->nth(index_formal);
        current_formal->type_check(classtable);
    }
    Symbol instan_return_type = get_instan_self_type(return_type);
    if (classtable->map_symbol_to_class.find(instan_return_type) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Undefined return type " << instan_return_type << " in method " << name << "." << endl;
    }
    Symbol body_expr_return_type = expr->type_check(classtable);
    if (classtable->no_conform_to(return_type, body_expr_return_type)) {
        classtable->semant_error(this) << "Inferred return type " << body_expr_return_type << " of method " << name << " does not conform to declared return type " << return_type << "." << endl;
    }
    object_name_to_type_table.exitscope();
}

void attr_class::type_check(ClassTable* classtable) {
    object_name_to_type_table.enterscope();
    object_name_to_type_table.addid(self, &SELF_TYPE);
    if (classtable->map_symbol_to_class.find(get_instan_self_type(type_decl)) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Class " << type_decl << " of attribute " << name << " is undefined." << endl;
    }
    Symbol init_expr_type = init->type_check(classtable);
    if (init_expr_type != No_type /* No_type means the expr type is no_expr_class, e.g. the init expr is empty. */ && classtable->no_conform_to(type_decl, init_expr_type)) {
        classtable->semant_error(this) << "Inferred type " << init_expr_type << " of initialization of attribute " << name << " does not conform to declared type " << type_decl << "." << endl;
    }
    object_name_to_type_table.exitscope();
}

void formal_class::type_check(ClassTable* classtable) {
    if (type_decl == SELF_TYPE) {
        classtable->semant_error(this) << "Formal parameter " << name << " cannot have type SELF_TYPE." << endl;
    }else if (classtable->map_symbol_to_class.find(type_decl) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Class " << type_decl << " of formal parameter " << name << " is undefined." << endl;
    }

    if (name == self) {
        classtable->semant_error(this) << "'self' cannot be the name of a formal parameter." << endl;
    } else if (object_name_to_type_table.probe(name)) {
        classtable->semant_error(this) << "Formal parameter " << name << " is multiply defined." << endl;
    } else {
        object_name_to_type_table.addid(name, &type_decl);
    }
}

Symbol typcase_class::type_check(ClassTable* classtable) {
    expr->type_check(classtable);
    std::vector<Symbol> cases_types;
    for (int index_case = cases->first(); cases->more(index_case); index_case = cases->next(index_case)) {
        cases_types.push_back(cases->nth(index_case)->type_check(classtable));
    }
    Symbol result_type = class_tree_handler->get_lca_wrapper(cases_types);
    set_type(result_type);
    return result_type;
}

Symbol branch_class::type_check(ClassTable* classtable) {
    object_name_to_type_table.enterscope();
    if (classtable->map_symbol_to_class.find(type_decl) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Class " << type_decl << " of case branch is undefined." << endl;
    }
    object_name_to_type_table.addid(name, &type_decl);
    Symbol result_type = expr->type_check(classtable);
    object_name_to_type_table.exitscope();
    return result_type;
}

Symbol dispatch_class::type_check(ClassTable* classtable) {
    Symbol result_type = No_type;
    Symbol expr_type = expr->type_check(classtable);
    std::vector<Symbol> arguments_types;
    for (int index_argument = actual->first(); actual->more(index_argument); index_argument = actual->next(index_argument)) {
        arguments_types.push_back(actual->nth(index_argument)->type_check(classtable));
    }
    if (classtable->map_symbol_to_class.find(get_instan_self_type(expr_type)) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Dispatch on undefined class " << expr_type << "." << endl;
    } else {
        auto& map_method_types = map_class_to_map_method_to_types[get_instan_self_type(expr_type)];
        if (map_method_types.find(name) == map_method_types.end()) {
            classtable->semant_error(this) << "Dispatch to undefined method " << name << "." << endl;
        } else {
            auto& method_types = map_method_types[name];
            result_type = get_instan_self_type(method_types.first);
            if (arguments_types.size() != (std::vector<Entry*>::size_type)(method_types.second)->len()) {
                classtable->semant_error(this) << "Method " << name << " called with wrong number of arguments." << endl;
            } else {
                for (int index_parameter = method_types.second->first(); method_types.second->more(index_parameter); index_parameter = method_types.second->next(index_parameter)) {
                    Symbol argument_type = arguments_types[index_parameter];
                    Symbol parameter_type = method_types.second->nth(index_parameter)->get_type_decl();
                    Symbol parameter_name = method_types.second->nth(index_parameter)->get_name();
                    if (classtable->no_conform_to(parameter_type, argument_type)) {
                        classtable->semant_error(this) << "In call of method " << name << ", type " << argument_type << " of parameter " << parameter_name << " does not conform to declared type " << parameter_type << "." << endl;
                    }
                }
            }
        }
    }
    if (result_type == No_type) {
        result_type = Object;
    }
    set_type(result_type);
    return result_type;
}

Symbol static_dispatch_class::type_check(ClassTable* classtable) {
    Symbol result_type = No_type;
    Symbol expr_type = expr->type_check(classtable);
    std::vector<Symbol> arguments_types;
    for (int index_argument = actual->first(); actual->more(index_argument); index_argument = actual->next(index_argument)) {
        arguments_types.push_back(actual->nth(index_argument)->type_check(classtable));
    }
    if (type_name == SELF_TYPE) {
        classtable->semant_error(this) << "Static dispatch to SELF_TYPE." << endl;
    }
    else if (classtable->map_symbol_to_class.find(type_name) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Static dispatch to undefined class " << type_name << "." << endl;
    } else {
        if (classtable->no_conform_to(type_name, expr_type)) {
            classtable->semant_error(this) << "Expression type " << expr_type << " does not conform to declared static dispatch type " << type_name << "." << endl;
        } else {
            auto& map_method_types = map_class_to_map_method_to_types[type_name];
            if (map_method_types.find(name) == map_method_types.end()) {
                classtable->semant_error(this) << "Static dispatch to undefined method " << name << "." << endl;
            } else {
                auto& method_types = map_method_types[name];
                result_type = get_instan_self_type(method_types.first);
                if (arguments_types.size() != (std::vector<Entry*>::size_type)(method_types.second)->len()) {
                    classtable->semant_error(this) << "Method " << name << " invoked with wrong number of arguments." << endl;
                } else {
                    for (int index_parameter = method_types.second->first(); method_types.second->more(index_parameter); index_parameter = method_types.second->next(index_parameter)) {
                        Symbol argument_type = arguments_types[index_parameter];
                        Symbol parameter_type = method_types.second->nth(index_parameter)->get_type_decl();
                        Symbol parameter_name = method_types.second->nth(index_parameter)->get_name();
                        if (classtable->no_conform_to(parameter_type, argument_type)) {
                            classtable->semant_error(this) << "In call of method " << name << ", type " << argument_type << " of parameter " << parameter_name << " does not conform to declared type " << parameter_type << "." << endl;
                        }
                    }
                }
            }
        }
    }
    if (result_type == No_type) {
        result_type = Object;
    }
    set_type(result_type);
    return result_type;
}

Symbol block_class::type_check(ClassTable* classtable) {
    assert(body->len()>=1 && "Block body expressions number: body->len() is smaller than 1!");
    for (int index_exp = body->first(); body->more(index_exp+1); index_exp = body->next(index_exp)) {
        body->nth(index_exp)->type_check(classtable);
    }
    Symbol result_type = body->nth(body->len()-1)->type_check(classtable);
    set_type(result_type);
    return result_type;
}

Symbol let_class::type_check(ClassTable* classtable) {
    object_name_to_type_table.enterscope();
    if (classtable->map_symbol_to_class.find(get_instan_self_type(type_decl)) == classtable->map_symbol_to_class.end()) {
        classtable->semant_error(this) << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined." << endl;
    }
    Symbol init_type = init->type_check(classtable);
    if (init_type != No_type /* No_type means the expr type is no_expr_class, e.g. the init expr is empty. */ && classtable->no_conform_to(type_decl, init_type)) {
        classtable->semant_error(this) << "Inferred type " << init_type << " of initialization of " << identifier << " does not conform to identifier's declared type " << type_decl << "." << endl;
    }
    if (identifier == self) {
        classtable->semant_error(this) << "'self' cannot be bound in a 'let' expression." << endl;
    } else {
        object_name_to_type_table.addid(identifier, &(get_instan_self_type(type_decl)));
    }
    Symbol result_type = body->type_check(classtable);
    object_name_to_type_table.exitscope();
    return result_type;
}

Symbol new__class::type_check(ClassTable*) {
    set_type(type_name);
    return type_name;
}

Symbol object_class::type_check(ClassTable* classtable) {
    Symbol* object_type_ptr = object_name_to_type_table.lookup(name);
    if (object_type_ptr == NULL) {
        classtable->semant_error(this) << "Undeclared identifier " << name << "." << endl;
        return Object;
    }
    set_type(*object_type_ptr);
    return *object_type_ptr;
}

Symbol bool_const_class::type_check(ClassTable* classtable) {
    set_type(Bool);
    return Bool;
}

Symbol int_const_class::type_check(ClassTable* classtable) {
    set_type(Int);
    return Int;
}

Symbol string_const_class::type_check(ClassTable* classtable) {
    set_type(Str);
    return Str;
}

Symbol no_expr_class::type_check(ClassTable* classtable) {
    set_type(No_type);
    return No_type;
}

Symbol assign_class::type_check(ClassTable* classtable) {
    if (name == self) {
        classtable->semant_error(this) << "Cannot assign to 'self'." << endl;
    }
    Symbol* var_type_ptr = object_name_to_type_table.lookup(name);
    if (var_type_ptr == NULL) {
        classtable->semant_error(this) << "Assignment to undeclared variable " << name << "." << endl;
    }
    Symbol expr_type = expr->type_check(classtable);
    if (var_type_ptr && classtable->no_conform_to(*var_type_ptr, expr_type)) {
        classtable->semant_error(this) << "Type " << expr_type << " of assigned expression does not conform to declared type " << *var_type_ptr << " of identifier " << name << "." << endl;
    }
    set_type(expr_type);
    return expr_type;
}

Symbol cond_class::type_check(ClassTable* classtable) {
    if (pred->type_check(classtable) != Bool) {
        classtable->semant_error(this) << "Predicate of 'if' does not have type Bool." << endl;
    }
    Symbol then_exp_type = then_exp->type_check(classtable);
    Symbol else_exp_type = else_exp->type_check(classtable);
    Symbol result_type = class_tree_handler->get_lca_wrapper({then_exp_type, else_exp_type});
    return result_type;
}

Symbol loop_class::type_check(ClassTable* classtable) {
    if (pred->type_check(classtable) != Bool) {
        classtable->semant_error(this) << "Loop condition does not have type Bool." << endl;
    }
    body->type_check(classtable);
    set_type(Object);
    return Object;
}

Symbol plus_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " + " << e2_type << endl;
    }
    set_type(Int);
    return Int;
}

Symbol sub_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " - " << e2_type << endl;
    }
    set_type(Int);
    return Int;
}

Symbol mul_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " * " << e2_type << endl;
    }
    set_type(Int);
    return Int;
}

Symbol divide_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " / " << e2_type << endl;
    }
    set_type(Int);
    return Int;
}

Symbol neg_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    if (e1_type != Int) {
        classtable->semant_error(this) << "Argument of '~' has type " << e1_type << " instead of Int." << endl;
    }
    set_type(Int);
    return Int;
}

Symbol lt_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " < " << e2_type << endl;
    }
    set_type(Bool);
    return Bool;
}

Symbol leq_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " <= " << e2_type << endl;
    }
    set_type(Bool);
    return Bool;
}

Symbol eq_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    Symbol e2_type = e2->type_check(classtable);
    if ((e1_type == Int || e2_type == Int || e1_type == Str || e2_type == Str || e1_type == Bool || e2_type == Bool)
        && e1_type != e2_type)
    {
        classtable->semant_error(this) << "Illegal comparison with a basic type." << endl;
    }
    set_type(Bool);
    return Bool;
}

Symbol comp_class::type_check(ClassTable* classtable) {
    Symbol e1_type = e1->type_check(classtable);
    if (e1_type != Bool) {
        classtable->semant_error(this) << "Argument of 'not' has type " << e1_type << " instead of Bool." << endl;
    }
    set_type(Bool);
    return Bool;
}

Symbol isvoid_class::type_check(ClassTable* classtable) {
    e1->type_check(classtable);
    set_type(Bool);
    return Bool;
}
