
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include "stringtab_functions.h"
#undef assert
namespace my_namespace {
void assert(bool b);
}

std::map<Symbol, std::vector<Symbol>> class_method_order;
std::map<Symbol, T_map_method> class_method_to_index_class;

std::map<Symbol, std::vector<Symbol>> class_attr_order;
std::map<Symbol, std::map<Symbol, std::pair<int, Symbol>>> class_attr_to_index_type;

SymbolTable<Symbol, Addressing> id_to_addr_table;
AddressingTable addr_table;

std::map<Symbol, std::pair<int, int>> class_tag_table;

TempObjHandler* global_tmp_obj_handler_ptr = NULL;

static int global_label_num = 0;
static char *curr_filename = NULL;
static Symbol global_current_class_name = NULL;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << STRINGNAME << DISPTAB_SUFFIX << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << INTNAME << DISPTAB_SUFFIX << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { my_namespace::assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << BOOLNAME << DISPTAB_SUFFIX << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

void CgenClassTable::code_class_nameTab()
{
  str << CLASSNAMETAB << LABEL;
  for (Symbol class_name : class_name_tag_order) {
    str << WORD;
    (stringtable.lookup_string(class_name->get_string()))->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_class_objTab()
{
  str << CLASSOBJTAB << LABEL;
  for (Symbol class_name : class_name_tag_order) {
    str << WORD;
    emit_protobj_ref(class_name, str);
    str << endl;
    str << WORD;
    emit_init_ref(class_name, str);
    str << endl;
  }
}

void CgenClassTable::code_class_dispTabs()
{
  for (Symbol class_name : class_name_pre_order) {
    str << class_name << DISPTAB_SUFFIX << LABEL;
    for (Symbol method_name : class_method_order[class_name]) {
      str << WORD << class_method_to_index_class[class_name][method_name].second << "." << method_name << endl;
    }
  }
}

void CgenClassTable::code_protObjs()
{
  for (Symbol class_name : class_name_pre_order) {
    str << WORD << "-1" << endl;
    str << class_name << PROTOBJ_SUFFIX << LABEL;
    str << WORD << class_tag_table[class_name].first << endl;
    str << WORD << class_attr_order[class_name].size()+3 << endl;
    str << WORD << class_name << DISPTAB_SUFFIX << endl;
    for (Symbol attr_name : class_attr_order[class_name]) {
      Symbol current_attr_type = class_attr_to_index_type[class_name][attr_name].second;
      if (current_attr_type == Int) {
        str << WORD;
        inttable.lookup_string("0")->code_ref(str); // default value of Int
        str << endl;
      } else if (current_attr_type == Bool) {
        str << WORD;
        falsebool.code_ref(str); // default value of Bool
        str << endl;
      } else if (current_attr_type == Str) {
        str << WORD;
        stringtable.lookup_string("")->code_ref(str); // default value of String
        str << endl;
      } else {
        str << WORD << "0" << endl;
      }
    }
  }
}

void CgenClassTable::code_object_initializer()
{
  for (Symbol class_name : class_name_pre_order) {
    id_to_addr_table.enterscope();
    id_to_addr_table.addid(self, addr_table.add_addr(SELF));
    CgenNodeP current_class_gen_ptr = lookup(class_name);
    Features current_features = current_class_gen_ptr->features;
    Expressions init_assign_exprs = NULL;
    for (int feature_index = current_features->first(); current_features->more(feature_index); feature_index = current_features->next(feature_index)) {
      Feature current_feature = current_features->nth(feature_index);
      attr_class* current_attr = dynamic_cast<attr_class*>(current_feature);
      if (!current_attr || dynamic_cast<no_expr_class*>(current_attr->init)) {
        continue;
      }
      if (init_assign_exprs == NULL) {
        init_assign_exprs = single_Expressions(assign(current_attr->name, current_attr->init));
      } else {
        init_assign_exprs = append_Expressions(init_assign_exprs, single_Expressions(assign(current_attr->name, current_attr->init)));
      }
    }
    // Add "self;" at the end of exprs.
    if (init_assign_exprs == NULL) {
      init_assign_exprs = single_Expressions(object(self));
    } else {
      init_assign_exprs = append_Expressions(init_assign_exprs, single_Expressions(object(self)));
    }
    Expression method_expr = block(init_assign_exprs);
    int current_method_max_temp_number = method_expr->get_max_temp_number();
    global_tmp_obj_handler_ptr = new TempObjHandler(current_method_max_temp_number, 0);
    auto current_attr_to_index_type = class_attr_to_index_type[class_name];
    for (Symbol attr_name : class_attr_order[class_name]) {
      int attr_index = current_attr_to_index_type[attr_name].first;
      int offset = 3+attr_index; // 0:class_tag 1:size 2:dispath_table 3:attr_0
      id_to_addr_table.addid(attr_name, addr_table.add_addr(SELF, offset));
    }
    emit_init_ref(class_name, str);
    str << ":" << endl;
    emit_addiu(SP, SP, -(3+current_method_max_temp_number)*WORD_SIZE, str); // move down SP. "3" stands for FP, self and return address
    emit_store(FP, 3+current_method_max_temp_number, SP, str); // store old FP in current frame stack
    emit_store(SELF, 2+current_method_max_temp_number, SP, str); // store old self object in current frame stack
    emit_store(RA, 1+current_method_max_temp_number, SP, str); // store return address of current method in current frame stack
    emit_addiu(FP, SP, WORD_SIZE, str); // FP = SP - 4
    emit_move(SELF, ACC, str); // store current self in $s0
    global_tmp_obj_handler_ptr->emit_temp_prepare(str);
    if (std::find(class_name_pre_order.begin(), class_name_pre_order.end(), current_class_gen_ptr->get_parent()) != class_name_pre_order.end()) {
      str << JAL << current_class_gen_ptr->get_parent() << CLASSINIT_SUFFIX << endl;
    }
    method_expr->code(str, addr_table.add_addr(ACC));
    global_tmp_obj_handler_ptr->emit_temp_restore(str);
    emit_load(FP, 3+current_method_max_temp_number, SP, str); // restore old FP from current frame stack
    emit_load(SELF, 2+current_method_max_temp_number, SP, str); // restore old self object from current frame stack
    emit_load(RA, 1+current_method_max_temp_number, SP, str); // restore return address of old method from current frame stack
    emit_addiu(SP, SP, (3+current_method_max_temp_number)*WORD_SIZE, str); // move up SP. "3" stands for FP, self and return address
    emit_return(str);
    delete global_tmp_obj_handler_ptr;
    id_to_addr_table.exitscope();
  }
}

void CgenClassTable::code_class_methods()
{
  for (Symbol class_name : class_name_pre_order) {
    global_current_class_name = class_name;
    CgenNodeP current_class_gen_ptr = lookup(class_name);
    if (current_class_gen_ptr->basic()) {
      continue;
    }
    curr_filename = current_class_gen_ptr->get_filename()->get_string();
    id_to_addr_table.enterscope();
    id_to_addr_table.addid(self, addr_table.add_addr(SELF));
    auto current_attr_to_index_type = class_attr_to_index_type[class_name];
    for (Symbol attr_name : class_attr_order[class_name]) {
      int attr_index = current_attr_to_index_type[attr_name].first;
      int offset = 3+attr_index; // 0:class_tag 1:size 2:dispath_table 3:attr_0
      id_to_addr_table.addid(attr_name, addr_table.add_addr(SELF, offset));
    }
    Features current_features = current_class_gen_ptr->features;
    for (int feature_index = current_features->first(); current_features->more(feature_index); feature_index = current_features->next(feature_index)) {
      Feature current_feature = current_features->nth(feature_index);
      method_class* current_method = dynamic_cast<method_class*>(current_feature);
      if (!current_method) {
        continue;
      }
      id_to_addr_table.enterscope();
      str << class_name << "." << current_method->name << ":" << endl;
      int current_method_max_temp_number = current_method->expr->get_max_temp_number();
      global_tmp_obj_handler_ptr = new TempObjHandler(current_method_max_temp_number, 0);
      emit_addiu(SP, SP, -(3+current_method_max_temp_number)*WORD_SIZE, str); // move down SP. "3" stands for FP, self and return address
      emit_store(FP, 3+current_method_max_temp_number, SP, str); // store old FP in current frame stack
      emit_store(SELF, 2+current_method_max_temp_number, SP, str); // store old self object in current frame stack
      emit_store(RA, 1+current_method_max_temp_number, SP, str); // store return address of current method in current frame stack
      emit_addiu(FP, SP, WORD_SIZE, str); // FP = SP - 4
      emit_move(SELF, ACC, str); // store current self in $s0
      global_tmp_obj_handler_ptr->emit_temp_prepare(str);
      Formals current_formals = current_method->formals;
      for (int formal_index = current_formals->first(); current_formals->more(formal_index); formal_index = current_formals->next(formal_index)) {
        formal_class* current_formal = dynamic_cast<formal_class*>(current_formals->nth(formal_index));
        my_namespace::assert(current_formal && "current_formal should not be NULL");
        int offset = 3+current_method_max_temp_number+current_formals->len()-formal_index-1;
        id_to_addr_table.addid(current_formal->name, addr_table.add_addr(FP, offset));
      }
      current_method->expr->code(str, addr_table.add_addr(ACC));
      global_tmp_obj_handler_ptr->emit_temp_restore(str);
      emit_load(FP, 3+current_method_max_temp_number, SP, str); // restore old FP from current frame stack
      emit_load(SELF, 2+current_method_max_temp_number, SP, str); // restore old self object from current frame stack
      emit_load(RA, 1+current_method_max_temp_number, SP, str); // restore return address of old method from current frame stack
      emit_addiu(SP, SP, (current_formals->len()+3+current_method_max_temp_number)*WORD_SIZE, str); // move up SP. callee restores the SP shift for argument. "3" stands for FP, self and return address
      emit_return(str);
      delete global_tmp_obj_handler_ptr;
      id_to_addr_table.exitscope();
    }
    id_to_addr_table.exitscope();
  }
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 5 /* Change to your String class tag here */;
   intclasstag =    3 /* Change to your Int class tag here */;
   boolclasstag =   4 /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes(classes);
   install_classes(classes);
   build_inheritance_tree();
   traverse_inheritance_tree_to_build();
   code();
   exitscope();
}

void CgenClassTable::install_basic_classes(Classes cs)
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

  for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
    if (cs->nth(i)->get_name() == Main) {
      install_class(new CgenNode(cs->nth(i),NotBasic,this));
    }
  }

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();
  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

// Install all user defined classes but Main.
void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
    if (cs->nth(i)->get_name() != Main) {
      install_class(new CgenNode(cs->nth(i),NotBasic,this));
    }
  }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  my_namespace::assert(parentnd == NULL);
  my_namespace::assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::traverse_inheritance_tree_to_build()
{
  current_class_tag = 0;
  visit1(root());
  visit2(root());
}

void CgenClassTable::visit1(CgenNodeP nd)
{
  my_namespace::assert(nd != NULL);
  Symbol current_class_name = nd->name;
  Symbol parent_class_name = nd->get_parent();
  Features current_features = nd->features;

  std::vector<Symbol> current_method_order;
  T_map_method current_method_to_index_class;
  std::vector<Symbol> current_attr_order;
  std::map<Symbol, std::pair<int, Symbol>> current_attr_to_index_type;
  if (current_class_name != Object) {
    current_method_order = class_method_order[parent_class_name];
    current_method_to_index_class = class_method_to_index_class[parent_class_name];
    current_attr_order = class_attr_order[parent_class_name];
    current_attr_to_index_type = class_attr_to_index_type[parent_class_name];
  }

  for (int feature_index = current_features->first(); current_features->more(feature_index); feature_index = current_features->next(feature_index)) {
    Feature current_feature = current_features->nth(feature_index);
    method_class* current_method = dynamic_cast<method_class*>(current_feature);
    attr_class* current_attr = dynamic_cast<attr_class*>(current_feature);
    if ((current_method && current_attr) || (!current_method && !current_attr)) {
        my_namespace::assert(0 && "Bad feature ptr!");
    }
    if (current_method) {
      // Override parent method
      if (std::find(current_method_order.begin(), current_method_order.end(), current_method->name) != current_method_order.end()) {
        current_method_to_index_class[current_method->name].second = current_class_name;
      } else {
        current_method_to_index_class[current_method->name] = {current_method_order.size(), current_class_name};
        current_method_order.push_back(current_method->name);
      }
    }
    if (current_attr) {
      current_attr_to_index_type[current_attr->name] = {current_attr_order.size(), current_attr->type_decl};
      current_attr_order.push_back(current_attr->name);
    }
  }

  class_method_order[current_class_name] = std::move(current_method_order);
  class_method_to_index_class[current_class_name] = std::move(current_method_to_index_class);
  class_attr_order[current_class_name] = std::move(current_attr_order);
  class_attr_to_index_type[current_class_name] = std::move(current_attr_to_index_type);

  class_name_tag_order.push_back(current_class_name);
  class_tag_table[current_class_name].first = current_class_tag++;
  stringtable.add_string(current_class_name->get_string());

  List<CgenNode> *current_children_ptr = nd->get_children();
  CgenNodeP current_child_ptr = NULL;
  while(current_children_ptr && (current_child_ptr = current_children_ptr->hd())) {
    visit1(current_child_ptr);
    current_children_ptr = current_children_ptr->tl();
  }
  class_tag_table[current_class_name].second = (current_class_tag - 1);
}

// Generate class_name_pre_order.
void CgenClassTable::visit2(CgenNodeP nd) {
  my_namespace::assert(nd != NULL);
  Symbol current_class_name = nd->name;
  class_name_pre_order.push_back(current_class_name);
  List<CgenNode> *current_children_ptr = nd->get_children();
  CgenNodeP current_child_ptr = NULL;
  // brother nodes' order is the same as their order in install_class, we want to reverse that order.
  std::vector<CgenNodeP> origin_brother_order;
  while(current_children_ptr && (current_child_ptr = current_children_ptr->hd())) {
    origin_brother_order.push_back(current_child_ptr);
    current_children_ptr = current_children_ptr->tl();
  }
  for (auto it = origin_brother_order.rbegin(); it != origin_brother_order.rend(); ++it) {
    visit2(*it);
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  code_class_nameTab();

  code_class_objTab();

  code_class_dispTabs();

  code_protObjs();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  code_object_initializer();

  code_class_methods();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ }


void code_addr1_to_addr2(ostream &s, Addressing* addr1, Addressing* addr2) {
  if (addr1->equal_addressing(addr2)) {
    return;
  }
  DirectAddressing* addr1_direct = dynamic_cast<DirectAddressing*>(addr1);
  IndirectAddressing* addr1_indirect = dynamic_cast<IndirectAddressing*>(addr1);
  DirectAddressing* addr2_direct = dynamic_cast<DirectAddressing*>(addr2);
  IndirectAddressing* addr2_indirect = dynamic_cast<IndirectAddressing*>(addr2);
  my_namespace::assert((addr1_direct || addr1_indirect) && (addr2_direct || addr2_indirect));

  if(addr1_direct && addr2_direct) {
    emit_move(addr2_direct->get_reg_name(), addr1_direct->get_reg_name(), s);
  }

  if(addr1_direct && addr2_indirect) {
    emit_store(addr1_direct->get_reg_name(), addr2_indirect->get_offset(), addr2_indirect->get_reg_name(), s);
  }

  if(addr1_indirect && addr2_direct) {
    emit_load(addr2_direct->get_reg_name(), addr1_indirect->get_offset(), addr1_indirect->get_reg_name(), s);
  }

  if(addr1_indirect && addr2_indirect) {
    emit_load(ACC, addr1_indirect->get_offset(), addr1_indirect->get_reg_name(), s);
    emit_store(ACC, addr2_indirect->get_offset(), addr2_indirect->get_reg_name(), s);
  }
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, Addressing* result_addr) {
  Addressing* id_addr = id_to_addr_table.lookup(name);
  if (dynamic_cast<DirectAddressing*>(id_addr)) {
    expr->code(s, id_addr);
    code_addr1_to_addr2(s, id_addr, result_addr);
  } else if (dynamic_cast<DirectAddressing*>(result_addr)) {
    expr->code(s, result_addr);
    code_addr1_to_addr2(s, result_addr, id_addr);
  } else {
    expr->code(s, addr_table.add_addr(ACC));
    code_addr1_to_addr2(s, addr_table.add_addr(ACC), id_addr);
    code_addr1_to_addr2(s, addr_table.add_addr(ACC), result_addr);
  }
}

int assign_class::get_max_temp_number() {
  return expr->get_max_temp_number();
}

void static_dispatch_class::code(ostream &s, Addressing* result_addr) {
  for (int arg_index = actual->first(); actual->more(arg_index); arg_index = actual->next(arg_index)) {
    actual->nth(arg_index)->code(s, addr_table.add_addr(ACC));
    emit_store(ACC, 0, SP, s);
    emit_addiu(SP, SP, -4, s);
  }
  expr->code(s, addr_table.add_addr(ACC));
  emit_bne(ACC, ZERO, global_label_num, s);
  my_namespace::assert(curr_filename);
  emit_load_string(ACC, stringtable.lookup_string(curr_filename), s);
  emit_load_imm(T1, expr->get_line_number(), s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(global_label_num++, s);
  s << LA << T1 << " ";
  emit_disptable_ref(type_name, s);
  s << endl;
  emit_load(T1, class_method_to_index_class[type_name][name].first, T1, s);
  emit_jalr(T1, s);
  code_addr1_to_addr2(s, addr_table.add_addr(ACC), result_addr);
}

int static_dispatch_class::get_max_temp_number() {
  int result = 0;
  for (int arg_index = actual->first(); actual->more(arg_index); arg_index = actual->next(arg_index)) {
    result = std::max(result, actual->nth(arg_index)->get_max_temp_number());
  }
  return std::max(result, expr->get_max_temp_number());
}

void dispatch_class::code(ostream &s, Addressing* result_addr) {
  for (int arg_index = actual->first(); actual->more(arg_index); arg_index = actual->next(arg_index)) {
    actual->nth(arg_index)->code(s, addr_table.add_addr(ACC));
    emit_store(ACC, 0, SP, s);
    emit_addiu(SP, SP, -4, s);
  }
  expr->code(s, addr_table.add_addr(ACC));
  int tmp_label_num = global_label_num++;
  emit_bne(ACC, ZERO, tmp_label_num, s);
  my_namespace::assert(curr_filename);
  emit_load_string(ACC, stringtable.lookup_string(curr_filename), s);
  emit_load_imm(T1, expr->get_line_number(), s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(tmp_label_num, s);
  emit_load(T1, 2, ACC, s);
  Symbol type_name = expr->type;
  if (type_name == SELF_TYPE) {
    type_name = global_current_class_name;
  }
  my_namespace::assert((class_method_to_index_class.find(type_name) != class_method_to_index_class.end()) &&
         (class_method_to_index_class[type_name].find(name) != class_method_to_index_class[type_name].end()));
  emit_load(T1, class_method_to_index_class[type_name][name].first, T1, s);
  emit_jalr(T1, s);
  code_addr1_to_addr2(s, addr_table.add_addr(ACC), result_addr);
}

int dispatch_class::get_max_temp_number() {
  int result = 0;
  for (int arg_index = actual->first(); actual->more(arg_index); arg_index = actual->next(arg_index)) {
    result = std::max(result, actual->nth(arg_index)->get_max_temp_number());
  }
  return std::max(result, expr->get_max_temp_number());
}

void cond_class::code(ostream &s, Addressing* result_addr) {
  int false_branch_label_num = global_label_num++;
  int if_end_label_num = global_label_num++;
  pred->code(s, addr_table.add_addr(ACC));
  emit_load(T1, 3, ACC, s);
  emit_beqz(T1, false_branch_label_num, s);
  then_exp->code(s, result_addr);
  emit_branch(if_end_label_num, s);
  emit_label_def(false_branch_label_num, s);
  else_exp->code(s, result_addr);
  emit_label_def(if_end_label_num, s);
}

int cond_class::get_max_temp_number() {
  return std::max({pred->get_max_temp_number(), then_exp->get_max_temp_number(), else_exp->get_max_temp_number()});
}

void loop_class::code(ostream &s, Addressing* result_addr) {
  int pred_label_num = global_label_num++;
  int loop_end_label_num = global_label_num++;
  emit_label_def(pred_label_num, s);
  pred->code(s, addr_table.add_addr(ACC));
  emit_load(T1, 3, ACC, s);
  emit_beq(T1, ZERO, loop_end_label_num, s);
  body->code(s, addr_table.add_addr(ACC));
  emit_branch(pred_label_num, s);
  emit_label_def(loop_end_label_num, s);
  emit_move(ACC, ZERO, s);
}

int loop_class::get_max_temp_number() {
  return std::max(pred->get_max_temp_number(), body->get_max_temp_number());
}

void typcase_class::code(ostream &s, Addressing* result_addr) {
  std::vector<branch_class*> cases_vector;
  for (int case_index = cases->first(); cases->more(case_index); case_index = cases->next(case_index)) {
    branch_class* current_branch = dynamic_cast<branch_class*>(cases->nth(case_index));
    my_namespace::assert(current_branch);
    cases_vector.push_back(current_branch);
  }
  std::sort(cases_vector.begin(), cases_vector.end(), [](branch_class* case1, branch_class* case2) -> bool {
    return class_tag_table[case1->type_decl].first > class_tag_table[case2->type_decl].first;
  });
  expr->code(s, addr_table.add_addr(ACC));
  int case_end_label_num = global_label_num++;
  int first_branch_label_num = global_label_num++;
  emit_bne(ACC, ZERO, first_branch_label_num, s);
  my_namespace::assert(curr_filename);
  emit_load_string(ACC, stringtable.lookup_string(curr_filename), s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_case_abort2", s);
  int next_branch_label_num = -1;
  for (std::size_t branch_index = 0; branch_index < cases_vector.size(); ++branch_index) {
    branch_class* current_branch = cases_vector[branch_index];
    if (branch_index == 0) {
      emit_label_def(first_branch_label_num, s);
      emit_load(T2, 0, ACC, s);
    } else {
      emit_label_def(next_branch_label_num, s);
    }
    next_branch_label_num = global_label_num++;
    emit_blti(T2, class_tag_table[current_branch->type_decl].first, next_branch_label_num, s);
    emit_bgti(T2, class_tag_table[current_branch->type_decl].second, next_branch_label_num, s);
    Addressing* id_addr = global_tmp_obj_handler_ptr->get_next_free_temp_addr();
    code_addr1_to_addr2(s, addr_table.add_addr(ACC), id_addr);
    global_tmp_obj_handler_ptr->occupy_next_free_temp_addr();
    id_to_addr_table.enterscope();
    id_to_addr_table.addid(current_branch->name, id_addr);
    current_branch->expr->code(s, result_addr);
    id_to_addr_table.exitscope();
    global_tmp_obj_handler_ptr->free_last_occupied_temp_addr();
    emit_branch(case_end_label_num, s);
  }
  emit_label_def(next_branch_label_num, s);
  emit_jal("_case_abort", s);
  emit_label_def(case_end_label_num, s);
}

int typcase_class::get_max_temp_number() {
  int result = expr->get_max_temp_number();
  for (int case_index = cases->first(); cases->more(case_index); case_index = cases->next(case_index)) {
    branch_class* current_branch = dynamic_cast<branch_class*>(cases->nth(case_index));
    my_namespace::assert(current_branch);
    result = std::max(result, 1 + current_branch->expr->get_max_temp_number());
  }
  return result;
}

void block_class::code(ostream &s, Addressing* result_addr) {
  for (int expr_index = body->first(); body->more(expr_index+1); expr_index = body->next(expr_index)) {
    body->nth(expr_index)->code(s, addr_table.add_addr(ACC));
  }
  body->nth(body->len()-1)->code(s, result_addr);
}

int block_class::get_max_temp_number() {
  int result = 0;
  for (int expr_index = body->first(); body->more(expr_index); expr_index = body->next(expr_index)) {
    result = std::max(result, body->nth(expr_index)->get_max_temp_number());
  }
  return result;
}

void let_class::code(ostream &s, Addressing* result_addr) {
  id_to_addr_table.enterscope();
  Addressing* id_addr = global_tmp_obj_handler_ptr->get_next_free_temp_addr();
  if (!dynamic_cast<no_expr_class*>(init)) {
    init->code(s, id_addr);
  } else {
    DirectAddressing* id_addr_direct = dynamic_cast<DirectAddressing*>(id_addr);
    IndirectAddressing* id_addr_indirect = dynamic_cast<IndirectAddressing*>(id_addr);
    my_namespace::assert(id_addr_direct||id_addr_indirect);
    if (type_decl == Int) {
      if (id_addr_direct) {
        emit_load_int(id_addr_direct->get_reg_name(), inttable.lookup_string("0"), s);
      }
      if (id_addr_indirect) {
        emit_load_int(ACC, inttable.lookup_string("0"), s);
        emit_store(ACC, id_addr_indirect->get_offset(), id_addr_indirect->get_reg_name(), s);
      }
    }
    else if (type_decl == Bool) {
      if (id_addr_direct) {
        emit_load_bool(id_addr_direct->get_reg_name(), BoolConst(0), s);
      }
      if (id_addr_indirect) {
        emit_load_bool(ACC, BoolConst(0), s);
        emit_store(ACC, id_addr_indirect->get_offset(), id_addr_indirect->get_reg_name(), s);
      }
    } else if (type_decl == Str) {
      if (id_addr_direct) {
        emit_load_string(id_addr_direct->get_reg_name(), stringtable.lookup_string(""), s);
      }
      if (id_addr_indirect) {
        emit_load_string(ACC, stringtable.lookup_string(""), s);
        emit_store(ACC, id_addr_indirect->get_offset(), id_addr_indirect->get_reg_name(), s);
      }
    } else {
      if (id_addr_direct) {
        emit_move(id_addr_direct->get_reg_name(), ZERO, s);
      }
      if (id_addr_indirect) {
        emit_store(ZERO, id_addr_indirect->get_offset(), id_addr_indirect->get_reg_name(), s);
      }
    }
  }
  global_tmp_obj_handler_ptr->occupy_next_free_temp_addr();
  id_to_addr_table.addid(identifier, id_addr);
  body->code(s, result_addr);
  global_tmp_obj_handler_ptr->free_last_occupied_temp_addr();
  id_to_addr_table.exitscope();
}

int let_class::get_max_temp_number() {
  return std::max(init->get_max_temp_number(), 1+body->get_max_temp_number());
}

void plus_class::code(ostream &s, Addressing* result_addr) {
  Addressing* e1_result_addr = global_tmp_obj_handler_ptr->get_next_free_temp_addr();
  e1->code(s, e1_result_addr);
  global_tmp_obj_handler_ptr->occupy_next_free_temp_addr();
  e2->code(s, addr_table.add_addr(ACC));
  emit_jal("Object.copy", s);
  DirectAddressing* e1_result_addr_direct = dynamic_cast<DirectAddressing*>(e1_result_addr);
  IndirectAddressing* e1_result_addr_indirect = dynamic_cast<IndirectAddressing*>(e1_result_addr);
  my_namespace::assert(e1_result_addr_direct||e1_result_addr_indirect);
  if (e1_result_addr_direct) {
    emit_load(T2, 3, ACC, s);
    emit_load(T1, 3, e1_result_addr_direct->get_reg_name(), s);
  }
  if (e1_result_addr_indirect) {
    emit_load(T1, e1_result_addr_indirect->get_offset(), e1_result_addr_indirect->get_reg_name(), s);
    emit_load(T2, 3, ACC, s);
    emit_load(T1, 3, T1, s);
  }
  emit_add(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
  code_addr1_to_addr2(s, addr_table.add_addr(ACC), result_addr);
  global_tmp_obj_handler_ptr->free_last_occupied_temp_addr();
}

int plus_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void sub_class::code(ostream &s, Addressing* result_addr) {
  Addressing* e1_result_addr = global_tmp_obj_handler_ptr->get_next_free_temp_addr();
  e1->code(s, e1_result_addr);
  global_tmp_obj_handler_ptr->occupy_next_free_temp_addr();
  e2->code(s, addr_table.add_addr(ACC));
  emit_jal("Object.copy", s);
  DirectAddressing* e1_result_addr_direct = dynamic_cast<DirectAddressing*>(e1_result_addr);
  IndirectAddressing* e1_result_addr_indirect = dynamic_cast<IndirectAddressing*>(e1_result_addr);
  my_namespace::assert(e1_result_addr_direct||e1_result_addr_indirect);
  if (e1_result_addr_direct) {
    emit_load(T2, 3, ACC, s);
    emit_load(T1, 3, e1_result_addr_direct->get_reg_name(), s);
  }
  if (e1_result_addr_indirect) {
    emit_load(T1, e1_result_addr_indirect->get_offset(), e1_result_addr_indirect->get_reg_name(), s);
    emit_load(T2, 3, ACC, s);
    emit_load(T1, 3, T1, s);
  }
  emit_sub(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
  code_addr1_to_addr2(s, addr_table.add_addr(ACC), result_addr);
  global_tmp_obj_handler_ptr->free_last_occupied_temp_addr();
}

int sub_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void mul_class::code(ostream &s, Addressing* result_addr) {
}

int mul_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void divide_class::code(ostream &s, Addressing* result_addr) {
}

int divide_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void neg_class::code(ostream &s, Addressing* result_addr) {
}

int neg_class::get_max_temp_number() {
  return e1->get_max_temp_number();
}

void lt_class::code(ostream &s, Addressing* result_addr) {
}

int lt_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void eq_class::code(ostream &s, Addressing* result_addr) {
}

int eq_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void leq_class::code(ostream &s, Addressing* result_addr) {
}

int leq_class::get_max_temp_number() {
  return std::max(e1->get_max_temp_number(), 1+e2->get_max_temp_number());
}

void comp_class::code(ostream &s, Addressing* result_addr) {
}

int comp_class::get_max_temp_number() {
  return e1->get_max_temp_number();
}

void int_const_class::code(ostream &s, Addressing* result_addr)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  DirectAddressing* result_addr_direct = dynamic_cast<DirectAddressing*>(result_addr);
  IndirectAddressing* result_addr_indirect = dynamic_cast<IndirectAddressing*>(result_addr);
  my_namespace::assert(result_addr_direct||result_addr_indirect);
  if (result_addr_direct) {
    emit_load_int(result_addr_direct->get_reg_name(), inttable.lookup_string(token->get_string()), s);
  }
  if (result_addr_indirect) {
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
    emit_store(ACC, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
  }
}

int int_const_class::get_max_temp_number() {
  return 0;
}

void string_const_class::code(ostream &s, Addressing* result_addr)
{
  DirectAddressing* result_addr_direct = dynamic_cast<DirectAddressing*>(result_addr);
  IndirectAddressing* result_addr_indirect = dynamic_cast<IndirectAddressing*>(result_addr);
  my_namespace::assert(result_addr_direct||result_addr_indirect);
  if (result_addr_direct) {
    emit_load_string(result_addr_direct->get_reg_name(), stringtable.lookup_string(token->get_string()), s);
  }
  if (result_addr_indirect) {
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
    emit_store(ACC, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
  }
}

int string_const_class::get_max_temp_number() {
  return 0;
}

void bool_const_class::code(ostream &s, Addressing* result_addr)
{
  DirectAddressing* result_addr_direct = dynamic_cast<DirectAddressing*>(result_addr);
  IndirectAddressing* result_addr_indirect = dynamic_cast<IndirectAddressing*>(result_addr);
  my_namespace::assert(result_addr_direct||result_addr_indirect);
  if (result_addr_direct) {
    emit_load_bool(result_addr_direct->get_reg_name(), BoolConst(val), s);
  }
  if (result_addr_indirect) {
    emit_load_bool(ACC, BoolConst(val), s);
    emit_store(ACC, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
  }
}

int bool_const_class::get_max_temp_number() {
  return 0;
}

void new__class::code(ostream &s, Addressing* result_addr) {
  if (type_name == SELF_TYPE) {
    Addressing* protobj_addr = global_tmp_obj_handler_ptr->get_next_free_temp_addr();
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    code_addr1_to_addr2(s, addr_table.add_addr(T1), protobj_addr);
    global_tmp_obj_handler_ptr->occupy_next_free_temp_addr();
    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);
    DirectAddressing* protobj_addr_direct = dynamic_cast<DirectAddressing*>(protobj_addr);
    IndirectAddressing* protobj_addr_indirect = dynamic_cast<IndirectAddressing*>(protobj_addr);
    if (protobj_addr_direct) {
      emit_load(T1, 1, protobj_addr_direct->get_reg_name(), s);
    }
    if (protobj_addr_indirect) {
      emit_load(T1, protobj_addr_indirect->get_offset(), protobj_addr_indirect->get_reg_name(), s);
      emit_load(T1, 1, T1, s);
    }
    emit_jalr(T1, s);
    global_tmp_obj_handler_ptr->free_last_occupied_temp_addr();
  } else {
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(type_name, s);
    s << endl;
    emit_jal("Object.copy", s);
    s << JAL;
    emit_init_ref(type_name, s);
    s << endl;
  }
}

int new__class::get_max_temp_number() {
  if (type_name == SELF_TYPE) {
    return 1;
  }
  return 0;
}

void isvoid_class::code(ostream &s, Addressing* result_addr) {
}

int isvoid_class::get_max_temp_number() {
  return e1->get_max_temp_number();
}

void no_expr_class::code(ostream &s, Addressing* result_addr) {
  DirectAddressing* result_addr_direct = dynamic_cast<DirectAddressing*>(result_addr);
  IndirectAddressing* result_addr_indirect = dynamic_cast<IndirectAddressing*>(result_addr);
  my_namespace::assert(result_addr_direct||result_addr_indirect);
  if (type == Int) {
    if (result_addr_direct) {
      emit_load_int(result_addr_direct->get_reg_name(), inttable.lookup_string("0"), s);
    }
    if (result_addr_indirect) {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
      emit_store(ACC, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
    }
  }
  else if (type == Bool) {
    if (result_addr_direct) {
      emit_load_bool(result_addr_direct->get_reg_name(), BoolConst(0), s);
    }
    if (result_addr_indirect) {
      emit_load_bool(ACC, BoolConst(0), s);
      emit_store(ACC, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
    }
  } else if (type == Str) {
    if (result_addr_direct) {
      emit_load_string(result_addr_direct->get_reg_name(), stringtable.lookup_string(""), s);
    }
    if (result_addr_indirect) {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
      emit_store(ACC, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
    }
  } else {
    if (result_addr_direct) {
      emit_move(result_addr_direct->get_reg_name(), ZERO, s);
    }
    if (result_addr_indirect) {
      emit_store(ZERO, result_addr_indirect->get_offset(), result_addr_indirect->get_reg_name(), s);
    }
  }

}

int no_expr_class::get_max_temp_number() {
  return 0;
}

void object_class::code(ostream &s, Addressing* result_addr) {
  Addressing* id_addr = id_to_addr_table.lookup(name);
  code_addr1_to_addr2(s, id_addr, result_addr);
}

int object_class::get_max_temp_number() {
  return 0;
}

DirectAddressing::DirectAddressing(char* reg_name_) {
  int len = min(strlen(reg_name_), MAXSIZE);
  reg_name = new char[len+1];
  strncpy(reg_name, reg_name_, len);
  reg_name[len] = '\0';
}

bool DirectAddressing::equal_addressing(Addressing* addr) const {
  DirectAddressing* direct_addr = dynamic_cast<DirectAddressing*>(addr);
  if(!direct_addr) {
    return false;
  }
  if(strcmp(get_reg_name(), direct_addr->get_reg_name()) != 0) {
    return false;
  }
  return true;
}

IndirectAddressing::IndirectAddressing(char* reg_name_, int offset_) {
  int len = min(strlen(reg_name_), MAXSIZE);
  reg_name = new char[len+1];
  strncpy(reg_name, reg_name_, len);
  reg_name[len] = '\0';
  offset = offset_;
}

bool IndirectAddressing::equal_addressing(Addressing* addr) const {
  IndirectAddressing* indirect_addr = dynamic_cast<IndirectAddressing*>(addr);
  if(!indirect_addr) {
    return false;
  }
  if(strcmp(get_reg_name(), indirect_addr->get_reg_name()) !=0 || get_offset() != indirect_addr->get_offset()) {
    return false;
  }
  return true;
}

Addressing* AddressingTable::add_addr(char* reg_name) {
  for (auto addr_ptr : addressing_ptr_vec) {
    DirectAddressing* direct_addr = dynamic_cast<DirectAddressing*>(addr_ptr);
    if (direct_addr && strcmp(direct_addr->get_reg_name(), reg_name) == 0) {
      return direct_addr;
    }
  }
  DirectAddressing* direct_addr = new DirectAddressing(reg_name);
  addressing_ptr_vec.push_back(direct_addr);
  return direct_addr;
}

Addressing* AddressingTable::add_addr(char* reg_name, int offset) {
  for (auto addr_ptr : addressing_ptr_vec) {
    IndirectAddressing* indirect_addr = dynamic_cast<IndirectAddressing*>(addr_ptr);
    if (indirect_addr && strcmp(indirect_addr->get_reg_name(), reg_name) == 0 && indirect_addr->get_offset() == offset) {
      return indirect_addr;
    }
  }
  IndirectAddressing* indirect_addr = new IndirectAddressing(reg_name, offset);
  addressing_ptr_vec.push_back(indirect_addr);
  return indirect_addr;
}

// emit the code for storing old $s1...$s6 to current frame stack
void TempObjHandler::emit_temp_prepare(ostream &s) {
  for (int i = 0; i < min(6, max_temp_number); ++i) {
    s << SW << "$s" << i+1 << " " << (max_temp_number-1-i) * WORD_SIZE << "(" << FP << ")"
      << endl;
  }
}

// emit the code for restoring old $s1...$s6
void TempObjHandler::emit_temp_restore(ostream &s) {
  for (int i = 0; i < min(6, max_temp_number); ++i) {
    s << LW << "$s" << i+1 << " " << (max_temp_number-1-i) * WORD_SIZE << "(" << FP << ")"
      << endl;
  }
}

Addressing* TempObjHandler::get_next_free_temp_addr() const {
  my_namespace::assert(current_used_temp_number < max_temp_number);
  if(current_used_temp_number < 6) {
    if (max_temp_number <= 6) {
      my_namespace::assert((max_temp_number - 1 - current_used_temp_number >= 0) && (max_temp_number - 1 - current_used_temp_number < 6));
      return addr_table.add_addr(temp_register[max_temp_number - 1 - current_used_temp_number]);
    }
    my_namespace::assert((6 - 1 - current_used_temp_number >= 0) && (6 - 1 - current_used_temp_number < 6));
    return addr_table.add_addr(temp_register[6 - 1 - current_used_temp_number]);
  }
  return addr_table.add_addr(FP, current_used_temp_number - 6);
}

void TempObjHandler::occupy_next_free_temp_addr() {
  my_namespace::assert(current_used_temp_number < max_temp_number);
  ++current_used_temp_number;
}

void TempObjHandler::free_last_occupied_temp_addr() {
  my_namespace::assert(current_used_temp_number > 0);
  --current_used_temp_number;
}

void my_namespace::assert(bool b) {
  if (b) {
    return;
  }
  cout << (char*)(b) << endl;
  void* callstack[128];
  int i, frames = backtrace(callstack, 128);
  char** strs = backtrace_symbols(callstack, frames);
  for (i = 0; i < frames; ++i) {
    printf("%s\n", strs[i]);
  }
  free(strs);
  exit(1);
}
