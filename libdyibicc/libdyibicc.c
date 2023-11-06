//
// Amalgamated (single file) build of https://github.com/sgraham/dyibicc.
// Revision: 433890aaa6d91861ba3bd805503b077e53e5c4b0
//
// This file should not be edited or modified, patches should be to the
// non-amalgamated files in src/. The user-facing API is in libdyibicc.h
// which should be located in the same directory as this .c file.
//

#undef C
#undef L
#undef VOID
//
// START OF ../../src/dyibicc.h
//
#define _POSIX_C_SOURCE 200809L
#define _DEFAULT_SOURCE

#ifdef _MSC_VER
#pragma warning(disable : 4061)  // enumerator 'X' in switch of enum 'Y' is not explicitly handled
                                 // by a case label
#pragma warning(disable : 4062)  // enumerator 'X' in switch of enum 'Y' is not handled
#pragma warning(disable : 4668)  // C:\Program Files (x86)\Windows
                                 // Kits\10\\include\10.0.22621.0\\um\winioctl.h(10847): warning
                                 // C4668: '_WIN32_WINNT_WIN10_TH2' is not defined as a preprocessor
                                 // macro, replacing with '0' for '#if/#elif'
#pragma warning(disable : 4820)  // Padding bytes added
#pragma warning(disable : 5045)  // Compiler will insert Spectre mitigation for memory load if
                                 // /Qspectre switch specified
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif
#endif

#ifdef __clang__
#pragma clang diagnostic ignored "-Wmissing-field-initializers"
#pragma clang diagnostic ignored "-Wswitch"
#endif

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wswitch"
#endif

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

// We use this as the global indication that we should be targeting the Win ABI
// rather than SysV. It's here rather than in config so that libdyibcc.c doesn't
// need special defines added.
#if defined(_WIN64) && defined(_M_AMD64)
#define X64WIN 1
#endif

#include "libdyibicc.h"

#ifdef _MSC_VER
#define NORETURN __declspec(noreturn)
#define strdup _strdup
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#define NORETURN _Noreturn
#include <unistd.h>
#endif

#if !X64WIN
#include <strings.h>
#endif

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#ifndef __GNUC__
#define __attribute__(x)
#endif

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;
typedef struct Hideset Hideset;
typedef struct Token Token;
typedef struct HashMap HashMap;
typedef struct UserContext UserContext;
typedef struct DbpContext DbpContext;
typedef struct DbpFunctionSymbol DbpFunctionSymbol;

//
// alloc.c
//
typedef enum AllocLifetime {
  AL_Compile = 0,  // Must be 0 so that 0-initialized structs default to this storage.
  AL_Temp,
  AL_Link,
  AL_UserContext,
  NUM_BUMP_HEAPS,
  AL_Manual = NUM_BUMP_HEAPS,
} AllocLifetime;

static void alloc_init(AllocLifetime lifetime);
static void alloc_reset(AllocLifetime lifetime);

static void* bumpcalloc(size_t num, size_t size, AllocLifetime lifetime);
static void* bumplamerealloc(void* old,
                                 size_t old_size,
                                 size_t new_size,
                                 AllocLifetime lifetime);
static void alloc_free(void* p, AllocLifetime lifetime);  // AL_Manual only.

static void* aligned_allocate(size_t size, size_t alignment);
static void aligned_free(void* p);
static void* allocate_writable_memory(size_t size);
static bool make_memory_readwrite(void* m, size_t size);
static bool make_memory_executable(void* m, size_t size);
static void free_executable_memory(void* p, size_t size);

//
// util.c
//

typedef struct File File;

typedef struct {
  char** data;
  int capacity;
  int len;
} StringArray;

typedef struct StringInt {
  char* str;
  int i;
} StringInt;

typedef struct StringIntArray {
  StringInt* data;
  int capacity;
  int len;
} StringIntArray;

typedef struct FilePtrArray {
  File** data;
  int capacity;
  int len;
} FilePtrArray;

typedef struct TokenPtrArray {
  Token** data;
  int capacity;
  int len;
} TokenPtrArray;

typedef struct IntIntInt {
  int a;
  int b;
  int c;
} IntIntInt;

typedef struct IntIntIntArray {
  IntIntInt* data;
  int capacity;
  int len;
} IntIntIntArray;

static char* bumpstrndup(const char* s, size_t n, AllocLifetime lifetime);
static char* bumpstrdup(const char* s, AllocLifetime lifetime);
static char* dirname(char* s);
static uint64_t align_to_u(uint64_t n, uint64_t align);
static int64_t align_to_s(int64_t n, int64_t align);
static unsigned int get_page_size(void);
static void strarray_push(StringArray* arr, char* s, AllocLifetime lifetime);
static void strintarray_push(StringIntArray* arr, StringInt item, AllocLifetime lifetime);
static void fileptrarray_push(FilePtrArray* arr, File* item, AllocLifetime lifetime);
static void tokenptrarray_push(TokenPtrArray* arr, Token* item, AllocLifetime lifetime);
#if X64WIN
static void intintintarray_push(IntIntIntArray* arr, IntIntInt item, AllocLifetime lifetime);
#endif
static char* format(AllocLifetime lifetime, char* fmt, ...)
    __attribute__((format(printf, 2, 3)));
static char* read_file_wrap_user(char* path, AllocLifetime lifetime);
static NORETURN void error(char* fmt, ...) __attribute__((format(printf, 1, 2)));
static NORETURN void error_at(char* loc, char* fmt, ...) __attribute__((format(printf, 2, 3)));
static NORETURN void error_tok(Token* tok, char* fmt, ...)
    __attribute__((format(printf, 2, 3)));
static NORETURN void error_internal(char* file, int line, char* msg);
static int outaf(const char* fmt, ...) __attribute__((format(printf, 1, 2)));
static void warn_tok(Token* tok, char* fmt, ...) __attribute__((format(printf, 2, 3)));
#if X64WIN
static void register_function_table_data(UserContext* ctx, int func_count, char* base_addr);
static void unregister_and_free_function_table_data(UserContext* ctx);
static char* get_temp_pdb_filename(AllocLifetime lifetime);
#endif

//
// tokenize.c
//

// Token
typedef enum {
  TK_IDENT,    // Identifiers
  TK_PUNCT,    // Punctuators
  TK_KEYWORD,  // Keywords
  TK_STR,      // String literals
  TK_NUM,      // Numeric literals
  TK_PP_NUM,   // Preprocessing numbers
  TK_EOF,      // End-of-file markers
} TokenKind;

struct File {
  char* name;
  char* contents;
  int file_no;  // Index into tokenize__all_tokenized_files.

  // For #line directive
  char* display_name;
  int line_delta;
};

// Token type
typedef struct Token Token;
struct Token {
  TokenKind kind;    // Token kind
  Token* next;       // Next token
  int64_t val;       // If kind is TK_NUM, its value
  long double fval;  // If kind is TK_NUM, its value
  char* loc;         // Token location
  int len;           // Token length
  Type* ty;          // Used if TK_NUM or TK_STR
  char* str;         // String literal contents including terminating '\0'

  File* file;        // Source location
  char* filename;    // Filename
  int line_no;       // Line number
  int line_delta;    // Line number
  bool at_bol;       // True if this token is at beginning of line
  bool has_space;    // True if this token follows a space character
  Hideset* hideset;  // For macro expansion
  Token* origin;     // If this is expanded from a macro, the original token
};

static bool equal(Token* tok, char* op);
static Token* skip(Token* tok, char* op);
static bool consume(Token** rest, Token* tok, char* str);
static void convert_pp_tokens(Token* tok);
static File* new_file(char* name, char* contents);
static Token* tokenize_string_literal(Token* tok, Type* basety);
static Token* tokenize(File* file);
static Token* tokenize_file(char* filename);
static Token* tokenize_filecontents(char* path, char* contents);

#define unreachable() error_internal(__FILE__, __LINE__, "unreachable")
#define ABORT(msg) error_internal(__FILE__, __LINE__, msg)

//
// preprocess.c
//

static char* search_include_paths(char* filename);
static void init_macros(void);
static void define_macro(char* name, char* buf);
static void undef_macro(char* name);
static Token* preprocess(Token* tok);
static Token* add_container_instantiations(Token* tok);

//
// parse.c
//

// Variable or function
typedef struct Obj Obj;
struct Obj {
  Obj* next;
  char* name;     // Variable name
  Type* ty;       // Type
  Token* tok;     // representative token
  bool is_local;  // local or global/function
#if X64WIN
  bool is_param_passed_by_reference;
#endif
  int align;  // alignment

  // Local variable
  int offset;

  // Global variable or function
  bool is_function;
  bool is_definition;
  bool is_static;
  int dasm_entry_label;
  int dasm_return_label;
  int dasm_end_of_function_label;
  int dasm_unwind_info_label;
#if X64WIN
  IntIntIntArray file_line_label_data;
#endif

  // Global variable
  bool is_tentative;
  bool is_tls;
  bool is_rodata;
  char* init_data;
  Relocation* rel;

  // Function
  bool is_inline;
  Obj* params;
  Node* body;
  Obj* locals;
  Obj* va_area;
  Obj* alloca_bottom;
  int stack_size;

  // Static inline function
  bool is_live;  // No code is emitted for "static inline" functions if no one is referencing them.
  bool is_root;
  StringArray refs;
};

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
struct Relocation {
  Relocation* next;
  int offset;
  char** string_label;
  int* internal_code_label;
  long addend;
};

// AST node
typedef enum {
  ND_NULL_EXPR,         // Do nothing
  ND_ADD,               // +
  ND_SUB,               // -
  ND_MUL,               // *
  ND_DIV,               // /
  ND_NEG,               // unary -
  ND_MOD,               // %
  ND_BITAND,            // &
  ND_BITOR,             // |
  ND_BITXOR,            // ^
  ND_SHL,               // <<
  ND_SHR,               // >>
  ND_EQ,                // ==
  ND_NE,                // !=
  ND_LT,                // <
  ND_LE,                // <=
  ND_ASSIGN,            // =
  ND_COND,              // ?:
  ND_COMMA,             // ,
  ND_MEMBER,            // . (struct member access)
  ND_ADDR,              // unary &
  ND_DEREF,             // unary *
  ND_NOT,               // !
  ND_BITNOT,            // ~
  ND_LOGAND,            // &&
  ND_LOGOR,             // ||
  ND_RETURN,            // "return"
  ND_IF,                // "if"
  ND_FOR,               // "for" or "while"
  ND_DO,                // "do"
  ND_SWITCH,            // "switch"
  ND_CASE,              // "case"
  ND_BLOCK,             // { ... }
  ND_GOTO,              // "goto"
  ND_GOTO_EXPR,         // "goto" labels-as-values
  ND_LABEL,             // Labeled statement
  ND_LABEL_VAL,         // [GNU] Labels-as-values
  ND_FUNCALL,           // Function call
  ND_EXPR_STMT,         // Expression statement
  ND_STMT_EXPR,         // Statement expression
  ND_VAR,               // Variable
  ND_VLA_PTR,           // VLA designator
  ND_REFLECT_TYPE_PTR,  // _ReflectType*
  ND_NUM,               // Integer
  ND_CAST,              // Type cast
  ND_MEMZERO,           // Zero-clear a stack variable
  ND_ASM,               // "asm"
  ND_CAS,               // Atomic compare-and-swap
  ND_LOCKCE,            // _InterlockedCompareExchange
  ND_EXCH,              // Atomic exchange
} NodeKind;

// AST node type
struct Node {
  NodeKind kind;  // Node kind
  Node* next;     // Next node
  Type* ty;       // Type, e.g. int or pointer to int
  Token* tok;     // Representative token

  Node* lhs;  // Left-hand side
  Node* rhs;  // Right-hand side

  // "if" or "for" statement
  Node* cond;
  Node* then;
  Node* els;
  Node* init;
  Node* inc;

  // "break" and "continue" labels
  int brk_pc_label;
  int cont_pc_label;

  // Block or statement expression
  Node* body;

  // Struct member access
  Member* member;

  // Function call
  Type* func_ty;
  Node* args;
  bool pass_by_stack;
#if X64WIN
  int pass_by_reference;  // Offset to copy of large struct.
#endif
  Obj* ret_buffer;

  // Goto or labeled statement, or labels-as-values
  char* label;
  int pc_label;
  Node* goto_next;

  // Switch
  Node* case_next;
  Node* default_case;

  // Case
  long begin;
  long end;

  // "asm" string literal
  char* asm_str;

  // Atomic compare-and-swap
  Node* cas_addr;
  Node* cas_old;
  Node* cas_new;

  // Atomic op= operators
  Obj* atomic_addr;
  Node* atomic_expr;

  // Variable
  Obj* var;

  // Numeric literal
  int64_t val;
  long double fval;

  uintptr_t reflect_ty;
};

static Node* new_cast(Node* expr, Type* ty);
static int64_t pp_const_expr(Token** rest, Token* tok);
static Obj* parse(Token* tok);

//
// type.c
//

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_FLOAT,
  TY_DOUBLE,
#if X64WIN
  TY_LDOUBLE = TY_DOUBLE,
#else
  TY_LDOUBLE,
#endif
  TY_ENUM = 9,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
  TY_VLA,  // variable-length array
  TY_STRUCT,
  TY_UNION,
} TypeKind;

struct Type {
  TypeKind kind;
  int size;          // sizeof() value
  int align;         // alignment
  bool is_unsigned;  // unsigned or signed
  bool is_atomic;    // true if _Atomic
  Type* origin;      // for type compatibility check

  // Pointer-to or array-of type. We intentionally use the same member
  // to represent pointer/array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer or not. That means in many contexts "array of T" is
  // naturally handled as if it were "pointer to T", as required by
  // the C spec.
  Type* base;

  // Declaration
  Token* name;
  Token* name_pos;

  // Array
  int array_len;

  // Variable-length array
  Node* vla_len;  // # of elements
  Obj* vla_size;  // sizeof() value

  // Struct
  Member* members;
  bool is_flexible;
  bool is_packed;
  Token* methodcall_prefix;

  // Function type
  Type* return_ty;
  Type* params;
  bool is_variadic;
  Type* next;
};

// Struct member
struct Member {
  Member* next;
  Type* ty;
  Token* tok;  // for error message
  Token* name;
  int idx;
  int align;
  int offset;

  // Bitfield
  bool is_bitfield;
  int bit_offset;
  int bit_width;
};






static bool is_integer(Type* ty);
static bool is_flonum(Type* ty);
static bool is_numeric(Type* ty);
static bool is_void(Type* ty);
static bool is_compatible(Type* t1, Type* t2);
static Type* copy_type(Type* ty);
static Type* pointer_to(Type* base);
static Type* func_type(Type* return_ty);
static Type* array_of(Type* base, int size, Token* err_tok);
static Type* vla_of(Type* base, Node* expr);
static Type* enum_type(void);
static Type* struct_type(void);
static void add_type(Node* node);

//
// codegen.c
//
typedef struct CompileOutputs {
  HashMap* codeseg_static_symbols;
  HashMap* codeseg_global_symbols;
} CompileOutputs;

static void codegen_init(void);
static void codegen(Obj* prog, size_t file_index);
static void codegen_free(void);
static int codegen_pclabel(void);
#if X64WIN
static bool type_passed_in_register(Type* ty);
#endif

//
// unicode.c
//

static int encode_utf8(char* buf, uint32_t c);
static uint32_t decode_utf8(char** new_pos, char* p);
static bool is_ident1(uint32_t c);
static bool is_ident2(uint32_t c);
static int display_width(char* p, int len);

//
// hashmap.c
//

typedef struct {
  char* key;
  int keylen;
  void* val;
} HashEntry;

struct HashMap {
  HashEntry* buckets;
  int capacity;
  int used;
  AllocLifetime alloc_lifetime;
};

static void* hashmap_get(HashMap* map, char* key);
static void* hashmap_get2(HashMap* map, char* key, int keylen);
static void hashmap_put(HashMap* map, char* key, void* val);
static void hashmap_put2(HashMap* map, char* key, int keylen, void* val);
static void hashmap_delete(HashMap* map, char* key);
static void hashmap_delete2(HashMap* map, char* key, int keylen);
static void hashmap_clear_manual_key_owned_value_owned_aligned(HashMap* map);
static void hashmap_clear_manual_key_owned_value_unowned(HashMap* map);

//
// link.c
//
static bool link_all_files(void);

//
// Entire compiler state in one struct and linker in a second for clearing, esp.
// after longjmp. There should be no globals outside of these structures.
//

typedef struct CondIncl CondIncl;

typedef struct Scope Scope;
// Represents a block scope.
struct Scope {
  Scope* next;

  // C has two block scopes; one is for variables/typedefs and
  // the other is for struct/union/enum tags.
  HashMap vars;
  HashMap tags;
};

typedef struct LinkFixup {
  // The address to fix up.
  void* at;

  // Name of the symbol at which the fixup should point.
  // TODO: Intern pool for all the import/export/global names.
  char* name;

  // Added to the address that |name| resolves to.
  int addend;
} LinkFixup;

typedef struct FileLinkData {
  char* source_name;
  char* codeseg_base_address;  // Just the address, not a string.
  size_t codeseg_size;

  LinkFixup* fixups;
  int flen;
  int fcap;
} FileLinkData;

static void free_link_fixups(FileLinkData* fld);

struct UserContext {
  DyibiccLoadFileContents load_file_contents;
  DyibiccFunctionLookupFn get_function_address;
  DyibiccOutputFn output_function;
  bool use_ansi_codes;
  bool generate_debug_symbols;

  size_t num_include_paths;
  char** include_paths;

  size_t num_files;
  FileLinkData* files;

  // This is an array of num_files+1; 0..num_files-1 correspond to static
  // globals in the files in FileLinkData, and global_data[num_files] is the
  // fully global (exported) symbols. These HashMaps are also special because
  // they're lifetime == AL_Manual.
  HashMap* global_data;

  HashMap* exports;

  HashMap reflect_types;

#if X64WIN
  char* function_table_data;
  DbpContext* dbp_ctx;
#endif
};

typedef struct dasm_State dasm_State;

typedef struct CompilerState {
  // tokenize.c
  File* tokenize__current_file;  // Input file
  bool tokenize__at_bol;         // True if the current position is at the beginning of a line
  bool tokenize__has_space;      // True if the current position follows a space character
  HashMap tokenize__keyword_map;
  FilePtrArray tokenize__all_tokenized_files;

  // preprocess.c
  HashMap preprocess__macros;
  CondIncl* preprocess__cond_incl;
  HashMap preprocess__pragma_once;
  HashMap preprocess__container_included;
  TokenPtrArray preprocess__container_tokens;
  HashMap preprocess__builtin_includes_map;

  int preprocess__include_next_idx;
  HashMap preprocess__include_path_cache;
  HashMap preprocess__include_guards;
  int preprocess__counter_macro_i;

  // parse.c
  Obj* parse__locals;   // All local variable instances created during parsing are accumulated to
                        // this list.
  Obj* parse__globals;  // Likewise, global variables are accumulated to this list.
  Scope* parse__scope;  // NOTE: needs to be reinitialized after clear to point at empty_scope.
  Scope parse__empty_scope;
  Obj* parse__current_fn;  // Points to the function object the parser is currently parsing.
  Node* parse__gotos;      // Lists of all goto statements and labels in the curent function.
  Node* parse__labels;
  int parse__brk_pc_label;  // Current "goto" and "continue" jump targets.
  int parse__cont_pc_label;
  Node* parse__current_switch;  // Points to a node representing a switch if we are parsing a switch
                                // statement. Otherwise, NULL.
  Obj* parse__builtin_alloca;
  int parse__unique_name_id;
  HashMap parse__typename_map;
  bool parse__evaluating_pp_const;

  // codegen.in.c
  int codegen__depth;
  size_t codegen__file_index;
  dasm_State* codegen__dynasm;
  Obj* codegen__current_fn;
  int codegen__numlabels;
  StringIntArray codegen__fixups;

  // main.c
  char* main__base_file;
} CompilerState;

typedef struct LinkerState {
  // link.c
  HashMap link__runtime_function_map;
} LinkerState;

//
// END OF ../../src/dyibicc.h
//
#undef C
#undef L
#undef VOID
//
// START OF ../../include/all/reflect.h
//

#include <stddef.h>
#include <stdint.h>

typedef struct _ReflectType _ReflectType;
typedef struct _ReflectTypeMember _ReflectTypeMember;
typedef struct _ReflectTypeEnumerant _ReflectTypeEnumerant;

#define _REFLECT_KIND_VOID 0
#define _REFLECT_KIND_BOOL 1
#define _REFLECT_KIND_CHAR 2
#define _REFLECT_KIND_SHORT 3
#define _REFLECT_KIND_INT 4
#define _REFLECT_KIND_LONG 5
#define _REFLECT_KIND_FLOAT 6
#define _REFLECT_KIND_DOUBLE 7
// TODO: LDOUBLE
#define _REFLECT_KIND_ENUM 9
#define _REFLECT_KIND_PTR 10
#define _REFLECT_KIND_FUNC 11
#define _REFLECT_KIND_ARRAY 12
#define _REFLECT_KIND_VLA 13
#define _REFLECT_KIND_STRUCT 14
#define _REFLECT_KIND_UNION 15

#define _REFLECT_TYPEFLAG_UNSIGNED 0x0001  // Integer types only
#define _REFLECT_TYPEFLAG_ATOMIC 0x0002    // Integer types only
#define _REFLECT_TYPEFLAG_FLEXIBLE 0x0004  // Arrays at end of structs only
#define _REFLECT_TYPEFLAG_PACKED 0x0008    // Structs and unions only
#define _REFLECT_TYPEFLAG_VARIADIC 0x0010  // Functions only

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4200)  // Zero-sized array.
#pragma warning(disable : 4201)  // Unnamed union.
#endif

struct _ReflectTypeMember {
  _ReflectType* type;
  char* name;
  int32_t align;
  int32_t offset;
  int32_t bit_width;   // -1 if not bitfield
  int32_t bit_offset;  // -1 if not bitfield
};

struct _ReflectTypeEnumerant {
  _ReflectTypeEnumerant* next;
  char* name;
  int32_t value;
};

struct _ReflectType {
  char* name;  // Either the built-in typename, or the user declared one.
  int32_t size;
  int32_t align;
  int32_t kind;   // One of _REFLECT_KIND_*.
  int32_t flags;  // Combination of _REFLECT_TYPEFLAG_*.

  union {
    struct {
      _ReflectType* base;
      int32_t len;
    } arr;
    struct {
      _ReflectType* base;
    } ptr;
    struct {
      size_t num_members;
      _ReflectTypeMember members[];
    } su;
    struct {
      _ReflectType* return_ty;
      size_t num_params;
      _ReflectType* params[];
    } func;
    struct {
      _ReflectTypeEnumerant* enums;
    } enumer;
    struct {
      _ReflectType* vla_size;
      // len?
    } vla;
  };
};
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#if __dyibicc__
extern _ReflectType* _ReflectTypeOf(...);
#endif
//
// END OF ../../include/all/reflect.h
//
#undef C
#undef L
#undef VOID
//
// START OF compincl.h
//
typedef struct CompilerInclude {
    char* path;
    int offset;
} CompilerInclude;

static CompilerInclude compiler_includes[12] = {
    { "__include__/all/reflect.h", 0 },
    { "__include__/all/stdalign.h", 2262 },
    { "__include__/all/stdatomic.h", 2426 },
    { "__include__/all/stdnoreturn.h", 6305 },
    { "__include__/all/_map.h", 6390 },
    { "__include__/all/_vec.h", 48381 },
    { "__include__/linux/float.h", 87492 },
    { "__include__/linux/stdarg.h", 88534 },
    { "__include__/linux/stdbool.h", 90166 },
    { "__include__/linux/stddef.h", 90306 },
    { "__include__/linux/stdnoreturn.h", 90555 },
    { "__include__/win/stddef.h", 90640 },
};
static unsigned char compiler_include_blob[90777] = {
35, 112, 114, 97, 103, 109, 97, 32, 111, 110, 99, 101, 10, 10, 35,
105, 110, 99, 108, 117, 100, 101, 32, 60, 115, 116, 100, 100, 101,
102, 46, 104, 62, 10, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60,
115, 116, 100, 105, 110, 116, 46, 104, 62, 10, 10, 116, 121, 112, 101,
100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 82, 101, 102,
108, 101, 99, 116, 84, 121, 112, 101, 32, 95, 82, 101, 102, 108, 101,
99, 116, 84, 121, 112, 101, 59, 10, 116, 121, 112, 101, 100, 101, 102,
32, 115, 116, 114, 117, 99, 116, 32, 95, 82, 101, 102, 108, 101, 99,
116, 84, 121, 112, 101, 77, 101, 109, 98, 101, 114, 32, 95, 82, 101,
102, 108, 101, 99, 116, 84, 121, 112, 101, 77, 101, 109, 98, 101, 114,
59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99,
116, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 69,
110, 117, 109, 101, 114, 97, 110, 116, 32, 95, 82, 101, 102, 108, 101,
99, 116, 84, 121, 112, 101, 69, 110, 117, 109, 101, 114, 97, 110, 116,
59, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70, 76,
69, 67, 84, 95, 75, 73, 78, 68, 95, 86, 79, 73, 68, 32, 48, 10, 35,
100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95,
75, 73, 78, 68, 95, 66, 79, 79, 76, 32, 49, 10, 35, 100, 101, 102,
105, 110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68,
95, 67, 72, 65, 82, 32, 50, 10, 35, 100, 101, 102, 105, 110, 101, 32,
95, 82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 83, 72, 79,
82, 84, 32, 51, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69,
70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 73, 78, 84, 32, 52, 10,
35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84,
95, 75, 73, 78, 68, 95, 76, 79, 78, 71, 32, 53, 10, 35, 100, 101, 102,
105, 110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68,
95, 70, 76, 79, 65, 84, 32, 54, 10, 35, 100, 101, 102, 105, 110, 101,
32, 95, 82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 68, 79,
85, 66, 76, 69, 32, 55, 10, 47, 47, 32, 84, 79, 68, 79, 58, 32, 76,
68, 79, 85, 66, 76, 69, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 69, 78, 85, 77,
32, 57, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70, 76,
69, 67, 84, 95, 75, 73, 78, 68, 95, 80, 84, 82, 32, 49, 48, 10, 35,
100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95,
75, 73, 78, 68, 95, 70, 85, 78, 67, 32, 49, 49, 10, 35, 100, 101, 102,
105, 110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68,
95, 65, 82, 82, 65, 89, 32, 49, 50, 10, 35, 100, 101, 102, 105, 110,
101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 86,
76, 65, 32, 49, 51, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82,
69, 70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 83, 84, 82, 85, 67,
84, 32, 49, 52, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69,
70, 76, 69, 67, 84, 95, 75, 73, 78, 68, 95, 85, 78, 73, 79, 78, 32,
49, 53, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70,
76, 69, 67, 84, 95, 84, 89, 80, 69, 70, 76, 65, 71, 95, 85, 78, 83,
73, 71, 78, 69, 68, 32, 48, 120, 48, 48, 48, 49, 32, 32, 47, 47, 32,
73, 110, 116, 101, 103, 101, 114, 32, 116, 121, 112, 101, 115, 32,
111, 110, 108, 121, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82,
69, 70, 76, 69, 67, 84, 95, 84, 89, 80, 69, 70, 76, 65, 71, 95, 65,
84, 79, 77, 73, 67, 32, 48, 120, 48, 48, 48, 50, 32, 32, 32, 32, 47,
47, 32, 73, 110, 116, 101, 103, 101, 114, 32, 116, 121, 112, 101, 115,
32, 111, 110, 108, 121, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
82, 69, 70, 76, 69, 67, 84, 95, 84, 89, 80, 69, 70, 76, 65, 71, 95,
70, 76, 69, 88, 73, 66, 76, 69, 32, 48, 120, 48, 48, 48, 52, 32, 32,
47, 47, 32, 65, 114, 114, 97, 121, 115, 32, 97, 116, 32, 101, 110,
100, 32, 111, 102, 32, 115, 116, 114, 117, 99, 116, 115, 32, 111, 110,
108, 121, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 82, 69, 70,
76, 69, 67, 84, 95, 84, 89, 80, 69, 70, 76, 65, 71, 95, 80, 65, 67,
75, 69, 68, 32, 48, 120, 48, 48, 48, 56, 32, 32, 32, 32, 47, 47, 32,
83, 116, 114, 117, 99, 116, 115, 32, 97, 110, 100, 32, 117, 110, 105,
111, 110, 115, 32, 111, 110, 108, 121, 10, 35, 100, 101, 102, 105,
110, 101, 32, 95, 82, 69, 70, 76, 69, 67, 84, 95, 84, 89, 80, 69, 70,
76, 65, 71, 95, 86, 65, 82, 73, 65, 68, 73, 67, 32, 48, 120, 48, 48,
49, 48, 32, 32, 47, 47, 32, 70, 117, 110, 99, 116, 105, 111, 110, 115,
32, 111, 110, 108, 121, 10, 10, 35, 105, 102, 100, 101, 102, 32, 95,
77, 83, 67, 95, 86, 69, 82, 10, 35, 112, 114, 97, 103, 109, 97, 32,
119, 97, 114, 110, 105, 110, 103, 40, 112, 117, 115, 104, 41, 10, 35,
112, 114, 97, 103, 109, 97, 32, 119, 97, 114, 110, 105, 110, 103, 40,
100, 105, 115, 97, 98, 108, 101, 32, 58, 32, 52, 50, 48, 48, 41, 32,
32, 47, 47, 32, 90, 101, 114, 111, 45, 115, 105, 122, 101, 100, 32,
97, 114, 114, 97, 121, 46, 10, 35, 112, 114, 97, 103, 109, 97, 32,
119, 97, 114, 110, 105, 110, 103, 40, 100, 105, 115, 97, 98, 108, 101,
32, 58, 32, 52, 50, 48, 49, 41, 32, 32, 47, 47, 32, 85, 110, 110, 97,
109, 101, 100, 32, 117, 110, 105, 111, 110, 46, 10, 35, 101, 110, 100,
105, 102, 10, 10, 115, 116, 114, 117, 99, 116, 32, 95, 82, 101, 102,
108, 101, 99, 116, 84, 121, 112, 101, 77, 101, 109, 98, 101, 114, 32,
123, 10, 32, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112,
101, 42, 32, 116, 121, 112, 101, 59, 10, 32, 32, 99, 104, 97, 114, 42,
32, 110, 97, 109, 101, 59, 10, 32, 32, 105, 110, 116, 51, 50, 95, 116,
32, 97, 108, 105, 103, 110, 59, 10, 32, 32, 105, 110, 116, 51, 50, 95,
116, 32, 111, 102, 102, 115, 101, 116, 59, 10, 32, 32, 105, 110, 116,
51, 50, 95, 116, 32, 98, 105, 116, 95, 119, 105, 100, 116, 104, 59,
32, 32, 32, 47, 47, 32, 45, 49, 32, 105, 102, 32, 110, 111, 116, 32,
98, 105, 116, 102, 105, 101, 108, 100, 10, 32, 32, 105, 110, 116, 51,
50, 95, 116, 32, 98, 105, 116, 95, 111, 102, 102, 115, 101, 116, 59,
32, 32, 47, 47, 32, 45, 49, 32, 105, 102, 32, 110, 111, 116, 32, 98,
105, 116, 102, 105, 101, 108, 100, 10, 125, 59, 10, 10, 115, 116, 114,
117, 99, 116, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112,
101, 69, 110, 117, 109, 101, 114, 97, 110, 116, 32, 123, 10, 32, 32,
95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 69, 110, 117,
109, 101, 114, 97, 110, 116, 42, 32, 110, 101, 120, 116, 59, 10, 32,
32, 99, 104, 97, 114, 42, 32, 110, 97, 109, 101, 59, 10, 32, 32, 105,
110, 116, 51, 50, 95, 116, 32, 118, 97, 108, 117, 101, 59, 10, 125,
59, 10, 10, 115, 116, 114, 117, 99, 116, 32, 95, 82, 101, 102, 108,
101, 99, 116, 84, 121, 112, 101, 32, 123, 10, 32, 32, 99, 104, 97,
114, 42, 32, 110, 97, 109, 101, 59, 32, 32, 47, 47, 32, 69, 105, 116,
104, 101, 114, 32, 116, 104, 101, 32, 98, 117, 105, 108, 116, 45, 105,
110, 32, 116, 121, 112, 101, 110, 97, 109, 101, 44, 32, 111, 114, 32,
116, 104, 101, 32, 117, 115, 101, 114, 32, 100, 101, 99, 108, 97, 114,
101, 100, 32, 111, 110, 101, 46, 10, 32, 32, 105, 110, 116, 51, 50,
95, 116, 32, 115, 105, 122, 101, 59, 10, 32, 32, 105, 110, 116, 51,
50, 95, 116, 32, 97, 108, 105, 103, 110, 59, 10, 32, 32, 105, 110,
116, 51, 50, 95, 116, 32, 107, 105, 110, 100, 59, 32, 32, 32, 47, 47,
32, 79, 110, 101, 32, 111, 102, 32, 95, 82, 69, 70, 76, 69, 67, 84,
95, 75, 73, 78, 68, 95, 42, 46, 10, 32, 32, 105, 110, 116, 51, 50, 95,
116, 32, 102, 108, 97, 103, 115, 59, 32, 32, 47, 47, 32, 67, 111, 109,
98, 105, 110, 97, 116, 105, 111, 110, 32, 111, 102, 32, 95, 82, 69,
70, 76, 69, 67, 84, 95, 84, 89, 80, 69, 70, 76, 65, 71, 95, 42, 46,
10, 10, 32, 32, 117, 110, 105, 111, 110, 32, 123, 10, 32, 32, 32, 32,
115, 116, 114, 117, 99, 116, 32, 123, 10, 32, 32, 32, 32, 32, 32, 95,
82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 42, 32, 98, 97,
115, 101, 59, 10, 32, 32, 32, 32, 32, 32, 105, 110, 116, 51, 50, 95,
116, 32, 108, 101, 110, 59, 10, 32, 32, 32, 32, 125, 32, 97, 114, 114,
59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 123, 10, 32,
32, 32, 32, 32, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112,
101, 42, 32, 98, 97, 115, 101, 59, 10, 32, 32, 32, 32, 125, 32, 112,
116, 114, 59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32,
123, 10, 32, 32, 32, 32, 32, 32, 115, 105, 122, 101, 95, 116, 32, 110,
117, 109, 95, 109, 101, 109, 98, 101, 114, 115, 59, 10, 32, 32, 32,
32, 32, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101,
77, 101, 109, 98, 101, 114, 32, 109, 101, 109, 98, 101, 114, 115, 91,
93, 59, 10, 32, 32, 32, 32, 125, 32, 115, 117, 59, 10, 32, 32, 32, 32,
115, 116, 114, 117, 99, 116, 32, 123, 10, 32, 32, 32, 32, 32, 32, 95,
82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 42, 32, 114, 101,
116, 117, 114, 110, 95, 116, 121, 59, 10, 32, 32, 32, 32, 32, 32, 115,
105, 122, 101, 95, 116, 32, 110, 117, 109, 95, 112, 97, 114, 97, 109,
115, 59, 10, 32, 32, 32, 32, 32, 32, 95, 82, 101, 102, 108, 101, 99,
116, 84, 121, 112, 101, 42, 32, 112, 97, 114, 97, 109, 115, 91, 93,
59, 10, 32, 32, 32, 32, 125, 32, 102, 117, 110, 99, 59, 10, 32, 32,
32, 32, 115, 116, 114, 117, 99, 116, 32, 123, 10, 32, 32, 32, 32, 32,
32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 69, 110,
117, 109, 101, 114, 97, 110, 116, 42, 32, 101, 110, 117, 109, 115, 59,
10, 32, 32, 32, 32, 125, 32, 101, 110, 117, 109, 101, 114, 59, 10, 32,
32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 123, 10, 32, 32, 32, 32,
32, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 42,
32, 118, 108, 97, 95, 115, 105, 122, 101, 59, 10, 32, 32, 32, 32, 32,
32, 47, 47, 32, 108, 101, 110, 63, 10, 32, 32, 32, 32, 125, 32, 118,
108, 97, 59, 10, 32, 32, 125, 59, 10, 125, 59, 10, 35, 105, 102, 100,
101, 102, 32, 95, 77, 83, 67, 95, 86, 69, 82, 10, 35, 112, 114, 97,
103, 109, 97, 32, 119, 97, 114, 110, 105, 110, 103, 40, 112, 111, 112,
41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 95, 95,
100, 121, 105, 98, 105, 99, 99, 95, 95, 10, 101, 120, 116, 101, 114,
110, 32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 42,
32, 95, 82, 101, 102, 108, 101, 99, 116, 84, 121, 112, 101, 79, 102,
40, 46, 46, 46, 41, 59, 10, 35, 101, 110, 100, 105, 102, 10, 0, 35,
105, 102, 110, 100, 101, 102, 32, 95, 95, 83, 84, 68, 65, 76, 73, 71,
78, 95, 72, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 83, 84,
68, 65, 76, 73, 71, 78, 95, 72, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 97, 108, 105, 103, 110, 97, 115, 32, 95, 65, 108, 105, 103,
110, 97, 115, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97, 108, 105,
103, 110, 111, 102, 32, 95, 65, 108, 105, 103, 110, 111, 102, 10, 35,
100, 101, 102, 105, 110, 101, 32, 95, 95, 97, 108, 105, 103, 110, 97,
115, 95, 105, 115, 95, 100, 101, 102, 105, 110, 101, 100, 32, 49, 10,
35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 97, 108, 105, 103, 110,
111, 102, 95, 105, 115, 95, 100, 101, 102, 105, 110, 101, 100, 32, 49,
10, 10, 35, 101, 110, 100, 105, 102, 10, 0, 35, 105, 102, 110, 100,
101, 102, 32, 95, 95, 83, 84, 68, 65, 84, 79, 77, 73, 67, 95, 72, 10,
35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 83, 84, 68, 65, 84, 79,
77, 73, 67, 95, 72, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 65,
84, 79, 77, 73, 67, 95, 66, 79, 79, 76, 95, 76, 79, 67, 75, 95, 70,
82, 69, 69, 32, 49, 10, 35, 100, 101, 102, 105, 110, 101, 32, 65, 84,
79, 77, 73, 67, 95, 67, 72, 65, 82, 95, 76, 79, 67, 75, 95, 70, 82,
69, 69, 32, 49, 10, 35, 100, 101, 102, 105, 110, 101, 32, 65, 84, 79,
77, 73, 67, 95, 67, 72, 65, 82, 49, 54, 95, 84, 95, 76, 79, 67, 75,
95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102, 105, 110, 101, 32,
65, 84, 79, 77, 73, 67, 95, 67, 72, 65, 82, 51, 50, 95, 84, 95, 76,
79, 67, 75, 95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102, 105,
110, 101, 32, 65, 84, 79, 77, 73, 67, 95, 87, 67, 72, 65, 82, 95, 84,
95, 76, 79, 67, 75, 95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102,
105, 110, 101, 32, 65, 84, 79, 77, 73, 67, 95, 83, 72, 79, 82, 84, 95,
76, 79, 67, 75, 95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102,
105, 110, 101, 32, 65, 84, 79, 77, 73, 67, 95, 73, 78, 84, 95, 76, 79,
67, 75, 95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102, 105, 110,
101, 32, 65, 84, 79, 77, 73, 67, 95, 76, 79, 78, 71, 95, 76, 79, 67,
75, 95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102, 105, 110, 101,
32, 65, 84, 79, 77, 73, 67, 95, 76, 76, 79, 78, 71, 95, 76, 79, 67,
75, 95, 70, 82, 69, 69, 32, 49, 10, 35, 100, 101, 102, 105, 110, 101,
32, 65, 84, 79, 77, 73, 67, 95, 80, 79, 73, 78, 84, 69, 82, 95, 76,
79, 67, 75, 95, 70, 82, 69, 69, 32, 49, 10, 10, 116, 121, 112, 101,
100, 101, 102, 32, 101, 110, 117, 109, 32, 123, 10, 32, 32, 109, 101,
109, 111, 114, 121, 95, 111, 114, 100, 101, 114, 95, 114, 101, 108,
97, 120, 101, 100, 44, 10, 32, 32, 109, 101, 109, 111, 114, 121, 95,
111, 114, 100, 101, 114, 95, 99, 111, 110, 115, 117, 109, 101, 44, 10,
32, 32, 109, 101, 109, 111, 114, 121, 95, 111, 114, 100, 101, 114, 95,
97, 99, 113, 117, 105, 114, 101, 44, 10, 32, 32, 109, 101, 109, 111,
114, 121, 95, 111, 114, 100, 101, 114, 95, 114, 101, 108, 101, 97,
115, 101, 44, 10, 32, 32, 109, 101, 109, 111, 114, 121, 95, 111, 114,
100, 101, 114, 95, 97, 99, 113, 95, 114, 101, 108, 44, 10, 32, 32,
109, 101, 109, 111, 114, 121, 95, 111, 114, 100, 101, 114, 95, 115,
101, 113, 95, 99, 115, 116, 44, 10, 125, 32, 109, 101, 109, 111, 114,
121, 95, 111, 114, 100, 101, 114, 59, 10, 10, 35, 100, 101, 102, 105,
110, 101, 32, 65, 84, 79, 77, 73, 67, 95, 70, 76, 65, 71, 95, 73, 78,
73, 84, 40, 120, 41, 32, 40, 120, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 97, 116, 111, 109, 105, 99, 95, 105, 110, 105, 116, 40, 97,
100, 100, 114, 44, 32, 118, 97, 108, 41, 32, 40, 42, 40, 97, 100, 100,
114, 41, 32, 61, 32, 40, 118, 97, 108, 41, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 107, 105, 108, 108, 95, 100, 101, 112, 101, 110,
100, 101, 110, 99, 121, 40, 120, 41, 32, 40, 120, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 116, 104,
114, 101, 97, 100, 95, 102, 101, 110, 99, 101, 40, 111, 114, 100, 101,
114, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109,
105, 99, 95, 115, 105, 103, 110, 97, 108, 95, 102, 101, 110, 99, 101,
40, 111, 114, 100, 101, 114, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 97, 116, 111, 109, 105, 99, 95, 105, 115, 95, 108, 111, 99, 107,
95, 102, 114, 101, 101, 40, 120, 41, 32, 49, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 108, 111, 97,
100, 40, 97, 100, 100, 114, 41, 32, 40, 42, 40, 97, 100, 100, 114, 41,
41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105,
99, 95, 115, 116, 111, 114, 101, 40, 97, 100, 100, 114, 44, 32, 118,
97, 108, 41, 32, 40, 42, 40, 97, 100, 100, 114, 41, 32, 61, 32, 40,
118, 97, 108, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
97, 116, 111, 109, 105, 99, 95, 108, 111, 97, 100, 95, 101, 120, 112,
108, 105, 99, 105, 116, 40, 97, 100, 100, 114, 44, 32, 111, 114, 100,
101, 114, 41, 32, 40, 42, 40, 97, 100, 100, 114, 41, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 115, 116,
111, 114, 101, 95, 101, 120, 112, 108, 105, 99, 105, 116, 40, 97, 100,
100, 114, 44, 32, 118, 97, 108, 44, 32, 111, 114, 100, 101, 114, 41,
32, 40, 42, 40, 97, 100, 100, 114, 41, 32, 61, 32, 40, 118, 97, 108,
41, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97, 116, 111,
109, 105, 99, 95, 102, 101, 116, 99, 104, 95, 97, 100, 100, 40, 111,
98, 106, 44, 32, 118, 97, 108, 41, 32, 40, 42, 40, 111, 98, 106, 41,
32, 43, 61, 32, 40, 118, 97, 108, 41, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102, 101, 116, 99, 104,
95, 115, 117, 98, 40, 111, 98, 106, 44, 32, 118, 97, 108, 41, 32, 40,
42, 40, 111, 98, 106, 41, 32, 45, 61, 32, 40, 118, 97, 108, 41, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99,
95, 102, 101, 116, 99, 104, 95, 111, 114, 40, 111, 98, 106, 44, 32,
118, 97, 108, 41, 32, 40, 42, 40, 111, 98, 106, 41, 32, 124, 61, 32,
40, 118, 97, 108, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
97, 116, 111, 109, 105, 99, 95, 102, 101, 116, 99, 104, 95, 120, 111,
114, 40, 111, 98, 106, 44, 32, 118, 97, 108, 41, 32, 40, 42, 40, 111,
98, 106, 41, 32, 94, 61, 32, 40, 118, 97, 108, 41, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102, 101,
116, 99, 104, 95, 97, 110, 100, 40, 111, 98, 106, 44, 32, 118, 97,
108, 41, 32, 40, 42, 40, 111, 98, 106, 41, 32, 38, 61, 32, 40, 118,
97, 108, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97,
116, 111, 109, 105, 99, 95, 102, 101, 116, 99, 104, 95, 97, 100, 100,
95, 101, 120, 112, 108, 105, 99, 105, 116, 40, 111, 98, 106, 44, 32,
118, 97, 108, 44, 32, 111, 114, 100, 101, 114, 41, 32, 40, 42, 40,
111, 98, 106, 41, 32, 43, 61, 32, 40, 118, 97, 108, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102,
101, 116, 99, 104, 95, 115, 117, 98, 95, 101, 120, 112, 108, 105, 99,
105, 116, 40, 111, 98, 106, 44, 32, 118, 97, 108, 44, 32, 111, 114,
100, 101, 114, 41, 32, 40, 42, 40, 111, 98, 106, 41, 32, 45, 61, 32,
40, 118, 97, 108, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
97, 116, 111, 109, 105, 99, 95, 102, 101, 116, 99, 104, 95, 111, 114,
95, 101, 120, 112, 108, 105, 99, 105, 116, 40, 111, 98, 106, 44, 32,
118, 97, 108, 44, 32, 111, 114, 100, 101, 114, 41, 32, 40, 42, 40,
111, 98, 106, 41, 32, 124, 61, 32, 40, 118, 97, 108, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102,
101, 116, 99, 104, 95, 120, 111, 114, 95, 101, 120, 112, 108, 105, 99,
105, 116, 40, 111, 98, 106, 44, 32, 118, 97, 108, 44, 32, 111, 114,
100, 101, 114, 41, 32, 40, 42, 40, 111, 98, 106, 41, 32, 94, 61, 32,
40, 118, 97, 108, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
97, 116, 111, 109, 105, 99, 95, 102, 101, 116, 99, 104, 95, 97, 110,
100, 95, 101, 120, 112, 108, 105, 99, 105, 116, 40, 111, 98, 106, 44,
32, 118, 97, 108, 44, 32, 111, 114, 100, 101, 114, 41, 32, 40, 42, 40,
111, 98, 106, 41, 32, 38, 61, 32, 40, 118, 97, 108, 41, 41, 10, 10,
35, 100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95,
99, 111, 109, 112, 97, 114, 101, 95, 101, 120, 99, 104, 97, 110, 103,
101, 95, 119, 101, 97, 107, 40, 112, 44, 32, 111, 108, 100, 44, 32,
110, 101, 119, 41, 32, 95, 95, 98, 117, 105, 108, 116, 105, 110, 95,
99, 111, 109, 112, 97, 114, 101, 95, 97, 110, 100, 95, 115, 119, 97,
112, 40, 40, 112, 41, 44, 32, 40, 111, 108, 100, 41, 44, 32, 40, 110,
101, 119, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97,
116, 111, 109, 105, 99, 95, 99, 111, 109, 112, 97, 114, 101, 95, 101,
120, 99, 104, 97, 110, 103, 101, 95, 115, 116, 114, 111, 110, 103, 40,
112, 44, 32, 111, 108, 100, 44, 32, 110, 101, 119, 41, 32, 95, 95, 98,
117, 105, 108, 116, 105, 110, 95, 99, 111, 109, 112, 97, 114, 101, 95,
97, 110, 100, 95, 115, 119, 97, 112, 40, 40, 112, 41, 44, 32, 40, 111,
108, 100, 41, 44, 32, 40, 110, 101, 119, 41, 41, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 101, 120, 99,
104, 97, 110, 103, 101, 40, 111, 98, 106, 44, 32, 118, 97, 108, 41,
32, 95, 95, 98, 117, 105, 108, 116, 105, 110, 95, 97, 116, 111, 109,
105, 99, 95, 101, 120, 99, 104, 97, 110, 103, 101, 40, 40, 111, 98,
106, 41, 44, 32, 40, 118, 97, 108, 41, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 101, 120, 99, 104, 97,
110, 103, 101, 95, 101, 120, 112, 108, 105, 99, 105, 116, 40, 111, 98,
106, 44, 32, 118, 97, 108, 44, 32, 111, 114, 100, 101, 114, 41, 32,
95, 95, 98, 117, 105, 108, 116, 105, 110, 95, 97, 116, 111, 109, 105,
99, 95, 101, 120, 99, 104, 97, 110, 103, 101, 40, 40, 111, 98, 106,
41, 44, 32, 40, 118, 97, 108, 41, 41, 10, 10, 35, 100, 101, 102, 105,
110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102, 108, 97, 103, 95,
116, 101, 115, 116, 95, 97, 110, 100, 95, 115, 101, 116, 40, 111, 98,
106, 41, 32, 97, 116, 111, 109, 105, 99, 95, 101, 120, 99, 104, 97,
110, 103, 101, 40, 40, 111, 98, 106, 41, 44, 32, 49, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102, 108,
97, 103, 95, 116, 101, 115, 116, 95, 97, 110, 100, 95, 115, 101, 116,
95, 101, 120, 112, 108, 105, 99, 105, 116, 40, 111, 98, 106, 44, 32,
111, 114, 100, 101, 114, 41, 32, 97, 116, 111, 109, 105, 99, 95, 101,
120, 99, 104, 97, 110, 103, 101, 40, 40, 111, 98, 106, 41, 44, 32, 49,
41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105,
99, 95, 102, 108, 97, 103, 95, 99, 108, 101, 97, 114, 40, 111, 98,
106, 41, 32, 40, 42, 40, 111, 98, 106, 41, 32, 61, 32, 48, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 97, 116, 111, 109, 105, 99, 95, 102,
108, 97, 103, 95, 99, 108, 101, 97, 114, 95, 101, 120, 112, 108, 105,
99, 105, 116, 40, 111, 98, 106, 44, 32, 111, 114, 100, 101, 114, 41,
32, 40, 42, 40, 111, 98, 106, 41, 32, 61, 32, 48, 41, 10, 10, 116,
121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32,
95, 66, 111, 111, 108, 32, 97, 116, 111, 109, 105, 99, 95, 102, 108,
97, 103, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116,
111, 109, 105, 99, 32, 95, 66, 111, 111, 108, 32, 97, 116, 111, 109,
105, 99, 95, 98, 111, 111, 108, 59, 10, 116, 121, 112, 101, 100, 101,
102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 99, 104, 97, 114, 32, 97,
116, 111, 109, 105, 99, 95, 99, 104, 97, 114, 59, 10, 116, 121, 112,
101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 115, 105,
103, 110, 101, 100, 32, 99, 104, 97, 114, 32, 97, 116, 111, 109, 105,
99, 95, 115, 99, 104, 97, 114, 59, 10, 116, 121, 112, 101, 100, 101,
102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103,
110, 101, 100, 32, 99, 104, 97, 114, 32, 97, 116, 111, 109, 105, 99,
95, 117, 99, 104, 97, 114, 59, 10, 116, 121, 112, 101, 100, 101, 102,
32, 95, 65, 116, 111, 109, 105, 99, 32, 115, 104, 111, 114, 116, 32,
97, 116, 111, 109, 105, 99, 95, 115, 104, 111, 114, 116, 59, 10, 116,
121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32,
117, 110, 115, 105, 103, 110, 101, 100, 32, 115, 104, 111, 114, 116,
32, 97, 116, 111, 109, 105, 99, 95, 117, 115, 104, 111, 114, 116, 59,
10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105,
99, 32, 105, 110, 116, 32, 97, 116, 111, 109, 105, 99, 95, 105, 110,
116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111,
109, 105, 99, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32, 105,
110, 116, 32, 97, 116, 111, 109, 105, 99, 95, 117, 105, 110, 116, 59,
10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105,
99, 32, 108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 108,
111, 110, 103, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65,
116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110, 101, 100,
32, 108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 117, 108,
111, 110, 103, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65,
116, 111, 109, 105, 99, 32, 108, 111, 110, 103, 32, 108, 111, 110,
103, 32, 97, 116, 111, 109, 105, 99, 95, 108, 108, 111, 110, 103, 59,
10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105,
99, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32, 108, 111, 110,
103, 32, 108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 117,
108, 108, 111, 110, 103, 59, 10, 116, 121, 112, 101, 100, 101, 102,
32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110,
101, 100, 32, 115, 104, 111, 114, 116, 32, 97, 116, 111, 109, 105, 99,
95, 99, 104, 97, 114, 49, 54, 95, 116, 59, 10, 116, 121, 112, 101,
100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115,
105, 103, 110, 101, 100, 32, 97, 116, 111, 109, 105, 99, 95, 99, 104,
97, 114, 51, 50, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102,
32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110,
101, 100, 32, 97, 116, 111, 109, 105, 99, 95, 119, 99, 104, 97, 114,
95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116,
111, 109, 105, 99, 32, 115, 105, 103, 110, 101, 100, 32, 99, 104, 97,
114, 32, 97, 116, 111, 109, 105, 99, 95, 105, 110, 116, 95, 108, 101,
97, 115, 116, 56, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102,
32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110,
101, 100, 32, 99, 104, 97, 114, 32, 97, 116, 111, 109, 105, 99, 95,
117, 105, 110, 116, 95, 108, 101, 97, 115, 116, 56, 95, 116, 59, 10,
116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99,
32, 115, 104, 111, 114, 116, 32, 97, 116, 111, 109, 105, 99, 95, 105,
110, 116, 95, 108, 101, 97, 115, 116, 49, 54, 95, 116, 59, 10, 116,
121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32,
117, 110, 115, 105, 103, 110, 101, 100, 32, 115, 104, 111, 114, 116,
32, 97, 116, 111, 109, 105, 99, 95, 117, 105, 110, 116, 95, 108, 101,
97, 115, 116, 49, 54, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101,
102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 105, 110, 116, 32, 97,
116, 111, 109, 105, 99, 95, 105, 110, 116, 95, 108, 101, 97, 115, 116,
51, 50, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95,
65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110, 101,
100, 32, 105, 110, 116, 32, 97, 116, 111, 109, 105, 99, 95, 117, 105,
110, 116, 95, 108, 101, 97, 115, 116, 51, 50, 95, 116, 59, 10, 116,
121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32,
108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 105, 110, 116,
95, 108, 101, 97, 115, 116, 54, 52, 95, 116, 59, 10, 116, 121, 112,
101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110,
115, 105, 103, 110, 101, 100, 32, 108, 111, 110, 103, 32, 97, 116,
111, 109, 105, 99, 95, 117, 105, 110, 116, 95, 108, 101, 97, 115, 116,
54, 52, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95,
65, 116, 111, 109, 105, 99, 32, 115, 105, 103, 110, 101, 100, 32, 99,
104, 97, 114, 32, 97, 116, 111, 109, 105, 99, 95, 105, 110, 116, 95,
102, 97, 115, 116, 56, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101,
102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103,
110, 101, 100, 32, 99, 104, 97, 114, 32, 97, 116, 111, 109, 105, 99,
95, 117, 105, 110, 116, 95, 102, 97, 115, 116, 56, 95, 116, 59, 10,
116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99,
32, 115, 104, 111, 114, 116, 32, 97, 116, 111, 109, 105, 99, 95, 105,
110, 116, 95, 102, 97, 115, 116, 49, 54, 95, 116, 59, 10, 116, 121,
112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 117,
110, 115, 105, 103, 110, 101, 100, 32, 115, 104, 111, 114, 116, 32,
97, 116, 111, 109, 105, 99, 95, 117, 105, 110, 116, 95, 102, 97, 115,
116, 49, 54, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32,
95, 65, 116, 111, 109, 105, 99, 32, 105, 110, 116, 32, 97, 116, 111,
109, 105, 99, 95, 105, 110, 116, 95, 102, 97, 115, 116, 51, 50, 95,
116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111,
109, 105, 99, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32, 105,
110, 116, 32, 97, 116, 111, 109, 105, 99, 95, 117, 105, 110, 116, 95,
102, 97, 115, 116, 51, 50, 95, 116, 59, 10, 116, 121, 112, 101, 100,
101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 108, 111, 110, 103,
32, 97, 116, 111, 109, 105, 99, 95, 105, 110, 116, 95, 102, 97, 115,
116, 54, 52, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32,
95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110, 101,
100, 32, 108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 117,
105, 110, 116, 95, 102, 97, 115, 116, 54, 52, 95, 116, 59, 10, 116,
121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32,
108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 105, 110, 116,
112, 116, 114, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32,
95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115, 105, 103, 110, 101,
100, 32, 108, 111, 110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 117,
105, 110, 116, 112, 116, 114, 95, 116, 59, 10, 116, 121, 112, 101,
100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 117, 110, 115,
105, 103, 110, 101, 100, 32, 108, 111, 110, 103, 32, 97, 116, 111,
109, 105, 99, 95, 115, 105, 122, 101, 95, 116, 59, 10, 116, 121, 112,
101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32, 108, 111,
110, 103, 32, 97, 116, 111, 109, 105, 99, 95, 112, 116, 114, 100, 105,
102, 102, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95,
65, 116, 111, 109, 105, 99, 32, 108, 111, 110, 103, 32, 97, 116, 111,
109, 105, 99, 95, 105, 110, 116, 109, 97, 120, 95, 116, 59, 10, 116,
121, 112, 101, 100, 101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 32,
117, 110, 115, 105, 103, 110, 101, 100, 32, 108, 111, 110, 103, 32,
97, 116, 111, 109, 105, 99, 95, 117, 105, 110, 116, 109, 97, 120, 95,
116, 59, 10, 10, 35, 101, 110, 100, 105, 102, 10, 0, 35, 105, 102,
110, 100, 101, 102, 32, 95, 95, 83, 84, 68, 78, 79, 82, 69, 84, 85,
82, 78, 95, 72, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 83,
84, 68, 78, 79, 82, 69, 84, 85, 82, 78, 95, 72, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 110, 111, 114, 101, 116, 117, 114, 110, 32,
95, 78, 111, 114, 101, 116, 117, 114, 110, 10, 10, 35, 101, 110, 100,
105, 102, 10, 0, 47, 47, 32, 66, 69, 71, 73, 78, 32, 114, 101, 103,
101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101,
97, 100, 101, 114, 115, 46, 112, 121, 10, 35, 105, 102, 110, 100, 101,
102, 32, 95, 95, 100, 121, 105, 98, 105, 99, 99, 95, 105, 110, 116,
101, 114, 110, 97, 108, 95, 105, 110, 99, 108, 117, 100, 101, 95, 95,
10, 35, 101, 114, 114, 111, 114, 32, 67, 97, 110, 32, 111, 110, 108,
121, 32, 98, 101, 32, 105, 110, 99, 108, 117, 100, 101, 100, 32, 98,
121, 32, 116, 104, 101, 32, 99, 111, 109, 112, 105, 108, 101, 114, 44,
32, 111, 114, 32, 99, 111, 110, 102, 117, 115, 105, 110, 103, 32, 101,
114, 114, 111, 114, 115, 32, 119, 105, 108, 108, 32, 114, 101, 115,
117, 108, 116, 33, 10, 35, 101, 110, 100, 105, 102, 10, 35, 117, 110,
100, 101, 102, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101,
95, 95, 10, 116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 115, 105,
103, 110, 101, 100, 32, 99, 104, 97, 114, 32, 117, 105, 110, 116, 56,
95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 115,
105, 103, 110, 101, 100, 32, 108, 111, 110, 103, 32, 108, 111, 110,
103, 32, 117, 105, 110, 116, 54, 52, 95, 116, 59, 10, 116, 121, 112,
101, 100, 101, 102, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32,
108, 111, 110, 103, 32, 108, 111, 110, 103, 32, 115, 105, 122, 101,
95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 108, 111, 110,
103, 32, 108, 111, 110, 103, 32, 105, 110, 116, 54, 52, 95, 116, 59,
10, 116, 121, 112, 101, 100, 101, 102, 32, 108, 111, 110, 103, 32,
108, 111, 110, 103, 32, 105, 110, 116, 112, 116, 114, 95, 116, 59, 10,
116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 115, 105, 103, 110,
101, 100, 32, 105, 110, 116, 32, 117, 105, 110, 116, 51, 50, 95, 116,
59, 10, 118, 111, 105, 100, 42, 32, 109, 101, 109, 115, 101, 116, 40,
118, 111, 105, 100, 42, 32, 100, 101, 115, 116, 44, 32, 105, 110, 116,
32, 99, 104, 44, 32, 115, 105, 122, 101, 95, 116, 32, 99, 111, 117,
110, 116, 41, 59, 10, 118, 111, 105, 100, 42, 32, 109, 101, 109, 99,
112, 121, 40, 118, 111, 105, 100, 42, 32, 100, 101, 115, 116, 44, 32,
99, 111, 110, 115, 116, 32, 118, 111, 105, 100, 42, 32, 115, 114, 99,
44, 32, 115, 105, 122, 101, 95, 116, 32, 99, 111, 117, 110, 116, 41,
59, 10, 105, 110, 116, 32, 109, 101, 109, 99, 109, 112, 40, 99, 111,
110, 115, 116, 32, 118, 111, 105, 100, 42, 32, 108, 104, 115, 44, 32,
99, 111, 110, 115, 116, 32, 118, 111, 105, 100, 42, 32, 114, 104, 115,
44, 32, 115, 105, 122, 101, 95, 116, 32, 99, 111, 117, 110, 116, 41,
59, 10, 118, 111, 105, 100, 42, 32, 109, 101, 109, 109, 111, 118, 101,
40, 118, 111, 105, 100, 42, 32, 100, 101, 115, 116, 44, 32, 99, 111,
110, 115, 116, 32, 118, 111, 105, 100, 42, 32, 115, 114, 99, 44, 32,
115, 105, 122, 101, 95, 116, 32, 99, 111, 117, 110, 116, 41, 59, 10,
115, 105, 122, 101, 95, 116, 32, 115, 116, 114, 108, 101, 110, 40, 99,
111, 110, 115, 116, 32, 99, 104, 97, 114, 42, 32, 115, 116, 114, 41,
59, 10, 118, 111, 105, 100, 32, 102, 114, 101, 101, 40, 118, 111, 105,
100, 42, 32, 112, 116, 114, 41, 59, 10, 118, 111, 105, 100, 32, 42,
109, 97, 108, 108, 111, 99, 40, 115, 105, 122, 101, 95, 116, 32, 115,
105, 122, 101, 41, 59, 10, 118, 111, 105, 100, 32, 42, 99, 97, 108,
108, 111, 99, 40, 115, 105, 122, 101, 95, 116, 32, 110, 117, 109, 44,
32, 115, 105, 122, 101, 95, 116, 32, 115, 105, 122, 101, 41, 59, 10,
118, 111, 105, 100, 42, 32, 114, 101, 97, 108, 108, 111, 99, 40, 118,
111, 105, 100, 42, 32, 112, 116, 114, 44, 32, 115, 105, 122, 101, 95,
116, 32, 110, 101, 119, 95, 115, 105, 122, 101, 41, 59, 10, 35, 100,
101, 102, 105, 110, 101, 32, 78, 85, 76, 76, 32, 40, 40, 118, 111,
105, 100, 42, 41, 48, 41, 32, 47, 42, 32, 116, 111, 100, 111, 33, 32,
42, 47, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 32, 114, 101, 103,
101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101,
97, 100, 101, 114, 115, 46, 112, 121, 10, 47, 47, 32, 35, 35, 35, 32,
66, 69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68,
69, 58, 32, 99, 109, 97, 112, 46, 104, 10, 10, 47, 47, 32, 85, 110,
111, 114, 100, 101, 114, 101, 100, 32, 115, 101, 116, 47, 109, 97,
112, 32, 45, 32, 105, 109, 112, 108, 101, 109, 101, 110, 116, 101,
100, 32, 97, 115, 32, 99, 108, 111, 115, 101, 100, 32, 104, 97, 115,
104, 105, 110, 103, 32, 119, 105, 116, 104, 32, 108, 105, 110, 101,
97, 114, 32, 112, 114, 111, 98, 105, 110, 103, 32, 97, 110, 100, 32,
110, 111, 32, 116, 111, 109, 98, 115, 116, 111, 110, 101, 115, 46, 10,
47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73, 78, 95, 70, 73, 76, 69,
95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 108, 105, 110, 107, 97, 103,
101, 46, 104, 10, 35, 117, 110, 100, 101, 102, 32, 83, 84, 67, 95, 65,
80, 73, 10, 35, 117, 110, 100, 101, 102, 32, 83, 84, 67, 95, 68, 69,
70, 10, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100,
32, 105, 95, 115, 116, 97, 116, 105, 99, 32, 32, 38, 38, 32, 33, 100,
101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 83, 84, 65, 84, 73,
67, 32, 32, 38, 38, 32, 40, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 104, 101, 97, 100, 101, 114, 32, 124, 124, 32, 100, 101, 102,
105, 110, 101, 100, 32, 83, 84, 67, 95, 72, 69, 65, 68, 69, 82, 32,
32, 124, 124, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105,
95, 105, 109, 112, 108, 101, 109, 101, 110, 116, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 73, 77, 80, 76,
69, 77, 69, 78, 84, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 83, 84, 67, 95, 65, 80, 73, 32, 101, 120, 116, 101, 114, 110, 10,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 68, 69,
70, 10, 35, 101, 108, 115, 101, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 115, 116, 97, 116, 105, 99, 10, 32, 32, 35,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78,
85, 67, 95, 95, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100,
32, 95, 95, 99, 108, 97, 110, 103, 95, 95, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 65, 80, 73, 32, 115,
116, 97, 116, 105, 99, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117,
116, 101, 95, 95, 40, 40, 117, 110, 117, 115, 101, 100, 41, 41, 10,
32, 32, 35, 101, 108, 115, 101, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 83, 84, 67, 95, 65, 80, 73, 32, 115, 116, 97, 116,
105, 99, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 68, 69, 70, 32, 115, 116,
97, 116, 105, 99, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102,
32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 73, 77, 80,
76, 69, 77, 69, 78, 84, 32, 124, 124, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 105, 109, 112, 111, 114, 116, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 105, 109, 112, 108, 101,
109, 101, 110, 116, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 65,
76, 76, 79, 67, 65, 84, 79, 82, 32, 38, 38, 32, 33, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111,
114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 97,
108, 108, 111, 99, 97, 116, 111, 114, 32, 83, 84, 67, 95, 65, 76, 76,
79, 67, 65, 84, 79, 82, 10, 35, 101, 108, 105, 102, 32, 33, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 97, 108, 108, 111, 99, 97, 116,
111, 114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
97, 108, 108, 111, 99, 97, 116, 111, 114, 32, 99, 10, 35, 101, 110,
100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 109,
97, 108, 108, 111, 99, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 109, 97, 108, 108, 111, 99, 32, 99, 95, 74, 79, 73, 78,
40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114, 44, 32, 95,
109, 97, 108, 108, 111, 99, 41, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 99, 97, 108, 108, 111, 99, 32, 99, 95, 74, 79,
73, 78, 40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114, 44, 32,
95, 99, 97, 108, 108, 111, 99, 41, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 114, 101, 97, 108, 108, 111, 99, 32, 99, 95,
74, 79, 73, 78, 40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114,
44, 32, 95, 114, 101, 97, 108, 108, 111, 99, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 102, 114, 101, 101, 32, 99, 95,
74, 79, 73, 78, 40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114,
44, 32, 95, 102, 114, 101, 101, 41, 10, 35, 101, 110, 100, 105, 102,
10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95,
95, 99, 108, 97, 110, 103, 95, 95, 32, 38, 38, 32, 33, 100, 101, 102,
105, 110, 101, 100, 32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117,
115, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110,
103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 112, 117,
115, 104, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97,
110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119,
97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 97, 108, 108, 34, 10, 32,
32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110,
105, 110, 103, 32, 34, 45, 87, 101, 120, 116, 114, 97, 34, 10, 32, 32,
35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103, 32, 100,
105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110, 105,
110, 103, 32, 34, 45, 87, 112, 101, 100, 97, 110, 116, 105, 99, 34,
10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103,
32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114,
110, 105, 110, 103, 32, 34, 45, 87, 99, 111, 110, 118, 101, 114, 115,
105, 111, 110, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99,
108, 97, 110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99,
32, 119, 97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 100, 111, 117,
98, 108, 101, 45, 112, 114, 111, 109, 111, 116, 105, 111, 110, 34, 10,
32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110,
105, 110, 103, 32, 34, 45, 87, 119, 114, 105, 116, 101, 45, 115, 116,
114, 105, 110, 103, 115, 34, 10, 32, 32, 47, 47, 32, 105, 103, 110,
111, 114, 101, 100, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32,
99, 108, 97, 110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105,
99, 32, 105, 103, 110, 111, 114, 101, 100, 32, 34, 45, 87, 109, 105,
115, 115, 105, 110, 103, 45, 102, 105, 101, 108, 100, 45, 105, 110,
105, 116, 105, 97, 108, 105, 122, 101, 114, 115, 34, 10, 35, 101, 108,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78,
85, 67, 95, 95, 32, 38, 38, 32, 33, 100, 101, 102, 105, 110, 101, 100,
32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117, 115, 10, 32, 32,
35, 112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32, 100, 105, 97, 103,
110, 111, 115, 116, 105, 99, 32, 112, 117, 115, 104, 10, 32, 32, 35,
112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32, 100, 105, 97, 103,
110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110, 105, 110, 103, 32,
34, 45, 87, 97, 108, 108, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109,
97, 32, 71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105,
99, 32, 119, 97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 101, 120,
116, 114, 97, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71,
67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119,
97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 112, 101, 100, 97, 110,
116, 105, 99, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71,
67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119,
97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 99, 111, 110, 118, 101,
114, 115, 105, 111, 110, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109,
97, 32, 71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105,
99, 32, 119, 97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 100, 111,
117, 98, 108, 101, 45, 112, 114, 111, 109, 111, 116, 105, 111, 110,
34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110,
105, 110, 103, 32, 34, 45, 87, 119, 114, 105, 116, 101, 45, 115, 116,
114, 105, 110, 103, 115, 34, 10, 32, 32, 47, 47, 32, 105, 103, 110,
111, 114, 101, 100, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32,
71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32,
105, 103, 110, 111, 114, 101, 100, 32, 34, 45, 87, 109, 105, 115, 115,
105, 110, 103, 45, 102, 105, 101, 108, 100, 45, 105, 110, 105, 116,
105, 97, 108, 105, 122, 101, 114, 115, 34, 10, 35, 101, 110, 100, 105,
102, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73, 76, 69,
95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 108, 105, 110, 107, 97, 103,
101, 46, 104, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 67, 77,
65, 80, 95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 47, 47, 32,
35, 35, 35, 32, 66, 69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73, 78,
67, 76, 85, 68, 69, 58, 32, 99, 99, 111, 109, 109, 111, 110, 46, 104,
10, 35, 105, 102, 110, 100, 101, 102, 32, 67, 67, 79, 77, 77, 79, 78,
95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 35, 100, 101, 102,
105, 110, 101, 32, 67, 67, 79, 77, 77, 79, 78, 95, 72, 95, 73, 78, 67,
76, 85, 68, 69, 68, 10, 10, 35, 105, 102, 100, 101, 102, 32, 95, 77,
83, 67, 95, 86, 69, 82, 10, 32, 32, 32, 32, 35, 112, 114, 97, 103,
109, 97, 32, 119, 97, 114, 110, 105, 110, 103, 40, 100, 105, 115, 97,
98, 108, 101, 58, 32, 52, 49, 49, 54, 32, 52, 57, 57, 54, 41, 32, 47,
47, 32, 117, 110, 110, 97, 109, 101, 100, 32, 116, 121, 112, 101, 32,
100, 101, 102, 105, 110, 105, 116, 105, 111, 110, 32, 105, 110, 32,
112, 97, 114, 101, 110, 116, 104, 101, 115, 101, 115, 10, 35, 101,
110, 100, 105, 102, 10, 47, 47, 32, 69, 88, 67, 76, 85, 68, 69, 68,
32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110, 116, 97,
105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46, 112,
121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 105, 110, 116,
116, 121, 112, 101, 115, 46, 104, 62, 10, 47, 47, 32, 69, 88, 67, 76,
85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111,
110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114,
115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60,
115, 116, 100, 100, 101, 102, 46, 104, 62, 10, 47, 47, 32, 69, 88, 67,
76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99,
111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101,
114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32,
60, 115, 116, 100, 98, 111, 111, 108, 46, 104, 62, 10, 47, 47, 32, 69,
88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110,
95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100,
101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101,
32, 60, 115, 116, 114, 105, 110, 103, 46, 104, 62, 10, 47, 47, 32, 69,
88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110,
95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100,
101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101,
32, 60, 97, 115, 115, 101, 114, 116, 46, 104, 62, 10, 10, 116, 121,
112, 101, 100, 101, 102, 32, 108, 111, 110, 103, 32, 108, 111, 110,
103, 32, 95, 108, 108, 111, 110, 103, 59, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 78, 80, 79, 83, 32, 73, 78, 84, 80, 84, 82, 95,
77, 65, 88, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 90, 73,
32, 80, 82, 73, 105, 80, 84, 82, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 90, 85, 32, 80, 82, 73, 117, 80, 84, 82, 10, 10, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78, 85,
67, 95, 95, 32, 47, 47, 32, 105, 110, 99, 108, 117, 100, 101, 115, 32,
95, 95, 99, 108, 97, 110, 103, 95, 95, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69,
32, 115, 116, 97, 116, 105, 99, 32, 105, 110, 108, 105, 110, 101, 32,
95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 40, 40, 117, 110,
117, 115, 101, 100, 41, 41, 10, 35, 101, 108, 115, 101, 10, 32, 32,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 115, 116, 97, 116, 105, 99, 32, 105, 110, 108,
105, 110, 101, 10, 35, 101, 110, 100, 105, 102, 10, 10, 47, 42, 32,
77, 97, 99, 114, 111, 32, 111, 118, 101, 114, 108, 111, 97, 100, 105,
110, 103, 32, 102, 101, 97, 116, 117, 114, 101, 32, 115, 117, 112,
112, 111, 114, 116, 32, 98, 97, 115, 101, 100, 32, 111, 110, 58, 32,
104, 116, 116, 112, 115, 58, 47, 47, 114, 101, 120, 116, 101, 115,
116, 101, 114, 46, 99, 111, 109, 47, 79, 78, 80, 56, 48, 49, 48, 55,
32, 42, 47, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 77, 65,
67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 110, 97, 109, 101,
44, 32, 46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 99, 95, 74, 79,
73, 78, 40, 99, 95, 74, 79, 73, 78, 48, 40, 110, 97, 109, 101, 44, 95,
41, 44, 99, 95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 41, 41, 40, 95, 95, 86, 65, 95, 65, 82, 71,
83, 95, 95, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 74,
79, 73, 78, 48, 40, 97, 44, 32, 98, 41, 32, 97, 32, 35, 35, 32, 98,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 74, 79, 73, 78, 40,
97, 44, 32, 98, 41, 32, 99, 95, 74, 79, 73, 78, 48, 40, 97, 44, 32,
98, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 69, 88, 80,
65, 78, 68, 40, 46, 46, 46, 41, 32, 95, 95, 86, 65, 95, 65, 82, 71,
83, 95, 95, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 78, 85,
77, 65, 82, 71, 83, 40, 46, 46, 46, 41, 32, 95, 99, 95, 65, 80, 80,
76, 89, 95, 65, 82, 71, 95, 78, 40, 40, 95, 95, 86, 65, 95, 65, 82,
71, 83, 95, 95, 44, 32, 95, 99, 95, 82, 83, 69, 81, 95, 78, 41, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 65, 80, 80, 76,
89, 95, 65, 82, 71, 95, 78, 40, 97, 114, 103, 115, 41, 32, 99, 95, 69,
88, 80, 65, 78, 68, 40, 95, 99, 95, 65, 82, 71, 95, 78, 32, 97, 114,
103, 115, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95,
82, 83, 69, 81, 95, 78, 32, 49, 54, 44, 32, 49, 53, 44, 32, 49, 52,
44, 32, 49, 51, 44, 32, 49, 50, 44, 32, 49, 49, 44, 32, 49, 48, 44,
32, 57, 44, 32, 56, 44, 32, 55, 44, 32, 54, 44, 32, 53, 44, 32, 52,
44, 32, 51, 44, 32, 50, 44, 32, 49, 44, 32, 48, 10, 35, 100, 101, 102,
105, 110, 101, 32, 95, 99, 95, 65, 82, 71, 95, 78, 40, 95, 49, 44, 32,
95, 50, 44, 32, 95, 51, 44, 32, 95, 52, 44, 32, 95, 53, 44, 32, 95,
54, 44, 32, 95, 55, 44, 32, 95, 56, 44, 32, 95, 57, 44, 32, 95, 49,
48, 44, 32, 95, 49, 49, 44, 32, 95, 49, 50, 44, 32, 95, 49, 51, 44,
32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 95, 49, 52, 44, 32, 95, 49, 53, 44, 32, 95, 49, 54, 44,
32, 78, 44, 32, 46, 46, 46, 41, 32, 78, 10, 10, 35, 105, 102, 110,
100, 101, 102, 32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117, 115,
32, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105,
95, 97, 108, 108, 111, 99, 40, 84, 41, 32, 32, 32, 32, 32, 32, 32, 32,
32, 40, 40, 84, 42, 41, 105, 95, 109, 97, 108, 108, 111, 99, 40, 99,
95, 115, 105, 122, 101, 111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 110, 101, 119,
40, 84, 44, 32, 46, 46, 46, 41, 32, 32, 32, 32, 32, 32, 40, 40, 84,
42, 41, 109, 101, 109, 99, 112, 121, 40, 95, 105, 95, 97, 108, 108,
111, 99, 40, 84, 41, 44, 32, 40, 40, 84, 91, 93, 41, 123, 95, 95, 86,
65, 95, 65, 82, 71, 83, 95, 95, 125, 41, 44, 32, 115, 105, 122, 101,
111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 110, 101, 119, 40, 84, 44, 32, 46, 46, 46,
41, 32, 32, 32, 32, 32, 32, 32, 40, 40, 84, 42, 41, 109, 101, 109, 99,
112, 121, 40, 109, 97, 108, 108, 111, 99, 40, 115, 105, 122, 101, 111,
102, 40, 84, 41, 41, 44, 32, 40, 40, 84, 91, 93, 41, 123, 95, 95, 86,
65, 95, 65, 82, 71, 83, 95, 95, 125, 41, 44, 32, 115, 105, 122, 101,
111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 84, 41, 32,
32, 32, 32, 32, 32, 32, 32, 40, 84, 41, 10, 35, 101, 108, 115, 101,
10, 47, 47, 32, 69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114,
101, 103, 101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95,
104, 101, 97, 100, 101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32,
35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 110, 101, 119, 62, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 97,
108, 108, 111, 99, 40, 84, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32,
115, 116, 97, 116, 105, 99, 95, 99, 97, 115, 116, 60, 84, 42, 62, 40,
105, 95, 109, 97, 108, 108, 111, 99, 40, 99, 95, 115, 105, 122, 101,
111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 105, 95, 110, 101, 119, 40, 84, 44, 32, 46, 46,
46, 41, 32, 32, 32, 32, 32, 32, 110, 101, 119, 32, 40, 95, 105, 95,
97, 108, 108, 111, 99, 40, 84, 41, 41, 32, 84, 40, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 110, 101, 119, 40, 84, 44, 32, 46, 46, 46,
41, 32, 32, 32, 32, 32, 32, 32, 110, 101, 119, 32, 40, 109, 97, 108,
108, 111, 99, 40, 115, 105, 122, 101, 111, 102, 40, 84, 41, 41, 41,
32, 84, 40, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 32,
32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 76, 73, 84,
69, 82, 65, 76, 40, 84, 41, 32, 32, 32, 32, 32, 32, 32, 32, 84, 10,
35, 101, 110, 100, 105, 102, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 110, 101, 119, 95, 110, 40, 84, 44, 32, 110, 41, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 40, 40, 84, 42, 41, 109, 97, 108, 108,
111, 99, 40, 115, 105, 122, 101, 111, 102, 40, 84, 41, 42, 99, 95,
105, 50, 117, 95, 115, 105, 122, 101, 40, 110, 41, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 109, 97, 108, 108, 111, 99,
40, 115, 122, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 109,
97, 108, 108, 111, 99, 40, 99, 95, 105, 50, 117, 95, 115, 105, 122,
101, 40, 115, 122, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 99, 97, 108, 108, 111, 99, 40, 110, 44, 32, 115, 122, 41, 32,
32, 32, 32, 32, 32, 32, 32, 32, 99, 97, 108, 108, 111, 99, 40, 99, 95,
105, 50, 117, 95, 115, 105, 122, 101, 40, 110, 41, 44, 32, 99, 95,
105, 50, 117, 95, 115, 105, 122, 101, 40, 115, 122, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 114, 101, 97, 108, 108, 111,
99, 40, 112, 44, 32, 111, 108, 100, 95, 115, 122, 44, 32, 115, 122,
41, 32, 114, 101, 97, 108, 108, 111, 99, 40, 112, 44, 32, 99, 95, 105,
50, 117, 95, 115, 105, 122, 101, 40, 49, 32, 63, 32, 40, 115, 122, 41,
32, 58, 32, 40, 111, 108, 100, 95, 115, 122, 41, 41, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 102, 114, 101, 101, 40, 112, 44,
32, 115, 122, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 100,
111, 32, 123, 32, 40, 118, 111, 105, 100, 41, 40, 115, 122, 41, 59,
32, 102, 114, 101, 101, 40, 112, 41, 59, 32, 125, 32, 119, 104, 105,
108, 101, 40, 48, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99,
95, 100, 101, 108, 101, 116, 101, 40, 84, 44, 32, 112, 116, 114, 41,
32, 32, 32, 32, 32, 32, 32, 32, 100, 111, 32, 123, 32, 84, 32, 42, 95,
116, 112, 32, 61, 32, 112, 116, 114, 59, 32, 84, 35, 35, 95, 100, 114,
111, 112, 40, 95, 116, 112, 41, 59, 32, 102, 114, 101, 101, 40, 95,
116, 112, 41, 59, 32, 125, 32, 119, 104, 105, 108, 101, 32, 40, 48,
41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 116,
97, 116, 105, 99, 95, 97, 115, 115, 101, 114, 116, 40, 101, 120, 112,
114, 41, 32, 32, 32, 40, 49, 32, 63, 32, 48, 32, 58, 32, 40, 105, 110,
116, 41, 115, 105, 122, 101, 111, 102, 40, 105, 110, 116, 91, 40, 101,
120, 112, 114, 41, 32, 63, 32, 49, 32, 58, 32, 45, 49, 93, 41, 41, 10,
35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67,
95, 78, 68, 69, 66, 85, 71, 32, 124, 124, 32, 100, 101, 102, 105, 110,
101, 100, 32, 78, 68, 69, 66, 85, 71, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 97, 115, 115, 101, 114, 116, 40,
101, 120, 112, 114, 41, 32, 32, 32, 32, 32, 32, 40, 40, 118, 111, 105,
100, 41, 48, 41, 10, 35, 101, 108, 115, 101, 10, 35, 105, 102, 110,
100, 101, 102, 32, 83, 84, 67, 95, 65, 83, 83, 69, 82, 84, 10, 35,
100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 65, 83, 83, 69, 82,
84, 40, 101, 120, 112, 114, 41, 10, 35, 101, 110, 100, 105, 102, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 97, 115,
115, 101, 114, 116, 40, 101, 120, 112, 114, 41, 32, 32, 32, 32, 32,
32, 83, 84, 67, 95, 65, 83, 83, 69, 82, 84, 40, 101, 120, 112, 114,
41, 10, 35, 101, 110, 100, 105, 102, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 111,
102, 40, 112, 44, 32, 67, 44, 32, 109, 41, 32, 40, 40, 67, 42, 41, 40,
40, 99, 104, 97, 114, 42, 41, 40, 49, 32, 63, 32, 40, 112, 41, 32, 58,
32, 38, 40, 40, 67, 42, 41, 48, 41, 45, 62, 109, 41, 32, 45, 32, 111,
102, 102, 115, 101, 116, 111, 102, 40, 67, 44, 32, 109, 41, 41, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 99, 111, 110, 115,
116, 95, 99, 97, 115, 116, 40, 84, 112, 44, 32, 112, 41, 32, 32, 32,
32, 32, 40, 40, 84, 112, 41, 40, 49, 32, 63, 32, 40, 112, 41, 32, 58,
32, 40, 84, 112, 41, 48, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 115, 97, 102, 101, 95, 99, 97, 115, 116, 40, 84, 44, 32,
70, 44, 32, 120, 41, 32, 32, 32, 32, 40, 40, 84, 41, 40, 49, 32, 63,
32, 40, 120, 41, 32, 58, 32, 40, 70, 41, 123, 48, 125, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 119, 97, 112, 40, 84,
44, 32, 120, 112, 44, 32, 121, 112, 41, 32, 32, 32, 32, 32, 32, 32,
100, 111, 32, 123, 32, 84, 32, 42, 95, 120, 112, 32, 61, 32, 120, 112,
44, 32, 42, 95, 121, 112, 32, 61, 32, 121, 112, 44, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 95, 116, 118, 32, 61, 32, 42, 95, 120, 112, 59, 32, 42, 95, 120,
112, 32, 61, 32, 42, 95, 121, 112, 59, 32, 42, 95, 121, 112, 32, 61,
32, 95, 116, 118, 59, 32, 125, 32, 119, 104, 105, 108, 101, 32, 40,
48, 41, 10, 47, 47, 32, 117, 115, 101, 32, 119, 105, 116, 104, 32,
103, 99, 99, 32, 45, 87, 99, 111, 110, 118, 101, 114, 115, 105, 111,
110, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 105, 122,
101, 111, 102, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116, 41, 115, 105, 122,
101, 111, 102, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115,
116, 114, 108, 101, 110, 40, 115, 41, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116, 41,
115, 116, 114, 108, 101, 110, 40, 115, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 115, 116, 114, 110, 99, 109, 112, 40, 97, 44,
32, 98, 44, 32, 105, 108, 101, 110, 41, 32, 32, 32, 115, 116, 114,
110, 99, 109, 112, 40, 97, 44, 32, 98, 44, 32, 99, 95, 105, 50, 117,
95, 115, 105, 122, 101, 40, 105, 108, 101, 110, 41, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 109, 101, 109, 99, 112, 121, 40,
100, 44, 32, 115, 44, 32, 105, 108, 101, 110, 41, 32, 32, 32, 32, 109,
101, 109, 99, 112, 121, 40, 100, 44, 32, 115, 44, 32, 99, 95, 105, 50,
117, 95, 115, 105, 122, 101, 40, 105, 108, 101, 110, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 109, 101, 109, 109, 111,
118, 101, 40, 100, 44, 32, 115, 44, 32, 105, 108, 101, 110, 41, 32,
32, 32, 109, 101, 109, 109, 111, 118, 101, 40, 100, 44, 32, 115, 44,
32, 99, 95, 105, 50, 117, 95, 115, 105, 122, 101, 40, 105, 108, 101,
110, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 109,
101, 109, 115, 101, 116, 40, 100, 44, 32, 118, 97, 108, 44, 32, 105,
108, 101, 110, 41, 32, 32, 109, 101, 109, 115, 101, 116, 40, 100, 44,
32, 118, 97, 108, 44, 32, 99, 95, 105, 50, 117, 95, 115, 105, 122,
101, 40, 105, 108, 101, 110, 41, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 109, 101, 109, 99, 109, 112, 40, 97, 44, 32, 98, 44,
32, 105, 108, 101, 110, 41, 32, 32, 32, 32, 109, 101, 109, 99, 109,
112, 40, 97, 44, 32, 98, 44, 32, 99, 95, 105, 50, 117, 95, 115, 105,
122, 101, 40, 105, 108, 101, 110, 41, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 117, 50, 105, 95, 115, 105, 122, 101, 40, 117,
41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 105, 110, 116,
112, 116, 114, 95, 116, 41, 40, 49, 32, 63, 32, 40, 117, 41, 32, 58,
32, 40, 115, 105, 122, 101, 95, 116, 41, 49, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 105, 50, 117, 95, 115, 105, 122, 101,
40, 105, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 115, 105,
122, 101, 95, 116, 41, 40, 49, 32, 63, 32, 40, 105, 41, 32, 58, 32,
45, 49, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 108,
101, 115, 115, 95, 117, 110, 115, 105, 103, 110, 101, 100, 40, 97, 44,
32, 98, 41, 32, 32, 32, 40, 40, 115, 105, 122, 101, 95, 116, 41, 40,
97, 41, 32, 60, 32, 40, 115, 105, 122, 101, 95, 116, 41, 40, 98, 41,
41, 10, 10, 47, 47, 32, 120, 32, 97, 110, 100, 32, 121, 32, 97, 114,
101, 32, 105, 95, 107, 101, 121, 114, 97, 119, 42, 32, 116, 121, 112,
101, 44, 32, 100, 101, 102, 97, 117, 108, 116, 115, 32, 116, 111, 32,
105, 95, 107, 101, 121, 42, 58, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 109, 112, 40,
120, 44, 32, 121, 41, 32, 32, 32, 32, 32, 40, 99, 95, 100, 101, 102,
97, 117, 108, 116, 95, 108, 101, 115, 115, 40, 121, 44, 32, 120, 41,
32, 45, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 108, 101,
115, 115, 40, 120, 44, 32, 121, 41, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 108, 101,
115, 115, 40, 120, 44, 32, 121, 41, 32, 32, 32, 32, 40, 42, 40, 120,
41, 32, 60, 32, 42, 40, 121, 41, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 101, 113, 40,
120, 44, 32, 121, 41, 32, 32, 32, 32, 32, 32, 40, 42, 40, 120, 41, 32,
61, 61, 32, 42, 40, 121, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 109, 101, 109, 99, 109, 112, 95, 101, 113, 40, 120, 44,
32, 121, 41, 32, 32, 32, 32, 32, 32, 32, 40, 109, 101, 109, 99, 109,
112, 40, 120, 44, 32, 121, 44, 32, 115, 105, 122, 101, 111, 102, 32,
42, 40, 120, 41, 41, 32, 61, 61, 32, 48, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 104,
97, 115, 104, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 115, 116, 99,
95, 104, 97, 115, 104, 95, 49, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 108, 111,
110, 101, 40, 118, 41, 32, 32, 32, 32, 32, 32, 40, 118, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108,
116, 95, 116, 111, 114, 97, 119, 40, 118, 112, 41, 32, 32, 32, 32, 32,
40, 42, 40, 118, 112, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 100, 114, 111, 112,
40, 118, 112, 41, 32, 32, 32, 32, 32, 32, 40, 40, 118, 111, 105, 100,
41, 32, 40, 118, 112, 41, 41, 10, 10, 47, 42, 32, 70, 117, 110, 99,
116, 105, 111, 110, 32, 109, 97, 99, 114, 111, 115, 32, 97, 110, 100,
32, 111, 116, 104, 101, 114, 115, 32, 42, 47, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 108, 105, 116, 115, 116, 114, 108,
101, 110, 40, 108, 105, 116, 101, 114, 97, 108, 41, 32, 40, 99, 95,
115, 105, 122, 101, 111, 102, 40, 34, 34, 32, 108, 105, 116, 101, 114,
97, 108, 41, 32, 45, 32, 49, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 97, 114, 114, 97, 121, 108, 101, 110, 40, 97, 41, 32, 40,
105, 110, 116, 112, 116, 114, 95, 116, 41, 40, 115, 105, 122, 101,
111, 102, 40, 97, 41, 47, 115, 105, 122, 101, 111, 102, 32, 48, 91,
97, 93, 41, 10, 10, 47, 47, 32, 78, 111, 110, 45, 111, 119, 110, 105,
110, 103, 32, 99, 45, 115, 116, 114, 105, 110, 103, 32, 34, 99, 108,
97, 115, 115, 34, 10, 116, 121, 112, 101, 100, 101, 102, 32, 99, 111,
110, 115, 116, 32, 99, 104, 97, 114, 42, 32, 99, 99, 104, 97, 114,
112, 116, 114, 59, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 99,
104, 97, 114, 112, 116, 114, 95, 99, 109, 112, 40, 120, 112, 44, 32,
121, 112, 41, 32, 115, 116, 114, 99, 109, 112, 40, 42, 40, 120, 112,
41, 44, 32, 42, 40, 121, 112, 41, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 99, 104, 97, 114, 112, 116, 114, 95, 104, 97, 115, 104,
40, 112, 41, 32, 115, 116, 99, 95, 115, 116, 114, 104, 97, 115, 104,
40, 42, 40, 112, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99,
99, 104, 97, 114, 112, 116, 114, 95, 99, 108, 111, 110, 101, 40, 115,
41, 32, 40, 115, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 99,
104, 97, 114, 112, 116, 114, 95, 100, 114, 111, 112, 40, 112, 41, 32,
40, 40, 118, 111, 105, 100, 41, 112, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 115, 118, 40, 46, 46, 46, 41, 32, 99, 95,
77, 65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95,
115, 118, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 118, 95, 49, 40,
108, 105, 116, 101, 114, 97, 108, 41, 32, 99, 95, 115, 118, 95, 50,
40, 108, 105, 116, 101, 114, 97, 108, 44, 32, 99, 95, 108, 105, 116,
115, 116, 114, 108, 101, 110, 40, 108, 105, 116, 101, 114, 97, 108,
41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 118,
95, 50, 40, 115, 116, 114, 44, 32, 110, 41, 32, 40, 99, 95, 76, 73,
84, 69, 82, 65, 76, 40, 99, 115, 118, 105, 101, 119, 41, 123, 115,
116, 114, 44, 32, 110, 125, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 83, 86, 40, 115, 118, 41, 32, 40, 105, 110, 116, 41, 40,
115, 118, 41, 46, 115, 105, 122, 101, 44, 32, 40, 115, 118, 41, 46,
98, 117, 102, 32, 47, 47, 32, 112, 114, 105, 110, 116, 102, 40, 34,
37, 46, 42, 115, 92, 110, 34, 44, 32, 99, 95, 83, 86, 40, 115, 118,
41, 41, 59, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 114,
115, 40, 108, 105, 116, 101, 114, 97, 108, 41, 32, 99, 95, 114, 115,
95, 50, 40, 108, 105, 116, 101, 114, 97, 108, 44, 32, 99, 95, 108,
105, 116, 115, 116, 114, 108, 101, 110, 40, 108, 105, 116, 101, 114,
97, 108, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95,
114, 115, 95, 50, 40, 115, 116, 114, 44, 32, 110, 41, 32, 40, 99, 95,
76, 73, 84, 69, 82, 65, 76, 40, 99, 114, 97, 119, 115, 116, 114, 41,
123, 115, 116, 114, 44, 32, 110, 125, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 82, 79, 84, 76, 40, 120, 44, 32, 107, 41,
32, 40, 120, 32, 60, 60, 32, 40, 107, 41, 32, 124, 32, 120, 32, 62,
62, 32, 40, 56, 42, 115, 105, 122, 101, 111, 102, 40, 120, 41, 32, 45,
32, 40, 107, 41, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
115, 116, 99, 95, 104, 97, 115, 104, 40, 46, 46, 46, 41, 32, 99, 95,
77, 65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 115, 116,
99, 95, 104, 97, 115, 104, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83,
95, 95, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 115, 116, 99,
95, 104, 97, 115, 104, 95, 49, 40, 120, 41, 32, 115, 116, 99, 95, 104,
97, 115, 104, 95, 50, 40, 120, 44, 32, 99, 95, 115, 105, 122, 101,
111, 102, 40, 42, 40, 120, 41, 41, 41, 10, 10, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 117, 105, 110, 116, 54, 52, 95, 116, 32, 115, 116,
99, 95, 104, 97, 115, 104, 95, 50, 40, 99, 111, 110, 115, 116, 32,
118, 111, 105, 100, 42, 32, 107, 101, 121, 44, 32, 105, 110, 116, 112,
116, 114, 95, 116, 32, 108, 101, 110, 41, 32, 123, 10, 32, 32, 32, 32,
117, 105, 110, 116, 51, 50, 95, 116, 32, 117, 52, 59, 32, 117, 105,
110, 116, 54, 52, 95, 116, 32, 117, 56, 59, 10, 32, 32, 32, 32, 115,
119, 105, 116, 99, 104, 32, 40, 108, 101, 110, 41, 32, 123, 10, 32,
32, 32, 32, 32, 32, 32, 32, 99, 97, 115, 101, 32, 56, 58, 32, 109,
101, 109, 99, 112, 121, 40, 38, 117, 56, 44, 32, 107, 101, 121, 44,
32, 56, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32, 117, 56, 42, 48,
120, 99, 54, 97, 52, 97, 55, 57, 51, 53, 98, 100, 49, 101, 57, 57,
100, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 97, 115, 101, 32, 52,
58, 32, 109, 101, 109, 99, 112, 121, 40, 38, 117, 52, 44, 32, 107,
101, 121, 44, 32, 52, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32,
117, 52, 42, 48, 120, 99, 54, 97, 52, 97, 55, 57, 51, 53, 98, 100, 49,
101, 57, 57, 100, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 97, 115,
101, 32, 48, 58, 32, 114, 101, 116, 117, 114, 110, 32, 49, 59, 10, 32,
32, 32, 32, 125, 10, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 117,
105, 110, 116, 56, 95, 116, 32, 42, 120, 32, 61, 32, 40, 99, 111, 110,
115, 116, 32, 117, 105, 110, 116, 56, 95, 116, 42, 41, 107, 101, 121,
59, 10, 32, 32, 32, 32, 117, 105, 110, 116, 54, 52, 95, 116, 32, 104,
32, 61, 32, 40, 117, 105, 110, 116, 54, 52, 95, 116, 41, 42, 120, 32,
60, 60, 32, 55, 44, 32, 110, 32, 61, 32, 40, 117, 105, 110, 116, 54,
52, 95, 116, 41, 108, 101, 110, 32, 62, 62, 32, 51, 59, 10, 32, 32,
32, 32, 108, 101, 110, 32, 38, 61, 32, 55, 59, 10, 32, 32, 32, 32,
119, 104, 105, 108, 101, 32, 40, 110, 45, 45, 41, 32, 123, 10, 32, 32,
32, 32, 32, 32, 32, 32, 109, 101, 109, 99, 112, 121, 40, 38, 117, 56,
44, 32, 120, 44, 32, 56, 41, 44, 32, 120, 32, 43, 61, 32, 56, 59, 10,
32, 32, 32, 32, 32, 32, 32, 32, 104, 32, 61, 32, 40, 104, 32, 94, 32,
117, 56, 41, 42, 48, 120, 99, 54, 97, 52, 97, 55, 57, 51, 53, 98, 100,
49, 101, 57, 57, 100, 59, 10, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32,
119, 104, 105, 108, 101, 32, 40, 108, 101, 110, 45, 45, 41, 32, 104,
32, 61, 32, 40, 104, 32, 94, 32, 42, 120, 43, 43, 41, 42, 48, 120, 49,
48, 48, 48, 48, 48, 48, 48, 49, 98, 51, 59, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 104, 32, 94, 32, 99, 95, 82, 79, 84, 76,
40, 104, 44, 32, 50, 54, 41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73,
78, 76, 73, 78, 69, 32, 117, 105, 110, 116, 54, 52, 95, 116, 32, 115,
116, 99, 95, 115, 116, 114, 104, 97, 115, 104, 40, 99, 111, 110, 115,
116, 32, 99, 104, 97, 114, 32, 42, 115, 116, 114, 41, 10, 32, 32, 32,
32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 115, 116, 99, 95, 104,
97, 115, 104, 95, 50, 40, 115, 116, 114, 44, 32, 99, 95, 115, 116,
114, 108, 101, 110, 40, 115, 116, 114, 41, 41, 59, 32, 125, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 117, 105, 110, 116, 54,
52, 95, 116, 32, 95, 115, 116, 99, 95, 104, 97, 115, 104, 95, 109,
105, 120, 40, 117, 105, 110, 116, 54, 52, 95, 116, 32, 104, 91, 93,
44, 32, 105, 110, 116, 32, 110, 41, 32, 123, 32, 47, 47, 32, 110, 32,
62, 32, 48, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110, 116,
32, 105, 32, 61, 32, 49, 59, 32, 105, 32, 60, 32, 110, 59, 32, 43, 43,
105, 41, 32, 104, 91, 48, 93, 32, 94, 61, 32, 104, 91, 48, 93, 32, 43,
32, 104, 91, 105, 93, 59, 32, 47, 47, 32, 110, 111, 110, 45, 99, 111,
109, 109, 117, 116, 97, 116, 105, 118, 101, 33, 10, 32, 32, 32, 32,
114, 101, 116, 117, 114, 110, 32, 104, 91, 48, 93, 59, 10, 125, 10,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 99, 104, 97, 114, 42,
32, 115, 116, 99, 95, 115, 116, 114, 110, 115, 116, 114, 110, 40, 99,
111, 110, 115, 116, 32, 99, 104, 97, 114, 32, 42, 115, 116, 114, 44,
32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 108, 101, 110, 44,
32, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111,
110, 115, 116, 32, 99, 104, 97, 114, 32, 42, 110, 101, 101, 100, 108,
101, 44, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 108, 101,
110, 41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 33, 110, 108,
101, 110, 41, 32, 114, 101, 116, 117, 114, 110, 32, 40, 99, 104, 97,
114, 32, 42, 41, 115, 116, 114, 59, 10, 32, 32, 32, 32, 105, 102, 32,
40, 110, 108, 101, 110, 32, 62, 32, 115, 108, 101, 110, 41, 32, 114,
101, 116, 117, 114, 110, 32, 78, 85, 76, 76, 59, 10, 32, 32, 32, 32,
115, 108, 101, 110, 32, 45, 61, 32, 110, 108, 101, 110, 59, 10, 32,
32, 32, 32, 100, 111, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 102, 32, 40, 42, 115, 116, 114, 32, 61, 61, 32, 42, 110, 101,
101, 100, 108, 101, 32, 38, 38, 32, 33, 99, 95, 109, 101, 109, 99,
109, 112, 40, 115, 116, 114, 44, 32, 110, 101, 101, 100, 108, 101, 44,
32, 110, 108, 101, 110, 41, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 40, 99, 104, 97,
114, 32, 42, 41, 115, 116, 114, 59, 10, 32, 32, 32, 32, 32, 32, 32,
32, 43, 43, 115, 116, 114, 59, 10, 32, 32, 32, 32, 125, 32, 119, 104,
105, 108, 101, 32, 40, 115, 108, 101, 110, 45, 45, 41, 59, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 78, 85, 76, 76, 59, 10, 125,
10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 115, 116, 99, 95, 110, 101, 120, 116, 112,
111, 119, 50, 40, 105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 41,
32, 123, 10, 32, 32, 32, 32, 110, 45, 45, 59, 10, 32, 32, 32, 32, 110,
32, 124, 61, 32, 110, 32, 62, 62, 32, 49, 44, 32, 110, 32, 124, 61,
32, 110, 32, 62, 62, 32, 50, 59, 10, 32, 32, 32, 32, 110, 32, 124, 61,
32, 110, 32, 62, 62, 32, 52, 44, 32, 110, 32, 124, 61, 32, 110, 32,
62, 62, 32, 56, 59, 10, 32, 32, 32, 32, 110, 32, 124, 61, 32, 110, 32,
62, 62, 32, 49, 54, 59, 10, 32, 32, 32, 32, 35, 105, 102, 32, 73, 78,
84, 80, 84, 82, 95, 77, 65, 88, 32, 61, 61, 32, 73, 78, 84, 54, 52,
95, 77, 65, 88, 10, 32, 32, 32, 32, 110, 32, 124, 61, 32, 110, 32, 62,
62, 32, 51, 50, 59, 10, 32, 32, 32, 32, 35, 101, 110, 100, 105, 102,
10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 110, 32, 43, 32,
49, 59, 10, 125, 10, 47, 42, 32, 67, 111, 110, 116, 114, 111, 108, 32,
98, 108, 111, 99, 107, 32, 109, 97, 99, 114, 111, 115, 32, 42, 47, 10,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111, 114, 101,
97, 99, 104, 40, 46, 46, 46, 41, 32, 99, 95, 77, 65, 67, 82, 79, 95,
79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95, 102, 111, 114, 101, 97,
99, 104, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111, 114, 101, 97,
99, 104, 95, 51, 40, 105, 116, 44, 32, 67, 44, 32, 99, 110, 116, 41,
32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 67, 35, 35, 95,
105, 116, 101, 114, 32, 105, 116, 32, 61, 32, 67, 35, 35, 95, 98, 101,
103, 105, 110, 40, 38, 99, 110, 116, 41, 59, 32, 105, 116, 46, 114,
101, 102, 59, 32, 67, 35, 35, 95, 110, 101, 120, 116, 40, 38, 105,
116, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102,
111, 114, 101, 97, 99, 104, 95, 52, 40, 105, 116, 44, 32, 67, 44, 32,
115, 116, 97, 114, 116, 44, 32, 102, 105, 110, 105, 115, 104, 41, 32,
92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 67, 35, 35, 95, 105,
116, 101, 114, 32, 105, 116, 32, 61, 32, 40, 115, 116, 97, 114, 116,
41, 44, 32, 42, 95, 101, 110, 100, 114, 101, 102, 32, 61, 32, 99, 95,
115, 97, 102, 101, 95, 99, 97, 115, 116, 40, 67, 35, 35, 95, 105, 116,
101, 114, 42, 44, 32, 67, 35, 35, 95, 118, 97, 108, 117, 101, 42, 44,
32, 40, 102, 105, 110, 105, 115, 104, 41, 46, 114, 101, 102, 41, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 105, 116, 46, 114,
101, 102, 32, 33, 61, 32, 40, 67, 35, 35, 95, 118, 97, 108, 117, 101,
42, 41, 95, 101, 110, 100, 114, 101, 102, 59, 32, 67, 35, 35, 95, 110,
101, 120, 116, 40, 38, 105, 116, 41, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 112, 97, 105, 114, 40, 107,
101, 121, 44, 32, 118, 97, 108, 44, 32, 67, 44, 32, 99, 110, 116, 41,
32, 47, 42, 32, 115, 116, 114, 117, 99, 116, 117, 114, 101, 100, 32,
98, 105, 110, 100, 105, 110, 103, 32, 42, 47, 32, 92, 10, 32, 32, 32,
32, 102, 111, 114, 32, 40, 115, 116, 114, 117, 99, 116, 32, 123, 67,
35, 35, 95, 105, 116, 101, 114, 32, 105, 116, 101, 114, 59, 32, 99,
111, 110, 115, 116, 32, 67, 35, 35, 95, 107, 101, 121, 42, 32, 107,
101, 121, 59, 32, 67, 35, 35, 95, 109, 97, 112, 112, 101, 100, 42, 32,
118, 97, 108, 59, 125, 32, 95, 32, 61, 32, 123, 46, 105, 116, 101,
114, 61, 67, 35, 35, 95, 98, 101, 103, 105, 110, 40, 38, 99, 110, 116,
41, 125, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 95,
46, 105, 116, 101, 114, 46, 114, 101, 102, 32, 38, 38, 32, 40, 95, 46,
107, 101, 121, 32, 61, 32, 38, 95, 46, 105, 116, 101, 114, 46, 114,
101, 102, 45, 62, 102, 105, 114, 115, 116, 44, 32, 95, 46, 118, 97,
108, 32, 61, 32, 38, 95, 46, 105, 116, 101, 114, 46, 114, 101, 102,
45, 62, 115, 101, 99, 111, 110, 100, 41, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 32, 59, 32, 67, 35, 35, 95, 110, 101, 120, 116, 40,
38, 95, 46, 105, 116, 101, 114, 41, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 105, 110, 100, 101, 120,
101, 100, 40, 105, 116, 44, 32, 67, 44, 32, 99, 110, 116, 41, 32, 92,
10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 115, 116, 114, 117, 99,
116, 32, 123, 67, 35, 35, 95, 105, 116, 101, 114, 32, 105, 116, 101,
114, 59, 32, 67, 35, 35, 95, 118, 97, 108, 117, 101, 42, 32, 114, 101,
102, 59, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 110, 100,
101, 120, 59, 125, 32, 105, 116, 32, 61, 32, 123, 46, 105, 116, 101,
114, 61, 67, 35, 35, 95, 98, 101, 103, 105, 110, 40, 38, 99, 110, 116,
41, 125, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 40,
105, 116, 46, 114, 101, 102, 32, 61, 32, 105, 116, 46, 105, 116, 101,
114, 46, 114, 101, 102, 41, 32, 59, 32, 67, 35, 35, 95, 110, 101, 120,
116, 40, 38, 105, 116, 46, 105, 116, 101, 114, 41, 44, 32, 43, 43,
105, 116, 46, 105, 110, 100, 101, 120, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 105, 116, 101, 114, 40, 101,
120, 105, 115, 116, 105, 110, 103, 95, 105, 116, 101, 114, 44, 32, 67,
44, 32, 99, 110, 116, 41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114,
32, 40, 101, 120, 105, 115, 116, 105, 110, 103, 95, 105, 116, 101,
114, 32, 61, 32, 67, 35, 35, 95, 98, 101, 103, 105, 110, 40, 38, 99,
110, 116, 41, 59, 32, 40, 101, 120, 105, 115, 116, 105, 110, 103, 95,
105, 116, 101, 114, 41, 46, 114, 101, 102, 59, 32, 67, 35, 35, 95,
110, 101, 120, 116, 40, 38, 101, 120, 105, 115, 116, 105, 110, 103,
95, 105, 116, 101, 114, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 40, 46, 46,
46, 41, 32, 99, 95, 77, 65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79,
65, 68, 40, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 44, 32, 95,
95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 95,
49, 40, 115, 116, 111, 112, 41, 32, 99, 95, 102, 111, 114, 114, 97,
110, 103, 101, 95, 51, 40, 95, 105, 44, 32, 48, 44, 32, 115, 116, 111,
112, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111,
114, 114, 97, 110, 103, 101, 95, 50, 40, 105, 44, 32, 115, 116, 111,
112, 41, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 95, 51,
40, 105, 44, 32, 48, 44, 32, 115, 116, 111, 112, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101,
95, 51, 40, 105, 44, 32, 115, 116, 97, 114, 116, 44, 32, 115, 116,
111, 112, 41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 95,
108, 108, 111, 110, 103, 32, 105, 61, 115, 116, 97, 114, 116, 44, 32,
95, 101, 110, 100, 61, 115, 116, 111, 112, 59, 32, 105, 32, 60, 32,
95, 101, 110, 100, 59, 32, 43, 43, 105, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 95,
52, 40, 105, 44, 32, 115, 116, 97, 114, 116, 44, 32, 115, 116, 111,
112, 44, 32, 115, 116, 101, 112, 41, 32, 92, 10, 32, 32, 32, 32, 102,
111, 114, 32, 40, 95, 108, 108, 111, 110, 103, 32, 105, 61, 115, 116,
97, 114, 116, 44, 32, 95, 105, 110, 99, 61, 115, 116, 101, 112, 44,
32, 95, 101, 110, 100, 61, 40, 95, 108, 108, 111, 110, 103, 41, 40,
115, 116, 111, 112, 41, 32, 45, 32, 40, 95, 105, 110, 99, 32, 62, 32,
48, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 40,
95, 105, 110, 99, 32, 62, 32, 48, 41, 32, 94, 32, 40, 105, 32, 62, 32,
95, 101, 110, 100, 41, 59, 32, 105, 32, 43, 61, 32, 95, 105, 110, 99,
41, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 95, 95, 99, 112,
108, 117, 115, 112, 108, 117, 115, 10, 32, 32, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 105, 110, 105, 116, 40, 67, 44, 32,
46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 67, 35,
35, 95, 102, 114, 111, 109, 95, 110, 40, 40, 67, 35, 35, 95, 114, 97,
119, 91, 93, 41, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 44, 32,
99, 95, 115, 105, 122, 101, 111, 102, 40, 40, 67, 35, 35, 95, 114, 97,
119, 91, 93, 41, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 47,
99, 95, 115, 105, 122, 101, 111, 102, 40, 67, 35, 35, 95, 114, 97,
119, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 102, 111, 114, 108, 105, 115, 116, 40, 105, 116, 44, 32, 84,
44, 32, 46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
102, 111, 114, 32, 40, 115, 116, 114, 117, 99, 116, 32, 123, 84, 42,
32, 114, 101, 102, 59, 32, 105, 110, 116, 32, 115, 105, 122, 101, 44,
32, 105, 110, 100, 101, 120, 59, 125, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 105, 116, 32, 61, 32, 123, 46, 114,
101, 102, 61, 40, 84, 91, 93, 41, 95, 95, 86, 65, 95, 65, 82, 71, 83,
95, 95, 44, 32, 46, 115, 105, 122, 101, 61, 40, 105, 110, 116, 41, 40,
115, 105, 122, 101, 111, 102, 40, 40, 84, 91, 93, 41, 95, 95, 86, 65,
95, 65, 82, 71, 83, 95, 95, 41, 47, 115, 105, 122, 101, 111, 102, 40,
84, 41, 41, 125, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 59, 32, 105, 116, 46, 105, 110, 100, 101, 120, 32, 60, 32,
105, 116, 46, 115, 105, 122, 101, 59, 32, 43, 43, 105, 116, 46, 114,
101, 102, 44, 32, 43, 43, 105, 116, 46, 105, 110, 100, 101, 120, 41,
10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 115, 116,
99, 95, 104, 97, 115, 104, 95, 109, 105, 120, 40, 46, 46, 46, 41, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 115, 116, 99, 95, 104, 97,
115, 104, 95, 109, 105, 120, 40, 40, 117, 105, 110, 116, 54, 52, 95,
116, 91, 93, 41, 123, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 125,
44, 32, 99, 95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 41, 41, 10, 35, 101, 108, 115, 101, 10, 47,
47, 32, 69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103,
101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101,
97, 100, 101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32, 35, 105,
110, 99, 108, 117, 100, 101, 32, 60, 105, 110, 105, 116, 105, 97, 108,
105, 122, 101, 114, 95, 108, 105, 115, 116, 62, 10, 47, 47, 32, 69,
88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110,
95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100,
101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32, 35, 105, 110, 99,
108, 117, 100, 101, 32, 60, 97, 114, 114, 97, 121, 62, 10, 32, 32, 32,
32, 116, 101, 109, 112, 108, 97, 116, 101, 32, 60, 99, 108, 97, 115,
115, 32, 67, 44, 32, 99, 108, 97, 115, 115, 32, 84, 62, 10, 32, 32,
32, 32, 105, 110, 108, 105, 110, 101, 32, 67, 32, 95, 102, 114, 111,
109, 95, 110, 40, 67, 32, 40, 42, 102, 117, 110, 99, 41, 40, 99, 111,
110, 115, 116, 32, 84, 91, 93, 44, 32, 105, 110, 116, 112, 116, 114,
95, 116, 41, 44, 32, 115, 116, 100, 58, 58, 105, 110, 105, 116, 105,
97, 108, 105, 122, 101, 114, 95, 108, 105, 115, 116, 60, 84, 62, 32,
105, 108, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 114, 101,
116, 117, 114, 110, 32, 102, 117, 110, 99, 40, 38, 42, 105, 108, 46,
98, 101, 103, 105, 110, 40, 41, 44, 32, 105, 108, 46, 115, 105, 122,
101, 40, 41, 41, 59, 32, 125, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 105, 110, 105, 116, 40, 67, 44, 32, 46, 46,
46, 41, 32, 95, 102, 114, 111, 109, 95, 110, 60, 67, 44, 67, 35, 35,
95, 114, 97, 119, 62, 40, 67, 35, 35, 95, 102, 114, 111, 109, 95, 110,
44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 32, 32,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111, 114,
108, 105, 115, 116, 40, 105, 116, 44, 32, 84, 44, 32, 46, 46, 46, 41,
32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 102, 111, 114, 32, 40,
115, 116, 114, 117, 99, 116, 32, 123, 115, 116, 100, 58, 58, 105, 110,
105, 116, 105, 97, 108, 105, 122, 101, 114, 95, 108, 105, 115, 116,
60, 84, 62, 32, 95, 105, 108, 59, 32, 115, 116, 100, 58, 58, 105, 110,
105, 116, 105, 97, 108, 105, 122, 101, 114, 95, 108, 105, 115, 116,
60, 84, 62, 58, 58, 105, 116, 101, 114, 97, 116, 111, 114, 32, 114,
101, 102, 59, 32, 115, 105, 122, 101, 95, 116, 32, 115, 105, 122, 101,
44, 32, 105, 110, 100, 101, 120, 59, 125, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 116, 32, 61, 32, 123, 46, 95,
105, 108, 61, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 44, 32, 46,
114, 101, 102, 61, 105, 116, 46, 95, 105, 108, 46, 98, 101, 103, 105,
110, 40, 41, 44, 32, 46, 115, 105, 122, 101, 61, 105, 116, 46, 95,
105, 108, 46, 115, 105, 122, 101, 40, 41, 125, 32, 92, 10, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 105, 116, 46, 105,
110, 100, 101, 120, 32, 60, 32, 105, 116, 46, 115, 105, 122, 101, 59,
32, 43, 43, 105, 116, 46, 114, 101, 102, 44, 32, 43, 43, 105, 116, 46,
105, 110, 100, 101, 120, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 115, 116, 99, 95, 104, 97, 115, 104, 95, 109, 105,
120, 40, 46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
95, 115, 116, 99, 95, 104, 97, 115, 104, 95, 109, 105, 120, 40, 115,
116, 100, 58, 58, 97, 114, 114, 97, 121, 60, 117, 105, 110, 116, 54,
52, 95, 116, 44, 32, 99, 95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95,
86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 62, 123, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 125, 46, 100, 97, 116, 97, 40, 41, 44, 32, 99,
95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95, 86, 65, 95, 65, 82, 71,
83, 95, 95, 41, 41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 100, 101, 102, 101, 114, 40, 46,
46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105,
110, 116, 32, 95, 105, 32, 61, 32, 49, 59, 32, 95, 105, 59, 32, 95,
105, 32, 61, 32, 48, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95,
95, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 119,
105, 116, 104, 40, 46, 46, 46, 41, 32, 99, 95, 77, 65, 67, 82, 79, 95,
79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95, 119, 105, 116, 104, 44,
32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 119, 105, 116, 104, 95, 50, 40, 100,
101, 99, 108, 118, 97, 114, 44, 32, 100, 114, 111, 112, 41, 32, 92,
10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 100, 101, 99, 108, 118, 97,
114, 44, 32, 42, 95, 105, 44, 32, 42, 42, 95, 105, 112, 32, 61, 32,
38, 95, 105, 59, 32, 95, 105, 112, 59, 32, 95, 105, 112, 32, 61, 32,
48, 44, 32, 100, 114, 111, 112, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 119, 105, 116, 104, 95, 51, 40, 100, 101, 99, 108,
118, 97, 114, 44, 32, 112, 114, 101, 100, 44, 32, 100, 114, 111, 112,
41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 100, 101, 99,
108, 118, 97, 114, 44, 32, 42, 95, 105, 44, 32, 42, 42, 95, 105, 112,
32, 61, 32, 38, 95, 105, 59, 32, 95, 105, 112, 32, 38, 38, 32, 40,
112, 114, 101, 100, 41, 59, 32, 95, 105, 112, 32, 61, 32, 48, 44, 32,
100, 114, 111, 112, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 115, 99, 111, 112, 101, 40, 46, 46, 46, 41, 32, 99, 95, 77,
65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95, 115,
99, 111, 112, 101, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95,
41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 99, 111,
112, 101, 95, 50, 40, 105, 110, 105, 116, 44, 32, 100, 114, 111, 112,
41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110, 116,
32, 95, 105, 32, 61, 32, 40, 105, 110, 105, 116, 44, 32, 49, 41, 59,
32, 95, 105, 59, 32, 95, 105, 32, 61, 32, 48, 44, 32, 100, 114, 111,
112, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 99,
111, 112, 101, 95, 51, 40, 105, 110, 105, 116, 44, 32, 112, 114, 101,
100, 44, 32, 100, 114, 111, 112, 41, 32, 92, 10, 32, 32, 32, 32, 102,
111, 114, 32, 40, 105, 110, 116, 32, 95, 105, 32, 61, 32, 40, 105,
110, 105, 116, 44, 32, 49, 41, 59, 32, 95, 105, 32, 38, 38, 32, 40,
112, 114, 101, 100, 41, 59, 32, 95, 105, 32, 61, 32, 48, 44, 32, 100,
114, 111, 112, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99,
95, 100, 114, 111, 112, 40, 67, 44, 32, 46, 46, 46, 41, 32, 92, 10,
32, 32, 32, 32, 100, 111, 32, 123, 32, 99, 95, 102, 111, 114, 108,
105, 115, 116, 32, 40, 95, 105, 44, 32, 67, 42, 44, 32, 123, 95, 95,
86, 65, 95, 65, 82, 71, 83, 95, 95, 125, 41, 32, 67, 35, 35, 95, 100,
114, 111, 112, 40, 42, 95, 105, 46, 114, 101, 102, 41, 59, 32, 125,
32, 119, 104, 105, 108, 101, 40, 48, 41, 10, 10, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 40, 95, 95, 83, 73, 90, 69, 79, 70,
95, 73, 78, 84, 49, 50, 56, 95, 95, 41, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 117, 109, 117, 108, 49, 50, 56,
40, 97, 44, 32, 98, 44, 32, 108, 111, 44, 32, 104, 105, 41, 32, 92,
10, 32, 32, 32, 32, 32, 32, 32, 32, 100, 111, 32, 123, 32, 95, 95,
117, 105, 110, 116, 49, 50, 56, 95, 116, 32, 95, 122, 32, 61, 32, 40,
95, 95, 117, 105, 110, 116, 49, 50, 56, 95, 116, 41, 40, 97, 41, 42,
40, 98, 41, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 42, 40, 108, 111, 41, 32, 61, 32, 40, 117, 105, 110, 116,
54, 52, 95, 116, 41, 95, 122, 44, 32, 42, 40, 104, 105, 41, 32, 61,
32, 40, 117, 105, 110, 116, 54, 52, 95, 116, 41, 40, 95, 122, 32, 62,
62, 32, 54, 52, 85, 41, 59, 32, 125, 32, 119, 104, 105, 108, 101, 40,
48, 41, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101,
100, 40, 95, 77, 83, 67, 95, 86, 69, 82, 41, 32, 38, 38, 32, 100, 101,
102, 105, 110, 101, 100, 40, 95, 87, 73, 78, 54, 52, 41, 10, 47, 47,
32, 69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103,
101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101,
97, 100, 101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32, 35, 105,
110, 99, 108, 117, 100, 101, 32, 60, 105, 110, 116, 114, 105, 110, 46,
104, 62, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99,
95, 117, 109, 117, 108, 49, 50, 56, 40, 97, 44, 32, 98, 44, 32, 108,
111, 44, 32, 104, 105, 41, 32, 40, 40, 118, 111, 105, 100, 41, 40, 42,
40, 108, 111, 41, 32, 61, 32, 95, 117, 109, 117, 108, 49, 50, 56, 40,
97, 44, 32, 98, 44, 32, 104, 105, 41, 41, 41, 10, 35, 101, 108, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 40, 95, 95, 120, 56, 54,
95, 54, 52, 95, 95, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 117, 109, 117, 108, 49, 50, 56, 40, 97, 44, 32,
98, 44, 32, 108, 111, 44, 32, 104, 105, 41, 32, 92, 10, 32, 32, 32,
32, 32, 32, 32, 32, 97, 115, 109, 40, 34, 109, 117, 108, 113, 32, 37,
51, 34, 32, 58, 32, 34, 61, 97, 34, 40, 42, 40, 108, 111, 41, 41, 44,
32, 34, 61, 100, 34, 40, 42, 40, 104, 105, 41, 41, 32, 58, 32, 34, 97,
34, 40, 97, 41, 44, 32, 34, 114, 109, 34, 40, 98, 41, 41, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 101, 110, 100, 105, 102, 32, 47, 47,
32, 67, 67, 79, 77, 77, 79, 78, 95, 72, 95, 73, 78, 67, 76, 85, 68,
69, 68, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73, 76,
69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 99, 99, 111, 109, 109,
111, 110, 46, 104, 10, 47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73, 78,
95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 102, 111,
114, 119, 97, 114, 100, 46, 104, 10, 35, 105, 102, 110, 100, 101, 102,
32, 83, 84, 67, 95, 70, 79, 82, 87, 65, 82, 68, 95, 72, 95, 73, 78,
67, 76, 85, 68, 69, 68, 10, 35, 100, 101, 102, 105, 110, 101, 32, 83,
84, 67, 95, 70, 79, 82, 87, 65, 82, 68, 95, 72, 95, 73, 78, 67, 76,
85, 68, 69, 68, 10, 10, 47, 47, 32, 69, 88, 67, 76, 85, 68, 69, 68,
32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110, 116, 97,
105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46, 112,
121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 115, 116, 100,
105, 110, 116, 46, 104, 62, 10, 47, 47, 32, 69, 88, 67, 76, 85, 68,
69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110,
116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46,
112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 115, 116,
100, 100, 101, 102, 46, 104, 62, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 97, 114, 99, 40,
67, 44, 32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 97, 114, 99, 95, 116,
121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 98,
111, 120, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 98, 111,
120, 95, 116, 121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 99, 100, 101, 113, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95,
99, 95, 99, 100, 101, 113, 95, 116, 121, 112, 101, 115, 40, 67, 44,
32, 86, 65, 76, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102,
111, 114, 119, 97, 114, 100, 95, 99, 108, 105, 115, 116, 40, 67, 44,
32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 108, 105, 115, 116, 95, 116,
121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 109,
97, 112, 40, 67, 44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 41, 32, 95,
99, 95, 99, 104, 97, 115, 104, 95, 116, 121, 112, 101, 115, 40, 67,
44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 44, 32, 99, 95, 116, 114, 117,
101, 44, 32, 99, 95, 102, 97, 108, 115, 101, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 115,
101, 116, 40, 67, 44, 32, 75, 69, 89, 41, 32, 95, 99, 95, 99, 104, 97,
115, 104, 95, 116, 121, 112, 101, 115, 40, 67, 44, 32, 99, 115, 101,
116, 44, 32, 75, 69, 89, 44, 32, 75, 69, 89, 44, 32, 99, 95, 102, 97,
108, 115, 101, 44, 32, 99, 95, 116, 114, 117, 101, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99,
115, 109, 97, 112, 40, 67, 44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 41,
32, 95, 99, 95, 97, 97, 116, 114, 101, 101, 95, 116, 121, 112, 101,
115, 40, 67, 44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 44, 32, 99, 95,
116, 114, 117, 101, 44, 32, 99, 95, 102, 97, 108, 115, 101, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 99, 115, 115, 101, 116, 40, 67, 44, 32, 75, 69, 89, 41, 32,
95, 99, 95, 97, 97, 116, 114, 101, 101, 95, 116, 121, 112, 101, 115,
40, 67, 44, 32, 75, 69, 89, 44, 32, 75, 69, 89, 44, 32, 99, 95, 102,
97, 108, 115, 101, 44, 32, 99, 95, 116, 114, 117, 101, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100,
95, 99, 115, 116, 97, 99, 107, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95,
99, 95, 99, 115, 116, 97, 99, 107, 95, 116, 121, 112, 101, 115, 40,
67, 44, 32, 86, 65, 76, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
102, 111, 114, 119, 97, 114, 100, 95, 99, 112, 113, 117, 101, 40, 67,
44, 32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 112, 113, 117, 101, 95,
116, 121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99,
113, 117, 101, 117, 101, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95, 99,
95, 99, 100, 101, 113, 95, 116, 121, 112, 101, 115, 40, 67, 44, 32,
86, 65, 76, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 99, 118, 101, 99, 40, 67, 44, 32, 86, 65,
76, 41, 32, 95, 99, 95, 99, 118, 101, 99, 95, 116, 121, 112, 101, 115,
40, 67, 44, 32, 86, 65, 76, 41, 10, 47, 47, 32, 97, 108, 116, 101,
114, 110, 97, 116, 105, 118, 101, 32, 110, 97, 109, 101, 115, 32, 40,
105, 110, 99, 108, 117, 100, 101, 47, 115, 116, 120, 41, 58, 10, 35,
100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100,
95, 97, 114, 99, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 97,
114, 99, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119,
97, 114, 100, 95, 98, 111, 120, 32, 102, 111, 114, 119, 97, 114, 100,
95, 99, 98, 111, 120, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102,
111, 114, 119, 97, 114, 100, 95, 100, 101, 113, 32, 102, 111, 114,
119, 97, 114, 100, 95, 99, 100, 101, 113, 10, 35, 100, 101, 102, 105,
110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 108, 105, 115,
116, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 108, 105, 115, 116,
10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 104, 109, 97, 112, 32, 102, 111, 114, 119, 97, 114, 100, 95,
99, 109, 97, 112, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 104, 115, 101, 116, 32, 102, 111, 114,
119, 97, 114, 100, 95, 99, 115, 101, 116, 10, 35, 100, 101, 102, 105,
110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 115, 109, 97, 112,
32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 115, 109, 97, 112, 10,
35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 115, 115, 101, 116, 32, 102, 111, 114, 119, 97, 114, 100, 95,
99, 115, 115, 101, 116, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102,
111, 114, 119, 97, 114, 100, 95, 115, 116, 97, 99, 107, 32, 102, 111,
114, 119, 97, 114, 100, 95, 99, 115, 116, 97, 99, 107, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95,
112, 113, 117, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 112,
113, 117, 101, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 113, 117, 101, 117, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 99, 113, 117, 101, 117, 101, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95,
118, 101, 99, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 118, 101,
99, 10, 10, 47, 47, 32, 99, 115, 118, 105, 101, 119, 32, 58, 32, 110,
111, 110, 45, 110, 117, 108, 108, 32, 116, 101, 114, 109, 105, 110,
97, 116, 101, 100, 32, 115, 116, 114, 105, 110, 103, 32, 118, 105,
101, 119, 10, 116, 121, 112, 101, 100, 101, 102, 32, 99, 111, 110,
115, 116, 32, 99, 104, 97, 114, 32, 99, 115, 118, 105, 101, 119, 95,
118, 97, 108, 117, 101, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32,
115, 116, 114, 117, 99, 116, 32, 99, 115, 118, 105, 101, 119, 32, 123,
10, 32, 32, 32, 32, 99, 115, 118, 105, 101, 119, 95, 118, 97, 108,
117, 101, 42, 32, 98, 117, 102, 59, 10, 32, 32, 32, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 115, 105, 122, 101, 59, 10, 125, 32, 99,
115, 118, 105, 101, 119, 59, 10, 10, 116, 121, 112, 101, 100, 101,
102, 32, 117, 110, 105, 111, 110, 32, 123, 10, 32, 32, 32, 32, 99,
115, 118, 105, 101, 119, 95, 118, 97, 108, 117, 101, 42, 32, 114, 101,
102, 59, 10, 32, 32, 32, 32, 99, 115, 118, 105, 101, 119, 32, 99, 104,
114, 59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32,
99, 115, 118, 105, 101, 119, 32, 99, 104, 114, 59, 32, 99, 115, 118,
105, 101, 119, 95, 118, 97, 108, 117, 101, 42, 32, 101, 110, 100, 59,
32, 125, 32, 117, 56, 59, 10, 125, 32, 99, 115, 118, 105, 101, 119,
95, 105, 116, 101, 114, 59, 10, 10, 10, 47, 47, 32, 99, 114, 97, 119,
115, 116, 114, 32, 58, 32, 110, 117, 108, 108, 45, 116, 101, 114, 109,
105, 110, 97, 116, 101, 100, 32, 115, 116, 114, 105, 110, 103, 32,
118, 105, 101, 119, 10, 116, 121, 112, 101, 100, 101, 102, 32, 99,
115, 118, 105, 101, 119, 95, 118, 97, 108, 117, 101, 32, 99, 114, 97,
119, 115, 116, 114, 95, 118, 97, 108, 117, 101, 59, 10, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 99, 114, 97,
119, 115, 116, 114, 32, 123, 10, 32, 32, 32, 32, 99, 114, 97, 119,
115, 116, 114, 95, 118, 97, 108, 117, 101, 42, 32, 115, 116, 114, 59,
10, 32, 32, 32, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 115,
105, 122, 101, 59, 10, 125, 32, 99, 114, 97, 119, 115, 116, 114, 59,
10, 10, 116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 105, 111,
110, 32, 123, 10, 32, 32, 32, 32, 99, 114, 97, 119, 115, 116, 114, 95,
118, 97, 108, 117, 101, 42, 32, 114, 101, 102, 59, 10, 32, 32, 32, 32,
99, 115, 118, 105, 101, 119, 32, 99, 104, 114, 59, 10, 125, 32, 99,
114, 97, 119, 115, 116, 114, 95, 105, 116, 101, 114, 59, 10, 10, 10,
47, 47, 32, 99, 115, 116, 114, 32, 58, 32, 110, 117, 108, 108, 45,
116, 101, 114, 109, 105, 110, 97, 116, 101, 100, 32, 111, 119, 110,
105, 110, 103, 32, 115, 116, 114, 105, 110, 103, 32, 40, 115, 104,
111, 114, 116, 32, 115, 116, 114, 105, 110, 103, 32, 111, 112, 116,
105, 109, 105, 122, 101, 100, 32, 45, 32, 115, 115, 111, 41, 10, 116,
121, 112, 101, 100, 101, 102, 32, 99, 104, 97, 114, 32, 99, 115, 116,
114, 95, 118, 97, 108, 117, 101, 59, 10, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 99, 115, 116, 114,
95, 118, 97, 108, 117, 101, 42, 32, 100, 97, 116, 97, 59, 32, 105,
110, 116, 112, 116, 114, 95, 116, 32, 115, 105, 122, 101, 44, 32, 99,
97, 112, 59, 32, 125, 32, 99, 115, 116, 114, 95, 98, 117, 102, 59, 10,
116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 105, 111, 110, 32,
99, 115, 116, 114, 32, 123, 10, 32, 32, 32, 32, 115, 116, 114, 117,
99, 116, 32, 123, 32, 99, 115, 116, 114, 95, 118, 97, 108, 117, 101,
32, 100, 97, 116, 97, 91, 32, 115, 105, 122, 101, 111, 102, 40, 99,
115, 116, 114, 95, 98, 117, 102, 41, 32, 93, 59, 32, 125, 32, 115,
109, 108, 59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32,
123, 32, 99, 115, 116, 114, 95, 118, 97, 108, 117, 101, 42, 32, 100,
97, 116, 97, 59, 32, 115, 105, 122, 101, 95, 116, 32, 115, 105, 122,
101, 44, 32, 110, 99, 97, 112, 59, 32, 125, 32, 108, 111, 110, 59, 10,
125, 32, 99, 115, 116, 114, 59, 10, 10, 116, 121, 112, 101, 100, 101,
102, 32, 117, 110, 105, 111, 110, 32, 123, 10, 32, 32, 32, 32, 99,
115, 116, 114, 95, 118, 97, 108, 117, 101, 42, 32, 114, 101, 102, 59,
10, 32, 32, 32, 32, 99, 115, 118, 105, 101, 119, 32, 99, 104, 114, 59,
32, 47, 47, 32, 117, 116, 102, 56, 32, 99, 104, 97, 114, 97, 99, 116,
101, 114, 47, 99, 111, 100, 101, 112, 111, 105, 110, 116, 10, 125, 32,
99, 115, 116, 114, 95, 105, 116, 101, 114, 59, 10, 10, 10, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78, 85,
67, 95, 95, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32,
95, 95, 99, 108, 97, 110, 103, 95, 95, 32, 124, 124, 32, 100, 101,
102, 105, 110, 101, 100, 32, 95, 77, 83, 67, 95, 86, 69, 82, 10, 32,
32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 108, 111, 110, 103,
32, 99, 97, 116, 111, 109, 105, 99, 95, 108, 111, 110, 103, 59, 10,
35, 101, 108, 115, 101, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100,
101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 40, 108, 111, 110, 103,
41, 32, 99, 97, 116, 111, 109, 105, 99, 95, 108, 111, 110, 103, 59,
10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 116, 114, 117, 101, 40, 46, 46, 46, 41, 32, 95, 95,
86, 65, 95, 65, 82, 71, 83, 95, 95, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 102, 97, 108, 115, 101, 40, 46, 46, 46, 41, 10, 10,
35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 99, 97, 114, 99, 95,
116, 121, 112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 41,
32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 86,
65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59,
32, 92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117,
99, 116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 95,
95, 40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108, 108, 40, 83,
69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70, 32, 123, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 42, 32, 103, 101, 116, 59, 32, 92, 10, 32, 32,
32, 32, 32, 32, 32, 32, 99, 97, 116, 111, 109, 105, 99, 95, 108, 111,
110, 103, 42, 32, 117, 115, 101, 95, 99, 111, 117, 110, 116, 59, 32,
92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 95, 99, 95, 99, 98, 111, 120, 95, 116, 121,
112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10,
32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 86, 65, 76, 32,
83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10,
116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116,
32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 95, 95, 40, 40,
109, 101, 116, 104, 111, 100, 99, 97, 108, 108, 40, 83, 69, 76, 70,
35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108,
117, 101, 42, 32, 103, 101, 116, 59, 32, 92, 10, 32, 32, 32, 32, 125,
32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
99, 95, 99, 100, 101, 113, 95, 116, 121, 112, 101, 115, 40, 83, 69,
76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121,
112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35,
95, 118, 97, 108, 117, 101, 59, 32, 92, 10, 92, 10, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97,
116, 116, 114, 105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116,
104, 111, 100, 99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41,
41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
42, 99, 98, 117, 102, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 116, 97, 114, 116, 44,
32, 101, 110, 100, 44, 32, 99, 97, 112, 109, 97, 115, 107, 59, 32, 92,
10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 59, 32, 92, 10, 92, 10,
32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114,
117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83,
69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102,
59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 110, 116, 112,
116, 114, 95, 116, 32, 112, 111, 115, 59, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 83, 69, 76, 70, 42, 32,
95, 115, 59, 32, 92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 35,
35, 95, 105, 116, 101, 114, 10, 10, 35, 100, 101, 102, 105, 110, 101,
32, 95, 99, 95, 99, 108, 105, 115, 116, 95, 116, 121, 112, 101, 115,
40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32,
32, 116, 121, 112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76,
70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10, 32, 32, 32,
32, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99,
116, 32, 83, 69, 76, 70, 35, 35, 95, 110, 111, 100, 101, 32, 83, 69,
76, 70, 35, 35, 95, 110, 111, 100, 101, 59, 32, 92, 10, 92, 10, 32,
32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117,
99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69,
76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 59,
32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35,
95, 110, 111, 100, 101, 32, 42, 99, 111, 110, 115, 116, 32, 42, 95,
108, 97, 115, 116, 44, 32, 42, 112, 114, 101, 118, 59, 32, 92, 10, 32,
32, 32, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95, 105, 116, 101, 114,
59, 32, 92, 10, 92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115,
116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117,
116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108,
108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70,
32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70,
35, 35, 95, 110, 111, 100, 101, 32, 42, 108, 97, 115, 116, 59, 32, 92,
10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 95, 99, 95, 99, 104, 97, 115, 104, 95, 116,
121, 112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 75, 69, 89, 44, 32,
86, 65, 76, 44, 32, 77, 65, 80, 95, 79, 78, 76, 89, 44, 32, 83, 69,
84, 95, 79, 78, 76, 89, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112,
101, 100, 101, 102, 32, 75, 69, 89, 32, 83, 69, 76, 70, 35, 35, 95,
107, 101, 121, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101,
100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 109,
97, 112, 112, 101, 100, 59, 32, 92, 10, 92, 10, 32, 32, 32, 32, 116,
121, 112, 101, 100, 101, 102, 32, 83, 69, 84, 95, 79, 78, 76, 89, 40,
32, 83, 69, 76, 70, 35, 35, 95, 107, 101, 121, 32, 41, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 77, 65, 80, 95, 79, 78,
76, 89, 40, 32, 115, 116, 114, 117, 99, 116, 32, 83, 69, 76, 70, 35,
35, 95, 118, 97, 108, 117, 101, 32, 41, 32, 92, 10, 32, 32, 32, 32,
83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10,
92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 115,
116, 114, 117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32,
32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42,
114, 101, 102, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 66,
111, 111, 108, 32, 105, 110, 115, 101, 114, 116, 101, 100, 59, 32, 92,
10, 32, 32, 32, 32, 32, 32, 32, 32, 117, 105, 110, 116, 56, 95, 116,
32, 104, 97, 115, 104, 120, 59, 32, 92, 10, 32, 32, 32, 32, 125, 32,
83, 69, 76, 70, 35, 35, 95, 114, 101, 115, 117, 108, 116, 59, 32, 92,
10, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32,
115, 116, 114, 117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
42, 114, 101, 102, 44, 32, 42, 95, 101, 110, 100, 59, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 99, 104,
97, 115, 104, 95, 115, 108, 111, 116, 32, 42, 95, 115, 114, 101, 102,
59, 32, 92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95,
105, 116, 101, 114, 59, 32, 92, 10, 92, 10, 116, 121, 112, 101, 100,
101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116,
114, 105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111,
100, 99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32,
83, 69, 76, 70, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 42, 32, 116, 97,
98, 108, 101, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 115,
116, 114, 117, 99, 116, 32, 99, 104, 97, 115, 104, 95, 115, 108, 111,
116, 42, 32, 115, 108, 111, 116, 59, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 105, 122,
101, 44, 32, 98, 117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116,
59, 32, 92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35,
100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 97, 97, 116, 114, 101,
101, 95, 116, 121, 112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 75, 69,
89, 44, 32, 86, 65, 76, 44, 32, 77, 65, 80, 95, 79, 78, 76, 89, 44,
32, 83, 69, 84, 95, 79, 78, 76, 89, 41, 32, 92, 10, 32, 32, 32, 32,
116, 121, 112, 101, 100, 101, 102, 32, 75, 69, 89, 32, 83, 69, 76, 70,
35, 35, 95, 107, 101, 121, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121,
112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35,
95, 109, 97, 112, 112, 101, 100, 59, 32, 92, 10, 32, 32, 32, 32, 116,
121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 83,
69, 76, 70, 35, 35, 95, 110, 111, 100, 101, 32, 83, 69, 76, 70, 35,
35, 95, 110, 111, 100, 101, 59, 32, 92, 10, 92, 10, 32, 32, 32, 32,
116, 121, 112, 101, 100, 101, 102, 32, 83, 69, 84, 95, 79, 78, 76, 89,
40, 32, 83, 69, 76, 70, 35, 35, 95, 107, 101, 121, 32, 41, 32, 92, 10,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 77, 65, 80, 95, 79,
78, 76, 89, 40, 32, 115, 116, 114, 117, 99, 116, 32, 83, 69, 76, 70,
35, 35, 95, 118, 97, 108, 117, 101, 32, 41, 32, 92, 10, 32, 32, 32,
32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92,
10, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32,
115, 116, 114, 117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
42, 114, 101, 102, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95,
66, 111, 111, 108, 32, 105, 110, 115, 101, 114, 116, 101, 100, 59, 32,
92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95, 114, 101,
115, 117, 108, 116, 59, 32, 92, 10, 92, 10, 32, 32, 32, 32, 116, 121,
112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 59, 32, 92, 10, 32, 32,
32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 110, 111, 100,
101, 32, 42, 95, 100, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 110, 116, 32, 95, 116, 111, 112, 59, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 105, 110, 116, 51, 50, 95, 116, 32, 95, 116, 110, 44,
32, 95, 115, 116, 91, 51, 54, 93, 59, 32, 92, 10, 32, 32, 32, 32, 125,
32, 83, 69, 76, 70, 35, 35, 95, 105, 116, 101, 114, 59, 32, 92, 10,
92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99,
116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 95, 95,
40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108, 108, 40, 83, 69,
76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 92,
10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 110,
111, 100, 101, 32, 42, 110, 111, 100, 101, 115, 59, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 105, 110, 116, 51, 50, 95, 116, 32, 114,
111, 111, 116, 44, 32, 100, 105, 115, 112, 44, 32, 104, 101, 97, 100,
44, 32, 115, 105, 122, 101, 44, 32, 99, 97, 112, 59, 32, 92, 10, 32,
32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105,
110, 101, 32, 95, 99, 95, 99, 115, 116, 97, 99, 107, 95, 102, 105,
120, 101, 100, 40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 44, 32, 67, 65,
80, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102,
32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117,
101, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 83, 69, 76, 70, 35,
35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 44, 32, 42,
101, 110, 100, 59, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95, 105, 116,
101, 114, 59, 32, 92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115,
116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117,
116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108,
108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70,
32, 123, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
100, 97, 116, 97, 91, 67, 65, 80, 93, 59, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 95, 108, 101, 110, 59, 32, 125, 32, 83, 69, 76, 70,
10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 99, 115,
116, 97, 99, 107, 95, 116, 121, 112, 101, 115, 40, 83, 69, 76, 70, 44,
32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101,
100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 118,
97, 108, 117, 101, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101,
100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 83, 69,
76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 44,
32, 42, 101, 110, 100, 59, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95,
105, 116, 101, 114, 59, 32, 92, 10, 116, 121, 112, 101, 100, 101, 102,
32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114, 105,
98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100, 99,
97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69,
76, 70, 32, 123, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117,
101, 42, 32, 100, 97, 116, 97, 59, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 95, 108, 101, 110, 44, 32, 95, 99, 97, 112, 59, 32, 125,
32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
99, 95, 99, 118, 101, 99, 95, 116, 121, 112, 101, 115, 40, 83, 69, 76,
70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112,
101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 83,
69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102,
44, 32, 42, 101, 110, 100, 59, 32, 125, 32, 83, 69, 76, 70, 35, 35,
95, 105, 116, 101, 114, 59, 32, 92, 10, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114,
105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100,
99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83,
69, 76, 70, 32, 123, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108,
117, 101, 32, 42, 100, 97, 116, 97, 59, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 95, 108, 101, 110, 44, 32, 95, 99, 97, 112, 59, 32,
125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
95, 99, 95, 99, 112, 113, 117, 101, 95, 116, 121, 112, 101, 115, 40,
83, 69, 76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32,
116, 121, 112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70,
35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97,
116, 116, 114, 105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116,
104, 111, 100, 99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41,
41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 42, 32, 100, 97, 116, 97, 59, 32, 105, 110,
116, 112, 116, 114, 95, 116, 32, 95, 108, 101, 110, 44, 32, 95, 99,
97, 112, 59, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 101, 110, 100,
105, 102, 32, 47, 47, 32, 83, 84, 67, 95, 70, 79, 82, 87, 65, 82, 68,
95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 47, 47, 32, 35, 35,
35, 32, 69, 78, 68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68,
69, 58, 32, 102, 111, 114, 119, 97, 114, 100, 46, 104, 10, 47, 47, 32,
69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101,
110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97,
100, 101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100,
101, 32, 60, 115, 116, 100, 108, 105, 98, 46, 104, 62, 10, 47, 47, 32,
69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101,
110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97,
100, 101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100,
101, 32, 60, 115, 116, 114, 105, 110, 103, 46, 104, 62, 10, 115, 116,
114, 117, 99, 116, 32, 99, 104, 97, 115, 104, 95, 115, 108, 111, 116,
32, 123, 32, 117, 105, 110, 116, 56, 95, 116, 32, 104, 97, 115, 104,
120, 59, 32, 125, 59, 10, 35, 101, 110, 100, 105, 102, 32, 47, 47, 32,
67, 77, 65, 80, 95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 10,
35, 105, 102, 110, 100, 101, 102, 32, 95, 105, 95, 112, 114, 101, 102,
105, 120, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105,
95, 112, 114, 101, 102, 105, 120, 32, 99, 109, 97, 112, 95, 10, 35,
101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 95,
105, 95, 105, 115, 115, 101, 116, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 95, 105, 95, 105, 115, 109, 97, 112, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78,
76, 89, 32, 99, 95, 116, 114, 117, 101, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 105, 95, 83, 69, 84, 95, 79, 78, 76, 89, 32,
99, 95, 102, 97, 108, 115, 101, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 95, 105, 95, 107, 101, 121, 114, 101, 102, 40, 118, 112,
41, 32, 40, 38, 40, 118, 112, 41, 45, 62, 102, 105, 114, 115, 116, 41,
10, 35, 101, 108, 115, 101, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76, 89, 32, 99, 95, 102,
97, 108, 115, 101, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
95, 105, 95, 83, 69, 84, 95, 79, 78, 76, 89, 32, 99, 95, 116, 114,
117, 101, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105,
95, 107, 101, 121, 114, 101, 102, 40, 118, 112, 41, 32, 40, 118, 112,
41, 10, 35, 101, 110, 100, 105, 102, 10, 35, 100, 101, 102, 105, 110,
101, 32, 95, 105, 95, 105, 115, 104, 97, 115, 104, 10, 47, 47, 32, 35,
35, 35, 32, 66, 69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73, 78, 67,
76, 85, 68, 69, 58, 32, 116, 101, 109, 112, 108, 97, 116, 101, 46,
104, 10, 35, 105, 102, 110, 100, 101, 102, 32, 95, 105, 95, 116, 101,
109, 112, 108, 97, 116, 101, 10, 35, 100, 101, 102, 105, 110, 101, 32,
95, 105, 95, 116, 101, 109, 112, 108, 97, 116, 101, 10, 10, 35, 105,
102, 110, 100, 101, 102, 32, 83, 84, 67, 95, 84, 69, 77, 80, 76, 65,
84, 69, 95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 35, 100, 101,
102, 105, 110, 101, 32, 83, 84, 67, 95, 84, 69, 77, 80, 76, 65, 84,
69, 95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 110, 97,
109, 101, 41, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 116, 121, 112,
101, 44, 32, 110, 97, 109, 101, 41, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 99, 95, 68, 69, 70, 84, 89, 80, 69, 83, 40,
109, 97, 99, 114, 111, 44, 32, 83, 69, 76, 70, 44, 32, 46, 46, 46, 41,
32, 99, 95, 69, 88, 80, 65, 78, 68, 40, 109, 97, 99, 114, 111, 40, 83,
69, 76, 70, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41,
41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109, 95,
118, 97, 108, 117, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118,
97, 108, 117, 101, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 95, 109, 95, 107, 101, 121, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 107, 101, 121, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 95, 109, 95, 109, 97, 112, 112, 101, 100, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 109, 97, 112, 112, 101, 100, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 95, 109, 95, 114, 109, 97, 112, 112, 101,
100, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 109, 97, 112, 112,
101, 100, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95,
109, 95, 114, 97, 119, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114,
97, 119, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95,
109, 95, 107, 101, 121, 114, 97, 119, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 107, 101, 121, 114, 97, 119, 41, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 95, 109, 95, 105, 116, 101, 114, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 105, 116, 101, 114, 41, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 95, 109, 95, 114, 101, 115, 117,
108, 116, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 117,
108, 116, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95,
109, 95, 110, 111, 100, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
110, 111, 100, 101, 41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35,
105, 102, 110, 100, 101, 102, 32, 105, 95, 116, 121, 112, 101, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 116, 121, 112, 101,
32, 99, 95, 74, 79, 73, 78, 40, 95, 105, 95, 112, 114, 101, 102, 105,
120, 44, 32, 105, 95, 116, 97, 103, 41, 10, 35, 101, 110, 100, 105,
102, 10, 10, 35, 105, 102, 100, 101, 102, 32, 105, 95, 107, 101, 121,
99, 108, 97, 115, 115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101,
99, 97, 116, 101, 100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 105,
95, 107, 101, 121, 99, 108, 97, 115, 115, 10, 35, 101, 110, 100, 105,
102, 10, 35, 105, 102, 100, 101, 102, 32, 105, 95, 118, 97, 108, 99,
108, 97, 115, 115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99,
97, 116, 101, 100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 118, 97, 108, 95, 99, 108, 97, 115, 115, 32, 105, 95,
118, 97, 108, 99, 108, 97, 115, 115, 10, 35, 101, 110, 100, 105, 102,
10, 35, 105, 102, 100, 101, 102, 32, 105, 95, 114, 97, 119, 99, 108,
97, 115, 115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97,
116, 101, 100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 32, 105, 95, 114,
97, 119, 99, 108, 97, 115, 115, 10, 35, 101, 110, 100, 105, 102, 10,
35, 105, 102, 100, 101, 102, 32, 105, 95, 107, 101, 121, 98, 111, 120,
101, 100, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116,
101, 100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 95, 97, 114, 99, 98, 111, 120, 32, 105, 95, 107,
101, 121, 98, 111, 120, 101, 100, 10, 35, 101, 110, 100, 105, 102, 10,
35, 105, 102, 100, 101, 102, 32, 105, 95, 118, 97, 108, 98, 111, 120,
101, 100, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116,
101, 100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 32, 105, 95, 118, 97,
108, 98, 111, 120, 101, 100, 10, 35, 101, 110, 100, 105, 102, 10, 10,
35, 105, 102, 32, 33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105,
95, 107, 101, 121, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 95, 115, 116, 114, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95,
115, 115, 118, 32, 124, 124, 32, 92, 10, 32, 32, 32, 32, 32, 32, 100,
101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 99, 108,
97, 115, 115, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 95, 97, 114, 99, 98, 111, 120, 41, 10, 32, 32,
35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95,
105, 115, 109, 97, 112, 10, 32, 32, 32, 32, 35, 101, 114, 114, 111,
114, 32, 34, 105, 95, 107, 101, 121, 42, 32, 109, 117, 115, 116, 32,
98, 101, 32, 100, 101, 102, 105, 110, 101, 100, 32, 102, 111, 114, 32,
109, 97, 112, 115, 34, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10,
10, 32, 32, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 118, 97, 108, 95, 115, 116, 114, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 95, 115, 116,
114, 32, 105, 95, 118, 97, 108, 95, 115, 116, 114, 10, 32, 32, 35,
101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 95, 115, 115, 118, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107,
101, 121, 95, 115, 115, 118, 32, 105, 95, 118, 97, 108, 95, 115, 115,
118, 10, 32, 32, 35, 101, 110, 100, 105, 102, 32, 32, 10, 32, 32, 35,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 95, 97, 114, 99, 98, 111, 120, 10, 32, 32, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 95, 97, 114, 99, 98,
111, 120, 32, 105, 95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120,
10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 95, 99,
108, 97, 115, 115, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 105,
95, 118, 97, 108, 95, 99, 108, 97, 115, 115, 10, 32, 32, 35, 101, 110,
100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 118, 97, 108, 10, 32, 32, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 32, 105, 95, 118, 97,
108, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108,
114, 97, 119, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 107, 101, 121, 114, 97, 119, 32, 105, 95, 118, 97, 108,
114, 97, 119, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 99, 108, 111, 110, 101, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 32,
105, 95, 118, 97, 108, 99, 108, 111, 110, 101, 10, 32, 32, 35, 101,
110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 10, 32,
32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101,
121, 102, 114, 111, 109, 32, 105, 95, 118, 97, 108, 102, 114, 111,
109, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108,
116, 111, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 107, 101, 121, 116, 111, 32, 105, 95, 118, 97, 108, 116, 111,
10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 100,
114, 111, 112, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 32, 105, 95, 118, 97,
108, 100, 114, 111, 112, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10,
35, 101, 110, 100, 105, 102, 10, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 102, 108, 97, 103, 41,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 40, 105, 95, 111, 112,
116, 41, 32, 38, 32, 40, 102, 108, 97, 103, 41, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 105, 115, 95, 102, 111, 114, 119, 97,
114, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60,
60, 48, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 110,
111, 95, 97, 116, 111, 109, 105, 99, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 40, 49, 60, 60, 49, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 110, 111, 95, 99, 108, 111, 110, 101, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60, 60,
50, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 110, 111,
95, 101, 109, 112, 108, 97, 99, 101, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 40, 49, 60, 60, 51, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 110, 111, 95, 104, 97, 115, 104, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60, 60, 52, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 117, 115, 101, 95,
99, 109, 112, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 40, 49, 60, 60, 53, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 109, 111, 114, 101, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60, 60, 54, 41, 10, 10, 35,
105, 102, 32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 99, 95, 105,
115, 95, 102, 111, 114, 119, 97, 114, 100, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 105, 115, 95, 102, 111, 114,
119, 97, 114, 100, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102,
32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 99, 95, 110, 111, 95,
104, 97, 115, 104, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 110, 111, 95, 104, 97, 115, 104, 10, 35, 101, 110, 100,
105, 102, 10, 35, 105, 102, 32, 99, 95, 111, 112, 116, 105, 111, 110,
40, 99, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 41, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 110, 111, 95, 101,
109, 112, 108, 97, 99, 101, 10, 35, 101, 110, 100, 105, 102, 10, 35,
105, 102, 32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 99, 95, 117,
115, 101, 95, 99, 109, 112, 41, 32, 124, 124, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 99, 109, 112, 32, 124, 124, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115, 32, 124,
124, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 100, 101, 102,
105, 110, 101, 100, 32, 95, 105, 95, 105, 115, 109, 97, 112, 32, 124,
124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95, 105, 115,
115, 101, 116, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100,
32, 95, 105, 95, 105, 115, 112, 113, 117, 101, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 117, 115, 101, 95, 99, 109, 112,
10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 32, 99, 95, 111,
112, 116, 105, 111, 110, 40, 99, 95, 110, 111, 95, 99, 108, 111, 110,
101, 41, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95,
105, 95, 99, 97, 114, 99, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 10, 35, 101,
110, 100, 105, 102, 10, 35, 105, 102, 32, 99, 95, 111, 112, 116, 105,
111, 110, 40, 99, 95, 109, 111, 114, 101, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 109, 111, 114, 101, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 107, 101, 121, 95, 115, 116, 114, 10, 32, 32,
35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 95, 99,
108, 97, 115, 115, 32, 99, 115, 116, 114, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115,
115, 32, 99, 99, 104, 97, 114, 112, 116, 114, 10, 32, 32, 35, 105,
102, 110, 100, 101, 102, 32, 105, 95, 116, 97, 103, 10, 32, 32, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 116, 97, 103, 32,
115, 116, 114, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 35, 101,
108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
107, 101, 121, 95, 115, 115, 118, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32,
99, 115, 116, 114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 32, 99, 115, 118,
105, 101, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 102, 114, 111, 109, 32, 99, 115, 116, 114, 95, 102,
114, 111, 109, 95, 115, 118, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 107, 101, 121, 116, 111, 32, 99, 115, 116, 114, 95,
115, 118, 10, 32, 32, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95,
116, 97, 103, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 116, 97, 103, 32, 115, 115, 118, 10, 32, 32, 35, 101,
110, 100, 105, 102, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 97, 114, 99, 98,
111, 120, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 105, 95, 107, 101, 121,
95, 97, 114, 99, 98, 111, 120, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 32,
99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 95, 97, 114, 99,
98, 111, 120, 44, 32, 95, 114, 97, 119, 41, 10, 32, 32, 35, 105, 102,
32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 117, 115, 101, 95,
99, 109, 112, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 101, 113, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 107,
101, 121, 95, 97, 114, 99, 98, 111, 120, 44, 32, 95, 114, 97, 119, 95,
101, 113, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 10,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121,
114, 97, 119, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115,
10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 38, 38, 32, 33,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 114,
97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
114, 97, 119, 95, 99, 108, 97, 115, 115, 32, 105, 95, 107, 101, 121,
10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97,
115, 115, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
107, 101, 121, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115,
10, 32, 32, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101,
121, 99, 108, 111, 110, 101, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 32,
99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 44, 32, 95, 99,
108, 111, 110, 101, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10,
32, 32, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121,
100, 114, 111, 112, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 32, 99, 95, 74,
79, 73, 78, 40, 105, 95, 107, 101, 121, 44, 32, 95, 100, 114, 111,
112, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101,
121, 102, 114, 111, 109, 32, 38, 38, 32, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 102, 114,
111, 109, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 44,
32, 95, 102, 114, 111, 109, 41, 10, 32, 32, 35, 101, 110, 100, 105,
102, 10, 32, 32, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 116, 111, 32, 38, 38, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107,
101, 121, 116, 111, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101,
121, 44, 32, 95, 116, 111, 114, 97, 119, 41, 10, 32, 32, 35, 101, 110,
100, 105, 102, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102,
32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 114, 97, 119, 95,
99, 108, 97, 115, 115, 10, 32, 32, 35, 105, 102, 32, 33, 40, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 99, 109, 112, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115,
41, 32, 38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
117, 115, 101, 95, 99, 109, 112, 10, 32, 32, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 99, 109, 112, 32, 99, 95, 74, 79, 73,
78, 40, 105, 95, 107, 101, 121, 114, 97, 119, 44, 32, 95, 99, 109,
112, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 104,
97, 115, 104, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 110, 111, 95, 104, 97, 115, 104, 41, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 104, 97, 115, 104, 32, 99,
95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 114, 97, 119, 44, 32,
95, 104, 97, 115, 104, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102,
10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 99, 109, 112, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115,
32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 117,
115, 101, 95, 99, 109, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10, 35, 101,
110, 100, 105, 102, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 101, 113, 32, 124, 124, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 117, 115, 101, 95, 99, 109, 112, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 104, 97, 115,
95, 101, 113, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 32,
33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 104, 97, 115,
104, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
110, 111, 95, 104, 97, 115, 104, 41, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 104, 97, 115, 104, 32, 99, 95, 100, 101,
102, 97, 117, 108, 116, 95, 104, 97, 115, 104, 10, 35, 101, 110, 100,
105, 102, 10, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 10, 32, 32, 35, 101, 114, 114, 111,
114, 32, 34, 78, 111, 32, 105, 95, 107, 101, 121, 32, 111, 114, 32,
105, 95, 118, 97, 108, 32, 100, 101, 102, 105, 110, 101, 100, 34, 10,
35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 114, 97, 119, 32, 94, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 107, 101, 121, 116, 111, 10, 32, 32, 35,
101, 114, 114, 111, 114, 32, 34, 66, 111, 116, 104, 32, 105, 95, 107,
101, 121, 114, 97, 119, 47, 105, 95, 118, 97, 108, 114, 97, 119, 32,
97, 110, 100, 32, 105, 95, 107, 101, 121, 116, 111, 47, 105, 95, 118,
97, 108, 116, 111, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101,
102, 105, 110, 101, 100, 44, 32, 105, 102, 32, 97, 110, 121, 34, 10,
35, 101, 108, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 32, 38, 38, 32, 40,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 99,
108, 111, 110, 101, 32, 94, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 100, 114, 111, 112, 41, 10, 32, 32, 35, 101,
114, 114, 111, 114, 32, 34, 66, 111, 116, 104, 32, 105, 95, 107, 101,
121, 99, 108, 111, 110, 101, 47, 105, 95, 118, 97, 108, 99, 108, 111,
110, 101, 32, 97, 110, 100, 32, 105, 95, 107, 101, 121, 100, 114, 111,
112, 47, 105, 95, 118, 97, 108, 100, 114, 111, 112, 32, 109, 117, 115,
116, 32, 98, 101, 32, 100, 101, 102, 105, 110, 101, 100, 44, 32, 105,
102, 32, 97, 110, 121, 32, 40, 117, 110, 108, 101, 115, 115, 32, 105,
95, 110, 111, 95, 99, 108, 111, 110, 101, 32, 100, 101, 102, 105, 110,
101, 100, 41, 46, 34, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 102, 114, 111, 109, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 100, 114, 111, 112,
10, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34, 105, 95, 102, 114,
111, 109, 32, 47, 32, 105, 95, 100, 114, 111, 112, 32, 110, 111, 116,
32, 115, 117, 112, 112, 111, 114, 116, 101, 100, 46, 32, 68, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 102, 114, 111, 109, 47,
105, 95, 118, 97, 108, 102, 114, 111, 109, 32, 97, 110, 100, 47, 111,
114, 32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 47, 105, 95, 118,
97, 108, 100, 114, 111, 112, 32, 105, 110, 115, 116, 101, 97, 100, 34,
10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 114, 97, 119, 32, 38, 38, 32, 100, 101, 102,
105, 110, 101, 100, 32, 95, 105, 95, 105, 115, 104, 97, 115, 104, 32,
38, 38, 32, 33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
104, 97, 115, 104, 32, 38, 38, 32, 40, 100, 101, 102, 105, 110, 101,
100, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 32, 124, 124,
32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 101, 113, 41, 41,
10, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34, 70, 111, 114, 32, 99,
109, 97, 112, 47, 99, 115, 101, 116, 44, 32, 98, 111, 116, 104, 32,
105, 95, 104, 97, 115, 104, 32, 97, 110, 100, 32, 105, 95, 101, 113,
32, 40, 111, 114, 32, 105, 95, 108, 101, 115, 115, 32, 111, 114, 32,
105, 95, 99, 109, 112, 41, 32, 109, 117, 115, 116, 32, 98, 101, 32,
100, 101, 102, 105, 110, 101, 100, 32, 119, 104, 101, 110, 32, 105,
95, 107, 101, 121, 114, 97, 119, 32, 105, 115, 32, 100, 101, 102, 105,
110, 101, 100, 46, 34, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 32, 38,
38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 117, 115, 101,
95, 99, 109, 112, 32, 38, 38, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10, 32, 32, 35,
101, 114, 114, 111, 114, 32, 34, 70, 111, 114, 32, 99, 115, 109, 97,
112, 47, 99, 115, 115, 101, 116, 47, 99, 112, 113, 117, 101, 44, 32,
105, 95, 99, 109, 112, 32, 111, 114, 32, 105, 95, 108, 101, 115, 115,
32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105, 110, 101,
100, 32, 119, 104, 101, 110, 32, 105, 95, 107, 101, 121, 114, 97, 119,
32, 105, 115, 32, 100, 101, 102, 105, 110, 101, 100, 46, 34, 10, 35,
101, 110, 100, 105, 102, 10, 10, 47, 47, 32, 105, 95, 101, 113, 44,
32, 105, 95, 108, 101, 115, 115, 44, 32, 105, 95, 99, 109, 112, 10,
35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
101, 113, 32, 38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105,
95, 99, 109, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 101, 113, 40, 120, 44, 32, 121, 41, 32, 33, 40, 105, 95, 99,
109, 112, 40, 120, 44, 32, 121, 41, 41, 10, 35, 101, 108, 105, 102,
32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 101, 113, 32,
38, 38, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107,
101, 121, 114, 97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 101, 113, 40, 120, 44, 32, 121, 41, 32, 42, 120, 32, 61,
61, 32, 42, 121, 32, 47, 47, 32, 102, 111, 114, 32, 105, 110, 116,
101, 103, 114, 97, 108, 32, 116, 121, 112, 101, 115, 44, 32, 101, 108,
115, 101, 32, 100, 101, 102, 105, 110, 101, 32, 105, 95, 101, 113, 32,
111, 114, 32, 105, 95, 99, 109, 112, 32, 121, 111, 117, 114, 115, 101,
108, 102, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 32, 33,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115,
32, 38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 99,
109, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
108, 101, 115, 115, 40, 120, 44, 32, 121, 41, 32, 40, 105, 95, 99,
109, 112, 40, 120, 44, 32, 121, 41, 41, 32, 60, 32, 48, 10, 35, 101,
108, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
108, 101, 115, 115, 32, 38, 38, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 108, 101, 115, 115, 40, 120, 44,
32, 121, 41, 32, 42, 120, 32, 60, 32, 42, 121, 32, 47, 47, 32, 102,
111, 114, 32, 105, 110, 116, 101, 103, 114, 97, 108, 32, 116, 121,
112, 101, 115, 44, 32, 101, 108, 115, 101, 32, 100, 101, 102, 105,
110, 101, 32, 105, 95, 108, 101, 115, 115, 32, 111, 114, 32, 105, 95,
99, 109, 112, 32, 121, 111, 117, 114, 115, 101, 108, 102, 10, 35, 101,
110, 100, 105, 102, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 99, 109, 112, 32, 38, 38, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 99, 109, 112, 40, 120, 44,
32, 121, 41, 32, 40, 105, 95, 108, 101, 115, 115, 40, 121, 44, 32,
120, 41, 41, 32, 45, 32, 40, 105, 95, 108, 101, 115, 115, 40, 120, 44,
32, 121, 41, 41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105,
102, 110, 100, 101, 102, 32, 105, 95, 116, 97, 103, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 116, 97, 103, 32, 105, 95,
107, 101, 121, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 32, 32,
35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 114, 97,
119, 32, 105, 95, 107, 101, 121, 10, 35, 101, 110, 100, 105, 102, 10,
35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 102,
114, 111, 109, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 102, 114, 111, 109, 32, 99, 95, 100, 101, 102, 97,
117, 108, 116, 95, 99, 108, 111, 110, 101, 10, 35, 101, 108, 115, 101,
10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 104, 97,
115, 95, 101, 109, 112, 108, 97, 99, 101, 10, 35, 101, 110, 100, 105,
102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121,
116, 111, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
107, 101, 121, 116, 111, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116,
95, 116, 111, 114, 97, 119, 10, 35, 101, 110, 100, 105, 102, 10, 35,
105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 99, 108,
111, 110, 101, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 99, 108, 111, 110, 101, 32, 99, 95, 100, 101, 102,
97, 117, 108, 116, 95, 99, 108, 111, 110, 101, 10, 35, 101, 110, 100,
105, 102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101,
121, 100, 114, 111, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 32, 99, 95, 100, 101,
102, 97, 117, 108, 116, 95, 100, 114, 111, 112, 10, 35, 101, 110, 100,
105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100,
32, 95, 105, 95, 105, 115, 109, 97, 112, 32, 47, 47, 32, 45, 45, 45,
45, 32, 112, 114, 111, 99, 101, 115, 115, 32, 99, 109, 97, 112, 47,
99, 115, 109, 97, 112, 32, 118, 97, 108, 117, 101, 32, 105, 95, 118,
97, 108, 44, 32, 46, 46, 46, 32, 45, 45, 45, 45, 10, 10, 35, 105, 102,
100, 101, 102, 32, 105, 95, 118, 97, 108, 95, 115, 116, 114, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 95,
99, 108, 97, 115, 115, 32, 99, 115, 116, 114, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 114, 97, 119, 32,
99, 111, 110, 115, 116, 32, 99, 104, 97, 114, 42, 10, 35, 101, 108,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 95, 115, 115, 118, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 118, 97, 108, 95, 99, 108, 97, 115, 115, 32, 99, 115,
116, 114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
118, 97, 108, 114, 97, 119, 32, 99, 115, 118, 105, 101, 119, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 102,
114, 111, 109, 32, 99, 115, 116, 114, 95, 102, 114, 111, 109, 95, 115,
118, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118,
97, 108, 116, 111, 32, 99, 115, 116, 114, 95, 115, 118, 10, 35, 101,
108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 95, 99, 108, 97, 115,
115, 32, 105, 95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 114,
97, 119, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 118, 97, 108, 95,
97, 114, 99, 98, 111, 120, 44, 32, 95, 114, 97, 119, 41, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 105, 102, 100, 101, 102, 32, 105, 95,
118, 97, 108, 95, 99, 108, 97, 115, 115, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 32, 105, 95, 118, 97,
108, 95, 99, 108, 97, 115, 115, 10, 32, 32, 35, 105, 102, 110, 100,
101, 102, 32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101, 10, 32,
32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97,
108, 99, 108, 111, 110, 101, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95,
118, 97, 108, 44, 32, 95, 99, 108, 111, 110, 101, 41, 10, 32, 32, 35,
101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 100, 114, 111, 112, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 100, 114,
111, 112, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 118, 97, 108, 44,
32, 95, 100, 114, 111, 112, 41, 10, 32, 32, 35, 101, 110, 100, 105,
102, 10, 32, 32, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 32, 38, 38, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 114, 97,
119, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 118, 97, 108, 102, 114, 111, 109, 32, 99, 95, 74, 79, 73, 78, 40,
105, 95, 118, 97, 108, 44, 32, 95, 102, 114, 111, 109, 41, 10, 32, 32,
35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 33, 100,
101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 116, 111, 32,
38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 114, 97, 119, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 118, 97, 108, 116, 111, 32, 99, 95, 74, 79, 73, 78,
40, 105, 95, 118, 97, 108, 44, 32, 95, 116, 111, 114, 97, 119, 41, 10,
32, 32, 35, 101, 110, 100, 105, 102, 10, 35, 101, 110, 100, 105, 102,
10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108,
10, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34, 105, 95, 118, 97,
108, 42, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105,
110, 101, 100, 32, 102, 111, 114, 32, 109, 97, 112, 115, 34, 10, 35,
101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105,
95, 118, 97, 108, 114, 97, 119, 32, 94, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 118, 97, 108, 116, 111, 10, 32, 32, 35, 101,
114, 114, 111, 114, 32, 34, 66, 111, 116, 104, 32, 105, 95, 118, 97,
108, 114, 97, 119, 32, 97, 110, 100, 32, 105, 95, 118, 97, 108, 116,
111, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105, 110,
101, 100, 44, 32, 105, 102, 32, 97, 110, 121, 34, 10, 35, 101, 108,
105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110,
111, 95, 99, 108, 111, 110, 101, 32, 38, 38, 32, 40, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101,
32, 94, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 100, 114, 111, 112, 41, 10, 32, 32, 35, 101, 114, 114, 111, 114,
32, 34, 66, 111, 116, 104, 32, 105, 95, 118, 97, 108, 99, 108, 111,
110, 101, 32, 97, 110, 100, 32, 105, 95, 118, 97, 108, 100, 114, 111,
112, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105, 110,
101, 100, 44, 32, 105, 102, 32, 97, 110, 121, 34, 10, 35, 101, 110,
100, 105, 102, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95,
118, 97, 108, 114, 97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 118, 97, 108, 114, 97, 119, 32, 105, 95, 118, 97,
108, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101,
102, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 102, 114,
111, 109, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 108,
111, 110, 101, 10, 35, 101, 108, 115, 101, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 104, 97, 115, 95, 101, 109, 112, 108,
97, 99, 101, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 118, 97, 108, 116, 111, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 116, 111, 32,
99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 116, 111, 114, 97, 119,
10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 99, 108, 111,
110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 108,
111, 110, 101, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 118, 97, 108, 100, 114, 111, 112, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 100,
114, 111, 112, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 100,
114, 111, 112, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 101, 110,
100, 105, 102, 32, 47, 47, 32, 33, 95, 105, 95, 105, 115, 109, 97,
112, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 118, 97,
108, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118,
97, 108, 32, 105, 95, 107, 101, 121, 10, 35, 101, 110, 100, 105, 102,
10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108, 114,
97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
118, 97, 108, 114, 97, 119, 32, 105, 95, 107, 101, 121, 114, 97, 119,
10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102,
32, 105, 95, 104, 97, 115, 95, 101, 109, 112, 108, 97, 99, 101, 10,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 110, 111, 95,
101, 109, 112, 108, 97, 99, 101, 10, 35, 101, 110, 100, 105, 102, 10,
35, 101, 110, 100, 105, 102, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78,
68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 116,
101, 109, 112, 108, 97, 116, 101, 46, 104, 10, 35, 105, 102, 110, 100,
101, 102, 32, 105, 95, 105, 115, 95, 102, 111, 114, 119, 97, 114, 100,
10, 32, 32, 95, 99, 95, 68, 69, 70, 84, 89, 80, 69, 83, 40, 95, 99,
95, 99, 104, 97, 115, 104, 95, 116, 121, 112, 101, 115, 44, 32, 105,
95, 116, 121, 112, 101, 44, 32, 105, 95, 107, 101, 121, 44, 32, 105,
95, 118, 97, 108, 44, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76, 89,
44, 32, 95, 105, 95, 83, 69, 84, 95, 79, 78, 76, 89, 41, 59, 10, 35,
101, 110, 100, 105, 102, 10, 10, 95, 105, 95, 77, 65, 80, 95, 79, 78,
76, 89, 40, 32, 115, 116, 114, 117, 99, 116, 32, 95, 109, 95, 118, 97,
108, 117, 101, 32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 107, 101,
121, 32, 102, 105, 114, 115, 116, 59, 10, 32, 32, 32, 32, 95, 109, 95,
109, 97, 112, 112, 101, 100, 32, 115, 101, 99, 111, 110, 100, 59, 10,
125, 59, 32, 41, 10, 10, 116, 121, 112, 101, 100, 101, 102, 32, 105,
95, 107, 101, 121, 114, 97, 119, 32, 95, 109, 95, 107, 101, 121, 114,
97, 119, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 105, 95, 118,
97, 108, 114, 97, 119, 32, 95, 109, 95, 114, 109, 97, 112, 112, 101,
100, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 95, 105, 95, 83,
69, 84, 95, 79, 78, 76, 89, 40, 32, 105, 95, 107, 101, 121, 114, 97,
119, 32, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 105, 95, 77, 65,
80, 95, 79, 78, 76, 89, 40, 32, 115, 116, 114, 117, 99, 116, 32, 123,
32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 32, 102, 105, 114, 115,
116, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95,
109, 95, 114, 109, 97, 112, 112, 101, 100, 32, 115, 101, 99, 111, 110,
100, 59, 32, 125, 32, 41, 10, 95, 109, 95, 114, 97, 119, 59, 10, 10,
83, 84, 67, 95, 65, 80, 73, 32, 105, 95, 116, 121, 112, 101, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
119, 105, 116, 104, 95, 99, 97, 112, 97, 99, 105, 116, 121, 41, 40,
105, 110, 116, 112, 116, 114, 95, 116, 32, 99, 97, 112, 41, 59, 10,
35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
110, 111, 95, 99, 108, 111, 110, 101, 10, 83, 84, 67, 95, 65, 80, 73,
32, 105, 95, 116, 121, 112, 101, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 111, 110, 101, 41,
40, 105, 95, 116, 121, 112, 101, 32, 109, 97, 112, 41, 59, 10, 35,
101, 110, 100, 105, 102, 10, 83, 84, 67, 95, 65, 80, 73, 32, 118, 111,
105, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 100, 114, 111, 112, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 41, 59, 10, 83, 84, 67, 95, 65,
80, 73, 32, 118, 111, 105, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 101, 97, 114,
41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41,
59, 10, 83, 84, 67, 95, 65, 80, 73, 32, 95, 66, 111, 111, 108, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 114, 101, 115, 101, 114, 118, 101, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 99, 97, 112, 97, 99, 105, 116, 121, 41, 59, 10, 83,
84, 67, 95, 65, 80, 73, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116,
32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 98,
117, 99, 107, 101, 116, 95, 41, 40, 99, 111, 110, 115, 116, 32, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111,
110, 115, 116, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 42, 32,
114, 107, 101, 121, 112, 116, 114, 41, 59, 10, 83, 84, 67, 95, 65, 80,
73, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 32, 32, 32, 32,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115, 101, 114,
116, 95, 101, 110, 116, 114, 121, 95, 41, 40, 105, 95, 116, 121, 112,
101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121,
114, 97, 119, 32, 114, 107, 101, 121, 41, 59, 10, 83, 84, 67, 95, 65,
80, 73, 32, 118, 111, 105, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 114, 97, 115,
101, 95, 101, 110, 116, 114, 121, 41, 40, 105, 95, 116, 121, 112, 101,
42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 118, 97, 108, 117,
101, 42, 32, 118, 97, 108, 41, 59, 10, 83, 84, 67, 95, 65, 80, 73, 32,
102, 108, 111, 97, 116, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 109, 97, 120, 95, 108, 111, 97,
100, 95, 102, 97, 99, 116, 111, 114, 41, 40, 99, 111, 110, 115, 116,
32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41, 59,
10, 83, 84, 67, 95, 65, 80, 73, 32, 105, 110, 116, 112, 116, 114, 95,
116, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 99, 97, 112, 97, 99, 105, 116, 121, 41, 40, 99, 111, 110, 115,
116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 109, 97, 112, 41, 59,
10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 95, 116, 121,
112, 101, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 105, 110, 105, 116, 41, 40, 118, 111, 105, 100, 41, 32, 123, 32,
105, 95, 116, 121, 112, 101, 32, 109, 97, 112, 32, 61, 32, 123, 48,
125, 59, 32, 114, 101, 116, 117, 114, 110, 32, 109, 97, 112, 59, 32,
125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105,
100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 115, 104, 114, 105, 110, 107, 95, 116, 111, 95, 102, 105, 116,
41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41,
32, 123, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 101,
114, 118, 101, 41, 40, 115, 101, 108, 102, 44, 32, 40, 105, 110, 116,
112, 116, 114, 95, 116, 41, 115, 101, 108, 102, 45, 62, 115, 105, 122,
101, 41, 59, 32, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
95, 66, 111, 111, 108, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 101, 109, 112, 116, 121, 41, 40, 99, 111, 110,
115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 109, 97, 112, 41,
32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 33, 109, 97, 112, 45,
62, 115, 105, 122, 101, 59, 32, 125, 10, 83, 84, 67, 95, 73, 78, 76,
73, 78, 69, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 32, 32, 32,
32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 115, 105, 122, 101, 41, 40,
99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 109,
97, 112, 41, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 40, 105,
110, 116, 112, 116, 114, 95, 116, 41, 109, 97, 112, 45, 62, 115, 105,
122, 101, 59, 32, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95, 99, 111, 117,
110, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 109, 97, 112,
41, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 109, 97, 112, 45,
62, 98, 117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 59, 32,
125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 66, 111, 111,
108, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 99, 111, 110, 116, 97, 105, 110, 115, 41, 40, 99, 111, 110,
115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102,
44, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 32, 114, 107, 101,
121, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 114,
101, 116, 117, 114, 110, 32, 115, 101, 108, 102, 45, 62, 115, 105,
122, 101, 32, 38, 38, 32, 33, 95, 99, 95, 77, 69, 77, 66, 40, 95, 98,
117, 99, 107, 101, 116, 95, 41, 40, 115, 101, 108, 102, 44, 32, 38,
114, 107, 101, 121, 41, 46, 105, 110, 115, 101, 114, 116, 101, 100,
59, 32, 125, 10, 10, 35, 105, 102, 100, 101, 102, 32, 95, 105, 95,
105, 115, 109, 97, 112, 10, 32, 32, 32, 32, 83, 84, 67, 95, 65, 80,
73, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 105, 110, 115, 101, 114, 116, 95, 111, 114, 95,
97, 115, 115, 105, 103, 110, 41, 40, 105, 95, 116, 121, 112, 101, 42,
32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 32, 107,
101, 121, 44, 32, 95, 109, 95, 109, 97, 112, 112, 101, 100, 32, 109,
97, 112, 112, 101, 100, 41, 59, 10, 32, 32, 32, 32, 35, 105, 102, 32,
33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110, 111, 95, 101,
109, 112, 108, 97, 99, 101, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83,
84, 67, 95, 65, 80, 73, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 109, 112, 108, 97,
99, 101, 95, 111, 114, 95, 97, 115, 115, 105, 103, 110, 41, 40, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109,
95, 107, 101, 121, 114, 97, 119, 32, 114, 107, 101, 121, 44, 32, 95,
109, 95, 114, 109, 97, 112, 112, 101, 100, 32, 114, 109, 97, 112, 112,
101, 100, 41, 59, 10, 32, 32, 32, 32, 35, 101, 110, 100, 105, 102, 10,
10, 32, 32, 32, 32, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 99,
111, 110, 115, 116, 32, 95, 109, 95, 109, 97, 112, 112, 101, 100, 42,
10, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 97, 116, 41,
40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119,
32, 114, 107, 101, 121, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32,
32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 98, 32, 61, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95, 41,
40, 115, 101, 108, 102, 44, 32, 38, 114, 107, 101, 121, 41, 59, 10,
32, 32, 32, 32, 32, 32, 32, 32, 99, 95, 97, 115, 115, 101, 114, 116,
40, 33, 98, 46, 105, 110, 115, 101, 114, 116, 101, 100, 41, 59, 10,
32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 38,
98, 46, 114, 101, 102, 45, 62, 115, 101, 99, 111, 110, 100, 59, 10,
32, 32, 32, 32, 125, 10, 10, 32, 32, 32, 32, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 95, 109, 95, 109, 97, 112, 112, 101, 100, 42, 10,
32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 97, 116, 95, 109,
117, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 44, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 32, 114, 107,
101, 121, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 114, 101,
116, 117, 114, 110, 32, 40, 95, 109, 95, 109, 97, 112, 112, 101, 100,
42, 41, 95, 99, 95, 77, 69, 77, 66, 40, 95, 97, 116, 41, 40, 115, 101,
108, 102, 44, 32, 114, 107, 101, 121, 41, 59, 32, 125, 10, 35, 101,
110, 100, 105, 102, 32, 47, 47, 32, 95, 105, 95, 105, 115, 109, 97,
112, 10, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100,
32, 105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 10, 83, 84, 67, 95,
73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 99, 111, 112, 121, 41, 40, 105, 95, 116, 121, 112,
101, 32, 42, 115, 101, 108, 102, 44, 32, 99, 111, 110, 115, 116, 32,
105, 95, 116, 121, 112, 101, 42, 32, 111, 116, 104, 101, 114, 41, 32,
123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45, 62,
116, 97, 98, 108, 101, 32, 61, 61, 32, 111, 116, 104, 101, 114, 45,
62, 116, 97, 98, 108, 101, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32,
114, 101, 116, 117, 114, 110, 59, 10, 32, 32, 32, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 100, 114, 111, 112, 41, 40, 115, 101, 108, 102,
41, 59, 10, 32, 32, 32, 32, 42, 115, 101, 108, 102, 32, 61, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 111, 110, 101, 41, 40, 42,
111, 116, 104, 101, 114, 41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73,
78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108, 117, 101, 10, 95,
99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117, 101, 95, 99, 108,
111, 110, 101, 41, 40, 95, 109, 95, 118, 97, 108, 117, 101, 32, 95,
118, 97, 108, 41, 32, 123, 10, 32, 32, 32, 32, 42, 95, 105, 95, 107,
101, 121, 114, 101, 102, 40, 38, 95, 118, 97, 108, 41, 32, 61, 32,
105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 40, 40, 42, 95, 105,
95, 107, 101, 121, 114, 101, 102, 40, 38, 95, 118, 97, 108, 41, 41,
41, 59, 10, 32, 32, 32, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76,
89, 40, 32, 95, 118, 97, 108, 46, 115, 101, 99, 111, 110, 100, 32, 61,
32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101, 40, 95, 118, 97,
108, 46, 115, 101, 99, 111, 110, 100, 41, 59, 32, 41, 10, 32, 32, 32,
32, 114, 101, 116, 117, 114, 110, 32, 95, 118, 97, 108, 59, 10, 125,
10, 35, 101, 110, 100, 105, 102, 32, 47, 47, 32, 33, 105, 95, 110,
111, 95, 99, 108, 111, 110, 101, 10, 10, 35, 105, 102, 32, 33, 100,
101, 102, 105, 110, 101, 100, 32, 105, 95, 110, 111, 95, 101, 109,
112, 108, 97, 99, 101, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
95, 109, 95, 114, 101, 115, 117, 108, 116, 10, 95, 99, 95, 77, 69, 77,
66, 40, 95, 101, 109, 112, 108, 97, 99, 101, 41, 40, 105, 95, 116,
121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 107,
101, 121, 114, 97, 119, 32, 114, 107, 101, 121, 32, 95, 105, 95, 77,
65, 80, 95, 79, 78, 76, 89, 40, 44, 32, 95, 109, 95, 114, 109, 97,
112, 112, 101, 100, 32, 114, 109, 97, 112, 112, 101, 100, 41, 41, 32,
123, 10, 32, 32, 32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116,
32, 95, 114, 101, 115, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
105, 110, 115, 101, 114, 116, 95, 101, 110, 116, 114, 121, 95, 41, 40,
115, 101, 108, 102, 44, 32, 114, 107, 101, 121, 41, 59, 10, 32, 32,
32, 32, 105, 102, 32, 40, 95, 114, 101, 115, 46, 105, 110, 115, 101,
114, 116, 101, 100, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32,
42, 95, 105, 95, 107, 101, 121, 114, 101, 102, 40, 95, 114, 101, 115,
46, 114, 101, 102, 41, 32, 61, 32, 105, 95, 107, 101, 121, 102, 114,
111, 109, 40, 114, 107, 101, 121, 41, 59, 10, 32, 32, 32, 32, 32, 32,
32, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76, 89, 40, 32, 95, 114,
101, 115, 46, 114, 101, 102, 45, 62, 115, 101, 99, 111, 110, 100, 32,
61, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 40, 114, 109, 97,
112, 112, 101, 100, 41, 59, 32, 41, 10, 32, 32, 32, 32, 125, 10, 32,
32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 95, 114, 101, 115, 59,
10, 125, 10, 10, 35, 105, 102, 100, 101, 102, 32, 95, 105, 95, 105,
115, 109, 97, 112, 10, 32, 32, 32, 32, 83, 84, 67, 95, 73, 78, 76, 73,
78, 69, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 10, 32, 32, 32,
32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 109, 112, 108, 97, 99,
101, 95, 107, 101, 121, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119,
32, 114, 107, 101, 121, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32,
32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 95, 114, 101, 115,
32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115, 101,
114, 116, 95, 101, 110, 116, 114, 121, 95, 41, 40, 115, 101, 108, 102,
44, 32, 114, 107, 101, 121, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32,
32, 105, 102, 32, 40, 95, 114, 101, 115, 46, 105, 110, 115, 101, 114,
116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
95, 114, 101, 115, 46, 114, 101, 102, 45, 62, 102, 105, 114, 115, 116,
32, 61, 32, 105, 95, 107, 101, 121, 102, 114, 111, 109, 40, 114, 107,
101, 121, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116,
117, 114, 110, 32, 95, 114, 101, 115, 59, 10, 32, 32, 32, 32, 125, 10,
35, 101, 110, 100, 105, 102, 32, 47, 47, 32, 95, 105, 95, 105, 115,
109, 97, 112, 10, 35, 101, 110, 100, 105, 102, 32, 47, 47, 32, 33,
105, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 10, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 114, 97, 119, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117, 101, 95, 116,
111, 114, 97, 119, 41, 40, 99, 111, 110, 115, 116, 32, 95, 109, 95,
118, 97, 108, 117, 101, 42, 32, 118, 97, 108, 41, 32, 123, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 95, 105, 95, 83, 69, 84, 95,
79, 78, 76, 89, 40, 32, 105, 95, 107, 101, 121, 116, 111, 40, 118, 97,
108, 41, 32, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95,
105, 95, 77, 65, 80, 95, 79, 78, 76, 89, 40, 32, 99, 95, 76, 73, 84,
69, 82, 65, 76, 40, 95, 109, 95, 114, 97, 119, 41, 123, 105, 95, 107,
101, 121, 116, 111, 40, 40, 38, 118, 97, 108, 45, 62, 102, 105, 114,
115, 116, 41, 41, 44, 32, 105, 95, 118, 97, 108, 116, 111, 40, 40, 38,
118, 97, 108, 45, 62, 115, 101, 99, 111, 110, 100, 41, 41, 125, 32,
41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
118, 111, 105, 100, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97,
108, 117, 101, 95, 100, 114, 111, 112, 41, 40, 95, 109, 95, 118, 97,
108, 117, 101, 42, 32, 95, 118, 97, 108, 41, 32, 123, 10, 32, 32, 32,
32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 40, 95, 105, 95, 107,
101, 121, 114, 101, 102, 40, 95, 118, 97, 108, 41, 41, 59, 10, 32, 32,
32, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76, 89, 40, 32, 105, 95,
118, 97, 108, 100, 114, 111, 112, 40, 40, 38, 95, 118, 97, 108, 45,
62, 115, 101, 99, 111, 110, 100, 41, 41, 59, 32, 41, 10, 125, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 114, 101,
115, 117, 108, 116, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110,
115, 101, 114, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 32, 95, 107, 101,
121, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76, 89, 40, 44, 32, 95,
109, 95, 109, 97, 112, 112, 101, 100, 32, 95, 109, 97, 112, 112, 101,
100, 41, 41, 32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 114, 101, 115,
117, 108, 116, 32, 95, 114, 101, 115, 32, 61, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 105, 110, 115, 101, 114, 116, 95, 101, 110, 116, 114,
121, 95, 41, 40, 115, 101, 108, 102, 44, 32, 105, 95, 107, 101, 121,
116, 111, 40, 40, 38, 95, 107, 101, 121, 41, 41, 41, 59, 10, 32, 32,
32, 32, 105, 102, 32, 40, 95, 114, 101, 115, 46, 105, 110, 115, 101,
114, 116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32,
42, 95, 105, 95, 107, 101, 121, 114, 101, 102, 40, 95, 114, 101, 115,
46, 114, 101, 102, 41, 32, 61, 32, 95, 107, 101, 121, 59, 32, 95, 105,
95, 77, 65, 80, 95, 79, 78, 76, 89, 40, 32, 95, 114, 101, 115, 46,
114, 101, 102, 45, 62, 115, 101, 99, 111, 110, 100, 32, 61, 32, 95,
109, 97, 112, 112, 101, 100, 59, 32, 41, 125, 10, 32, 32, 32, 32, 101,
108, 115, 101, 10, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 105, 95,
107, 101, 121, 100, 114, 111, 112, 40, 40, 38, 95, 107, 101, 121, 41,
41, 59, 32, 95, 105, 95, 77, 65, 80, 95, 79, 78, 76, 89, 40, 32, 105,
95, 118, 97, 108, 100, 114, 111, 112, 40, 40, 38, 95, 109, 97, 112,
112, 101, 100, 41, 41, 59, 32, 41, 125, 10, 32, 32, 32, 32, 114, 101,
116, 117, 114, 110, 32, 95, 114, 101, 115, 59, 10, 125, 10, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108,
117, 101, 42, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112, 117, 115,
104, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102,
44, 32, 95, 109, 95, 118, 97, 108, 117, 101, 32, 95, 118, 97, 108, 41,
32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 114, 101, 115, 117, 108,
116, 32, 95, 114, 101, 115, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 105, 110, 115, 101, 114, 116, 95, 101, 110, 116, 114, 121, 95,
41, 40, 115, 101, 108, 102, 44, 32, 105, 95, 107, 101, 121, 116, 111,
40, 95, 105, 95, 107, 101, 121, 114, 101, 102, 40, 38, 95, 118, 97,
108, 41, 41, 41, 59, 10, 32, 32, 32, 32, 105, 102, 32, 40, 95, 114,
101, 115, 46, 105, 110, 115, 101, 114, 116, 101, 100, 41, 10, 32, 32,
32, 32, 32, 32, 32, 32, 42, 95, 114, 101, 115, 46, 114, 101, 102, 32,
61, 32, 95, 118, 97, 108, 59, 10, 32, 32, 32, 32, 101, 108, 115, 101,
10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 118, 97, 108, 117, 101, 95, 100, 114, 111, 112, 41, 40, 38, 95,
118, 97, 108, 41, 59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114,
110, 32, 95, 114, 101, 115, 46, 114, 101, 102, 59, 10, 125, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 112, 117, 116, 95, 110, 41, 40,
105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99,
111, 110, 115, 116, 32, 95, 109, 95, 114, 97, 119, 42, 32, 114, 97,
119, 44, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 41, 32,
123, 10, 32, 32, 32, 32, 119, 104, 105, 108, 101, 32, 40, 110, 45, 45,
41, 32, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
95, 105, 95, 105, 115, 115, 101, 116, 32, 38, 38, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 110, 111, 95, 101, 109, 112, 108, 97,
99, 101, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77,
66, 40, 95, 105, 110, 115, 101, 114, 116, 41, 40, 115, 101, 108, 102,
44, 32, 42, 114, 97, 119, 43, 43, 41, 59, 10, 35, 101, 108, 105, 102,
32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95, 105, 115, 115,
101, 116, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77,
66, 40, 95, 101, 109, 112, 108, 97, 99, 101, 41, 40, 115, 101, 108,
102, 44, 32, 42, 114, 97, 119, 43, 43, 41, 59, 10, 35, 101, 108, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110, 111, 95,
101, 109, 112, 108, 97, 99, 101, 10, 32, 32, 32, 32, 32, 32, 32, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115, 101, 114, 116, 95,
111, 114, 95, 97, 115, 115, 105, 103, 110, 41, 40, 115, 101, 108, 102,
44, 32, 114, 97, 119, 45, 62, 102, 105, 114, 115, 116, 44, 32, 114,
97, 119, 45, 62, 115, 101, 99, 111, 110, 100, 41, 44, 32, 43, 43, 114,
97, 119, 59, 10, 35, 101, 108, 115, 101, 10, 32, 32, 32, 32, 32, 32,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 109, 112, 108, 97,
99, 101, 95, 111, 114, 95, 97, 115, 115, 105, 103, 110, 41, 40, 115,
101, 108, 102, 44, 32, 114, 97, 119, 45, 62, 102, 105, 114, 115, 116,
44, 32, 114, 97, 119, 45, 62, 115, 101, 99, 111, 110, 100, 41, 44, 32,
43, 43, 114, 97, 119, 59, 10, 35, 101, 110, 100, 105, 102, 10, 125,
10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 95, 116, 121,
112, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 102, 114, 111, 109,
95, 110, 41, 40, 99, 111, 110, 115, 116, 32, 95, 109, 95, 114, 97,
119, 42, 32, 114, 97, 119, 44, 32, 105, 110, 116, 112, 116, 114, 95,
116, 32, 110, 41, 10, 32, 32, 32, 32, 123, 32, 105, 95, 116, 121, 112,
101, 32, 99, 120, 32, 61, 32, 123, 48, 125, 59, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 112, 117, 116, 95, 110, 41, 40, 38, 99, 120, 44,
32, 114, 97, 119, 44, 32, 110, 41, 59, 32, 114, 101, 116, 117, 114,
110, 32, 99, 120, 59, 32, 125, 10, 10, 83, 84, 67, 95, 65, 80, 73, 32,
95, 109, 95, 105, 116, 101, 114, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 98, 101, 103, 105, 110, 41, 40, 99, 111, 110, 115, 116, 32, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41, 59, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 105, 116,
101, 114, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 110, 100, 41,
40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 41, 10, 32, 32, 32, 32, 123, 32, 40, 118, 111,
105, 100, 41, 115, 101, 108, 102, 59, 32, 114, 101, 116, 117, 114,
110, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 95, 109, 95, 105,
116, 101, 114, 41, 123, 78, 85, 76, 76, 125, 59, 32, 125, 10, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 110, 101, 120, 116, 41, 40, 95, 109,
95, 105, 116, 101, 114, 42, 32, 105, 116, 41, 32, 123, 32, 10, 32, 32,
32, 32, 119, 104, 105, 108, 101, 32, 40, 40, 43, 43, 105, 116, 45, 62,
114, 101, 102, 44, 32, 40, 43, 43, 105, 116, 45, 62, 95, 115, 114,
101, 102, 41, 45, 62, 104, 97, 115, 104, 120, 32, 61, 61, 32, 48, 41,
41, 32, 59, 10, 32, 32, 32, 32, 105, 102, 32, 40, 105, 116, 45, 62,
114, 101, 102, 32, 61, 61, 32, 105, 116, 45, 62, 95, 101, 110, 100,
41, 32, 105, 116, 45, 62, 114, 101, 102, 32, 61, 32, 78, 85, 76, 76,
59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95,
109, 95, 105, 116, 101, 114, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
97, 100, 118, 97, 110, 99, 101, 41, 40, 95, 109, 95, 105, 116, 101,
114, 32, 105, 116, 44, 32, 115, 105, 122, 101, 95, 116, 32, 110, 41,
32, 123, 10, 32, 32, 32, 32, 119, 104, 105, 108, 101, 32, 40, 110, 45,
45, 32, 38, 38, 32, 105, 116, 46, 114, 101, 102, 41, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 110, 101, 120, 116, 41, 40, 38, 105, 116, 41,
59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 105, 116,
59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95,
109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95,
102, 105, 110, 100, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95, 116,
121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 107,
101, 121, 114, 97, 119, 32, 114, 107, 101, 121, 41, 32, 123, 10, 32,
32, 32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 98, 59, 10,
32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45, 62, 115,
105, 122, 101, 32, 38, 38, 32, 33, 40, 98, 32, 61, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95, 41, 40, 115, 101,
108, 102, 44, 32, 38, 114, 107, 101, 121, 41, 41, 46, 105, 110, 115,
101, 114, 116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40,
95, 109, 95, 105, 116, 101, 114, 41, 123, 98, 46, 114, 101, 102, 44,
32, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 115, 101, 108, 102, 45, 62, 116, 97, 98, 108, 101, 32, 43, 32,
115, 101, 108, 102, 45, 62, 98, 117, 99, 107, 101, 116, 95, 99, 111,
117, 110, 116, 44, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 115, 101, 108, 102, 45, 62, 115, 108, 111, 116,
32, 43, 32, 40, 98, 46, 114, 101, 102, 32, 45, 32, 115, 101, 108, 102,
45, 62, 116, 97, 98, 108, 101, 41, 125, 59, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101,
110, 100, 41, 40, 115, 101, 108, 102, 41, 59, 10, 125, 10, 10, 83, 84,
67, 95, 73, 78, 76, 73, 78, 69, 32, 99, 111, 110, 115, 116, 32, 95,
109, 95, 118, 97, 108, 117, 101, 42, 10, 95, 99, 95, 77, 69, 77, 66,
40, 95, 103, 101, 116, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95,
107, 101, 121, 114, 97, 119, 32, 114, 107, 101, 121, 41, 32, 123, 10,
32, 32, 32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 98, 59,
10, 32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45, 62, 115,
105, 122, 101, 32, 38, 38, 32, 33, 40, 98, 32, 61, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95, 41, 40, 115, 101,
108, 102, 44, 32, 38, 114, 107, 101, 121, 41, 41, 46, 105, 110, 115,
101, 114, 116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 98, 46, 114, 101, 102, 59, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 78, 85, 76, 76, 59, 10, 125,
10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118,
97, 108, 117, 101, 42, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 103,
101, 116, 95, 109, 117, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42,
32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 114, 97,
119, 32, 114, 107, 101, 121, 41, 10, 32, 32, 32, 32, 123, 32, 114,
101, 116, 117, 114, 110, 32, 40, 95, 109, 95, 118, 97, 108, 117, 101,
42, 41, 95, 99, 95, 77, 69, 77, 66, 40, 95, 103, 101, 116, 41, 40,
115, 101, 108, 102, 44, 32, 114, 107, 101, 121, 41, 59, 32, 125, 10,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 110, 116, 10, 95,
99, 95, 77, 69, 77, 66, 40, 95, 101, 114, 97, 115, 101, 41, 40, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109,
95, 107, 101, 121, 114, 97, 119, 32, 114, 107, 101, 121, 41, 32, 123,
10, 32, 32, 32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 98,
59, 10, 32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45, 62,
115, 105, 122, 101, 32, 38, 38, 32, 33, 40, 98, 32, 61, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95, 41, 40,
115, 101, 108, 102, 44, 32, 38, 114, 107, 101, 121, 41, 41, 46, 105,
110, 115, 101, 114, 116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32,
32, 123, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 114, 97, 115,
101, 95, 101, 110, 116, 114, 121, 41, 40, 115, 101, 108, 102, 44, 32,
98, 46, 114, 101, 102, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32,
49, 59, 32, 125, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32,
48, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
95, 109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40,
95, 101, 114, 97, 115, 101, 95, 97, 116, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 105, 116,
101, 114, 32, 105, 116, 41, 32, 123, 10, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 101, 114, 97, 115, 101, 95, 101, 110, 116,
114, 121, 41, 40, 115, 101, 108, 102, 44, 32, 105, 116, 46, 114, 101,
102, 41, 59, 10, 32, 32, 32, 32, 105, 102, 32, 40, 105, 116, 46, 95,
115, 114, 101, 102, 45, 62, 104, 97, 115, 104, 120, 32, 61, 61, 32,
48, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77,
66, 40, 95, 110, 101, 120, 116, 41, 40, 38, 105, 116, 41, 59, 10, 32,
32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 105, 116, 59, 10, 125,
10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 66, 111, 111,
108, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 113, 41, 40, 99,
111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101,
108, 102, 44, 32, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112,
101, 42, 32, 111, 116, 104, 101, 114, 41, 32, 123, 10, 32, 32, 32, 32,
105, 102, 32, 40, 95, 99, 95, 77, 69, 77, 66, 40, 95, 115, 105, 122,
101, 41, 40, 115, 101, 108, 102, 41, 32, 33, 61, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 115, 105, 122, 101, 41, 40, 111, 116, 104, 101,
114, 41, 41, 32, 114, 101, 116, 117, 114, 110, 32, 48, 59, 10, 32, 32,
32, 32, 102, 111, 114, 32, 40, 95, 109, 95, 105, 116, 101, 114, 32,
105, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 98, 101, 103,
105, 110, 41, 40, 115, 101, 108, 102, 41, 59, 32, 105, 46, 114, 101,
102, 59, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 110, 101, 120, 116,
41, 40, 38, 105, 41, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32,
99, 111, 110, 115, 116, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119,
32, 95, 114, 97, 119, 32, 61, 32, 105, 95, 107, 101, 121, 116, 111,
40, 95, 105, 95, 107, 101, 121, 114, 101, 102, 40, 105, 46, 114, 101,
102, 41, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40,
33, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 111, 110, 116, 97, 105,
110, 115, 41, 40, 111, 116, 104, 101, 114, 44, 32, 95, 114, 97, 119,
41, 41, 32, 114, 101, 116, 117, 114, 110, 32, 48, 59, 10, 32, 32, 32,
32, 125, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 49, 59,
10, 125, 10, 10, 47, 42, 32, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 32,
73, 77, 80, 76, 69, 77, 69, 78, 84, 65, 84, 73, 79, 78, 32, 45, 45,
45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
45, 45, 45, 45, 45, 45, 32, 42, 47, 10, 35, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 40, 105, 95, 105, 109, 112, 108, 101, 109,
101, 110, 116, 41, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101,
100, 40, 105, 95, 115, 116, 97, 116, 105, 99, 41, 10, 35, 105, 102,
110, 100, 101, 102, 32, 105, 95, 109, 97, 120, 95, 108, 111, 97, 100,
95, 102, 97, 99, 116, 111, 114, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 109, 97, 120, 95, 108, 111, 97, 100, 95, 102,
97, 99, 116, 111, 114, 32, 48, 46, 56, 48, 102, 10, 35, 101, 110, 100,
105, 102, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 97, 115, 116,
114, 97, 110, 103, 101, 95, 50, 40, 120, 44, 32, 110, 41, 32, 40, 105,
110, 116, 112, 116, 114, 95, 116, 41, 40, 40, 120, 41, 32, 38, 32, 40,
115, 105, 122, 101, 95, 116, 41, 40, 40, 110, 41, 32, 45, 32, 49, 41,
41, 32, 47, 47, 32, 110, 32, 112, 111, 119, 101, 114, 32, 111, 102,
32, 50, 46, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95, 105,
116, 101, 114, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 98, 101, 103,
105, 110, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112,
101, 42, 32, 115, 101, 108, 102, 41, 32, 123, 10, 32, 32, 32, 32, 95,
109, 95, 105, 116, 101, 114, 32, 105, 116, 32, 61, 32, 123, 115, 101,
108, 102, 45, 62, 116, 97, 98, 108, 101, 44, 32, 115, 101, 108, 102,
45, 62, 116, 97, 98, 108, 101, 43, 115, 101, 108, 102, 45, 62, 98,
117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 44, 32, 115, 101,
108, 102, 45, 62, 115, 108, 111, 116, 125, 59, 10, 32, 32, 32, 32,
105, 102, 32, 40, 105, 116, 46, 95, 115, 114, 101, 102, 41, 10, 32,
32, 32, 32, 32, 32, 32, 32, 119, 104, 105, 108, 101, 32, 40, 105, 116,
46, 95, 115, 114, 101, 102, 45, 62, 104, 97, 115, 104, 120, 32, 61,
61, 32, 48, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
43, 43, 105, 116, 46, 114, 101, 102, 44, 32, 43, 43, 105, 116, 46, 95,
115, 114, 101, 102, 59, 10, 32, 32, 32, 32, 105, 102, 32, 40, 105,
116, 46, 114, 101, 102, 32, 61, 61, 32, 105, 116, 46, 95, 101, 110,
100, 41, 32, 105, 116, 46, 114, 101, 102, 32, 61, 32, 78, 85, 76, 76,
59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 105, 116,
59, 10, 125, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 102, 108, 111,
97, 116, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 109, 97, 120, 95,
108, 111, 97, 100, 95, 102, 97, 99, 116, 111, 114, 41, 40, 99, 111,
110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 41, 32, 123, 10, 32, 32, 32, 32, 40, 118, 111, 105, 100, 41, 115,
101, 108, 102, 59, 32, 114, 101, 116, 117, 114, 110, 32, 40, 102, 108,
111, 97, 116, 41, 40, 105, 95, 109, 97, 120, 95, 108, 111, 97, 100,
95, 102, 97, 99, 116, 111, 114, 41, 59, 10, 125, 10, 10, 83, 84, 67,
95, 68, 69, 70, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 99, 97, 112, 97, 99, 105, 116, 121, 41,
40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32,
109, 97, 112, 41, 32, 123, 10, 32, 32, 32, 32, 114, 101, 116, 117,
114, 110, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116, 41, 40, 40,
102, 108, 111, 97, 116, 41, 109, 97, 112, 45, 62, 98, 117, 99, 107,
101, 116, 95, 99, 111, 117, 110, 116, 32, 42, 32, 40, 105, 95, 109,
97, 120, 95, 108, 111, 97, 100, 95, 102, 97, 99, 116, 111, 114, 41,
41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 105, 95, 116,
121, 112, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 119, 105, 116,
104, 95, 99, 97, 112, 97, 99, 105, 116, 121, 41, 40, 99, 111, 110,
115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 99, 97, 112,
41, 32, 123, 10, 32, 32, 32, 32, 105, 95, 116, 121, 112, 101, 32, 109,
97, 112, 32, 61, 32, 123, 48, 125, 59, 10, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 114, 101, 115, 101, 114, 118, 101, 41, 40, 38,
109, 97, 112, 44, 32, 99, 97, 112, 41, 59, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 109, 97, 112, 59, 10, 125, 10, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 119, 105, 112, 101, 95, 41, 40, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41, 32, 123, 10,
32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45, 62, 115,
105, 122, 101, 32, 61, 61, 32, 48, 41, 10, 32, 32, 32, 32, 32, 32, 32,
32, 114, 101, 116, 117, 114, 110, 59, 10, 32, 32, 32, 32, 95, 109, 95,
118, 97, 108, 117, 101, 42, 32, 100, 32, 61, 32, 115, 101, 108, 102,
45, 62, 116, 97, 98, 108, 101, 44, 32, 42, 95, 101, 110, 100, 32, 61,
32, 100, 32, 43, 32, 115, 101, 108, 102, 45, 62, 98, 117, 99, 107,
101, 116, 95, 99, 111, 117, 110, 116, 59, 10, 32, 32, 32, 32, 115,
116, 114, 117, 99, 116, 32, 99, 104, 97, 115, 104, 95, 115, 108, 111,
116, 42, 32, 115, 32, 61, 32, 115, 101, 108, 102, 45, 62, 115, 108,
111, 116, 59, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 59, 32, 100,
32, 33, 61, 32, 95, 101, 110, 100, 59, 32, 43, 43, 100, 41, 10, 32,
32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 40, 115, 43, 43, 41, 45,
62, 104, 97, 115, 104, 120, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117,
101, 95, 100, 114, 111, 112, 41, 40, 100, 41, 59, 10, 125, 10, 10, 83,
84, 67, 95, 68, 69, 70, 32, 118, 111, 105, 100, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 100, 114, 111, 112, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 41, 32, 123, 10, 32, 32, 32, 32,
105, 102, 32, 40, 115, 101, 108, 102, 45, 62, 98, 117, 99, 107, 101,
116, 95, 99, 111, 117, 110, 116, 32, 62, 32, 48, 41, 32, 123, 10, 32,
32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 119,
105, 112, 101, 95, 41, 40, 115, 101, 108, 102, 41, 59, 10, 32, 32, 32,
32, 32, 32, 32, 32, 105, 95, 102, 114, 101, 101, 40, 115, 101, 108,
102, 45, 62, 115, 108, 111, 116, 44, 32, 40, 115, 101, 108, 102, 45,
62, 98, 117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 32, 43,
32, 49, 41, 42, 99, 95, 115, 105, 122, 101, 111, 102, 32, 42, 115,
101, 108, 102, 45, 62, 115, 108, 111, 116, 41, 59, 10, 32, 32, 32, 32,
32, 32, 32, 32, 105, 95, 102, 114, 101, 101, 40, 115, 101, 108, 102,
45, 62, 116, 97, 98, 108, 101, 44, 32, 115, 101, 108, 102, 45, 62, 98,
117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 42, 99, 95, 115,
105, 122, 101, 111, 102, 32, 42, 115, 101, 108, 102, 45, 62, 116, 97,
98, 108, 101, 41, 59, 10, 32, 32, 32, 32, 125, 10, 125, 10, 10, 83,
84, 67, 95, 68, 69, 70, 32, 118, 111, 105, 100, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 99, 108, 101, 97, 114, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 41, 32, 123, 10, 32, 32, 32, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 119, 105, 112, 101, 95, 41, 40,
115, 101, 108, 102, 41, 59, 10, 32, 32, 32, 32, 115, 101, 108, 102,
45, 62, 115, 105, 122, 101, 32, 61, 32, 48, 59, 10, 32, 32, 32, 32,
99, 95, 109, 101, 109, 115, 101, 116, 40, 115, 101, 108, 102, 45, 62,
115, 108, 111, 116, 44, 32, 48, 44, 32, 99, 95, 115, 105, 122, 101,
111, 102, 40, 115, 116, 114, 117, 99, 116, 32, 99, 104, 97, 115, 104,
95, 115, 108, 111, 116, 41, 42, 115, 101, 108, 102, 45, 62, 98, 117,
99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 41, 59, 10, 125, 10,
10, 35, 105, 102, 100, 101, 102, 32, 95, 105, 95, 105, 115, 109, 97,
112, 10, 32, 32, 32, 32, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95,
114, 101, 115, 117, 108, 116, 10, 32, 32, 32, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 105, 110, 115, 101, 114, 116, 95, 111, 114, 95, 97,
115, 115, 105, 103, 110, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 32, 95, 107,
101, 121, 44, 32, 95, 109, 95, 109, 97, 112, 112, 101, 100, 32, 95,
109, 97, 112, 112, 101, 100, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32,
32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 95, 114, 101,
115, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115,
101, 114, 116, 95, 101, 110, 116, 114, 121, 95, 41, 40, 115, 101, 108,
102, 44, 32, 105, 95, 107, 101, 121, 116, 111, 40, 40, 38, 95, 107,
101, 121, 41, 41, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 109,
95, 109, 97, 112, 112, 101, 100, 42, 32, 95, 109, 112, 32, 61, 32, 95,
114, 101, 115, 46, 114, 101, 102, 32, 63, 32, 38, 95, 114, 101, 115,
46, 114, 101, 102, 45, 62, 115, 101, 99, 111, 110, 100, 32, 58, 32,
38, 95, 109, 97, 112, 112, 101, 100, 59, 10, 32, 32, 32, 32, 32, 32,
32, 32, 105, 102, 32, 40, 95, 114, 101, 115, 46, 105, 110, 115, 101,
114, 116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 95, 114, 101, 115, 46, 114, 101, 102, 45, 62, 102, 105, 114,
115, 116, 32, 61, 32, 95, 107, 101, 121, 59, 10, 32, 32, 32, 32, 32,
32, 32, 32, 101, 108, 115, 101, 32, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 123, 32, 105, 95, 107, 101, 121, 100, 114, 111,
112, 40, 40, 38, 95, 107, 101, 121, 41, 41, 59, 32, 105, 95, 118, 97,
108, 100, 114, 111, 112, 40, 95, 109, 112, 41, 59, 32, 125, 10, 32,
32, 32, 32, 32, 32, 32, 32, 42, 95, 109, 112, 32, 61, 32, 95, 109, 97,
112, 112, 101, 100, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 114, 101,
116, 117, 114, 110, 32, 95, 114, 101, 115, 59, 10, 32, 32, 32, 32,
125, 10, 10, 32, 32, 32, 32, 35, 105, 102, 32, 33, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99,
101, 10, 32, 32, 32, 32, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95,
114, 101, 115, 117, 108, 116, 10, 32, 32, 32, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 101, 109, 112, 108, 97, 99, 101, 95, 111, 114, 95, 97,
115, 115, 105, 103, 110, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 44, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119,
32, 114, 107, 101, 121, 44, 32, 95, 109, 95, 114, 109, 97, 112, 112,
101, 100, 32, 114, 109, 97, 112, 112, 101, 100, 41, 32, 123, 10, 32,
32, 32, 32, 32, 32, 32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116,
32, 95, 114, 101, 115, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
105, 110, 115, 101, 114, 116, 95, 101, 110, 116, 114, 121, 95, 41, 40,
115, 101, 108, 102, 44, 32, 114, 107, 101, 121, 41, 59, 10, 32, 32,
32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 95, 114, 101, 115, 46, 105,
110, 115, 101, 114, 116, 101, 100, 41, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 95, 114, 101, 115, 46, 114, 101, 102, 45, 62, 102,
105, 114, 115, 116, 32, 61, 32, 105, 95, 107, 101, 121, 102, 114, 111,
109, 40, 114, 107, 101, 121, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32,
32, 101, 108, 115, 101, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 105, 102, 32, 40, 33, 95, 114, 101, 115, 46, 114, 101,
102, 41, 32, 114, 101, 116, 117, 114, 110, 32, 95, 114, 101, 115, 59,
10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 95, 118, 97,
108, 100, 114, 111, 112, 40, 40, 38, 95, 114, 101, 115, 46, 114, 101,
102, 45, 62, 115, 101, 99, 111, 110, 100, 41, 41, 59, 10, 32, 32, 32,
32, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 114,
101, 115, 46, 114, 101, 102, 45, 62, 115, 101, 99, 111, 110, 100, 32,
61, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 40, 114, 109, 97,
112, 112, 101, 100, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 95, 114, 101, 115, 59, 10, 32, 32, 32,
32, 125, 10, 32, 32, 32, 32, 35, 101, 110, 100, 105, 102, 32, 47, 47,
32, 33, 105, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 10,
35, 101, 110, 100, 105, 102, 32, 47, 47, 32, 95, 105, 95, 105, 115,
109, 97, 112, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95,
114, 101, 115, 117, 108, 116, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95,
98, 117, 99, 107, 101, 116, 95, 41, 40, 99, 111, 110, 115, 116, 32,
105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99,
111, 110, 115, 116, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 42,
32, 114, 107, 101, 121, 112, 116, 114, 41, 32, 123, 10, 32, 32, 32,
32, 99, 111, 110, 115, 116, 32, 117, 105, 110, 116, 54, 52, 95, 116,
32, 95, 104, 97, 115, 104, 32, 61, 32, 105, 95, 104, 97, 115, 104, 40,
114, 107, 101, 121, 112, 116, 114, 41, 59, 10, 32, 32, 32, 32, 105,
110, 116, 112, 116, 114, 95, 116, 32, 95, 99, 97, 112, 32, 61, 32,
115, 101, 108, 102, 45, 62, 98, 117, 99, 107, 101, 116, 95, 99, 111,
117, 110, 116, 59, 10, 32, 32, 32, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 95, 105, 100, 120, 32, 61, 32, 102, 97, 115, 116, 114,
97, 110, 103, 101, 95, 50, 40, 95, 104, 97, 115, 104, 44, 32, 95, 99,
97, 112, 41, 59, 10, 32, 32, 32, 32, 95, 109, 95, 114, 101, 115, 117,
108, 116, 32, 98, 32, 61, 32, 123, 78, 85, 76, 76, 44, 32, 49, 44, 32,
40, 117, 105, 110, 116, 56, 95, 116, 41, 40, 95, 104, 97, 115, 104,
32, 124, 32, 48, 120, 56, 48, 41, 125, 59, 10, 32, 32, 32, 32, 99,
111, 110, 115, 116, 32, 115, 116, 114, 117, 99, 116, 32, 99, 104, 97,
115, 104, 95, 115, 108, 111, 116, 42, 32, 115, 32, 61, 32, 115, 101,
108, 102, 45, 62, 115, 108, 111, 116, 59, 10, 32, 32, 32, 32, 119,
104, 105, 108, 101, 32, 40, 115, 91, 95, 105, 100, 120, 93, 46, 104,
97, 115, 104, 120, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 102, 32, 40, 115, 91, 95, 105, 100, 120, 93, 46, 104, 97, 115,
104, 120, 32, 61, 61, 32, 98, 46, 104, 97, 115, 104, 120, 41, 32, 123,
10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111, 110, 115,
116, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 32, 95, 114, 97,
119, 32, 61, 32, 105, 95, 107, 101, 121, 116, 111, 40, 95, 105, 95,
107, 101, 121, 114, 101, 102, 40, 115, 101, 108, 102, 45, 62, 116, 97,
98, 108, 101, 32, 43, 32, 95, 105, 100, 120, 41, 41, 59, 10, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 105, 95,
101, 113, 40, 40, 38, 95, 114, 97, 119, 41, 44, 32, 114, 107, 101,
121, 112, 116, 114, 41, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 98, 46, 105, 110, 115, 101, 114,
116, 101, 100, 32, 61, 32, 48, 59, 32, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 98, 114, 101, 97, 107, 59, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32,
32, 32, 32, 32, 125, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32,
40, 43, 43, 95, 105, 100, 120, 32, 61, 61, 32, 95, 99, 97, 112, 41,
32, 95, 105, 100, 120, 32, 61, 32, 48, 59, 10, 32, 32, 32, 32, 125,
10, 32, 32, 32, 32, 98, 46, 114, 101, 102, 32, 61, 32, 115, 101, 108,
102, 45, 62, 116, 97, 98, 108, 101, 32, 43, 32, 95, 105, 100, 120, 59,
10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 98, 59, 10, 125,
10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95, 114, 101, 115,
117, 108, 116, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115,
101, 114, 116, 95, 101, 110, 116, 114, 121, 95, 41, 40, 105, 95, 116,
121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 107,
101, 121, 114, 97, 119, 32, 114, 107, 101, 121, 41, 32, 123, 10, 32,
32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45, 62, 115, 105,
122, 101, 32, 62, 61, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116,
41, 40, 40, 102, 108, 111, 97, 116, 41, 115, 101, 108, 102, 45, 62,
98, 117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 32, 42, 32,
40, 105, 95, 109, 97, 120, 95, 108, 111, 97, 100, 95, 102, 97, 99,
116, 111, 114, 41, 41, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105,
102, 32, 40, 33, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115,
101, 114, 118, 101, 41, 40, 115, 101, 108, 102, 44, 32, 40, 105, 110,
116, 112, 116, 114, 95, 116, 41, 40, 115, 101, 108, 102, 45, 62, 115,
105, 122, 101, 42, 51, 47, 50, 32, 43, 32, 50, 41, 41, 41, 10, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110,
32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 95, 109, 95, 114, 101,
115, 117, 108, 116, 41, 123, 78, 85, 76, 76, 125, 59, 10, 10, 32, 32,
32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 98, 32, 61, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95,
41, 40, 115, 101, 108, 102, 44, 32, 38, 114, 107, 101, 121, 41, 59,
10, 32, 32, 32, 32, 105, 102, 32, 40, 98, 46, 105, 110, 115, 101, 114,
116, 101, 100, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32, 115,
101, 108, 102, 45, 62, 115, 108, 111, 116, 91, 98, 46, 114, 101, 102,
32, 45, 32, 115, 101, 108, 102, 45, 62, 116, 97, 98, 108, 101, 93, 46,
104, 97, 115, 104, 120, 32, 61, 32, 98, 46, 104, 97, 115, 104, 120,
59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 43, 43, 115, 101, 108, 102,
45, 62, 115, 105, 122, 101, 59, 10, 32, 32, 32, 32, 125, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 98, 59, 10, 125, 10, 10, 35,
105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110,
111, 95, 99, 108, 111, 110, 101, 10, 83, 84, 67, 95, 68, 69, 70, 32,
105, 95, 116, 121, 112, 101, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95,
99, 108, 111, 110, 101, 41, 40, 105, 95, 116, 121, 112, 101, 32, 109,
41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 109, 46, 98, 117,
99, 107, 101, 116, 95, 99, 111, 117, 110, 116, 41, 32, 123, 10, 32,
32, 32, 32, 32, 32, 32, 32, 95, 109, 95, 118, 97, 108, 117, 101, 32,
42, 100, 32, 61, 32, 40, 95, 109, 95, 118, 97, 108, 117, 101, 32, 42,
41, 105, 95, 109, 97, 108, 108, 111, 99, 40, 109, 46, 98, 117, 99,
107, 101, 116, 95, 99, 111, 117, 110, 116, 42, 99, 95, 115, 105, 122,
101, 111, 102, 32, 42, 100, 41, 44, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 42, 95, 100, 115, 116, 32, 61,
32, 100, 44, 32, 42, 95, 101, 110, 100, 32, 61, 32, 109, 46, 116, 97,
98, 108, 101, 32, 43, 32, 109, 46, 98, 117, 99, 107, 101, 116, 95, 99,
111, 117, 110, 116, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111,
110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 95, 115,
98, 121, 116, 101, 115, 32, 61, 32, 40, 109, 46, 98, 117, 99, 107,
101, 116, 95, 99, 111, 117, 110, 116, 32, 43, 32, 49, 41, 42, 99, 95,
115, 105, 122, 101, 111, 102, 32, 42, 109, 46, 115, 108, 111, 116, 59,
10, 32, 32, 32, 32, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32,
99, 104, 97, 115, 104, 95, 115, 108, 111, 116, 32, 42, 115, 32, 61,
32, 40, 115, 116, 114, 117, 99, 116, 32, 99, 104, 97, 115, 104, 95,
115, 108, 111, 116, 32, 42, 41, 99, 95, 109, 101, 109, 99, 112, 121,
40, 105, 95, 109, 97, 108, 108, 111, 99, 40, 95, 115, 98, 121, 116,
101, 115, 41, 44, 32, 109, 46, 115, 108, 111, 116, 44, 32, 95, 115,
98, 121, 116, 101, 115, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 102, 32, 40, 33, 40, 100, 32, 38, 38, 32, 115, 41, 41, 32, 123,
10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 95, 102, 114,
101, 101, 40, 100, 44, 32, 109, 46, 98, 117, 99, 107, 101, 116, 95,
99, 111, 117, 110, 116, 42, 99, 95, 115, 105, 122, 101, 111, 102, 32,
42, 100, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
105, 102, 32, 40, 115, 41, 32, 105, 95, 102, 114, 101, 101, 40, 115,
44, 32, 95, 115, 98, 121, 116, 101, 115, 41, 59, 10, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 100, 32, 61, 32, 48, 44, 32, 115, 32,
61, 32, 48, 44, 32, 109, 46, 98, 117, 99, 107, 101, 116, 95, 99, 111,
117, 110, 116, 32, 61, 32, 48, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32,
125, 32, 101, 108, 115, 101, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 102, 111, 114, 32, 40, 59, 32, 109, 46, 116, 97, 98, 108,
101, 32, 33, 61, 32, 95, 101, 110, 100, 59, 32, 43, 43, 109, 46, 116,
97, 98, 108, 101, 44, 32, 43, 43, 109, 46, 115, 108, 111, 116, 44, 32,
43, 43, 95, 100, 115, 116, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 109, 46, 115, 108, 111,
116, 45, 62, 104, 97, 115, 104, 120, 41, 10, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 42, 95, 100,
115, 116, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97,
108, 117, 101, 95, 99, 108, 111, 110, 101, 41, 40, 42, 109, 46, 116,
97, 98, 108, 101, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 109, 46,
116, 97, 98, 108, 101, 32, 61, 32, 100, 44, 32, 109, 46, 115, 108,
111, 116, 32, 61, 32, 115, 59, 10, 32, 32, 32, 32, 125, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 109, 59, 10, 125, 10, 35,
101, 110, 100, 105, 102, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95,
66, 111, 111, 108, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101,
115, 101, 114, 118, 101, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 44, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 95, 110, 101, 119, 99, 97, 112, 41, 32,
123, 10, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 95, 111, 108, 100, 98, 117, 99, 107, 115,
32, 61, 32, 115, 101, 108, 102, 45, 62, 98, 117, 99, 107, 101, 116,
95, 99, 111, 117, 110, 116, 59, 10, 32, 32, 32, 32, 105, 102, 32, 40,
95, 110, 101, 119, 99, 97, 112, 32, 33, 61, 32, 115, 101, 108, 102,
45, 62, 115, 105, 122, 101, 32, 38, 38, 32, 95, 110, 101, 119, 99, 97,
112, 32, 60, 61, 32, 95, 111, 108, 100, 98, 117, 99, 107, 115, 41, 10,
32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 49,
59, 10, 32, 32, 32, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 95,
110, 101, 119, 98, 117, 99, 107, 115, 32, 61, 32, 40, 105, 110, 116,
112, 116, 114, 95, 116, 41, 40, 40, 102, 108, 111, 97, 116, 41, 95,
110, 101, 119, 99, 97, 112, 32, 47, 32, 40, 105, 95, 109, 97, 120, 95,
108, 111, 97, 100, 95, 102, 97, 99, 116, 111, 114, 41, 41, 32, 43, 32,
52, 59, 10, 32, 32, 32, 32, 95, 110, 101, 119, 98, 117, 99, 107, 115,
32, 61, 32, 115, 116, 99, 95, 110, 101, 120, 116, 112, 111, 119, 50,
40, 95, 110, 101, 119, 98, 117, 99, 107, 115, 41, 59, 10, 32, 32, 32,
32, 105, 95, 116, 121, 112, 101, 32, 109, 32, 61, 32, 123, 10, 32, 32,
32, 32, 32, 32, 32, 32, 40, 95, 109, 95, 118, 97, 108, 117, 101, 32,
42, 41, 105, 95, 109, 97, 108, 108, 111, 99, 40, 95, 110, 101, 119,
98, 117, 99, 107, 115, 42, 99, 95, 115, 105, 122, 101, 111, 102, 40,
95, 109, 95, 118, 97, 108, 117, 101, 41, 41, 44, 10, 32, 32, 32, 32,
32, 32, 32, 32, 40, 115, 116, 114, 117, 99, 116, 32, 99, 104, 97, 115,
104, 95, 115, 108, 111, 116, 32, 42, 41, 105, 95, 99, 97, 108, 108,
111, 99, 40, 95, 110, 101, 119, 98, 117, 99, 107, 115, 32, 43, 32, 49,
44, 32, 99, 95, 115, 105, 122, 101, 111, 102, 40, 115, 116, 114, 117,
99, 116, 32, 99, 104, 97, 115, 104, 95, 115, 108, 111, 116, 41, 41,
44, 10, 32, 32, 32, 32, 32, 32, 32, 32, 115, 101, 108, 102, 45, 62,
115, 105, 122, 101, 44, 32, 95, 110, 101, 119, 98, 117, 99, 107, 115,
10, 32, 32, 32, 32, 125, 59, 10, 32, 32, 32, 32, 95, 66, 111, 111,
108, 32, 111, 107, 32, 61, 32, 109, 46, 116, 97, 98, 108, 101, 32, 38,
38, 32, 109, 46, 115, 108, 111, 116, 59, 10, 32, 32, 32, 32, 105, 102,
32, 40, 111, 107, 41, 32, 123, 32, 32, 47, 47, 32, 82, 101, 104, 97,
115, 104, 58, 10, 32, 32, 32, 32, 32, 32, 32, 32, 109, 46, 115, 108,
111, 116, 91, 95, 110, 101, 119, 98, 117, 99, 107, 115, 93, 46, 104,
97, 115, 104, 120, 32, 61, 32, 48, 120, 102, 102, 59, 10, 32, 32, 32,
32, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 95, 109, 95, 118, 97,
108, 117, 101, 42, 32, 100, 32, 61, 32, 115, 101, 108, 102, 45, 62,
116, 97, 98, 108, 101, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99,
111, 110, 115, 116, 32, 115, 116, 114, 117, 99, 116, 32, 99, 104, 97,
115, 104, 95, 115, 108, 111, 116, 42, 32, 115, 32, 61, 32, 115, 101,
108, 102, 45, 62, 115, 108, 111, 116, 59, 10, 32, 32, 32, 32, 32, 32,
32, 32, 102, 111, 114, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116,
32, 105, 32, 61, 32, 48, 59, 32, 105, 32, 60, 32, 95, 111, 108, 100,
98, 117, 99, 107, 115, 59, 32, 43, 43, 105, 44, 32, 43, 43, 100, 41,
32, 105, 102, 32, 40, 40, 115, 43, 43, 41, 45, 62, 104, 97, 115, 104,
120, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
95, 109, 95, 107, 101, 121, 114, 97, 119, 32, 114, 32, 61, 32, 105,
95, 107, 101, 121, 116, 111, 40, 95, 105, 95, 107, 101, 121, 114, 101,
102, 40, 100, 41, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 98, 32, 61, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 98, 117, 99, 107, 101, 116, 95,
41, 40, 38, 109, 44, 32, 38, 114, 41, 59, 10, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 109, 46, 115, 108, 111, 116, 91, 98, 46, 114,
101, 102, 32, 45, 32, 109, 46, 116, 97, 98, 108, 101, 93, 46, 104, 97,
115, 104, 120, 32, 61, 32, 98, 46, 104, 97, 115, 104, 120, 59, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 42, 98, 46, 114, 101, 102,
32, 61, 32, 42, 100, 59, 32, 47, 47, 32, 109, 111, 118, 101, 10, 32,
32, 32, 32, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32, 32, 32, 32, 32,
99, 95, 115, 119, 97, 112, 40, 105, 95, 116, 121, 112, 101, 44, 32,
115, 101, 108, 102, 44, 32, 38, 109, 41, 59, 10, 32, 32, 32, 32, 125,
10, 32, 32, 32, 32, 105, 95, 102, 114, 101, 101, 40, 109, 46, 115,
108, 111, 116, 44, 32, 40, 109, 46, 98, 117, 99, 107, 101, 116, 95,
99, 111, 117, 110, 116, 32, 43, 32, 40, 105, 110, 116, 41, 40, 109,
46, 115, 108, 111, 116, 32, 33, 61, 32, 78, 85, 76, 76, 41, 41, 42,
99, 95, 115, 105, 122, 101, 111, 102, 32, 42, 109, 46, 115, 108, 111,
116, 41, 59, 10, 32, 32, 32, 32, 105, 95, 102, 114, 101, 101, 40, 109,
46, 116, 97, 98, 108, 101, 44, 32, 109, 46, 98, 117, 99, 107, 101,
116, 95, 99, 111, 117, 110, 116, 42, 99, 95, 115, 105, 122, 101, 111,
102, 32, 42, 109, 46, 116, 97, 98, 108, 101, 41, 59, 10, 32, 32, 32,
32, 114, 101, 116, 117, 114, 110, 32, 111, 107, 59, 10, 125, 10, 10,
83, 84, 67, 95, 68, 69, 70, 32, 118, 111, 105, 100, 10, 95, 99, 95,
77, 69, 77, 66, 40, 95, 101, 114, 97, 115, 101, 95, 101, 110, 116,
114, 121, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 44, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 95, 118, 97,
108, 41, 32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 118, 97, 108, 117,
101, 42, 32, 100, 32, 61, 32, 115, 101, 108, 102, 45, 62, 116, 97, 98,
108, 101, 59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 99,
104, 97, 115, 104, 95, 115, 108, 111, 116, 42, 32, 115, 32, 61, 32,
115, 101, 108, 102, 45, 62, 115, 108, 111, 116, 59, 10, 32, 32, 32,
32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 32, 61, 32, 95,
118, 97, 108, 32, 45, 32, 100, 44, 32, 106, 32, 61, 32, 105, 44, 32,
107, 59, 10, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 105, 110,
116, 112, 116, 114, 95, 116, 32, 95, 99, 97, 112, 32, 61, 32, 115,
101, 108, 102, 45, 62, 98, 117, 99, 107, 101, 116, 95, 99, 111, 117,
110, 116, 59, 10, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
118, 97, 108, 117, 101, 95, 100, 114, 111, 112, 41, 40, 95, 118, 97,
108, 41, 59, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 59, 59, 41,
32, 123, 32, 47, 47, 32, 100, 101, 108, 101, 116, 101, 32, 119, 105,
116, 104, 111, 117, 116, 32, 108, 101, 97, 118, 105, 110, 103, 32,
116, 111, 109, 98, 115, 116, 111, 110, 101, 10, 32, 32, 32, 32, 32,
32, 32, 32, 105, 102, 32, 40, 43, 43, 106, 32, 61, 61, 32, 95, 99, 97,
112, 41, 32, 106, 32, 61, 32, 48, 59, 10, 32, 32, 32, 32, 32, 32, 32,
32, 105, 102, 32, 40, 33, 32, 115, 91, 106, 93, 46, 104, 97, 115, 104,
120, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 98, 114,
101, 97, 107, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111, 110,
115, 116, 32, 95, 109, 95, 107, 101, 121, 114, 97, 119, 32, 95, 114,
97, 119, 32, 61, 32, 105, 95, 107, 101, 121, 116, 111, 40, 95, 105,
95, 107, 101, 121, 114, 101, 102, 40, 100, 32, 43, 32, 106, 41, 41,
59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 107, 32, 61, 32, 102, 97, 115,
116, 114, 97, 110, 103, 101, 95, 50, 40, 105, 95, 104, 97, 115, 104,
40, 40, 38, 95, 114, 97, 119, 41, 41, 44, 32, 95, 99, 97, 112, 41, 59,
10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 40, 106, 32, 60,
32, 105, 41, 32, 94, 32, 40, 107, 32, 60, 61, 32, 105, 41, 32, 94, 32,
40, 107, 32, 62, 32, 106, 41, 41, 32, 123, 32, 47, 47, 32, 105, 115,
32, 107, 32, 111, 117, 116, 115, 105, 100, 101, 32, 40, 105, 44, 32,
106, 93, 63, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 100,
91, 105, 93, 32, 61, 32, 100, 91, 106, 93, 59, 10, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 115, 91, 105, 93, 32, 61, 32, 115, 91,
106, 93, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 105,
32, 61, 32, 106, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 125, 10, 32,
32, 32, 32, 125, 10, 32, 32, 32, 32, 115, 91, 105, 93, 46, 104, 97,
115, 104, 120, 32, 61, 32, 48, 59, 10, 32, 32, 32, 32, 45, 45, 115,
101, 108, 102, 45, 62, 115, 105, 122, 101, 59, 10, 125, 10, 35, 101,
110, 100, 105, 102, 32, 47, 47, 32, 105, 95, 105, 109, 112, 108, 101,
109, 101, 110, 116, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 109,
97, 120, 95, 108, 111, 97, 100, 95, 102, 97, 99, 116, 111, 114, 10,
35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 105, 115, 115, 101, 116,
10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 105, 115, 109, 97,
112, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 105, 115, 104,
97, 115, 104, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 107,
101, 121, 114, 101, 102, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105,
95, 77, 65, 80, 95, 79, 78, 76, 89, 10, 35, 117, 110, 100, 101, 102,
32, 95, 105, 95, 83, 69, 84, 95, 79, 78, 76, 89, 10, 35, 100, 101,
102, 105, 110, 101, 32, 67, 77, 65, 80, 95, 72, 95, 73, 78, 67, 76,
85, 68, 69, 68, 10, 47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73, 78,
95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 116, 101,
109, 112, 108, 97, 116, 101, 50, 46, 104, 10, 35, 105, 102, 100, 101,
102, 32, 105, 95, 109, 111, 114, 101, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 109, 111, 114, 101, 10, 35, 101, 108, 115, 101, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 116, 121, 112, 101, 10, 35, 117,
110, 100, 101, 102, 32, 105, 95, 116, 97, 103, 10, 35, 117, 110, 100,
101, 102, 32, 105, 95, 105, 109, 112, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 111, 112, 116, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 108, 101, 115, 115, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95,
99, 109, 112, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 101, 113,
10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 104, 97, 115, 104, 10,
35, 117, 110, 100, 101, 102, 32, 105, 95, 99, 97, 112, 97, 99, 105,
116, 121, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 114, 97, 119,
95, 99, 108, 97, 115, 115, 10, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 118, 97, 108, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95,
118, 97, 108, 95, 115, 116, 114, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 118, 97, 108, 95, 115, 115, 118, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108, 95, 99, 108, 97,
115, 115, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108,
114, 97, 119, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 118, 97,
108, 99, 108, 111, 110, 101, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 118, 97, 108, 102, 114, 111, 109, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 116, 111, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 100, 114, 111, 112, 10, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 107, 101, 121, 95, 115, 116, 114, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 95, 115, 115, 118, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 95, 97, 114, 99,
98, 111, 120, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 107, 101,
121, 95, 99, 108, 97, 115, 115, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 107, 101, 121, 114, 97, 119, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 102, 114, 111, 109, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 116, 111, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 100, 114, 111,
112, 10, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 117, 115, 101,
95, 99, 109, 112, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 110,
111, 95, 104, 97, 115, 104, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 110, 111, 95, 99, 108, 111, 110, 101, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 10,
35, 117, 110, 100, 101, 102, 32, 105, 95, 105, 115, 95, 102, 111, 114,
119, 97, 114, 100, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 104,
97, 115, 95, 101, 109, 112, 108, 97, 99, 101, 10, 10, 35, 117, 110,
100, 101, 102, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10,
35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 104, 97, 115, 95, 101,
113, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 112, 114, 101,
102, 105, 120, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 116,
101, 109, 112, 108, 97, 116, 101, 10, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 107, 101, 121, 99, 108, 97, 115, 115, 32, 47, 47, 32, 91,
100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 118, 97, 108, 99, 108, 97, 115, 115, 32,
47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93,
10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 114, 97, 119, 99, 108,
97, 115, 115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97,
116, 101, 100, 93, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 107,
101, 121, 98, 111, 120, 101, 100, 32, 47, 47, 32, 91, 100, 101, 112,
114, 101, 99, 97, 116, 101, 100, 93, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 98, 111, 120, 101, 100, 32, 47, 47, 32, 91,
100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93, 10, 35, 101, 110,
100, 105, 102, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73,
76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 116, 101, 109, 112,
108, 97, 116, 101, 50, 46, 104, 10, 47, 47, 32, 35, 35, 35, 32, 66,
69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69,
58, 32, 108, 105, 110, 107, 97, 103, 101, 50, 46, 104, 10, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 97, 108, 108, 111, 99, 97, 116,
111, 114, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 109, 97, 108,
108, 111, 99, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 99, 97,
108, 108, 111, 99, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 114,
101, 97, 108, 108, 111, 99, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 102, 114, 101, 101, 10, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 115, 116, 97, 116, 105, 99, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 104, 101, 97, 100, 101, 114, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 105, 109, 112, 108, 101, 109, 101, 110, 116, 10, 35, 117,
110, 100, 101, 102, 32, 105, 95, 105, 109, 112, 111, 114, 116, 10, 10,
35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 99,
108, 97, 110, 103, 95, 95, 32, 38, 38, 32, 33, 100, 101, 102, 105,
110, 101, 100, 32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117, 115,
10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103,
32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 112, 111, 112,
10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
95, 95, 71, 78, 85, 67, 95, 95, 32, 38, 38, 32, 33, 100, 101, 102,
105, 110, 101, 100, 32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117,
115, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 112, 111, 112, 10,
35, 101, 110, 100, 105, 102, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78,
68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 108,
105, 110, 107, 97, 103, 101, 50, 46, 104, 10, 47, 47, 32, 35, 35, 35,
32, 69, 78, 68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69,
58, 32, 99, 109, 97, 112, 46, 104, 10, 10, 0, 47, 47, 32, 66, 69, 71,
73, 78, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110, 116, 97, 105,
110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46, 112, 121, 10,
35, 105, 102, 110, 100, 101, 102, 32, 95, 95, 100, 121, 105, 98, 105,
99, 99, 95, 105, 110, 116, 101, 114, 110, 97, 108, 95, 105, 110, 99,
108, 117, 100, 101, 95, 95, 10, 35, 101, 114, 114, 111, 114, 32, 67,
97, 110, 32, 111, 110, 108, 121, 32, 98, 101, 32, 105, 110, 99, 108,
117, 100, 101, 100, 32, 98, 121, 32, 116, 104, 101, 32, 99, 111, 109,
112, 105, 108, 101, 114, 44, 32, 111, 114, 32, 99, 111, 110, 102, 117,
115, 105, 110, 103, 32, 101, 114, 114, 111, 114, 115, 32, 119, 105,
108, 108, 32, 114, 101, 115, 117, 108, 116, 33, 10, 35, 101, 110, 100,
105, 102, 10, 35, 117, 110, 100, 101, 102, 32, 95, 95, 97, 116, 116,
114, 105, 98, 117, 116, 101, 95, 95, 10, 116, 121, 112, 101, 100, 101,
102, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32, 99, 104, 97, 114,
32, 117, 105, 110, 116, 56, 95, 116, 59, 10, 116, 121, 112, 101, 100,
101, 102, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32, 108, 111,
110, 103, 32, 108, 111, 110, 103, 32, 117, 105, 110, 116, 54, 52, 95,
116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 115,
105, 103, 110, 101, 100, 32, 108, 111, 110, 103, 32, 108, 111, 110,
103, 32, 115, 105, 122, 101, 95, 116, 59, 10, 116, 121, 112, 101, 100,
101, 102, 32, 108, 111, 110, 103, 32, 108, 111, 110, 103, 32, 105,
110, 116, 54, 52, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102,
32, 108, 111, 110, 103, 32, 108, 111, 110, 103, 32, 105, 110, 116,
112, 116, 114, 95, 116, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32,
117, 110, 115, 105, 103, 110, 101, 100, 32, 105, 110, 116, 32, 117,
105, 110, 116, 51, 50, 95, 116, 59, 10, 118, 111, 105, 100, 42, 32,
109, 101, 109, 115, 101, 116, 40, 118, 111, 105, 100, 42, 32, 100,
101, 115, 116, 44, 32, 105, 110, 116, 32, 99, 104, 44, 32, 115, 105,
122, 101, 95, 116, 32, 99, 111, 117, 110, 116, 41, 59, 10, 118, 111,
105, 100, 42, 32, 109, 101, 109, 99, 112, 121, 40, 118, 111, 105, 100,
42, 32, 100, 101, 115, 116, 44, 32, 99, 111, 110, 115, 116, 32, 118,
111, 105, 100, 42, 32, 115, 114, 99, 44, 32, 115, 105, 122, 101, 95,
116, 32, 99, 111, 117, 110, 116, 41, 59, 10, 105, 110, 116, 32, 109,
101, 109, 99, 109, 112, 40, 99, 111, 110, 115, 116, 32, 118, 111, 105,
100, 42, 32, 108, 104, 115, 44, 32, 99, 111, 110, 115, 116, 32, 118,
111, 105, 100, 42, 32, 114, 104, 115, 44, 32, 115, 105, 122, 101, 95,
116, 32, 99, 111, 117, 110, 116, 41, 59, 10, 118, 111, 105, 100, 42,
32, 109, 101, 109, 109, 111, 118, 101, 40, 118, 111, 105, 100, 42, 32,
100, 101, 115, 116, 44, 32, 99, 111, 110, 115, 116, 32, 118, 111, 105,
100, 42, 32, 115, 114, 99, 44, 32, 115, 105, 122, 101, 95, 116, 32,
99, 111, 117, 110, 116, 41, 59, 10, 115, 105, 122, 101, 95, 116, 32,
115, 116, 114, 108, 101, 110, 40, 99, 111, 110, 115, 116, 32, 99, 104,
97, 114, 42, 32, 115, 116, 114, 41, 59, 10, 118, 111, 105, 100, 32,
102, 114, 101, 101, 40, 118, 111, 105, 100, 42, 32, 112, 116, 114, 41,
59, 10, 118, 111, 105, 100, 32, 42, 109, 97, 108, 108, 111, 99, 40,
115, 105, 122, 101, 95, 116, 32, 115, 105, 122, 101, 41, 59, 10, 118,
111, 105, 100, 32, 42, 99, 97, 108, 108, 111, 99, 40, 115, 105, 122,
101, 95, 116, 32, 110, 117, 109, 44, 32, 115, 105, 122, 101, 95, 116,
32, 115, 105, 122, 101, 41, 59, 10, 118, 111, 105, 100, 42, 32, 114,
101, 97, 108, 108, 111, 99, 40, 118, 111, 105, 100, 42, 32, 112, 116,
114, 44, 32, 115, 105, 122, 101, 95, 116, 32, 110, 101, 119, 95, 115,
105, 122, 101, 41, 59, 10, 35, 100, 101, 102, 105, 110, 101, 32, 78,
85, 76, 76, 32, 40, 40, 118, 111, 105, 100, 42, 41, 48, 41, 32, 47,
42, 32, 116, 111, 100, 111, 33, 32, 42, 47, 10, 47, 47, 32, 35, 35,
35, 32, 69, 78, 68, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110,
116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46,
112, 121, 10, 47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73, 78, 95, 70,
73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 99, 118, 101, 99,
46, 104, 10, 10, 47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73, 78, 95,
70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 108, 105, 110,
107, 97, 103, 101, 46, 104, 10, 35, 117, 110, 100, 101, 102, 32, 83,
84, 67, 95, 65, 80, 73, 10, 35, 117, 110, 100, 101, 102, 32, 83, 84,
67, 95, 68, 69, 70, 10, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 115, 116, 97, 116, 105, 99, 32, 32, 38,
38, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 83,
84, 65, 84, 73, 67, 32, 32, 38, 38, 32, 40, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 104, 101, 97, 100, 101, 114, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 72, 69, 65, 68,
69, 82, 32, 32, 124, 124, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 100, 101, 102, 105, 110, 101, 100,
32, 105, 95, 105, 109, 112, 108, 101, 109, 101, 110, 116, 32, 124,
124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95, 73,
77, 80, 76, 69, 77, 69, 78, 84, 41, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 83, 84, 67, 95, 65, 80, 73, 32, 101, 120, 116, 101,
114, 110, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 83, 84,
67, 95, 68, 69, 70, 10, 35, 101, 108, 115, 101, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 115, 116, 97, 116, 105, 99, 10,
32, 32, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95,
95, 71, 78, 85, 67, 95, 95, 32, 124, 124, 32, 100, 101, 102, 105, 110,
101, 100, 32, 95, 95, 99, 108, 97, 110, 103, 95, 95, 10, 32, 32, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 65, 80, 73,
32, 115, 116, 97, 116, 105, 99, 32, 95, 95, 97, 116, 116, 114, 105,
98, 117, 116, 101, 95, 95, 40, 40, 117, 110, 117, 115, 101, 100, 41,
41, 10, 32, 32, 35, 101, 108, 115, 101, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 65, 80, 73, 32, 115, 116,
97, 116, 105, 99, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32,
35, 100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 68, 69, 70, 32,
115, 116, 97, 116, 105, 99, 10, 35, 101, 110, 100, 105, 102, 10, 35,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95,
73, 77, 80, 76, 69, 77, 69, 78, 84, 32, 124, 124, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 105, 109, 112, 111, 114, 116, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 105, 109, 112, 108,
101, 109, 101, 110, 116, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67, 95,
65, 76, 76, 79, 67, 65, 84, 79, 82, 32, 38, 38, 32, 33, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111,
114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 97,
108, 108, 111, 99, 97, 116, 111, 114, 32, 83, 84, 67, 95, 65, 76, 76,
79, 67, 65, 84, 79, 82, 10, 35, 101, 108, 105, 102, 32, 33, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 97, 108, 108, 111, 99, 97, 116,
111, 114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
97, 108, 108, 111, 99, 97, 116, 111, 114, 32, 99, 10, 35, 101, 110,
100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 109,
97, 108, 108, 111, 99, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 109, 97, 108, 108, 111, 99, 32, 99, 95, 74, 79, 73, 78,
40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114, 44, 32, 95,
109, 97, 108, 108, 111, 99, 41, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 99, 97, 108, 108, 111, 99, 32, 99, 95, 74, 79,
73, 78, 40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114, 44, 32,
95, 99, 97, 108, 108, 111, 99, 41, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 114, 101, 97, 108, 108, 111, 99, 32, 99, 95,
74, 79, 73, 78, 40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114,
44, 32, 95, 114, 101, 97, 108, 108, 111, 99, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 102, 114, 101, 101, 32, 99, 95,
74, 79, 73, 78, 40, 105, 95, 97, 108, 108, 111, 99, 97, 116, 111, 114,
44, 32, 95, 102, 114, 101, 101, 41, 10, 35, 101, 110, 100, 105, 102,
10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95,
95, 99, 108, 97, 110, 103, 95, 95, 32, 38, 38, 32, 33, 100, 101, 102,
105, 110, 101, 100, 32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117,
115, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110,
103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 112, 117,
115, 104, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97,
110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119,
97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 97, 108, 108, 34, 10, 32,
32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110,
105, 110, 103, 32, 34, 45, 87, 101, 120, 116, 114, 97, 34, 10, 32, 32,
35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103, 32, 100,
105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110, 105,
110, 103, 32, 34, 45, 87, 112, 101, 100, 97, 110, 116, 105, 99, 34,
10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103,
32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114,
110, 105, 110, 103, 32, 34, 45, 87, 99, 111, 110, 118, 101, 114, 115,
105, 111, 110, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99,
108, 97, 110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99,
32, 119, 97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 100, 111, 117,
98, 108, 101, 45, 112, 114, 111, 109, 111, 116, 105, 111, 110, 34, 10,
32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 99, 108, 97, 110, 103, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110,
105, 110, 103, 32, 34, 45, 87, 119, 114, 105, 116, 101, 45, 115, 116,
114, 105, 110, 103, 115, 34, 10, 32, 32, 47, 47, 32, 105, 103, 110,
111, 114, 101, 100, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32,
99, 108, 97, 110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105,
99, 32, 105, 103, 110, 111, 114, 101, 100, 32, 34, 45, 87, 109, 105,
115, 115, 105, 110, 103, 45, 102, 105, 101, 108, 100, 45, 105, 110,
105, 116, 105, 97, 108, 105, 122, 101, 114, 115, 34, 10, 35, 101, 108,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78,
85, 67, 95, 95, 32, 38, 38, 32, 33, 100, 101, 102, 105, 110, 101, 100,
32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117, 115, 10, 32, 32,
35, 112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32, 100, 105, 97, 103,
110, 111, 115, 116, 105, 99, 32, 112, 117, 115, 104, 10, 32, 32, 35,
112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32, 100, 105, 97, 103,
110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110, 105, 110, 103, 32,
34, 45, 87, 97, 108, 108, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109,
97, 32, 71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105,
99, 32, 119, 97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 101, 120,
116, 114, 97, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71,
67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119,
97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 112, 101, 100, 97, 110,
116, 105, 99, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71,
67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119,
97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 99, 111, 110, 118, 101,
114, 115, 105, 111, 110, 34, 10, 32, 32, 35, 112, 114, 97, 103, 109,
97, 32, 71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105,
99, 32, 119, 97, 114, 110, 105, 110, 103, 32, 34, 45, 87, 100, 111,
117, 98, 108, 101, 45, 112, 114, 111, 109, 111, 116, 105, 111, 110,
34, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32, 71, 67, 67, 32,
100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32, 119, 97, 114, 110,
105, 110, 103, 32, 34, 45, 87, 119, 114, 105, 116, 101, 45, 115, 116,
114, 105, 110, 103, 115, 34, 10, 32, 32, 47, 47, 32, 105, 103, 110,
111, 114, 101, 100, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97, 32,
71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116, 105, 99, 32,
105, 103, 110, 111, 114, 101, 100, 32, 34, 45, 87, 109, 105, 115, 115,
105, 110, 103, 45, 102, 105, 101, 108, 100, 45, 105, 110, 105, 116,
105, 97, 108, 105, 122, 101, 114, 115, 34, 10, 35, 101, 110, 100, 105,
102, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73, 76, 69,
95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 108, 105, 110, 107, 97, 103,
101, 46, 104, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 67, 86,
69, 67, 95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 47, 47, 32,
35, 35, 35, 32, 66, 69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73, 78,
67, 76, 85, 68, 69, 58, 32, 99, 99, 111, 109, 109, 111, 110, 46, 104,
10, 35, 105, 102, 110, 100, 101, 102, 32, 67, 67, 79, 77, 77, 79, 78,
95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 35, 100, 101, 102,
105, 110, 101, 32, 67, 67, 79, 77, 77, 79, 78, 95, 72, 95, 73, 78, 67,
76, 85, 68, 69, 68, 10, 10, 35, 105, 102, 100, 101, 102, 32, 95, 77,
83, 67, 95, 86, 69, 82, 10, 32, 32, 32, 32, 35, 112, 114, 97, 103,
109, 97, 32, 119, 97, 114, 110, 105, 110, 103, 40, 100, 105, 115, 97,
98, 108, 101, 58, 32, 52, 49, 49, 54, 32, 52, 57, 57, 54, 41, 32, 47,
47, 32, 117, 110, 110, 97, 109, 101, 100, 32, 116, 121, 112, 101, 32,
100, 101, 102, 105, 110, 105, 116, 105, 111, 110, 32, 105, 110, 32,
112, 97, 114, 101, 110, 116, 104, 101, 115, 101, 115, 10, 35, 101,
110, 100, 105, 102, 10, 47, 47, 32, 69, 88, 67, 76, 85, 68, 69, 68,
32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110, 116, 97,
105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46, 112,
121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 105, 110, 116,
116, 121, 112, 101, 115, 46, 104, 62, 10, 47, 47, 32, 69, 88, 67, 76,
85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111,
110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114,
115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60,
115, 116, 100, 100, 101, 102, 46, 104, 62, 10, 47, 47, 32, 69, 88, 67,
76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99,
111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101,
114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32,
60, 115, 116, 100, 98, 111, 111, 108, 46, 104, 62, 10, 47, 47, 32, 69,
88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110,
95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100,
101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101,
32, 60, 115, 116, 114, 105, 110, 103, 46, 104, 62, 10, 47, 47, 32, 69,
88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110,
95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100,
101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101,
32, 60, 97, 115, 115, 101, 114, 116, 46, 104, 62, 10, 10, 116, 121,
112, 101, 100, 101, 102, 32, 108, 111, 110, 103, 32, 108, 111, 110,
103, 32, 95, 108, 108, 111, 110, 103, 59, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 78, 80, 79, 83, 32, 73, 78, 84, 80, 84, 82, 95,
77, 65, 88, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 90, 73,
32, 80, 82, 73, 105, 80, 84, 82, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 90, 85, 32, 80, 82, 73, 117, 80, 84, 82, 10, 10, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78, 85,
67, 95, 95, 32, 47, 47, 32, 105, 110, 99, 108, 117, 100, 101, 115, 32,
95, 95, 99, 108, 97, 110, 103, 95, 95, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69,
32, 115, 116, 97, 116, 105, 99, 32, 105, 110, 108, 105, 110, 101, 32,
95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 40, 40, 117, 110,
117, 115, 101, 100, 41, 41, 10, 35, 101, 108, 115, 101, 10, 32, 32,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 115, 116, 97, 116, 105, 99, 32, 105, 110, 108,
105, 110, 101, 10, 35, 101, 110, 100, 105, 102, 10, 10, 47, 42, 32,
77, 97, 99, 114, 111, 32, 111, 118, 101, 114, 108, 111, 97, 100, 105,
110, 103, 32, 102, 101, 97, 116, 117, 114, 101, 32, 115, 117, 112,
112, 111, 114, 116, 32, 98, 97, 115, 101, 100, 32, 111, 110, 58, 32,
104, 116, 116, 112, 115, 58, 47, 47, 114, 101, 120, 116, 101, 115,
116, 101, 114, 46, 99, 111, 109, 47, 79, 78, 80, 56, 48, 49, 48, 55,
32, 42, 47, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 77, 65,
67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 110, 97, 109, 101,
44, 32, 46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 99, 95, 74, 79,
73, 78, 40, 99, 95, 74, 79, 73, 78, 48, 40, 110, 97, 109, 101, 44, 95,
41, 44, 99, 95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 41, 41, 40, 95, 95, 86, 65, 95, 65, 82, 71,
83, 95, 95, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 74,
79, 73, 78, 48, 40, 97, 44, 32, 98, 41, 32, 97, 32, 35, 35, 32, 98,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 74, 79, 73, 78, 40,
97, 44, 32, 98, 41, 32, 99, 95, 74, 79, 73, 78, 48, 40, 97, 44, 32,
98, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 69, 88, 80,
65, 78, 68, 40, 46, 46, 46, 41, 32, 95, 95, 86, 65, 95, 65, 82, 71,
83, 95, 95, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 78, 85,
77, 65, 82, 71, 83, 40, 46, 46, 46, 41, 32, 95, 99, 95, 65, 80, 80,
76, 89, 95, 65, 82, 71, 95, 78, 40, 40, 95, 95, 86, 65, 95, 65, 82,
71, 83, 95, 95, 44, 32, 95, 99, 95, 82, 83, 69, 81, 95, 78, 41, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 65, 80, 80, 76,
89, 95, 65, 82, 71, 95, 78, 40, 97, 114, 103, 115, 41, 32, 99, 95, 69,
88, 80, 65, 78, 68, 40, 95, 99, 95, 65, 82, 71, 95, 78, 32, 97, 114,
103, 115, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95,
82, 83, 69, 81, 95, 78, 32, 49, 54, 44, 32, 49, 53, 44, 32, 49, 52,
44, 32, 49, 51, 44, 32, 49, 50, 44, 32, 49, 49, 44, 32, 49, 48, 44,
32, 57, 44, 32, 56, 44, 32, 55, 44, 32, 54, 44, 32, 53, 44, 32, 52,
44, 32, 51, 44, 32, 50, 44, 32, 49, 44, 32, 48, 10, 35, 100, 101, 102,
105, 110, 101, 32, 95, 99, 95, 65, 82, 71, 95, 78, 40, 95, 49, 44, 32,
95, 50, 44, 32, 95, 51, 44, 32, 95, 52, 44, 32, 95, 53, 44, 32, 95,
54, 44, 32, 95, 55, 44, 32, 95, 56, 44, 32, 95, 57, 44, 32, 95, 49,
48, 44, 32, 95, 49, 49, 44, 32, 95, 49, 50, 44, 32, 95, 49, 51, 44,
32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 95, 49, 52, 44, 32, 95, 49, 53, 44, 32, 95, 49, 54, 44,
32, 78, 44, 32, 46, 46, 46, 41, 32, 78, 10, 10, 35, 105, 102, 110,
100, 101, 102, 32, 95, 95, 99, 112, 108, 117, 115, 112, 108, 117, 115,
32, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105,
95, 97, 108, 108, 111, 99, 40, 84, 41, 32, 32, 32, 32, 32, 32, 32, 32,
32, 40, 40, 84, 42, 41, 105, 95, 109, 97, 108, 108, 111, 99, 40, 99,
95, 115, 105, 122, 101, 111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 110, 101, 119,
40, 84, 44, 32, 46, 46, 46, 41, 32, 32, 32, 32, 32, 32, 40, 40, 84,
42, 41, 109, 101, 109, 99, 112, 121, 40, 95, 105, 95, 97, 108, 108,
111, 99, 40, 84, 41, 44, 32, 40, 40, 84, 91, 93, 41, 123, 95, 95, 86,
65, 95, 65, 82, 71, 83, 95, 95, 125, 41, 44, 32, 115, 105, 122, 101,
111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 110, 101, 119, 40, 84, 44, 32, 46, 46, 46,
41, 32, 32, 32, 32, 32, 32, 32, 40, 40, 84, 42, 41, 109, 101, 109, 99,
112, 121, 40, 109, 97, 108, 108, 111, 99, 40, 115, 105, 122, 101, 111,
102, 40, 84, 41, 41, 44, 32, 40, 40, 84, 91, 93, 41, 123, 95, 95, 86,
65, 95, 65, 82, 71, 83, 95, 95, 125, 41, 44, 32, 115, 105, 122, 101,
111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 84, 41, 32,
32, 32, 32, 32, 32, 32, 32, 40, 84, 41, 10, 35, 101, 108, 115, 101,
10, 47, 47, 32, 69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114,
101, 103, 101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95,
104, 101, 97, 100, 101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32,
35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 110, 101, 119, 62, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 97,
108, 108, 111, 99, 40, 84, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32,
115, 116, 97, 116, 105, 99, 95, 99, 97, 115, 116, 60, 84, 42, 62, 40,
105, 95, 109, 97, 108, 108, 111, 99, 40, 99, 95, 115, 105, 122, 101,
111, 102, 40, 84, 41, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 105, 95, 110, 101, 119, 40, 84, 44, 32, 46, 46,
46, 41, 32, 32, 32, 32, 32, 32, 110, 101, 119, 32, 40, 95, 105, 95,
97, 108, 108, 111, 99, 40, 84, 41, 41, 32, 84, 40, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 110, 101, 119, 40, 84, 44, 32, 46, 46, 46,
41, 32, 32, 32, 32, 32, 32, 32, 110, 101, 119, 32, 40, 109, 97, 108,
108, 111, 99, 40, 115, 105, 122, 101, 111, 102, 40, 84, 41, 41, 41,
32, 84, 40, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 32,
32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 76, 73, 84,
69, 82, 65, 76, 40, 84, 41, 32, 32, 32, 32, 32, 32, 32, 32, 84, 10,
35, 101, 110, 100, 105, 102, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 110, 101, 119, 95, 110, 40, 84, 44, 32, 110, 41, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 40, 40, 84, 42, 41, 109, 97, 108, 108,
111, 99, 40, 115, 105, 122, 101, 111, 102, 40, 84, 41, 42, 99, 95,
105, 50, 117, 95, 115, 105, 122, 101, 40, 110, 41, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 109, 97, 108, 108, 111, 99,
40, 115, 122, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 109,
97, 108, 108, 111, 99, 40, 99, 95, 105, 50, 117, 95, 115, 105, 122,
101, 40, 115, 122, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 99, 97, 108, 108, 111, 99, 40, 110, 44, 32, 115, 122, 41, 32,
32, 32, 32, 32, 32, 32, 32, 32, 99, 97, 108, 108, 111, 99, 40, 99, 95,
105, 50, 117, 95, 115, 105, 122, 101, 40, 110, 41, 44, 32, 99, 95,
105, 50, 117, 95, 115, 105, 122, 101, 40, 115, 122, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 114, 101, 97, 108, 108, 111,
99, 40, 112, 44, 32, 111, 108, 100, 95, 115, 122, 44, 32, 115, 122,
41, 32, 114, 101, 97, 108, 108, 111, 99, 40, 112, 44, 32, 99, 95, 105,
50, 117, 95, 115, 105, 122, 101, 40, 49, 32, 63, 32, 40, 115, 122, 41,
32, 58, 32, 40, 111, 108, 100, 95, 115, 122, 41, 41, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 102, 114, 101, 101, 40, 112, 44,
32, 115, 122, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 100,
111, 32, 123, 32, 40, 118, 111, 105, 100, 41, 40, 115, 122, 41, 59,
32, 102, 114, 101, 101, 40, 112, 41, 59, 32, 125, 32, 119, 104, 105,
108, 101, 40, 48, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99,
95, 100, 101, 108, 101, 116, 101, 40, 84, 44, 32, 112, 116, 114, 41,
32, 32, 32, 32, 32, 32, 32, 32, 100, 111, 32, 123, 32, 84, 32, 42, 95,
116, 112, 32, 61, 32, 112, 116, 114, 59, 32, 84, 35, 35, 95, 100, 114,
111, 112, 40, 95, 116, 112, 41, 59, 32, 102, 114, 101, 101, 40, 95,
116, 112, 41, 59, 32, 125, 32, 119, 104, 105, 108, 101, 32, 40, 48,
41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 116,
97, 116, 105, 99, 95, 97, 115, 115, 101, 114, 116, 40, 101, 120, 112,
114, 41, 32, 32, 32, 40, 49, 32, 63, 32, 48, 32, 58, 32, 40, 105, 110,
116, 41, 115, 105, 122, 101, 111, 102, 40, 105, 110, 116, 91, 40, 101,
120, 112, 114, 41, 32, 63, 32, 49, 32, 58, 32, 45, 49, 93, 41, 41, 10,
35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 83, 84, 67,
95, 78, 68, 69, 66, 85, 71, 32, 124, 124, 32, 100, 101, 102, 105, 110,
101, 100, 32, 78, 68, 69, 66, 85, 71, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 97, 115, 115, 101, 114, 116, 40,
101, 120, 112, 114, 41, 32, 32, 32, 32, 32, 32, 40, 40, 118, 111, 105,
100, 41, 48, 41, 10, 35, 101, 108, 115, 101, 10, 35, 105, 102, 110,
100, 101, 102, 32, 83, 84, 67, 95, 65, 83, 83, 69, 82, 84, 10, 35,
100, 101, 102, 105, 110, 101, 32, 83, 84, 67, 95, 65, 83, 83, 69, 82,
84, 40, 101, 120, 112, 114, 41, 10, 35, 101, 110, 100, 105, 102, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 97, 115,
115, 101, 114, 116, 40, 101, 120, 112, 114, 41, 32, 32, 32, 32, 32,
32, 83, 84, 67, 95, 65, 83, 83, 69, 82, 84, 40, 101, 120, 112, 114,
41, 10, 35, 101, 110, 100, 105, 102, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 111,
102, 40, 112, 44, 32, 67, 44, 32, 109, 41, 32, 40, 40, 67, 42, 41, 40,
40, 99, 104, 97, 114, 42, 41, 40, 49, 32, 63, 32, 40, 112, 41, 32, 58,
32, 38, 40, 40, 67, 42, 41, 48, 41, 45, 62, 109, 41, 32, 45, 32, 111,
102, 102, 115, 101, 116, 111, 102, 40, 67, 44, 32, 109, 41, 41, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 99, 111, 110, 115,
116, 95, 99, 97, 115, 116, 40, 84, 112, 44, 32, 112, 41, 32, 32, 32,
32, 32, 40, 40, 84, 112, 41, 40, 49, 32, 63, 32, 40, 112, 41, 32, 58,
32, 40, 84, 112, 41, 48, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 115, 97, 102, 101, 95, 99, 97, 115, 116, 40, 84, 44, 32,
70, 44, 32, 120, 41, 32, 32, 32, 32, 40, 40, 84, 41, 40, 49, 32, 63,
32, 40, 120, 41, 32, 58, 32, 40, 70, 41, 123, 48, 125, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 119, 97, 112, 40, 84,
44, 32, 120, 112, 44, 32, 121, 112, 41, 32, 32, 32, 32, 32, 32, 32,
100, 111, 32, 123, 32, 84, 32, 42, 95, 120, 112, 32, 61, 32, 120, 112,
44, 32, 42, 95, 121, 112, 32, 61, 32, 121, 112, 44, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 95, 116, 118, 32, 61, 32, 42, 95, 120, 112, 59, 32, 42, 95, 120,
112, 32, 61, 32, 42, 95, 121, 112, 59, 32, 42, 95, 121, 112, 32, 61,
32, 95, 116, 118, 59, 32, 125, 32, 119, 104, 105, 108, 101, 32, 40,
48, 41, 10, 47, 47, 32, 117, 115, 101, 32, 119, 105, 116, 104, 32,
103, 99, 99, 32, 45, 87, 99, 111, 110, 118, 101, 114, 115, 105, 111,
110, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 105, 122,
101, 111, 102, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116, 41, 115, 105, 122,
101, 111, 102, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115,
116, 114, 108, 101, 110, 40, 115, 41, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 40, 105, 110, 116, 112, 116, 114, 95, 116, 41,
115, 116, 114, 108, 101, 110, 40, 115, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 115, 116, 114, 110, 99, 109, 112, 40, 97, 44,
32, 98, 44, 32, 105, 108, 101, 110, 41, 32, 32, 32, 115, 116, 114,
110, 99, 109, 112, 40, 97, 44, 32, 98, 44, 32, 99, 95, 105, 50, 117,
95, 115, 105, 122, 101, 40, 105, 108, 101, 110, 41, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 109, 101, 109, 99, 112, 121, 40,
100, 44, 32, 115, 44, 32, 105, 108, 101, 110, 41, 32, 32, 32, 32, 109,
101, 109, 99, 112, 121, 40, 100, 44, 32, 115, 44, 32, 99, 95, 105, 50,
117, 95, 115, 105, 122, 101, 40, 105, 108, 101, 110, 41, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 109, 101, 109, 109, 111,
118, 101, 40, 100, 44, 32, 115, 44, 32, 105, 108, 101, 110, 41, 32,
32, 32, 109, 101, 109, 109, 111, 118, 101, 40, 100, 44, 32, 115, 44,
32, 99, 95, 105, 50, 117, 95, 115, 105, 122, 101, 40, 105, 108, 101,
110, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 109,
101, 109, 115, 101, 116, 40, 100, 44, 32, 118, 97, 108, 44, 32, 105,
108, 101, 110, 41, 32, 32, 109, 101, 109, 115, 101, 116, 40, 100, 44,
32, 118, 97, 108, 44, 32, 99, 95, 105, 50, 117, 95, 115, 105, 122,
101, 40, 105, 108, 101, 110, 41, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 109, 101, 109, 99, 109, 112, 40, 97, 44, 32, 98, 44,
32, 105, 108, 101, 110, 41, 32, 32, 32, 32, 109, 101, 109, 99, 109,
112, 40, 97, 44, 32, 98, 44, 32, 99, 95, 105, 50, 117, 95, 115, 105,
122, 101, 40, 105, 108, 101, 110, 41, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 117, 50, 105, 95, 115, 105, 122, 101, 40, 117,
41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 105, 110, 116,
112, 116, 114, 95, 116, 41, 40, 49, 32, 63, 32, 40, 117, 41, 32, 58,
32, 40, 115, 105, 122, 101, 95, 116, 41, 49, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 105, 50, 117, 95, 115, 105, 122, 101,
40, 105, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 115, 105,
122, 101, 95, 116, 41, 40, 49, 32, 63, 32, 40, 105, 41, 32, 58, 32,
45, 49, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 108,
101, 115, 115, 95, 117, 110, 115, 105, 103, 110, 101, 100, 40, 97, 44,
32, 98, 41, 32, 32, 32, 40, 40, 115, 105, 122, 101, 95, 116, 41, 40,
97, 41, 32, 60, 32, 40, 115, 105, 122, 101, 95, 116, 41, 40, 98, 41,
41, 10, 10, 47, 47, 32, 120, 32, 97, 110, 100, 32, 121, 32, 97, 114,
101, 32, 105, 95, 107, 101, 121, 114, 97, 119, 42, 32, 116, 121, 112,
101, 44, 32, 100, 101, 102, 97, 117, 108, 116, 115, 32, 116, 111, 32,
105, 95, 107, 101, 121, 42, 58, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 109, 112, 40,
120, 44, 32, 121, 41, 32, 32, 32, 32, 32, 40, 99, 95, 100, 101, 102,
97, 117, 108, 116, 95, 108, 101, 115, 115, 40, 121, 44, 32, 120, 41,
32, 45, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 108, 101,
115, 115, 40, 120, 44, 32, 121, 41, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 108, 101,
115, 115, 40, 120, 44, 32, 121, 41, 32, 32, 32, 32, 40, 42, 40, 120,
41, 32, 60, 32, 42, 40, 121, 41, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 101, 113, 40,
120, 44, 32, 121, 41, 32, 32, 32, 32, 32, 32, 40, 42, 40, 120, 41, 32,
61, 61, 32, 42, 40, 121, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 109, 101, 109, 99, 109, 112, 95, 101, 113, 40, 120, 44,
32, 121, 41, 32, 32, 32, 32, 32, 32, 32, 40, 109, 101, 109, 99, 109,
112, 40, 120, 44, 32, 121, 44, 32, 115, 105, 122, 101, 111, 102, 32,
42, 40, 120, 41, 41, 32, 61, 61, 32, 48, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 104,
97, 115, 104, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 115, 116, 99,
95, 104, 97, 115, 104, 95, 49, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 108, 111,
110, 101, 40, 118, 41, 32, 32, 32, 32, 32, 32, 40, 118, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108,
116, 95, 116, 111, 114, 97, 119, 40, 118, 112, 41, 32, 32, 32, 32, 32,
40, 42, 40, 118, 112, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 100, 114, 111, 112,
40, 118, 112, 41, 32, 32, 32, 32, 32, 32, 40, 40, 118, 111, 105, 100,
41, 32, 40, 118, 112, 41, 41, 10, 10, 47, 42, 32, 70, 117, 110, 99,
116, 105, 111, 110, 32, 109, 97, 99, 114, 111, 115, 32, 97, 110, 100,
32, 111, 116, 104, 101, 114, 115, 32, 42, 47, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 108, 105, 116, 115, 116, 114, 108,
101, 110, 40, 108, 105, 116, 101, 114, 97, 108, 41, 32, 40, 99, 95,
115, 105, 122, 101, 111, 102, 40, 34, 34, 32, 108, 105, 116, 101, 114,
97, 108, 41, 32, 45, 32, 49, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 97, 114, 114, 97, 121, 108, 101, 110, 40, 97, 41, 32, 40,
105, 110, 116, 112, 116, 114, 95, 116, 41, 40, 115, 105, 122, 101,
111, 102, 40, 97, 41, 47, 115, 105, 122, 101, 111, 102, 32, 48, 91,
97, 93, 41, 10, 10, 47, 47, 32, 78, 111, 110, 45, 111, 119, 110, 105,
110, 103, 32, 99, 45, 115, 116, 114, 105, 110, 103, 32, 34, 99, 108,
97, 115, 115, 34, 10, 116, 121, 112, 101, 100, 101, 102, 32, 99, 111,
110, 115, 116, 32, 99, 104, 97, 114, 42, 32, 99, 99, 104, 97, 114,
112, 116, 114, 59, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 99,
104, 97, 114, 112, 116, 114, 95, 99, 109, 112, 40, 120, 112, 44, 32,
121, 112, 41, 32, 115, 116, 114, 99, 109, 112, 40, 42, 40, 120, 112,
41, 44, 32, 42, 40, 121, 112, 41, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 99, 104, 97, 114, 112, 116, 114, 95, 104, 97, 115, 104,
40, 112, 41, 32, 115, 116, 99, 95, 115, 116, 114, 104, 97, 115, 104,
40, 42, 40, 112, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99,
99, 104, 97, 114, 112, 116, 114, 95, 99, 108, 111, 110, 101, 40, 115,
41, 32, 40, 115, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 99,
104, 97, 114, 112, 116, 114, 95, 100, 114, 111, 112, 40, 112, 41, 32,
40, 40, 118, 111, 105, 100, 41, 112, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 115, 118, 40, 46, 46, 46, 41, 32, 99, 95,
77, 65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95,
115, 118, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 118, 95, 49, 40,
108, 105, 116, 101, 114, 97, 108, 41, 32, 99, 95, 115, 118, 95, 50,
40, 108, 105, 116, 101, 114, 97, 108, 44, 32, 99, 95, 108, 105, 116,
115, 116, 114, 108, 101, 110, 40, 108, 105, 116, 101, 114, 97, 108,
41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 118,
95, 50, 40, 115, 116, 114, 44, 32, 110, 41, 32, 40, 99, 95, 76, 73,
84, 69, 82, 65, 76, 40, 99, 115, 118, 105, 101, 119, 41, 123, 115,
116, 114, 44, 32, 110, 125, 41, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 83, 86, 40, 115, 118, 41, 32, 40, 105, 110, 116, 41, 40,
115, 118, 41, 46, 115, 105, 122, 101, 44, 32, 40, 115, 118, 41, 46,
98, 117, 102, 32, 47, 47, 32, 112, 114, 105, 110, 116, 102, 40, 34,
37, 46, 42, 115, 92, 110, 34, 44, 32, 99, 95, 83, 86, 40, 115, 118,
41, 41, 59, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 114,
115, 40, 108, 105, 116, 101, 114, 97, 108, 41, 32, 99, 95, 114, 115,
95, 50, 40, 108, 105, 116, 101, 114, 97, 108, 44, 32, 99, 95, 108,
105, 116, 115, 116, 114, 108, 101, 110, 40, 108, 105, 116, 101, 114,
97, 108, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95,
114, 115, 95, 50, 40, 115, 116, 114, 44, 32, 110, 41, 32, 40, 99, 95,
76, 73, 84, 69, 82, 65, 76, 40, 99, 114, 97, 119, 115, 116, 114, 41,
123, 115, 116, 114, 44, 32, 110, 125, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 82, 79, 84, 76, 40, 120, 44, 32, 107, 41,
32, 40, 120, 32, 60, 60, 32, 40, 107, 41, 32, 124, 32, 120, 32, 62,
62, 32, 40, 56, 42, 115, 105, 122, 101, 111, 102, 40, 120, 41, 32, 45,
32, 40, 107, 41, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
115, 116, 99, 95, 104, 97, 115, 104, 40, 46, 46, 46, 41, 32, 99, 95,
77, 65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 115, 116,
99, 95, 104, 97, 115, 104, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83,
95, 95, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 115, 116, 99,
95, 104, 97, 115, 104, 95, 49, 40, 120, 41, 32, 115, 116, 99, 95, 104,
97, 115, 104, 95, 50, 40, 120, 44, 32, 99, 95, 115, 105, 122, 101,
111, 102, 40, 42, 40, 120, 41, 41, 41, 10, 10, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 117, 105, 110, 116, 54, 52, 95, 116, 32, 115, 116,
99, 95, 104, 97, 115, 104, 95, 50, 40, 99, 111, 110, 115, 116, 32,
118, 111, 105, 100, 42, 32, 107, 101, 121, 44, 32, 105, 110, 116, 112,
116, 114, 95, 116, 32, 108, 101, 110, 41, 32, 123, 10, 32, 32, 32, 32,
117, 105, 110, 116, 51, 50, 95, 116, 32, 117, 52, 59, 32, 117, 105,
110, 116, 54, 52, 95, 116, 32, 117, 56, 59, 10, 32, 32, 32, 32, 115,
119, 105, 116, 99, 104, 32, 40, 108, 101, 110, 41, 32, 123, 10, 32,
32, 32, 32, 32, 32, 32, 32, 99, 97, 115, 101, 32, 56, 58, 32, 109,
101, 109, 99, 112, 121, 40, 38, 117, 56, 44, 32, 107, 101, 121, 44,
32, 56, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32, 117, 56, 42, 48,
120, 99, 54, 97, 52, 97, 55, 57, 51, 53, 98, 100, 49, 101, 57, 57,
100, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 97, 115, 101, 32, 52,
58, 32, 109, 101, 109, 99, 112, 121, 40, 38, 117, 52, 44, 32, 107,
101, 121, 44, 32, 52, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32,
117, 52, 42, 48, 120, 99, 54, 97, 52, 97, 55, 57, 51, 53, 98, 100, 49,
101, 57, 57, 100, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 97, 115,
101, 32, 48, 58, 32, 114, 101, 116, 117, 114, 110, 32, 49, 59, 10, 32,
32, 32, 32, 125, 10, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 117,
105, 110, 116, 56, 95, 116, 32, 42, 120, 32, 61, 32, 40, 99, 111, 110,
115, 116, 32, 117, 105, 110, 116, 56, 95, 116, 42, 41, 107, 101, 121,
59, 10, 32, 32, 32, 32, 117, 105, 110, 116, 54, 52, 95, 116, 32, 104,
32, 61, 32, 40, 117, 105, 110, 116, 54, 52, 95, 116, 41, 42, 120, 32,
60, 60, 32, 55, 44, 32, 110, 32, 61, 32, 40, 117, 105, 110, 116, 54,
52, 95, 116, 41, 108, 101, 110, 32, 62, 62, 32, 51, 59, 10, 32, 32,
32, 32, 108, 101, 110, 32, 38, 61, 32, 55, 59, 10, 32, 32, 32, 32,
119, 104, 105, 108, 101, 32, 40, 110, 45, 45, 41, 32, 123, 10, 32, 32,
32, 32, 32, 32, 32, 32, 109, 101, 109, 99, 112, 121, 40, 38, 117, 56,
44, 32, 120, 44, 32, 56, 41, 44, 32, 120, 32, 43, 61, 32, 56, 59, 10,
32, 32, 32, 32, 32, 32, 32, 32, 104, 32, 61, 32, 40, 104, 32, 94, 32,
117, 56, 41, 42, 48, 120, 99, 54, 97, 52, 97, 55, 57, 51, 53, 98, 100,
49, 101, 57, 57, 100, 59, 10, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32,
119, 104, 105, 108, 101, 32, 40, 108, 101, 110, 45, 45, 41, 32, 104,
32, 61, 32, 40, 104, 32, 94, 32, 42, 120, 43, 43, 41, 42, 48, 120, 49,
48, 48, 48, 48, 48, 48, 48, 49, 98, 51, 59, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 104, 32, 94, 32, 99, 95, 82, 79, 84, 76,
40, 104, 44, 32, 50, 54, 41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73,
78, 76, 73, 78, 69, 32, 117, 105, 110, 116, 54, 52, 95, 116, 32, 115,
116, 99, 95, 115, 116, 114, 104, 97, 115, 104, 40, 99, 111, 110, 115,
116, 32, 99, 104, 97, 114, 32, 42, 115, 116, 114, 41, 10, 32, 32, 32,
32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 115, 116, 99, 95, 104,
97, 115, 104, 95, 50, 40, 115, 116, 114, 44, 32, 99, 95, 115, 116,
114, 108, 101, 110, 40, 115, 116, 114, 41, 41, 59, 32, 125, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 117, 105, 110, 116, 54,
52, 95, 116, 32, 95, 115, 116, 99, 95, 104, 97, 115, 104, 95, 109,
105, 120, 40, 117, 105, 110, 116, 54, 52, 95, 116, 32, 104, 91, 93,
44, 32, 105, 110, 116, 32, 110, 41, 32, 123, 32, 47, 47, 32, 110, 32,
62, 32, 48, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110, 116,
32, 105, 32, 61, 32, 49, 59, 32, 105, 32, 60, 32, 110, 59, 32, 43, 43,
105, 41, 32, 104, 91, 48, 93, 32, 94, 61, 32, 104, 91, 48, 93, 32, 43,
32, 104, 91, 105, 93, 59, 32, 47, 47, 32, 110, 111, 110, 45, 99, 111,
109, 109, 117, 116, 97, 116, 105, 118, 101, 33, 10, 32, 32, 32, 32,
114, 101, 116, 117, 114, 110, 32, 104, 91, 48, 93, 59, 10, 125, 10,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 99, 104, 97, 114, 42,
32, 115, 116, 99, 95, 115, 116, 114, 110, 115, 116, 114, 110, 40, 99,
111, 110, 115, 116, 32, 99, 104, 97, 114, 32, 42, 115, 116, 114, 44,
32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 108, 101, 110, 44,
32, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111,
110, 115, 116, 32, 99, 104, 97, 114, 32, 42, 110, 101, 101, 100, 108,
101, 44, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 108, 101,
110, 41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 33, 110, 108,
101, 110, 41, 32, 114, 101, 116, 117, 114, 110, 32, 40, 99, 104, 97,
114, 32, 42, 41, 115, 116, 114, 59, 10, 32, 32, 32, 32, 105, 102, 32,
40, 110, 108, 101, 110, 32, 62, 32, 115, 108, 101, 110, 41, 32, 114,
101, 116, 117, 114, 110, 32, 78, 85, 76, 76, 59, 10, 32, 32, 32, 32,
115, 108, 101, 110, 32, 45, 61, 32, 110, 108, 101, 110, 59, 10, 32,
32, 32, 32, 100, 111, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 102, 32, 40, 42, 115, 116, 114, 32, 61, 61, 32, 42, 110, 101,
101, 100, 108, 101, 32, 38, 38, 32, 33, 99, 95, 109, 101, 109, 99,
109, 112, 40, 115, 116, 114, 44, 32, 110, 101, 101, 100, 108, 101, 44,
32, 110, 108, 101, 110, 41, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 40, 99, 104, 97,
114, 32, 42, 41, 115, 116, 114, 59, 10, 32, 32, 32, 32, 32, 32, 32,
32, 43, 43, 115, 116, 114, 59, 10, 32, 32, 32, 32, 125, 32, 119, 104,
105, 108, 101, 32, 40, 115, 108, 101, 110, 45, 45, 41, 59, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 78, 85, 76, 76, 59, 10, 125,
10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 115, 116, 99, 95, 110, 101, 120, 116, 112,
111, 119, 50, 40, 105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 41,
32, 123, 10, 32, 32, 32, 32, 110, 45, 45, 59, 10, 32, 32, 32, 32, 110,
32, 124, 61, 32, 110, 32, 62, 62, 32, 49, 44, 32, 110, 32, 124, 61,
32, 110, 32, 62, 62, 32, 50, 59, 10, 32, 32, 32, 32, 110, 32, 124, 61,
32, 110, 32, 62, 62, 32, 52, 44, 32, 110, 32, 124, 61, 32, 110, 32,
62, 62, 32, 56, 59, 10, 32, 32, 32, 32, 110, 32, 124, 61, 32, 110, 32,
62, 62, 32, 49, 54, 59, 10, 32, 32, 32, 32, 35, 105, 102, 32, 73, 78,
84, 80, 84, 82, 95, 77, 65, 88, 32, 61, 61, 32, 73, 78, 84, 54, 52,
95, 77, 65, 88, 10, 32, 32, 32, 32, 110, 32, 124, 61, 32, 110, 32, 62,
62, 32, 51, 50, 59, 10, 32, 32, 32, 32, 35, 101, 110, 100, 105, 102,
10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 110, 32, 43, 32,
49, 59, 10, 125, 10, 47, 42, 32, 67, 111, 110, 116, 114, 111, 108, 32,
98, 108, 111, 99, 107, 32, 109, 97, 99, 114, 111, 115, 32, 42, 47, 10,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111, 114, 101,
97, 99, 104, 40, 46, 46, 46, 41, 32, 99, 95, 77, 65, 67, 82, 79, 95,
79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95, 102, 111, 114, 101, 97,
99, 104, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111, 114, 101, 97,
99, 104, 95, 51, 40, 105, 116, 44, 32, 67, 44, 32, 99, 110, 116, 41,
32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 67, 35, 35, 95,
105, 116, 101, 114, 32, 105, 116, 32, 61, 32, 67, 35, 35, 95, 98, 101,
103, 105, 110, 40, 38, 99, 110, 116, 41, 59, 32, 105, 116, 46, 114,
101, 102, 59, 32, 67, 35, 35, 95, 110, 101, 120, 116, 40, 38, 105,
116, 41, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102,
111, 114, 101, 97, 99, 104, 95, 52, 40, 105, 116, 44, 32, 67, 44, 32,
115, 116, 97, 114, 116, 44, 32, 102, 105, 110, 105, 115, 104, 41, 32,
92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 67, 35, 35, 95, 105,
116, 101, 114, 32, 105, 116, 32, 61, 32, 40, 115, 116, 97, 114, 116,
41, 44, 32, 42, 95, 101, 110, 100, 114, 101, 102, 32, 61, 32, 99, 95,
115, 97, 102, 101, 95, 99, 97, 115, 116, 40, 67, 35, 35, 95, 105, 116,
101, 114, 42, 44, 32, 67, 35, 35, 95, 118, 97, 108, 117, 101, 42, 44,
32, 40, 102, 105, 110, 105, 115, 104, 41, 46, 114, 101, 102, 41, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 105, 116, 46, 114,
101, 102, 32, 33, 61, 32, 40, 67, 35, 35, 95, 118, 97, 108, 117, 101,
42, 41, 95, 101, 110, 100, 114, 101, 102, 59, 32, 67, 35, 35, 95, 110,
101, 120, 116, 40, 38, 105, 116, 41, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 112, 97, 105, 114, 40, 107,
101, 121, 44, 32, 118, 97, 108, 44, 32, 67, 44, 32, 99, 110, 116, 41,
32, 47, 42, 32, 115, 116, 114, 117, 99, 116, 117, 114, 101, 100, 32,
98, 105, 110, 100, 105, 110, 103, 32, 42, 47, 32, 92, 10, 32, 32, 32,
32, 102, 111, 114, 32, 40, 115, 116, 114, 117, 99, 116, 32, 123, 67,
35, 35, 95, 105, 116, 101, 114, 32, 105, 116, 101, 114, 59, 32, 99,
111, 110, 115, 116, 32, 67, 35, 35, 95, 107, 101, 121, 42, 32, 107,
101, 121, 59, 32, 67, 35, 35, 95, 109, 97, 112, 112, 101, 100, 42, 32,
118, 97, 108, 59, 125, 32, 95, 32, 61, 32, 123, 46, 105, 116, 101,
114, 61, 67, 35, 35, 95, 98, 101, 103, 105, 110, 40, 38, 99, 110, 116,
41, 125, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 95,
46, 105, 116, 101, 114, 46, 114, 101, 102, 32, 38, 38, 32, 40, 95, 46,
107, 101, 121, 32, 61, 32, 38, 95, 46, 105, 116, 101, 114, 46, 114,
101, 102, 45, 62, 102, 105, 114, 115, 116, 44, 32, 95, 46, 118, 97,
108, 32, 61, 32, 38, 95, 46, 105, 116, 101, 114, 46, 114, 101, 102,
45, 62, 115, 101, 99, 111, 110, 100, 41, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 32, 59, 32, 67, 35, 35, 95, 110, 101, 120, 116, 40,
38, 95, 46, 105, 116, 101, 114, 41, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 105, 110, 100, 101, 120,
101, 100, 40, 105, 116, 44, 32, 67, 44, 32, 99, 110, 116, 41, 32, 92,
10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 115, 116, 114, 117, 99,
116, 32, 123, 67, 35, 35, 95, 105, 116, 101, 114, 32, 105, 116, 101,
114, 59, 32, 67, 35, 35, 95, 118, 97, 108, 117, 101, 42, 32, 114, 101,
102, 59, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 110, 100,
101, 120, 59, 125, 32, 105, 116, 32, 61, 32, 123, 46, 105, 116, 101,
114, 61, 67, 35, 35, 95, 98, 101, 103, 105, 110, 40, 38, 99, 110, 116,
41, 125, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 40,
105, 116, 46, 114, 101, 102, 32, 61, 32, 105, 116, 46, 105, 116, 101,
114, 46, 114, 101, 102, 41, 32, 59, 32, 67, 35, 35, 95, 110, 101, 120,
116, 40, 38, 105, 116, 46, 105, 116, 101, 114, 41, 44, 32, 43, 43,
105, 116, 46, 105, 110, 100, 101, 120, 41, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 105, 116, 101, 114, 40, 101,
120, 105, 115, 116, 105, 110, 103, 95, 105, 116, 101, 114, 44, 32, 67,
44, 32, 99, 110, 116, 41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114,
32, 40, 101, 120, 105, 115, 116, 105, 110, 103, 95, 105, 116, 101,
114, 32, 61, 32, 67, 35, 35, 95, 98, 101, 103, 105, 110, 40, 38, 99,
110, 116, 41, 59, 32, 40, 101, 120, 105, 115, 116, 105, 110, 103, 95,
105, 116, 101, 114, 41, 46, 114, 101, 102, 59, 32, 67, 35, 35, 95,
110, 101, 120, 116, 40, 38, 101, 120, 105, 115, 116, 105, 110, 103,
95, 105, 116, 101, 114, 41, 41, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 40, 46, 46,
46, 41, 32, 99, 95, 77, 65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79,
65, 68, 40, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 44, 32, 95,
95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 95,
49, 40, 115, 116, 111, 112, 41, 32, 99, 95, 102, 111, 114, 114, 97,
110, 103, 101, 95, 51, 40, 95, 105, 44, 32, 48, 44, 32, 115, 116, 111,
112, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111,
114, 114, 97, 110, 103, 101, 95, 50, 40, 105, 44, 32, 115, 116, 111,
112, 41, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 95, 51,
40, 105, 44, 32, 48, 44, 32, 115, 116, 111, 112, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101,
95, 51, 40, 105, 44, 32, 115, 116, 97, 114, 116, 44, 32, 115, 116,
111, 112, 41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 95,
108, 108, 111, 110, 103, 32, 105, 61, 115, 116, 97, 114, 116, 44, 32,
95, 101, 110, 100, 61, 115, 116, 111, 112, 59, 32, 105, 32, 60, 32,
95, 101, 110, 100, 59, 32, 43, 43, 105, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 102, 111, 114, 114, 97, 110, 103, 101, 95,
52, 40, 105, 44, 32, 115, 116, 97, 114, 116, 44, 32, 115, 116, 111,
112, 44, 32, 115, 116, 101, 112, 41, 32, 92, 10, 32, 32, 32, 32, 102,
111, 114, 32, 40, 95, 108, 108, 111, 110, 103, 32, 105, 61, 115, 116,
97, 114, 116, 44, 32, 95, 105, 110, 99, 61, 115, 116, 101, 112, 44,
32, 95, 101, 110, 100, 61, 40, 95, 108, 108, 111, 110, 103, 41, 40,
115, 116, 111, 112, 41, 32, 45, 32, 40, 95, 105, 110, 99, 32, 62, 32,
48, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 40,
95, 105, 110, 99, 32, 62, 32, 48, 41, 32, 94, 32, 40, 105, 32, 62, 32,
95, 101, 110, 100, 41, 59, 32, 105, 32, 43, 61, 32, 95, 105, 110, 99,
41, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 95, 95, 99, 112,
108, 117, 115, 112, 108, 117, 115, 10, 32, 32, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 105, 110, 105, 116, 40, 67, 44, 32,
46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 67, 35,
35, 95, 102, 114, 111, 109, 95, 110, 40, 40, 67, 35, 35, 95, 114, 97,
119, 91, 93, 41, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 44, 32,
99, 95, 115, 105, 122, 101, 111, 102, 40, 40, 67, 35, 35, 95, 114, 97,
119, 91, 93, 41, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 47,
99, 95, 115, 105, 122, 101, 111, 102, 40, 67, 35, 35, 95, 114, 97,
119, 41, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 102, 111, 114, 108, 105, 115, 116, 40, 105, 116, 44, 32, 84,
44, 32, 46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
102, 111, 114, 32, 40, 115, 116, 114, 117, 99, 116, 32, 123, 84, 42,
32, 114, 101, 102, 59, 32, 105, 110, 116, 32, 115, 105, 122, 101, 44,
32, 105, 110, 100, 101, 120, 59, 125, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 105, 116, 32, 61, 32, 123, 46, 114,
101, 102, 61, 40, 84, 91, 93, 41, 95, 95, 86, 65, 95, 65, 82, 71, 83,
95, 95, 44, 32, 46, 115, 105, 122, 101, 61, 40, 105, 110, 116, 41, 40,
115, 105, 122, 101, 111, 102, 40, 40, 84, 91, 93, 41, 95, 95, 86, 65,
95, 65, 82, 71, 83, 95, 95, 41, 47, 115, 105, 122, 101, 111, 102, 40,
84, 41, 41, 125, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 59, 32, 105, 116, 46, 105, 110, 100, 101, 120, 32, 60, 32,
105, 116, 46, 115, 105, 122, 101, 59, 32, 43, 43, 105, 116, 46, 114,
101, 102, 44, 32, 43, 43, 105, 116, 46, 105, 110, 100, 101, 120, 41,
10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 115, 116,
99, 95, 104, 97, 115, 104, 95, 109, 105, 120, 40, 46, 46, 46, 41, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 115, 116, 99, 95, 104, 97,
115, 104, 95, 109, 105, 120, 40, 40, 117, 105, 110, 116, 54, 52, 95,
116, 91, 93, 41, 123, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 125,
44, 32, 99, 95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 41, 41, 10, 35, 101, 108, 115, 101, 10, 47,
47, 32, 69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103,
101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101,
97, 100, 101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32, 35, 105,
110, 99, 108, 117, 100, 101, 32, 60, 105, 110, 105, 116, 105, 97, 108,
105, 122, 101, 114, 95, 108, 105, 115, 116, 62, 10, 47, 47, 32, 69,
88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110,
95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100,
101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32, 35, 105, 110, 99,
108, 117, 100, 101, 32, 60, 97, 114, 114, 97, 121, 62, 10, 32, 32, 32,
32, 116, 101, 109, 112, 108, 97, 116, 101, 32, 60, 99, 108, 97, 115,
115, 32, 67, 44, 32, 99, 108, 97, 115, 115, 32, 84, 62, 10, 32, 32,
32, 32, 105, 110, 108, 105, 110, 101, 32, 67, 32, 95, 102, 114, 111,
109, 95, 110, 40, 67, 32, 40, 42, 102, 117, 110, 99, 41, 40, 99, 111,
110, 115, 116, 32, 84, 91, 93, 44, 32, 105, 110, 116, 112, 116, 114,
95, 116, 41, 44, 32, 115, 116, 100, 58, 58, 105, 110, 105, 116, 105,
97, 108, 105, 122, 101, 114, 95, 108, 105, 115, 116, 60, 84, 62, 32,
105, 108, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 114, 101,
116, 117, 114, 110, 32, 102, 117, 110, 99, 40, 38, 42, 105, 108, 46,
98, 101, 103, 105, 110, 40, 41, 44, 32, 105, 108, 46, 115, 105, 122,
101, 40, 41, 41, 59, 32, 125, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 105, 110, 105, 116, 40, 67, 44, 32, 46, 46,
46, 41, 32, 95, 102, 114, 111, 109, 95, 110, 60, 67, 44, 67, 35, 35,
95, 114, 97, 119, 62, 40, 67, 35, 35, 95, 102, 114, 111, 109, 95, 110,
44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 32, 32,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 102, 111, 114,
108, 105, 115, 116, 40, 105, 116, 44, 32, 84, 44, 32, 46, 46, 46, 41,
32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 102, 111, 114, 32, 40,
115, 116, 114, 117, 99, 116, 32, 123, 115, 116, 100, 58, 58, 105, 110,
105, 116, 105, 97, 108, 105, 122, 101, 114, 95, 108, 105, 115, 116,
60, 84, 62, 32, 95, 105, 108, 59, 32, 115, 116, 100, 58, 58, 105, 110,
105, 116, 105, 97, 108, 105, 122, 101, 114, 95, 108, 105, 115, 116,
60, 84, 62, 58, 58, 105, 116, 101, 114, 97, 116, 111, 114, 32, 114,
101, 102, 59, 32, 115, 105, 122, 101, 95, 116, 32, 115, 105, 122, 101,
44, 32, 105, 110, 100, 101, 120, 59, 125, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 116, 32, 61, 32, 123, 46, 95,
105, 108, 61, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 44, 32, 46,
114, 101, 102, 61, 105, 116, 46, 95, 105, 108, 46, 98, 101, 103, 105,
110, 40, 41, 44, 32, 46, 115, 105, 122, 101, 61, 105, 116, 46, 95,
105, 108, 46, 115, 105, 122, 101, 40, 41, 125, 32, 92, 10, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 59, 32, 105, 116, 46, 105,
110, 100, 101, 120, 32, 60, 32, 105, 116, 46, 115, 105, 122, 101, 59,
32, 43, 43, 105, 116, 46, 114, 101, 102, 44, 32, 43, 43, 105, 116, 46,
105, 110, 100, 101, 120, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 115, 116, 99, 95, 104, 97, 115, 104, 95, 109, 105,
120, 40, 46, 46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
95, 115, 116, 99, 95, 104, 97, 115, 104, 95, 109, 105, 120, 40, 115,
116, 100, 58, 58, 97, 114, 114, 97, 121, 60, 117, 105, 110, 116, 54,
52, 95, 116, 44, 32, 99, 95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95,
86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 62, 123, 95, 95, 86, 65, 95,
65, 82, 71, 83, 95, 95, 125, 46, 100, 97, 116, 97, 40, 41, 44, 32, 99,
95, 78, 85, 77, 65, 82, 71, 83, 40, 95, 95, 86, 65, 95, 65, 82, 71,
83, 95, 95, 41, 41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 100, 101, 102, 101, 114, 40, 46,
46, 46, 41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105,
110, 116, 32, 95, 105, 32, 61, 32, 49, 59, 32, 95, 105, 59, 32, 95,
105, 32, 61, 32, 48, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95,
95, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 119,
105, 116, 104, 40, 46, 46, 46, 41, 32, 99, 95, 77, 65, 67, 82, 79, 95,
79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95, 119, 105, 116, 104, 44,
32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 119, 105, 116, 104, 95, 50, 40, 100,
101, 99, 108, 118, 97, 114, 44, 32, 100, 114, 111, 112, 41, 32, 92,
10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 100, 101, 99, 108, 118, 97,
114, 44, 32, 42, 95, 105, 44, 32, 42, 42, 95, 105, 112, 32, 61, 32,
38, 95, 105, 59, 32, 95, 105, 112, 59, 32, 95, 105, 112, 32, 61, 32,
48, 44, 32, 100, 114, 111, 112, 41, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 119, 105, 116, 104, 95, 51, 40, 100, 101, 99, 108,
118, 97, 114, 44, 32, 112, 114, 101, 100, 44, 32, 100, 114, 111, 112,
41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 100, 101, 99,
108, 118, 97, 114, 44, 32, 42, 95, 105, 44, 32, 42, 42, 95, 105, 112,
32, 61, 32, 38, 95, 105, 59, 32, 95, 105, 112, 32, 38, 38, 32, 40,
112, 114, 101, 100, 41, 59, 32, 95, 105, 112, 32, 61, 32, 48, 44, 32,
100, 114, 111, 112, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 115, 99, 111, 112, 101, 40, 46, 46, 46, 41, 32, 99, 95, 77,
65, 67, 82, 79, 95, 79, 86, 69, 82, 76, 79, 65, 68, 40, 99, 95, 115,
99, 111, 112, 101, 44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95,
41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 99, 111,
112, 101, 95, 50, 40, 105, 110, 105, 116, 44, 32, 100, 114, 111, 112,
41, 32, 92, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110, 116,
32, 95, 105, 32, 61, 32, 40, 105, 110, 105, 116, 44, 32, 49, 41, 59,
32, 95, 105, 59, 32, 95, 105, 32, 61, 32, 48, 44, 32, 100, 114, 111,
112, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 115, 99,
111, 112, 101, 95, 51, 40, 105, 110, 105, 116, 44, 32, 112, 114, 101,
100, 44, 32, 100, 114, 111, 112, 41, 32, 92, 10, 32, 32, 32, 32, 102,
111, 114, 32, 40, 105, 110, 116, 32, 95, 105, 32, 61, 32, 40, 105,
110, 105, 116, 44, 32, 49, 41, 59, 32, 95, 105, 32, 38, 38, 32, 40,
112, 114, 101, 100, 41, 59, 32, 95, 105, 32, 61, 32, 48, 44, 32, 100,
114, 111, 112, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99,
95, 100, 114, 111, 112, 40, 67, 44, 32, 46, 46, 46, 41, 32, 92, 10,
32, 32, 32, 32, 100, 111, 32, 123, 32, 99, 95, 102, 111, 114, 108,
105, 115, 116, 32, 40, 95, 105, 44, 32, 67, 42, 44, 32, 123, 95, 95,
86, 65, 95, 65, 82, 71, 83, 95, 95, 125, 41, 32, 67, 35, 35, 95, 100,
114, 111, 112, 40, 42, 95, 105, 46, 114, 101, 102, 41, 59, 32, 125,
32, 119, 104, 105, 108, 101, 40, 48, 41, 10, 10, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 40, 95, 95, 83, 73, 90, 69, 79, 70,
95, 73, 78, 84, 49, 50, 56, 95, 95, 41, 10, 32, 32, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 99, 95, 117, 109, 117, 108, 49, 50, 56,
40, 97, 44, 32, 98, 44, 32, 108, 111, 44, 32, 104, 105, 41, 32, 92,
10, 32, 32, 32, 32, 32, 32, 32, 32, 100, 111, 32, 123, 32, 95, 95,
117, 105, 110, 116, 49, 50, 56, 95, 116, 32, 95, 122, 32, 61, 32, 40,
95, 95, 117, 105, 110, 116, 49, 50, 56, 95, 116, 41, 40, 97, 41, 42,
40, 98, 41, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 42, 40, 108, 111, 41, 32, 61, 32, 40, 117, 105, 110, 116,
54, 52, 95, 116, 41, 95, 122, 44, 32, 42, 40, 104, 105, 41, 32, 61,
32, 40, 117, 105, 110, 116, 54, 52, 95, 116, 41, 40, 95, 122, 32, 62,
62, 32, 54, 52, 85, 41, 59, 32, 125, 32, 119, 104, 105, 108, 101, 40,
48, 41, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101,
100, 40, 95, 77, 83, 67, 95, 86, 69, 82, 41, 32, 38, 38, 32, 100, 101,
102, 105, 110, 101, 100, 40, 95, 87, 73, 78, 54, 52, 41, 10, 47, 47,
32, 69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103,
101, 110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101,
97, 100, 101, 114, 115, 46, 112, 121, 32, 32, 32, 32, 32, 35, 105,
110, 99, 108, 117, 100, 101, 32, 60, 105, 110, 116, 114, 105, 110, 46,
104, 62, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 99,
95, 117, 109, 117, 108, 49, 50, 56, 40, 97, 44, 32, 98, 44, 32, 108,
111, 44, 32, 104, 105, 41, 32, 40, 40, 118, 111, 105, 100, 41, 40, 42,
40, 108, 111, 41, 32, 61, 32, 95, 117, 109, 117, 108, 49, 50, 56, 40,
97, 44, 32, 98, 44, 32, 104, 105, 41, 41, 41, 10, 35, 101, 108, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 40, 95, 95, 120, 56, 54,
95, 54, 52, 95, 95, 41, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 117, 109, 117, 108, 49, 50, 56, 40, 97, 44, 32,
98, 44, 32, 108, 111, 44, 32, 104, 105, 41, 32, 92, 10, 32, 32, 32,
32, 32, 32, 32, 32, 97, 115, 109, 40, 34, 109, 117, 108, 113, 32, 37,
51, 34, 32, 58, 32, 34, 61, 97, 34, 40, 42, 40, 108, 111, 41, 41, 44,
32, 34, 61, 100, 34, 40, 42, 40, 104, 105, 41, 41, 32, 58, 32, 34, 97,
34, 40, 97, 41, 44, 32, 34, 114, 109, 34, 40, 98, 41, 41, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 101, 110, 100, 105, 102, 32, 47, 47,
32, 67, 67, 79, 77, 77, 79, 78, 95, 72, 95, 73, 78, 67, 76, 85, 68,
69, 68, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73, 76,
69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 99, 99, 111, 109, 109,
111, 110, 46, 104, 10, 47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73, 78,
95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 102, 111,
114, 119, 97, 114, 100, 46, 104, 10, 35, 105, 102, 110, 100, 101, 102,
32, 83, 84, 67, 95, 70, 79, 82, 87, 65, 82, 68, 95, 72, 95, 73, 78,
67, 76, 85, 68, 69, 68, 10, 35, 100, 101, 102, 105, 110, 101, 32, 83,
84, 67, 95, 70, 79, 82, 87, 65, 82, 68, 95, 72, 95, 73, 78, 67, 76,
85, 68, 69, 68, 10, 10, 47, 47, 32, 69, 88, 67, 76, 85, 68, 69, 68,
32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110, 116, 97,
105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46, 112,
121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 115, 116, 100,
105, 110, 116, 46, 104, 62, 10, 47, 47, 32, 69, 88, 67, 76, 85, 68,
69, 68, 32, 66, 89, 32, 114, 101, 103, 101, 110, 95, 99, 111, 110,
116, 97, 105, 110, 101, 114, 95, 104, 101, 97, 100, 101, 114, 115, 46,
112, 121, 32, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 115, 116,
100, 100, 101, 102, 46, 104, 62, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 97, 114, 99, 40,
67, 44, 32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 97, 114, 99, 95, 116,
121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 98,
111, 120, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 98, 111,
120, 95, 116, 121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 99, 100, 101, 113, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95,
99, 95, 99, 100, 101, 113, 95, 116, 121, 112, 101, 115, 40, 67, 44,
32, 86, 65, 76, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102,
111, 114, 119, 97, 114, 100, 95, 99, 108, 105, 115, 116, 40, 67, 44,
32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 108, 105, 115, 116, 95, 116,
121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 109,
97, 112, 40, 67, 44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 41, 32, 95,
99, 95, 99, 104, 97, 115, 104, 95, 116, 121, 112, 101, 115, 40, 67,
44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 44, 32, 99, 95, 116, 114, 117,
101, 44, 32, 99, 95, 102, 97, 108, 115, 101, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 115,
101, 116, 40, 67, 44, 32, 75, 69, 89, 41, 32, 95, 99, 95, 99, 104, 97,
115, 104, 95, 116, 121, 112, 101, 115, 40, 67, 44, 32, 99, 115, 101,
116, 44, 32, 75, 69, 89, 44, 32, 75, 69, 89, 44, 32, 99, 95, 102, 97,
108, 115, 101, 44, 32, 99, 95, 116, 114, 117, 101, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99,
115, 109, 97, 112, 40, 67, 44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 41,
32, 95, 99, 95, 97, 97, 116, 114, 101, 101, 95, 116, 121, 112, 101,
115, 40, 67, 44, 32, 75, 69, 89, 44, 32, 86, 65, 76, 44, 32, 99, 95,
116, 114, 117, 101, 44, 32, 99, 95, 102, 97, 108, 115, 101, 41, 10,
35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 99, 115, 115, 101, 116, 40, 67, 44, 32, 75, 69, 89, 41, 32,
95, 99, 95, 97, 97, 116, 114, 101, 101, 95, 116, 121, 112, 101, 115,
40, 67, 44, 32, 75, 69, 89, 44, 32, 75, 69, 89, 44, 32, 99, 95, 102,
97, 108, 115, 101, 44, 32, 99, 95, 116, 114, 117, 101, 41, 10, 35,
100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100,
95, 99, 115, 116, 97, 99, 107, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95,
99, 95, 99, 115, 116, 97, 99, 107, 95, 116, 121, 112, 101, 115, 40,
67, 44, 32, 86, 65, 76, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
102, 111, 114, 119, 97, 114, 100, 95, 99, 112, 113, 117, 101, 40, 67,
44, 32, 86, 65, 76, 41, 32, 95, 99, 95, 99, 112, 113, 117, 101, 95,
116, 121, 112, 101, 115, 40, 67, 44, 32, 86, 65, 76, 41, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99,
113, 117, 101, 117, 101, 40, 67, 44, 32, 86, 65, 76, 41, 32, 95, 99,
95, 99, 100, 101, 113, 95, 116, 121, 112, 101, 115, 40, 67, 44, 32,
86, 65, 76, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 99, 118, 101, 99, 40, 67, 44, 32, 86, 65,
76, 41, 32, 95, 99, 95, 99, 118, 101, 99, 95, 116, 121, 112, 101, 115,
40, 67, 44, 32, 86, 65, 76, 41, 10, 47, 47, 32, 97, 108, 116, 101,
114, 110, 97, 116, 105, 118, 101, 32, 110, 97, 109, 101, 115, 32, 40,
105, 110, 99, 108, 117, 100, 101, 47, 115, 116, 120, 41, 58, 10, 35,
100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100,
95, 97, 114, 99, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 97,
114, 99, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119,
97, 114, 100, 95, 98, 111, 120, 32, 102, 111, 114, 119, 97, 114, 100,
95, 99, 98, 111, 120, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102,
111, 114, 119, 97, 114, 100, 95, 100, 101, 113, 32, 102, 111, 114,
119, 97, 114, 100, 95, 99, 100, 101, 113, 10, 35, 100, 101, 102, 105,
110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 108, 105, 115,
116, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 108, 105, 115, 116,
10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 104, 109, 97, 112, 32, 102, 111, 114, 119, 97, 114, 100, 95,
99, 109, 97, 112, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 104, 115, 101, 116, 32, 102, 111, 114,
119, 97, 114, 100, 95, 99, 115, 101, 116, 10, 35, 100, 101, 102, 105,
110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 115, 109, 97, 112,
32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 115, 109, 97, 112, 10,
35, 100, 101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114,
100, 95, 115, 115, 101, 116, 32, 102, 111, 114, 119, 97, 114, 100, 95,
99, 115, 115, 101, 116, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102,
111, 114, 119, 97, 114, 100, 95, 115, 116, 97, 99, 107, 32, 102, 111,
114, 119, 97, 114, 100, 95, 99, 115, 116, 97, 99, 107, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95,
112, 113, 117, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 112,
113, 117, 101, 10, 35, 100, 101, 102, 105, 110, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 113, 117, 101, 117, 101, 32, 102, 111,
114, 119, 97, 114, 100, 95, 99, 113, 117, 101, 117, 101, 10, 35, 100,
101, 102, 105, 110, 101, 32, 102, 111, 114, 119, 97, 114, 100, 95,
118, 101, 99, 32, 102, 111, 114, 119, 97, 114, 100, 95, 99, 118, 101,
99, 10, 10, 47, 47, 32, 99, 115, 118, 105, 101, 119, 32, 58, 32, 110,
111, 110, 45, 110, 117, 108, 108, 32, 116, 101, 114, 109, 105, 110,
97, 116, 101, 100, 32, 115, 116, 114, 105, 110, 103, 32, 118, 105,
101, 119, 10, 116, 121, 112, 101, 100, 101, 102, 32, 99, 111, 110,
115, 116, 32, 99, 104, 97, 114, 32, 99, 115, 118, 105, 101, 119, 95,
118, 97, 108, 117, 101, 59, 10, 116, 121, 112, 101, 100, 101, 102, 32,
115, 116, 114, 117, 99, 116, 32, 99, 115, 118, 105, 101, 119, 32, 123,
10, 32, 32, 32, 32, 99, 115, 118, 105, 101, 119, 95, 118, 97, 108,
117, 101, 42, 32, 98, 117, 102, 59, 10, 32, 32, 32, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 115, 105, 122, 101, 59, 10, 125, 32, 99,
115, 118, 105, 101, 119, 59, 10, 10, 116, 121, 112, 101, 100, 101,
102, 32, 117, 110, 105, 111, 110, 32, 123, 10, 32, 32, 32, 32, 99,
115, 118, 105, 101, 119, 95, 118, 97, 108, 117, 101, 42, 32, 114, 101,
102, 59, 10, 32, 32, 32, 32, 99, 115, 118, 105, 101, 119, 32, 99, 104,
114, 59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32,
99, 115, 118, 105, 101, 119, 32, 99, 104, 114, 59, 32, 99, 115, 118,
105, 101, 119, 95, 118, 97, 108, 117, 101, 42, 32, 101, 110, 100, 59,
32, 125, 32, 117, 56, 59, 10, 125, 32, 99, 115, 118, 105, 101, 119,
95, 105, 116, 101, 114, 59, 10, 10, 10, 47, 47, 32, 99, 114, 97, 119,
115, 116, 114, 32, 58, 32, 110, 117, 108, 108, 45, 116, 101, 114, 109,
105, 110, 97, 116, 101, 100, 32, 115, 116, 114, 105, 110, 103, 32,
118, 105, 101, 119, 10, 116, 121, 112, 101, 100, 101, 102, 32, 99,
115, 118, 105, 101, 119, 95, 118, 97, 108, 117, 101, 32, 99, 114, 97,
119, 115, 116, 114, 95, 118, 97, 108, 117, 101, 59, 10, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 99, 114, 97,
119, 115, 116, 114, 32, 123, 10, 32, 32, 32, 32, 99, 114, 97, 119,
115, 116, 114, 95, 118, 97, 108, 117, 101, 42, 32, 115, 116, 114, 59,
10, 32, 32, 32, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 115,
105, 122, 101, 59, 10, 125, 32, 99, 114, 97, 119, 115, 116, 114, 59,
10, 10, 116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 105, 111,
110, 32, 123, 10, 32, 32, 32, 32, 99, 114, 97, 119, 115, 116, 114, 95,
118, 97, 108, 117, 101, 42, 32, 114, 101, 102, 59, 10, 32, 32, 32, 32,
99, 115, 118, 105, 101, 119, 32, 99, 104, 114, 59, 10, 125, 32, 99,
114, 97, 119, 115, 116, 114, 95, 105, 116, 101, 114, 59, 10, 10, 10,
47, 47, 32, 99, 115, 116, 114, 32, 58, 32, 110, 117, 108, 108, 45,
116, 101, 114, 109, 105, 110, 97, 116, 101, 100, 32, 111, 119, 110,
105, 110, 103, 32, 115, 116, 114, 105, 110, 103, 32, 40, 115, 104,
111, 114, 116, 32, 115, 116, 114, 105, 110, 103, 32, 111, 112, 116,
105, 109, 105, 122, 101, 100, 32, 45, 32, 115, 115, 111, 41, 10, 116,
121, 112, 101, 100, 101, 102, 32, 99, 104, 97, 114, 32, 99, 115, 116,
114, 95, 118, 97, 108, 117, 101, 59, 10, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 99, 115, 116, 114,
95, 118, 97, 108, 117, 101, 42, 32, 100, 97, 116, 97, 59, 32, 105,
110, 116, 112, 116, 114, 95, 116, 32, 115, 105, 122, 101, 44, 32, 99,
97, 112, 59, 32, 125, 32, 99, 115, 116, 114, 95, 98, 117, 102, 59, 10,
116, 121, 112, 101, 100, 101, 102, 32, 117, 110, 105, 111, 110, 32,
99, 115, 116, 114, 32, 123, 10, 32, 32, 32, 32, 115, 116, 114, 117,
99, 116, 32, 123, 32, 99, 115, 116, 114, 95, 118, 97, 108, 117, 101,
32, 100, 97, 116, 97, 91, 32, 115, 105, 122, 101, 111, 102, 40, 99,
115, 116, 114, 95, 98, 117, 102, 41, 32, 93, 59, 32, 125, 32, 115,
109, 108, 59, 10, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32,
123, 32, 99, 115, 116, 114, 95, 118, 97, 108, 117, 101, 42, 32, 100,
97, 116, 97, 59, 32, 115, 105, 122, 101, 95, 116, 32, 115, 105, 122,
101, 44, 32, 110, 99, 97, 112, 59, 32, 125, 32, 108, 111, 110, 59, 10,
125, 32, 99, 115, 116, 114, 59, 10, 10, 116, 121, 112, 101, 100, 101,
102, 32, 117, 110, 105, 111, 110, 32, 123, 10, 32, 32, 32, 32, 99,
115, 116, 114, 95, 118, 97, 108, 117, 101, 42, 32, 114, 101, 102, 59,
10, 32, 32, 32, 32, 99, 115, 118, 105, 101, 119, 32, 99, 104, 114, 59,
32, 47, 47, 32, 117, 116, 102, 56, 32, 99, 104, 97, 114, 97, 99, 116,
101, 114, 47, 99, 111, 100, 101, 112, 111, 105, 110, 116, 10, 125, 32,
99, 115, 116, 114, 95, 105, 116, 101, 114, 59, 10, 10, 10, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 71, 78, 85,
67, 95, 95, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32,
95, 95, 99, 108, 97, 110, 103, 95, 95, 32, 124, 124, 32, 100, 101,
102, 105, 110, 101, 100, 32, 95, 77, 83, 67, 95, 86, 69, 82, 10, 32,
32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 108, 111, 110, 103,
32, 99, 97, 116, 111, 109, 105, 99, 95, 108, 111, 110, 103, 59, 10,
35, 101, 108, 115, 101, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100,
101, 102, 32, 95, 65, 116, 111, 109, 105, 99, 40, 108, 111, 110, 103,
41, 32, 99, 97, 116, 111, 109, 105, 99, 95, 108, 111, 110, 103, 59,
10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 116, 114, 117, 101, 40, 46, 46, 46, 41, 32, 95, 95,
86, 65, 95, 65, 82, 71, 83, 95, 95, 10, 35, 100, 101, 102, 105, 110,
101, 32, 99, 95, 102, 97, 108, 115, 101, 40, 46, 46, 46, 41, 10, 10,
35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 99, 97, 114, 99, 95,
116, 121, 112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 41,
32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 86,
65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59,
32, 92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117,
99, 116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 95,
95, 40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108, 108, 40, 83,
69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70, 32, 123, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 42, 32, 103, 101, 116, 59, 32, 92, 10, 32, 32,
32, 32, 32, 32, 32, 32, 99, 97, 116, 111, 109, 105, 99, 95, 108, 111,
110, 103, 42, 32, 117, 115, 101, 95, 99, 111, 117, 110, 116, 59, 32,
92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 95, 99, 95, 99, 98, 111, 120, 95, 116, 121,
112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10,
32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 86, 65, 76, 32,
83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10,
116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116,
32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 95, 95, 40, 40,
109, 101, 116, 104, 111, 100, 99, 97, 108, 108, 40, 83, 69, 76, 70,
35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108,
117, 101, 42, 32, 103, 101, 116, 59, 32, 92, 10, 32, 32, 32, 32, 125,
32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
99, 95, 99, 100, 101, 113, 95, 116, 121, 112, 101, 115, 40, 83, 69,
76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121,
112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35,
95, 118, 97, 108, 117, 101, 59, 32, 92, 10, 92, 10, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97,
116, 116, 114, 105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116,
104, 111, 100, 99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41,
41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
42, 99, 98, 117, 102, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 116, 97, 114, 116, 44,
32, 101, 110, 100, 44, 32, 99, 97, 112, 109, 97, 115, 107, 59, 32, 92,
10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 59, 32, 92, 10, 92, 10,
32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114,
117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83,
69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102,
59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 110, 116, 112,
116, 114, 95, 116, 32, 112, 111, 115, 59, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 83, 69, 76, 70, 42, 32,
95, 115, 59, 32, 92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 35,
35, 95, 105, 116, 101, 114, 10, 10, 35, 100, 101, 102, 105, 110, 101,
32, 95, 99, 95, 99, 108, 105, 115, 116, 95, 116, 121, 112, 101, 115,
40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32,
32, 116, 121, 112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76,
70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10, 32, 32, 32,
32, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99,
116, 32, 83, 69, 76, 70, 35, 35, 95, 110, 111, 100, 101, 32, 83, 69,
76, 70, 35, 35, 95, 110, 111, 100, 101, 59, 32, 92, 10, 92, 10, 32,
32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117,
99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69,
76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 59,
32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35,
95, 110, 111, 100, 101, 32, 42, 99, 111, 110, 115, 116, 32, 42, 95,
108, 97, 115, 116, 44, 32, 42, 112, 114, 101, 118, 59, 32, 92, 10, 32,
32, 32, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95, 105, 116, 101, 114,
59, 32, 92, 10, 92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115,
116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117,
116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108,
108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70,
32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70,
35, 35, 95, 110, 111, 100, 101, 32, 42, 108, 97, 115, 116, 59, 32, 92,
10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101,
102, 105, 110, 101, 32, 95, 99, 95, 99, 104, 97, 115, 104, 95, 116,
121, 112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 75, 69, 89, 44, 32,
86, 65, 76, 44, 32, 77, 65, 80, 95, 79, 78, 76, 89, 44, 32, 83, 69,
84, 95, 79, 78, 76, 89, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112,
101, 100, 101, 102, 32, 75, 69, 89, 32, 83, 69, 76, 70, 35, 35, 95,
107, 101, 121, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101,
100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 109,
97, 112, 112, 101, 100, 59, 32, 92, 10, 92, 10, 32, 32, 32, 32, 116,
121, 112, 101, 100, 101, 102, 32, 83, 69, 84, 95, 79, 78, 76, 89, 40,
32, 83, 69, 76, 70, 35, 35, 95, 107, 101, 121, 32, 41, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 77, 65, 80, 95, 79, 78,
76, 89, 40, 32, 115, 116, 114, 117, 99, 116, 32, 83, 69, 76, 70, 35,
35, 95, 118, 97, 108, 117, 101, 32, 41, 32, 92, 10, 32, 32, 32, 32,
83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10,
92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32, 115,
116, 114, 117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32,
32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42,
114, 101, 102, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95, 66,
111, 111, 108, 32, 105, 110, 115, 101, 114, 116, 101, 100, 59, 32, 92,
10, 32, 32, 32, 32, 32, 32, 32, 32, 117, 105, 110, 116, 56, 95, 116,
32, 104, 97, 115, 104, 120, 59, 32, 92, 10, 32, 32, 32, 32, 125, 32,
83, 69, 76, 70, 35, 35, 95, 114, 101, 115, 117, 108, 116, 59, 32, 92,
10, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32,
115, 116, 114, 117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
42, 114, 101, 102, 44, 32, 42, 95, 101, 110, 100, 59, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 115, 116, 114, 117, 99, 116, 32, 99, 104,
97, 115, 104, 95, 115, 108, 111, 116, 32, 42, 95, 115, 114, 101, 102,
59, 32, 92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95,
105, 116, 101, 114, 59, 32, 92, 10, 92, 10, 116, 121, 112, 101, 100,
101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116,
114, 105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111,
100, 99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32,
83, 69, 76, 70, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 42, 32, 116, 97,
98, 108, 101, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 115,
116, 114, 117, 99, 116, 32, 99, 104, 97, 115, 104, 95, 115, 108, 111,
116, 42, 32, 115, 108, 111, 116, 59, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 105, 122,
101, 44, 32, 98, 117, 99, 107, 101, 116, 95, 99, 111, 117, 110, 116,
59, 32, 92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35,
100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 97, 97, 116, 114, 101,
101, 95, 116, 121, 112, 101, 115, 40, 83, 69, 76, 70, 44, 32, 75, 69,
89, 44, 32, 86, 65, 76, 44, 32, 77, 65, 80, 95, 79, 78, 76, 89, 44,
32, 83, 69, 84, 95, 79, 78, 76, 89, 41, 32, 92, 10, 32, 32, 32, 32,
116, 121, 112, 101, 100, 101, 102, 32, 75, 69, 89, 32, 83, 69, 76, 70,
35, 35, 95, 107, 101, 121, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121,
112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35,
95, 109, 97, 112, 112, 101, 100, 59, 32, 92, 10, 32, 32, 32, 32, 116,
121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 83,
69, 76, 70, 35, 35, 95, 110, 111, 100, 101, 32, 83, 69, 76, 70, 35,
35, 95, 110, 111, 100, 101, 59, 32, 92, 10, 92, 10, 32, 32, 32, 32,
116, 121, 112, 101, 100, 101, 102, 32, 83, 69, 84, 95, 79, 78, 76, 89,
40, 32, 83, 69, 76, 70, 35, 35, 95, 107, 101, 121, 32, 41, 32, 92, 10,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 77, 65, 80, 95, 79,
78, 76, 89, 40, 32, 115, 116, 114, 117, 99, 116, 32, 83, 69, 76, 70,
35, 35, 95, 118, 97, 108, 117, 101, 32, 41, 32, 92, 10, 32, 32, 32,
32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92,
10, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102, 32,
115, 116, 114, 117, 99, 116, 32, 123, 32, 92, 10, 32, 32, 32, 32, 32,
32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
42, 114, 101, 102, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 95,
66, 111, 111, 108, 32, 105, 110, 115, 101, 114, 116, 101, 100, 59, 32,
92, 10, 32, 32, 32, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95, 114, 101,
115, 117, 108, 116, 59, 32, 92, 10, 92, 10, 32, 32, 32, 32, 116, 121,
112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32,
92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 59, 32, 92, 10, 32, 32,
32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 110, 111, 100,
101, 32, 42, 95, 100, 59, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32,
105, 110, 116, 32, 95, 116, 111, 112, 59, 32, 92, 10, 32, 32, 32, 32,
32, 32, 32, 32, 105, 110, 116, 51, 50, 95, 116, 32, 95, 116, 110, 44,
32, 95, 115, 116, 91, 51, 54, 93, 59, 32, 92, 10, 32, 32, 32, 32, 125,
32, 83, 69, 76, 70, 35, 35, 95, 105, 116, 101, 114, 59, 32, 92, 10,
92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115, 116, 114, 117, 99,
116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117, 116, 101, 95, 95,
40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108, 108, 40, 83, 69,
76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 92,
10, 32, 32, 32, 32, 32, 32, 32, 32, 83, 69, 76, 70, 35, 35, 95, 110,
111, 100, 101, 32, 42, 110, 111, 100, 101, 115, 59, 32, 92, 10, 32,
32, 32, 32, 32, 32, 32, 32, 105, 110, 116, 51, 50, 95, 116, 32, 114,
111, 111, 116, 44, 32, 100, 105, 115, 112, 44, 32, 104, 101, 97, 100,
44, 32, 115, 105, 122, 101, 44, 32, 99, 97, 112, 59, 32, 92, 10, 32,
32, 32, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105,
110, 101, 32, 95, 99, 95, 99, 115, 116, 97, 99, 107, 95, 102, 105,
120, 101, 100, 40, 83, 69, 76, 70, 44, 32, 86, 65, 76, 44, 32, 67, 65,
80, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101, 102,
32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117,
101, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 83, 69, 76, 70, 35,
35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 44, 32, 42,
101, 110, 100, 59, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95, 105, 116,
101, 114, 59, 32, 92, 10, 116, 121, 112, 101, 100, 101, 102, 32, 115,
116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114, 105, 98, 117,
116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100, 99, 97, 108,
108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69, 76, 70,
32, 123, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32,
100, 97, 116, 97, 91, 67, 65, 80, 93, 59, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 95, 108, 101, 110, 59, 32, 125, 32, 83, 69, 76, 70,
10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 99, 95, 99, 115,
116, 97, 99, 107, 95, 116, 121, 112, 101, 115, 40, 83, 69, 76, 70, 44,
32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101,
100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95, 118,
97, 108, 117, 101, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112, 101,
100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 83, 69,
76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102, 44,
32, 42, 101, 110, 100, 59, 32, 125, 32, 83, 69, 76, 70, 35, 35, 95,
105, 116, 101, 114, 59, 32, 92, 10, 116, 121, 112, 101, 100, 101, 102,
32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114, 105,
98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100, 99,
97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83, 69,
76, 70, 32, 123, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108, 117,
101, 42, 32, 100, 97, 116, 97, 59, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 95, 108, 101, 110, 44, 32, 95, 99, 97, 112, 59, 32, 125,
32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
99, 95, 99, 118, 101, 99, 95, 116, 121, 112, 101, 115, 40, 83, 69, 76,
70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112,
101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 59, 32, 92, 10, 32, 32, 32, 32, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 32, 83,
69, 76, 70, 35, 35, 95, 118, 97, 108, 117, 101, 32, 42, 114, 101, 102,
44, 32, 42, 101, 110, 100, 59, 32, 125, 32, 83, 69, 76, 70, 35, 35,
95, 105, 116, 101, 114, 59, 32, 92, 10, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97, 116, 116, 114,
105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116, 104, 111, 100,
99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41, 41, 41, 32, 83,
69, 76, 70, 32, 123, 32, 83, 69, 76, 70, 35, 35, 95, 118, 97, 108,
117, 101, 32, 42, 100, 97, 116, 97, 59, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 95, 108, 101, 110, 44, 32, 95, 99, 97, 112, 59, 32,
125, 32, 83, 69, 76, 70, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
95, 99, 95, 99, 112, 113, 117, 101, 95, 116, 121, 112, 101, 115, 40,
83, 69, 76, 70, 44, 32, 86, 65, 76, 41, 32, 92, 10, 32, 32, 32, 32,
116, 121, 112, 101, 100, 101, 102, 32, 86, 65, 76, 32, 83, 69, 76, 70,
35, 35, 95, 118, 97, 108, 117, 101, 59, 32, 92, 10, 116, 121, 112,
101, 100, 101, 102, 32, 115, 116, 114, 117, 99, 116, 32, 95, 95, 97,
116, 116, 114, 105, 98, 117, 116, 101, 95, 95, 40, 40, 109, 101, 116,
104, 111, 100, 99, 97, 108, 108, 40, 83, 69, 76, 70, 35, 35, 95, 41,
41, 41, 32, 83, 69, 76, 70, 32, 123, 32, 83, 69, 76, 70, 35, 35, 95,
118, 97, 108, 117, 101, 42, 32, 100, 97, 116, 97, 59, 32, 105, 110,
116, 112, 116, 114, 95, 116, 32, 95, 108, 101, 110, 44, 32, 95, 99,
97, 112, 59, 32, 125, 32, 83, 69, 76, 70, 10, 10, 35, 101, 110, 100,
105, 102, 32, 47, 47, 32, 83, 84, 67, 95, 70, 79, 82, 87, 65, 82, 68,
95, 72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 47, 47, 32, 35, 35,
35, 32, 69, 78, 68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68,
69, 58, 32, 102, 111, 114, 119, 97, 114, 100, 46, 104, 10, 47, 47, 32,
69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101,
110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97,
100, 101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100,
101, 32, 60, 115, 116, 100, 108, 105, 98, 46, 104, 62, 10, 47, 47, 32,
69, 88, 67, 76, 85, 68, 69, 68, 32, 66, 89, 32, 114, 101, 103, 101,
110, 95, 99, 111, 110, 116, 97, 105, 110, 101, 114, 95, 104, 101, 97,
100, 101, 114, 115, 46, 112, 121, 32, 35, 105, 110, 99, 108, 117, 100,
101, 32, 60, 115, 116, 114, 105, 110, 103, 46, 104, 62, 10, 10, 35,
100, 101, 102, 105, 110, 101, 32, 95, 105, 116, 50, 95, 112, 116, 114,
40, 105, 116, 49, 44, 32, 105, 116, 50, 41, 32, 40, 105, 116, 49, 46,
114, 101, 102, 32, 38, 38, 32, 33, 105, 116, 50, 46, 114, 101, 102,
32, 63, 32, 105, 116, 50, 46, 101, 110, 100, 32, 58, 32, 105, 116, 50,
46, 114, 101, 102, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
105, 116, 95, 112, 116, 114, 40, 105, 116, 41, 32, 40, 105, 116, 46,
114, 101, 102, 32, 63, 32, 105, 116, 46, 114, 101, 102, 32, 58, 32,
105, 116, 46, 101, 110, 100, 41, 10, 35, 101, 110, 100, 105, 102, 32,
47, 47, 32, 67, 86, 69, 67, 95, 72, 95, 73, 78, 67, 76, 85, 68, 69,
68, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 95, 105, 95, 112,
114, 101, 102, 105, 120, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 95, 105, 95, 112, 114, 101, 102, 105, 120, 32, 99, 118, 101, 99,
95, 10, 35, 101, 110, 100, 105, 102, 10, 47, 47, 32, 35, 35, 35, 32,
66, 69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68,
69, 58, 32, 116, 101, 109, 112, 108, 97, 116, 101, 46, 104, 10, 35,
105, 102, 110, 100, 101, 102, 32, 95, 105, 95, 116, 101, 109, 112,
108, 97, 116, 101, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105,
95, 116, 101, 109, 112, 108, 97, 116, 101, 10, 10, 35, 105, 102, 110,
100, 101, 102, 32, 83, 84, 67, 95, 84, 69, 77, 80, 76, 65, 84, 69, 95,
72, 95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 35, 100, 101, 102, 105,
110, 101, 32, 83, 84, 67, 95, 84, 69, 77, 80, 76, 65, 84, 69, 95, 72,
95, 73, 78, 67, 76, 85, 68, 69, 68, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 110, 97, 109, 101,
41, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 116, 121, 112, 101, 44,
32, 110, 97, 109, 101, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 95, 99, 95, 68, 69, 70, 84, 89, 80, 69, 83, 40, 109, 97, 99,
114, 111, 44, 32, 83, 69, 76, 70, 44, 32, 46, 46, 46, 41, 32, 99, 95,
69, 88, 80, 65, 78, 68, 40, 109, 97, 99, 114, 111, 40, 83, 69, 76, 70,
44, 32, 95, 95, 86, 65, 95, 65, 82, 71, 83, 95, 95, 41, 41, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109, 95, 118, 97, 108,
117, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117,
101, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109,
95, 107, 101, 121, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 107, 101,
121, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109,
95, 109, 97, 112, 112, 101, 100, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 109, 97, 112, 112, 101, 100, 41, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 109, 95, 114, 109, 97, 112, 112, 101, 100, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 109, 97, 112, 112, 101, 100,
41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109, 95,
114, 97, 119, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 97, 119,
41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109, 95,
107, 101, 121, 114, 97, 119, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
107, 101, 121, 114, 97, 119, 41, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 95, 109, 95, 105, 116, 101, 114, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 105, 116, 101, 114, 41, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 95, 109, 95, 114, 101, 115, 117, 108, 116, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 117, 108, 116, 41, 10,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 109, 95, 110, 111,
100, 101, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 110, 111, 100, 101,
41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102, 110, 100,
101, 102, 32, 105, 95, 116, 121, 112, 101, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 116, 121, 112, 101, 32, 99, 95, 74,
79, 73, 78, 40, 95, 105, 95, 112, 114, 101, 102, 105, 120, 44, 32,
105, 95, 116, 97, 103, 41, 10, 35, 101, 110, 100, 105, 102, 10, 10,
35, 105, 102, 100, 101, 102, 32, 105, 95, 107, 101, 121, 99, 108, 97,
115, 115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116,
101, 100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 105, 95, 107, 101,
121, 99, 108, 97, 115, 115, 10, 35, 101, 110, 100, 105, 102, 10, 35,
105, 102, 100, 101, 102, 32, 105, 95, 118, 97, 108, 99, 108, 97, 115,
115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101,
100, 93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
118, 97, 108, 95, 99, 108, 97, 115, 115, 32, 105, 95, 118, 97, 108,
99, 108, 97, 115, 115, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105,
102, 100, 101, 102, 32, 105, 95, 114, 97, 119, 99, 108, 97, 115, 115,
32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100,
93, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 114,
97, 119, 95, 99, 108, 97, 115, 115, 32, 105, 95, 114, 97, 119, 99,
108, 97, 115, 115, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102,
100, 101, 102, 32, 105, 95, 107, 101, 121, 98, 111, 120, 101, 100, 32,
47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93,
10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101,
121, 95, 97, 114, 99, 98, 111, 120, 32, 105, 95, 107, 101, 121, 98,
111, 120, 101, 100, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102,
100, 101, 102, 32, 105, 95, 118, 97, 108, 98, 111, 120, 101, 100, 32,
47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93,
10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97,
108, 95, 97, 114, 99, 98, 111, 120, 32, 105, 95, 118, 97, 108, 98,
111, 120, 101, 100, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105,
102, 32, 33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107,
101, 121, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 95, 115, 116, 114, 32, 124, 124, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 115, 115,
118, 32, 124, 124, 32, 92, 10, 32, 32, 32, 32, 32, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115,
115, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
107, 101, 121, 95, 97, 114, 99, 98, 111, 120, 41, 10, 32, 32, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95, 105, 115,
109, 97, 112, 10, 32, 32, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34,
105, 95, 107, 101, 121, 42, 32, 109, 117, 115, 116, 32, 98, 101, 32,
100, 101, 102, 105, 110, 101, 100, 32, 102, 111, 114, 32, 109, 97,
112, 115, 34, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 10, 32, 32,
35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118,
97, 108, 95, 115, 116, 114, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 95, 115, 116, 114, 32, 105,
95, 118, 97, 108, 95, 115, 116, 114, 10, 32, 32, 35, 101, 110, 100,
105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 118, 97, 108, 95, 115, 115, 118, 10, 32, 32, 32, 32,
35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 95, 115,
115, 118, 32, 105, 95, 118, 97, 108, 95, 115, 115, 118, 10, 32, 32,
35, 101, 110, 100, 105, 102, 32, 32, 10, 32, 32, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 95, 97,
114, 99, 98, 111, 120, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 107, 101, 121, 95, 97, 114, 99, 98, 111, 120,
32, 105, 95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 10, 32, 32,
35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 95, 99, 108, 97,
115, 115, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 105, 95, 118,
97, 108, 95, 99, 108, 97, 115, 115, 10, 32, 32, 35, 101, 110, 100,
105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 118, 97, 108, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 32, 105, 95, 118, 97, 108,
10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 114, 97,
119, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 114, 97, 119, 32, 105, 95, 118, 97, 108, 114, 97,
119, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108,
99, 108, 111, 110, 101, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 32, 105,
95, 118, 97, 108, 99, 108, 111, 110, 101, 10, 32, 32, 35, 101, 110,
100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 10, 32, 32,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121,
102, 114, 111, 109, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 10,
32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 116,
111, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 116, 111, 32, 105, 95, 118, 97, 108, 116, 111, 10,
32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 100,
114, 111, 112, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 32, 105, 95, 118, 97,
108, 100, 114, 111, 112, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10,
35, 101, 110, 100, 105, 102, 10, 10, 35, 100, 101, 102, 105, 110, 101,
32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 102, 108, 97, 103, 41,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 40, 105, 95, 111, 112,
116, 41, 32, 38, 32, 40, 102, 108, 97, 103, 41, 41, 10, 35, 100, 101,
102, 105, 110, 101, 32, 99, 95, 105, 115, 95, 102, 111, 114, 119, 97,
114, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60,
60, 48, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 110,
111, 95, 97, 116, 111, 109, 105, 99, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 40, 49, 60, 60, 49, 41, 10, 35, 100, 101, 102,
105, 110, 101, 32, 99, 95, 110, 111, 95, 99, 108, 111, 110, 101, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60, 60,
50, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 110, 111,
95, 101, 109, 112, 108, 97, 99, 101, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 40, 49, 60, 60, 51, 41, 10, 35, 100, 101, 102, 105,
110, 101, 32, 99, 95, 110, 111, 95, 104, 97, 115, 104, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60, 60, 52, 41,
10, 35, 100, 101, 102, 105, 110, 101, 32, 99, 95, 117, 115, 101, 95,
99, 109, 112, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 40, 49, 60, 60, 53, 41, 10, 35, 100, 101, 102, 105, 110, 101, 32,
99, 95, 109, 111, 114, 101, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 40, 49, 60, 60, 54, 41, 10, 10, 35,
105, 102, 32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 99, 95, 105,
115, 95, 102, 111, 114, 119, 97, 114, 100, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 105, 115, 95, 102, 111, 114,
119, 97, 114, 100, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102,
32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 99, 95, 110, 111, 95,
104, 97, 115, 104, 41, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 110, 111, 95, 104, 97, 115, 104, 10, 35, 101, 110, 100,
105, 102, 10, 35, 105, 102, 32, 99, 95, 111, 112, 116, 105, 111, 110,
40, 99, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 41, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 110, 111, 95, 101,
109, 112, 108, 97, 99, 101, 10, 35, 101, 110, 100, 105, 102, 10, 35,
105, 102, 32, 99, 95, 111, 112, 116, 105, 111, 110, 40, 99, 95, 117,
115, 101, 95, 99, 109, 112, 41, 32, 124, 124, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 99, 109, 112, 32, 124, 124, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115, 32, 124,
124, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 100, 101, 102,
105, 110, 101, 100, 32, 95, 105, 95, 105, 115, 109, 97, 112, 32, 124,
124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95, 105, 115,
115, 101, 116, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100,
32, 95, 105, 95, 105, 115, 112, 113, 117, 101, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 117, 115, 101, 95, 99, 109, 112,
10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 32, 99, 95, 111,
112, 116, 105, 111, 110, 40, 99, 95, 110, 111, 95, 99, 108, 111, 110,
101, 41, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95,
105, 95, 99, 97, 114, 99, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 10, 35, 101,
110, 100, 105, 102, 10, 35, 105, 102, 32, 99, 95, 111, 112, 116, 105,
111, 110, 40, 99, 95, 109, 111, 114, 101, 41, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 109, 111, 114, 101, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 107, 101, 121, 95, 115, 116, 114, 10, 32, 32,
35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 95, 99,
108, 97, 115, 115, 32, 99, 115, 116, 114, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115,
115, 32, 99, 99, 104, 97, 114, 112, 116, 114, 10, 32, 32, 35, 105,
102, 110, 100, 101, 102, 32, 105, 95, 116, 97, 103, 10, 32, 32, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 116, 97, 103, 32,
115, 116, 114, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 35, 101,
108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
107, 101, 121, 95, 115, 115, 118, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32,
99, 115, 116, 114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 32, 99, 115, 118,
105, 101, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 102, 114, 111, 109, 32, 99, 115, 116, 114, 95, 102,
114, 111, 109, 95, 115, 118, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 107, 101, 121, 116, 111, 32, 99, 115, 116, 114, 95,
115, 118, 10, 32, 32, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95,
116, 97, 103, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 116, 97, 103, 32, 115, 115, 118, 10, 32, 32, 35, 101,
110, 100, 105, 102, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 97, 114, 99, 98,
111, 120, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 105, 95, 107, 101, 121,
95, 97, 114, 99, 98, 111, 120, 10, 32, 32, 35, 100, 101, 102, 105,
110, 101, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 32,
99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 95, 97, 114, 99,
98, 111, 120, 44, 32, 95, 114, 97, 119, 41, 10, 32, 32, 35, 105, 102,
32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 117, 115, 101, 95,
99, 109, 112, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 101, 113, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 107,
101, 121, 95, 97, 114, 99, 98, 111, 120, 44, 32, 95, 114, 97, 119, 95,
101, 113, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115, 10,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121,
114, 97, 119, 32, 105, 95, 114, 97, 119, 95, 99, 108, 97, 115, 115,
10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 32, 38, 38, 32, 33,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 114,
97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
114, 97, 119, 95, 99, 108, 97, 115, 115, 32, 105, 95, 107, 101, 121,
10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97,
115, 115, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
107, 101, 121, 32, 105, 95, 107, 101, 121, 95, 99, 108, 97, 115, 115,
10, 32, 32, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101,
121, 99, 108, 111, 110, 101, 10, 32, 32, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 32,
99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 44, 32, 95, 99,
108, 111, 110, 101, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10,
32, 32, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121,
100, 114, 111, 112, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 32, 99, 95, 74,
79, 73, 78, 40, 105, 95, 107, 101, 121, 44, 32, 95, 100, 114, 111,
112, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101,
121, 102, 114, 111, 109, 32, 38, 38, 32, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 102, 114,
111, 109, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 44,
32, 95, 102, 114, 111, 109, 41, 10, 32, 32, 35, 101, 110, 100, 105,
102, 10, 32, 32, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 116, 111, 32, 38, 38, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10,
32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107,
101, 121, 116, 111, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 107, 101,
121, 44, 32, 95, 116, 111, 114, 97, 119, 41, 10, 32, 32, 35, 101, 110,
100, 105, 102, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102,
32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 114, 97, 119, 95,
99, 108, 97, 115, 115, 10, 32, 32, 35, 105, 102, 32, 33, 40, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 99, 109, 112, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115,
41, 32, 38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
117, 115, 101, 95, 99, 109, 112, 10, 32, 32, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 99, 109, 112, 32, 99, 95, 74, 79, 73,
78, 40, 105, 95, 107, 101, 121, 114, 97, 119, 44, 32, 95, 99, 109,
112, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105,
102, 32, 33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 104,
97, 115, 104, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 110, 111, 95, 104, 97, 115, 104, 41, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 104, 97, 115, 104, 32, 99,
95, 74, 79, 73, 78, 40, 105, 95, 107, 101, 121, 114, 97, 119, 44, 32,
95, 104, 97, 115, 104, 41, 10, 32, 32, 35, 101, 110, 100, 105, 102,
10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 99, 109, 112, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115,
32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 117,
115, 101, 95, 99, 109, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10, 35, 101,
110, 100, 105, 102, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 101, 113, 32, 124, 124, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 117, 115, 101, 95, 99, 109, 112, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 95, 105, 95, 104, 97, 115,
95, 101, 113, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 32,
33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 104, 97, 115,
104, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
110, 111, 95, 104, 97, 115, 104, 41, 10, 32, 32, 35, 100, 101, 102,
105, 110, 101, 32, 105, 95, 104, 97, 115, 104, 32, 99, 95, 100, 101,
102, 97, 117, 108, 116, 95, 104, 97, 115, 104, 10, 35, 101, 110, 100,
105, 102, 10, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 10, 32, 32, 35, 101, 114, 114, 111,
114, 32, 34, 78, 111, 32, 105, 95, 107, 101, 121, 32, 111, 114, 32,
105, 95, 118, 97, 108, 32, 100, 101, 102, 105, 110, 101, 100, 34, 10,
35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 114, 97, 119, 32, 94, 32, 100, 101, 102, 105,
110, 101, 100, 32, 105, 95, 107, 101, 121, 116, 111, 10, 32, 32, 35,
101, 114, 114, 111, 114, 32, 34, 66, 111, 116, 104, 32, 105, 95, 107,
101, 121, 114, 97, 119, 47, 105, 95, 118, 97, 108, 114, 97, 119, 32,
97, 110, 100, 32, 105, 95, 107, 101, 121, 116, 111, 47, 105, 95, 118,
97, 108, 116, 111, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101,
102, 105, 110, 101, 100, 44, 32, 105, 102, 32, 97, 110, 121, 34, 10,
35, 101, 108, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 32, 38, 38, 32, 40,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 99,
108, 111, 110, 101, 32, 94, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 100, 114, 111, 112, 41, 10, 32, 32, 35, 101,
114, 114, 111, 114, 32, 34, 66, 111, 116, 104, 32, 105, 95, 107, 101,
121, 99, 108, 111, 110, 101, 47, 105, 95, 118, 97, 108, 99, 108, 111,
110, 101, 32, 97, 110, 100, 32, 105, 95, 107, 101, 121, 100, 114, 111,
112, 47, 105, 95, 118, 97, 108, 100, 114, 111, 112, 32, 109, 117, 115,
116, 32, 98, 101, 32, 100, 101, 102, 105, 110, 101, 100, 44, 32, 105,
102, 32, 97, 110, 121, 32, 40, 117, 110, 108, 101, 115, 115, 32, 105,
95, 110, 111, 95, 99, 108, 111, 110, 101, 32, 100, 101, 102, 105, 110,
101, 100, 41, 46, 34, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 102, 114, 111, 109, 32, 124, 124, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 100, 114, 111, 112,
10, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34, 105, 95, 102, 114,
111, 109, 32, 47, 32, 105, 95, 100, 114, 111, 112, 32, 110, 111, 116,
32, 115, 117, 112, 112, 111, 114, 116, 101, 100, 46, 32, 68, 101, 102,
105, 110, 101, 32, 105, 95, 107, 101, 121, 102, 114, 111, 109, 47,
105, 95, 118, 97, 108, 102, 114, 111, 109, 32, 97, 110, 100, 47, 111,
114, 32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 47, 105, 95, 118,
97, 108, 100, 114, 111, 112, 32, 105, 110, 115, 116, 101, 97, 100, 34,
10, 35, 101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32,
105, 95, 107, 101, 121, 114, 97, 119, 32, 38, 38, 32, 100, 101, 102,
105, 110, 101, 100, 32, 95, 105, 95, 105, 115, 104, 97, 115, 104, 32,
38, 38, 32, 33, 40, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
104, 97, 115, 104, 32, 38, 38, 32, 40, 100, 101, 102, 105, 110, 101,
100, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 32, 124, 124,
32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 101, 113, 41, 41,
10, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34, 70, 111, 114, 32, 99,
109, 97, 112, 47, 99, 115, 101, 116, 44, 32, 98, 111, 116, 104, 32,
105, 95, 104, 97, 115, 104, 32, 97, 110, 100, 32, 105, 95, 101, 113,
32, 40, 111, 114, 32, 105, 95, 108, 101, 115, 115, 32, 111, 114, 32,
105, 95, 99, 109, 112, 41, 32, 109, 117, 115, 116, 32, 98, 101, 32,
100, 101, 102, 105, 110, 101, 100, 32, 119, 104, 101, 110, 32, 105,
95, 107, 101, 121, 114, 97, 119, 32, 105, 115, 32, 100, 101, 102, 105,
110, 101, 100, 46, 34, 10, 35, 101, 108, 105, 102, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 32, 38,
38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 117, 115, 101,
95, 99, 109, 112, 32, 38, 38, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10, 32, 32, 35,
101, 114, 114, 111, 114, 32, 34, 70, 111, 114, 32, 99, 115, 109, 97,
112, 47, 99, 115, 115, 101, 116, 47, 99, 112, 113, 117, 101, 44, 32,
105, 95, 99, 109, 112, 32, 111, 114, 32, 105, 95, 108, 101, 115, 115,
32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105, 110, 101,
100, 32, 119, 104, 101, 110, 32, 105, 95, 107, 101, 121, 114, 97, 119,
32, 105, 115, 32, 100, 101, 102, 105, 110, 101, 100, 46, 34, 10, 35,
101, 110, 100, 105, 102, 10, 10, 47, 47, 32, 105, 95, 101, 113, 44,
32, 105, 95, 108, 101, 115, 115, 44, 32, 105, 95, 99, 109, 112, 10,
35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
101, 113, 32, 38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105,
95, 99, 109, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32,
105, 95, 101, 113, 40, 120, 44, 32, 121, 41, 32, 33, 40, 105, 95, 99,
109, 112, 40, 120, 44, 32, 121, 41, 41, 10, 35, 101, 108, 105, 102,
32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 101, 113, 32,
38, 38, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 107,
101, 121, 114, 97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 101, 113, 40, 120, 44, 32, 121, 41, 32, 42, 120, 32, 61,
61, 32, 42, 121, 32, 47, 47, 32, 102, 111, 114, 32, 105, 110, 116,
101, 103, 114, 97, 108, 32, 116, 121, 112, 101, 115, 44, 32, 101, 108,
115, 101, 32, 100, 101, 102, 105, 110, 101, 32, 105, 95, 101, 113, 32,
111, 114, 32, 105, 95, 99, 109, 112, 32, 121, 111, 117, 114, 115, 101,
108, 102, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 32, 33,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115,
32, 38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 99,
109, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
108, 101, 115, 115, 40, 120, 44, 32, 121, 41, 32, 40, 105, 95, 99,
109, 112, 40, 120, 44, 32, 121, 41, 41, 32, 60, 32, 48, 10, 35, 101,
108, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
108, 101, 115, 115, 32, 38, 38, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 108, 101, 115, 115, 40, 120, 44,
32, 121, 41, 32, 42, 120, 32, 60, 32, 42, 121, 32, 47, 47, 32, 102,
111, 114, 32, 105, 110, 116, 101, 103, 114, 97, 108, 32, 116, 121,
112, 101, 115, 44, 32, 101, 108, 115, 101, 32, 100, 101, 102, 105,
110, 101, 32, 105, 95, 108, 101, 115, 115, 32, 111, 114, 32, 105, 95,
99, 109, 112, 32, 121, 111, 117, 114, 115, 101, 108, 102, 10, 35, 101,
110, 100, 105, 102, 10, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 99, 109, 112, 32, 38, 38, 32, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 108, 101, 115, 115, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 99, 109, 112, 40, 120, 44,
32, 121, 41, 32, 40, 105, 95, 108, 101, 115, 115, 40, 121, 44, 32,
120, 41, 41, 32, 45, 32, 40, 105, 95, 108, 101, 115, 115, 40, 120, 44,
32, 121, 41, 41, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 105,
102, 110, 100, 101, 102, 32, 105, 95, 116, 97, 103, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 116, 97, 103, 32, 105, 95,
107, 101, 121, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 32, 32,
35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 107, 101, 121, 114, 97,
119, 32, 105, 95, 107, 101, 121, 10, 35, 101, 110, 100, 105, 102, 10,
35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 102,
114, 111, 109, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 102, 114, 111, 109, 32, 99, 95, 100, 101, 102, 97,
117, 108, 116, 95, 99, 108, 111, 110, 101, 10, 35, 101, 108, 115, 101,
10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 104, 97,
115, 95, 101, 109, 112, 108, 97, 99, 101, 10, 35, 101, 110, 100, 105,
102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121,
116, 111, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
107, 101, 121, 116, 111, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116,
95, 116, 111, 114, 97, 119, 10, 35, 101, 110, 100, 105, 102, 10, 35,
105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 99, 108,
111, 110, 101, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 107, 101, 121, 99, 108, 111, 110, 101, 32, 99, 95, 100, 101, 102,
97, 117, 108, 116, 95, 99, 108, 111, 110, 101, 10, 35, 101, 110, 100,
105, 102, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 107, 101,
121, 100, 114, 111, 112, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 32, 99, 95, 100, 101,
102, 97, 117, 108, 116, 95, 100, 114, 111, 112, 10, 35, 101, 110, 100,
105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100,
32, 95, 105, 95, 105, 115, 109, 97, 112, 32, 47, 47, 32, 45, 45, 45,
45, 32, 112, 114, 111, 99, 101, 115, 115, 32, 99, 109, 97, 112, 47,
99, 115, 109, 97, 112, 32, 118, 97, 108, 117, 101, 32, 105, 95, 118,
97, 108, 44, 32, 46, 46, 46, 32, 45, 45, 45, 45, 10, 10, 35, 105, 102,
100, 101, 102, 32, 105, 95, 118, 97, 108, 95, 115, 116, 114, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 95,
99, 108, 97, 115, 115, 32, 99, 115, 116, 114, 10, 32, 32, 35, 100,
101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 114, 97, 119, 32,
99, 111, 110, 115, 116, 32, 99, 104, 97, 114, 42, 10, 35, 101, 108,
105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 95, 115, 115, 118, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101,
32, 105, 95, 118, 97, 108, 95, 99, 108, 97, 115, 115, 32, 99, 115,
116, 114, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
118, 97, 108, 114, 97, 119, 32, 99, 115, 118, 105, 101, 119, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 102,
114, 111, 109, 32, 99, 115, 116, 114, 95, 102, 114, 111, 109, 95, 115,
118, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118,
97, 108, 116, 111, 32, 99, 115, 116, 114, 95, 115, 118, 10, 35, 101,
108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 95, 99, 108, 97, 115,
115, 32, 105, 95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 114,
97, 119, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 118, 97, 108, 95,
97, 114, 99, 98, 111, 120, 44, 32, 95, 114, 97, 119, 41, 10, 35, 101,
110, 100, 105, 102, 10, 10, 35, 105, 102, 100, 101, 102, 32, 105, 95,
118, 97, 108, 95, 99, 108, 97, 115, 115, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 32, 105, 95, 118, 97,
108, 95, 99, 108, 97, 115, 115, 10, 32, 32, 35, 105, 102, 110, 100,
101, 102, 32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101, 10, 32,
32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97,
108, 99, 108, 111, 110, 101, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95,
118, 97, 108, 44, 32, 95, 99, 108, 111, 110, 101, 41, 10, 32, 32, 35,
101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 100, 114, 111, 112, 10, 32, 32, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 100, 114,
111, 112, 32, 99, 95, 74, 79, 73, 78, 40, 105, 95, 118, 97, 108, 44,
32, 95, 100, 114, 111, 112, 41, 10, 32, 32, 35, 101, 110, 100, 105,
102, 10, 32, 32, 35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101,
100, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 32, 38, 38, 32,
100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 114, 97,
119, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105,
95, 118, 97, 108, 102, 114, 111, 109, 32, 99, 95, 74, 79, 73, 78, 40,
105, 95, 118, 97, 108, 44, 32, 95, 102, 114, 111, 109, 41, 10, 32, 32,
35, 101, 110, 100, 105, 102, 10, 32, 32, 35, 105, 102, 32, 33, 100,
101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 116, 111, 32,
38, 38, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 114, 97, 119, 10, 32, 32, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 118, 97, 108, 116, 111, 32, 99, 95, 74, 79, 73, 78,
40, 105, 95, 118, 97, 108, 44, 32, 95, 116, 111, 114, 97, 119, 41, 10,
32, 32, 35, 101, 110, 100, 105, 102, 10, 35, 101, 110, 100, 105, 102,
10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108,
10, 32, 32, 35, 101, 114, 114, 111, 114, 32, 34, 105, 95, 118, 97,
108, 42, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105,
110, 101, 100, 32, 102, 111, 114, 32, 109, 97, 112, 115, 34, 10, 35,
101, 108, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105,
95, 118, 97, 108, 114, 97, 119, 32, 94, 32, 100, 101, 102, 105, 110,
101, 100, 32, 105, 95, 118, 97, 108, 116, 111, 10, 32, 32, 35, 101,
114, 114, 111, 114, 32, 34, 66, 111, 116, 104, 32, 105, 95, 118, 97,
108, 114, 97, 119, 32, 97, 110, 100, 32, 105, 95, 118, 97, 108, 116,
111, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105, 110,
101, 100, 44, 32, 105, 102, 32, 97, 110, 121, 34, 10, 35, 101, 108,
105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110,
111, 95, 99, 108, 111, 110, 101, 32, 38, 38, 32, 40, 100, 101, 102,
105, 110, 101, 100, 32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101,
32, 94, 32, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 118, 97,
108, 100, 114, 111, 112, 41, 10, 32, 32, 35, 101, 114, 114, 111, 114,
32, 34, 66, 111, 116, 104, 32, 105, 95, 118, 97, 108, 99, 108, 111,
110, 101, 32, 97, 110, 100, 32, 105, 95, 118, 97, 108, 100, 114, 111,
112, 32, 109, 117, 115, 116, 32, 98, 101, 32, 100, 101, 102, 105, 110,
101, 100, 44, 32, 105, 102, 32, 97, 110, 121, 34, 10, 35, 101, 110,
100, 105, 102, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95,
118, 97, 108, 114, 97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110,
101, 32, 105, 95, 118, 97, 108, 114, 97, 119, 32, 105, 95, 118, 97,
108, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101,
102, 32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 102, 114,
111, 109, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 108,
111, 110, 101, 10, 35, 101, 108, 115, 101, 10, 32, 32, 35, 100, 101,
102, 105, 110, 101, 32, 105, 95, 104, 97, 115, 95, 101, 109, 112, 108,
97, 99, 101, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 118, 97, 108, 116, 111, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 116, 111, 32,
99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 116, 111, 114, 97, 119,
10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 99, 108, 111, 110, 101, 10, 32, 32, 35,
100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 99, 108, 111,
110, 101, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 99, 108,
111, 110, 101, 10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 118, 97, 108, 100, 114, 111, 112, 10, 32,
32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118, 97, 108, 100,
114, 111, 112, 32, 99, 95, 100, 101, 102, 97, 117, 108, 116, 95, 100,
114, 111, 112, 10, 35, 101, 110, 100, 105, 102, 10, 10, 35, 101, 110,
100, 105, 102, 32, 47, 47, 32, 33, 95, 105, 95, 105, 115, 109, 97,
112, 10, 10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 118, 97,
108, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 118,
97, 108, 32, 105, 95, 107, 101, 121, 10, 35, 101, 110, 100, 105, 102,
10, 35, 105, 102, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108, 114,
97, 119, 10, 32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95,
118, 97, 108, 114, 97, 119, 32, 105, 95, 107, 101, 121, 114, 97, 119,
10, 35, 101, 110, 100, 105, 102, 10, 35, 105, 102, 110, 100, 101, 102,
32, 105, 95, 104, 97, 115, 95, 101, 109, 112, 108, 97, 99, 101, 10,
32, 32, 35, 100, 101, 102, 105, 110, 101, 32, 105, 95, 110, 111, 95,
101, 109, 112, 108, 97, 99, 101, 10, 35, 101, 110, 100, 105, 102, 10,
35, 101, 110, 100, 105, 102, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78,
68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 116,
101, 109, 112, 108, 97, 116, 101, 46, 104, 10, 10, 35, 105, 102, 110,
100, 101, 102, 32, 105, 95, 105, 115, 95, 102, 111, 114, 119, 97, 114,
100, 10, 32, 32, 32, 95, 99, 95, 68, 69, 70, 84, 89, 80, 69, 83, 40,
95, 99, 95, 99, 118, 101, 99, 95, 116, 121, 112, 101, 115, 44, 32,
105, 95, 116, 121, 112, 101, 44, 32, 105, 95, 107, 101, 121, 41, 59,
10, 35, 101, 110, 100, 105, 102, 10, 116, 121, 112, 101, 100, 101,
102, 32, 105, 95, 107, 101, 121, 114, 97, 119, 32, 95, 109, 95, 114,
97, 119, 59, 10, 83, 84, 67, 95, 65, 80, 73, 32, 105, 95, 116, 121,
112, 101, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 105, 110, 105, 116, 41, 40, 118, 111, 105, 100, 41,
59, 10, 83, 84, 67, 95, 65, 80, 73, 32, 118, 111, 105, 100, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 100, 114, 111, 112, 41, 40, 105, 95, 116, 121, 112, 101, 42,
32, 115, 101, 108, 102, 41, 59, 10, 83, 84, 67, 95, 65, 80, 73, 32,
118, 111, 105, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 101, 97, 114, 41, 40,
105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41, 59, 10,
83, 84, 67, 95, 65, 80, 73, 32, 95, 66, 111, 111, 108, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
114, 101, 115, 101, 114, 118, 101, 41, 40, 105, 95, 116, 121, 112,
101, 42, 32, 115, 101, 108, 102, 44, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 99, 97, 112, 41, 59, 10, 83, 84, 67, 95, 65, 80, 73, 32,
95, 66, 111, 111, 108, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 105, 122, 101, 41,
40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 115, 105, 122, 101, 44, 32,
95, 109, 95, 118, 97, 108, 117, 101, 32, 110, 117, 108, 108, 41, 59,
10, 83, 84, 67, 95, 65, 80, 73, 32, 95, 109, 95, 105, 116, 101, 114,
32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 101, 114, 97, 115, 101, 95, 110, 41, 40, 105, 95, 116, 121, 112,
101, 42, 32, 115, 101, 108, 102, 44, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 105, 100, 120, 44, 32, 105, 110, 116, 112, 116, 114, 95,
116, 32, 110, 41, 59, 10, 83, 84, 67, 95, 65, 80, 73, 32, 95, 109, 95,
105, 116, 101, 114, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 105, 110, 115, 101, 114, 116, 95, 117, 110,
105, 110, 105, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105,
100, 120, 44, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 41,
59, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95,
105, 95, 104, 97, 115, 95, 101, 113, 32, 124, 124, 32, 100, 101, 102,
105, 110, 101, 100, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112,
10, 83, 84, 67, 95, 65, 80, 73, 32, 95, 109, 95, 105, 116, 101, 114,
32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 102, 105, 110, 100, 95, 105, 110, 41, 40, 95, 109, 95, 105, 116,
101, 114, 32, 105, 116, 49, 44, 32, 95, 109, 95, 105, 116, 101, 114,
32, 105, 116, 50, 44, 32, 95, 109, 95, 114, 97, 119, 32, 114, 97, 119,
41, 59, 10, 35, 101, 110, 100, 105, 102, 10, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 32, 32, 32, 32, 32, 32,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117, 101,
95, 100, 114, 111, 112, 41, 40, 95, 109, 95, 118, 97, 108, 117, 101,
42, 32, 118, 97, 108, 41, 32, 123, 32, 105, 95, 107, 101, 121, 100,
114, 111, 112, 40, 118, 97, 108, 41, 59, 32, 125, 10, 10, 83, 84, 67,
95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108, 117, 101,
42, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112, 117, 115,
104, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102,
44, 32, 95, 109, 95, 118, 97, 108, 117, 101, 32, 118, 97, 108, 117,
101, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 105,
102, 32, 40, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 32, 61,
61, 32, 115, 101, 108, 102, 45, 62, 95, 99, 97, 112, 41, 10, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 33, 95, 99, 95,
77, 69, 77, 66, 40, 95, 114, 101, 115, 101, 114, 118, 101, 41, 40,
115, 101, 108, 102, 44, 32, 115, 101, 108, 102, 45, 62, 95, 108, 101,
110, 42, 50, 32, 43, 32, 52, 41, 41, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 78,
85, 76, 76, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95,
109, 95, 118, 97, 108, 117, 101, 32, 42, 118, 32, 61, 32, 115, 101,
108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 115, 101, 108, 102,
45, 62, 95, 108, 101, 110, 43, 43, 59, 10, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 42, 118, 32, 61, 32, 118, 97, 108, 117, 101, 59, 10,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114,
110, 32, 118, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 125, 10, 10, 35, 105,
102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110, 111,
95, 101, 109, 112, 108, 97, 99, 101, 10, 83, 84, 67, 95, 65, 80, 73,
32, 95, 109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66,
40, 95, 101, 109, 112, 108, 97, 99, 101, 95, 110, 41, 40, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 105, 110, 116,
112, 116, 114, 95, 116, 32, 105, 100, 120, 44, 32, 99, 111, 110, 115,
116, 32, 95, 109, 95, 114, 97, 119, 32, 114, 97, 119, 91, 93, 44, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 41, 59, 10, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108,
117, 101, 42, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 109, 112,
108, 97, 99, 101, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 95, 109, 95, 114, 97, 119, 32, 114, 97, 119,
41, 32, 123, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 112, 117, 115, 104, 41, 40, 115, 101,
108, 102, 44, 32, 105, 95, 107, 101, 121, 102, 114, 111, 109, 40, 114,
97, 119, 41, 41, 59, 10, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78,
69, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 101, 109, 112, 108, 97, 99, 101, 95, 98, 97, 99,
107, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102,
44, 32, 95, 109, 95, 114, 97, 119, 32, 114, 97, 119, 41, 32, 123, 10,
32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 112, 117, 115, 104, 41, 40, 115, 101, 108, 102,
44, 32, 105, 95, 107, 101, 121, 102, 114, 111, 109, 40, 114, 97, 119,
41, 41, 59, 10, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
95, 109, 95, 105, 116, 101, 114, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 101, 109, 112, 108, 97, 99, 101, 95, 97, 116, 41, 40, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95,
105, 116, 101, 114, 32, 105, 116, 44, 32, 95, 109, 95, 114, 97, 119,
32, 114, 97, 119, 41, 32, 123, 10, 32, 32, 32, 32, 114, 101, 116, 117,
114, 110, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 109, 112, 108,
97, 99, 101, 95, 110, 41, 40, 115, 101, 108, 102, 44, 32, 95, 105,
116, 95, 112, 116, 114, 40, 105, 116, 41, 32, 45, 32, 115, 101, 108,
102, 45, 62, 100, 97, 116, 97, 44, 32, 38, 114, 97, 119, 44, 32, 49,
41, 59, 10, 125, 10, 35, 101, 110, 100, 105, 102, 32, 47, 47, 32, 33,
105, 95, 110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 10, 10, 35,
105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95, 110,
111, 95, 99, 108, 111, 110, 101, 10, 83, 84, 67, 95, 65, 80, 73, 32,
105, 95, 116, 121, 112, 101, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 111, 110, 101, 41, 40,
105, 95, 116, 121, 112, 101, 32, 99, 120, 41, 59, 10, 83, 84, 67, 95,
65, 80, 73, 32, 95, 109, 95, 105, 116, 101, 114, 32, 32, 32, 32, 32,
32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 111, 112, 121,
95, 110, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 44, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 100, 120,
44, 32, 99, 111, 110, 115, 116, 32, 95, 109, 95, 118, 97, 108, 117,
101, 32, 97, 114, 114, 91, 93, 44, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 110, 41, 59, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69,
32, 118, 111, 105, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 112, 117, 116, 95, 110, 41, 40, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110,
115, 116, 32, 95, 109, 95, 114, 97, 119, 42, 32, 114, 97, 119, 44, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 110, 41, 10, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 119, 104, 105, 108, 101, 32,
40, 110, 45, 45, 41, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112, 117,
115, 104, 41, 40, 115, 101, 108, 102, 44, 32, 105, 95, 107, 101, 121,
102, 114, 111, 109, 40, 42, 114, 97, 119, 43, 43, 41, 41, 59, 32, 125,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 95, 116, 121,
112, 101, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 102, 114, 111, 109, 95, 110, 41, 40, 99, 111, 110, 115, 116, 32,
95, 109, 95, 114, 97, 119, 42, 32, 114, 97, 119, 44, 32, 105, 110,
116, 112, 116, 114, 95, 116, 32, 110, 41, 10, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 123, 32, 105, 95, 116, 121, 112, 101, 32, 99, 120,
32, 61, 32, 123, 48, 125, 59, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
112, 117, 116, 95, 110, 41, 40, 38, 99, 120, 44, 32, 114, 97, 119, 44,
32, 110, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32, 99, 120, 59,
32, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95,
118, 97, 108, 117, 101, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77,
66, 40, 95, 118, 97, 108, 117, 101, 95, 99, 108, 111, 110, 101, 41,
40, 95, 109, 95, 118, 97, 108, 117, 101, 32, 118, 97, 108, 41, 10, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 114, 101, 116, 117,
114, 110, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 40, 118,
97, 108, 41, 59, 32, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69,
32, 118, 111, 105, 100, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 99, 111, 112, 121, 41, 40, 105, 95, 116,
121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110, 115,
116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 111, 116, 104, 101, 114,
41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102,
32, 40, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 61, 61, 32,
111, 116, 104, 101, 114, 45, 62, 100, 97, 116, 97, 41, 32, 114, 101,
116, 117, 114, 110, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 101, 97, 114, 41, 40,
115, 101, 108, 102, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 111, 112, 121, 95,
110, 41, 40, 115, 101, 108, 102, 44, 32, 48, 44, 32, 111, 116, 104,
101, 114, 45, 62, 100, 97, 116, 97, 44, 32, 111, 116, 104, 101, 114,
45, 62, 95, 108, 101, 110, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 125,
10, 35, 101, 110, 100, 105, 102, 32, 47, 47, 32, 33, 105, 95, 110,
111, 95, 99, 108, 111, 110, 101, 10, 10, 83, 84, 67, 95, 73, 78, 76,
73, 78, 69, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 32, 32, 32,
32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 115, 105, 122, 101, 41, 40,
99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 41, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 115,
101, 108, 102, 45, 62, 95, 108, 101, 110, 59, 32, 125, 10, 83, 84, 67,
95, 73, 78, 76, 73, 78, 69, 32, 105, 110, 116, 112, 116, 114, 95, 116,
32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 97, 112,
97, 99, 105, 116, 121, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 41, 32, 123, 32, 114,
101, 116, 117, 114, 110, 32, 115, 101, 108, 102, 45, 62, 95, 99, 97,
112, 59, 32, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95,
66, 111, 111, 108, 32, 32, 32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 101, 109, 112, 116, 121, 41, 40, 99, 111, 110,
115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102,
41, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 33, 115, 101, 108,
102, 45, 62, 95, 108, 101, 110, 59, 32, 125, 10, 83, 84, 67, 95, 73,
78, 76, 73, 78, 69, 32, 95, 109, 95, 114, 97, 119, 32, 32, 32, 32, 32,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117, 101,
95, 116, 111, 114, 97, 119, 41, 40, 99, 111, 110, 115, 116, 32, 95,
109, 95, 118, 97, 108, 117, 101, 42, 32, 118, 97, 108, 41, 32, 123,
32, 114, 101, 116, 117, 114, 110, 32, 105, 95, 107, 101, 121, 116,
111, 40, 118, 97, 108, 41, 59, 32, 125, 10, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 32,
32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 102, 114, 111, 110, 116,
41, 40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42,
32, 115, 101, 108, 102, 41, 32, 123, 32, 114, 101, 116, 117, 114, 110,
32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 59, 32, 125, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108,
117, 101, 42, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 98,
97, 99, 107, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 41, 10, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 115,
101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 115, 101, 108,
102, 45, 62, 95, 108, 101, 110, 32, 45, 32, 49, 59, 32, 125, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 32,
32, 32, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112,
111, 112, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 99,
95, 97, 115, 115, 101, 114, 116, 40, 115, 101, 108, 102, 45, 62, 95,
108, 101, 110, 41, 59, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42,
32, 112, 32, 61, 32, 38, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97,
91, 45, 45, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 93, 59, 32,
105, 95, 107, 101, 121, 100, 114, 111, 112, 40, 112, 41, 59, 32, 125,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97,
108, 117, 101, 32, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
112, 117, 108, 108, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 123,
32, 99, 95, 97, 115, 115, 101, 114, 116, 40, 115, 101, 108, 102, 45,
62, 95, 108, 101, 110, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32,
115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 91, 45, 45, 115, 101,
108, 102, 45, 62, 95, 108, 101, 110, 93, 59, 32, 125, 10, 83, 84, 67,
95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108, 117, 101,
42, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112, 117, 115,
104, 95, 98, 97, 99, 107, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 44, 32, 95, 109, 95, 118, 97, 108, 117, 101, 32,
118, 97, 108, 117, 101, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 112, 117, 115, 104, 41, 40, 115, 101, 108, 102, 44,
32, 118, 97, 108, 117, 101, 41, 59, 32, 125, 10, 83, 84, 67, 95, 73,
78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 32, 32, 32, 32, 32,
32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112, 111, 112, 95, 98,
97, 99, 107, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101,
108, 102, 41, 32, 123, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 112,
111, 112, 41, 40, 115, 101, 108, 102, 41, 59, 32, 125, 10, 10, 83, 84,
67, 95, 73, 78, 76, 73, 78, 69, 32, 105, 95, 116, 121, 112, 101, 10,
95, 99, 95, 77, 69, 77, 66, 40, 95, 119, 105, 116, 104, 95, 115, 105,
122, 101, 41, 40, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 115, 105, 122, 101, 44, 32, 95, 109, 95, 118, 97,
108, 117, 101, 32, 110, 117, 108, 108, 41, 32, 123, 10, 32, 32, 32,
32, 105, 95, 116, 121, 112, 101, 32, 99, 120, 32, 61, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 105, 110, 105, 116, 41, 40, 41, 59, 10, 32,
32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 105,
122, 101, 41, 40, 38, 99, 120, 44, 32, 115, 105, 122, 101, 44, 32,
110, 117, 108, 108, 41, 59, 10, 32, 32, 32, 32, 114, 101, 116, 117,
114, 110, 32, 99, 120, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 105, 95, 116, 121, 112, 101, 10, 95, 99, 95, 77,
69, 77, 66, 40, 95, 119, 105, 116, 104, 95, 99, 97, 112, 97, 99, 105,
116, 121, 41, 40, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 99, 97, 112, 41, 32, 123, 10, 32, 32, 32, 32, 105,
95, 116, 121, 112, 101, 32, 99, 120, 32, 61, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 105, 110, 105, 116, 41, 40, 41, 59, 10, 32, 32, 32,
32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 101, 114, 118,
101, 41, 40, 38, 99, 120, 44, 32, 99, 97, 112, 41, 59, 10, 32, 32, 32,
32, 114, 101, 116, 117, 114, 110, 32, 99, 120, 59, 10, 125, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 10,
95, 99, 95, 77, 69, 77, 66, 40, 95, 115, 104, 114, 105, 110, 107, 95,
116, 111, 95, 102, 105, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42,
32, 115, 101, 108, 102, 41, 32, 123, 10, 32, 32, 32, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 114, 101, 115, 101, 114, 118, 101, 41, 40,
115, 101, 108, 102, 44, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 115,
105, 122, 101, 41, 40, 115, 101, 108, 102, 41, 41, 59, 10, 125, 10,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 105, 116,
101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115, 101,
114, 116, 95, 110, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112,
116, 114, 95, 116, 32, 105, 100, 120, 44, 32, 99, 111, 110, 115, 116,
32, 95, 109, 95, 118, 97, 108, 117, 101, 32, 97, 114, 114, 91, 93, 44,
32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116,
32, 110, 41, 32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 105, 116, 101,
114, 32, 105, 116, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
105, 110, 115, 101, 114, 116, 95, 117, 110, 105, 110, 105, 116, 41,
40, 115, 101, 108, 102, 44, 32, 105, 100, 120, 44, 32, 110, 41, 59,
10, 32, 32, 32, 32, 105, 102, 32, 40, 105, 116, 46, 114, 101, 102, 41,
10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 95, 109, 101, 109, 99, 112,
121, 40, 105, 116, 46, 114, 101, 102, 44, 32, 97, 114, 114, 44, 32,
110, 42, 99, 95, 115, 105, 122, 101, 111, 102, 32, 42, 97, 114, 114,
41, 59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 105,
116, 59, 10, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95,
109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95,
105, 110, 115, 101, 114, 116, 95, 97, 116, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 105, 116,
101, 114, 32, 105, 116, 44, 32, 99, 111, 110, 115, 116, 32, 95, 109,
95, 118, 97, 108, 117, 101, 32, 118, 97, 108, 117, 101, 41, 32, 123,
10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 95, 99, 95, 77,
69, 77, 66, 40, 95, 105, 110, 115, 101, 114, 116, 95, 110, 41, 40,
115, 101, 108, 102, 44, 32, 95, 105, 116, 95, 112, 116, 114, 40, 105,
116, 41, 32, 45, 32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 44,
32, 38, 118, 97, 108, 117, 101, 44, 32, 49, 41, 59, 10, 125, 10, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 105, 116,
101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 114, 97, 115,
101, 95, 97, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 95, 109, 95, 105, 116, 101, 114, 32, 105, 116,
41, 32, 123, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 95,
99, 95, 77, 69, 77, 66, 40, 95, 101, 114, 97, 115, 101, 95, 110, 41,
40, 115, 101, 108, 102, 44, 32, 105, 116, 46, 114, 101, 102, 32, 45,
32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 44, 32, 49, 41, 59,
10, 125, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95,
105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 114,
97, 115, 101, 95, 114, 97, 110, 103, 101, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95, 105, 116,
101, 114, 32, 105, 49, 44, 32, 95, 109, 95, 105, 116, 101, 114, 32,
105, 50, 41, 32, 123, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114,
110, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 114, 97, 115, 101,
95, 110, 41, 40, 115, 101, 108, 102, 44, 32, 105, 49, 46, 114, 101,
102, 32, 45, 32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 44, 32,
95, 105, 116, 50, 95, 112, 116, 114, 40, 105, 49, 44, 32, 105, 50, 41,
32, 45, 32, 105, 49, 46, 114, 101, 102, 41, 59, 10, 125, 10, 10, 83,
84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 99, 111, 110, 115, 116, 32,
95, 109, 95, 118, 97, 108, 117, 101, 42, 10, 95, 99, 95, 77, 69, 77,
66, 40, 95, 97, 116, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95, 116,
121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110, 115,
116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 100, 120, 41,
32, 123, 10, 32, 32, 32, 32, 99, 95, 97, 115, 115, 101, 114, 116, 40,
105, 100, 120, 32, 60, 32, 115, 101, 108, 102, 45, 62, 95, 108, 101,
110, 41, 59, 32, 114, 101, 116, 117, 114, 110, 32, 115, 101, 108, 102,
45, 62, 100, 97, 116, 97, 32, 43, 32, 105, 100, 120, 59, 10, 125, 10,
83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 118, 97, 108,
117, 101, 42, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 97, 116, 95,
109, 117, 116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101,
108, 102, 44, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 105, 100, 120, 41, 32, 123, 10, 32, 32, 32, 32, 99,
95, 97, 115, 115, 101, 114, 116, 40, 105, 100, 120, 32, 60, 32, 115,
101, 108, 102, 45, 62, 95, 108, 101, 110, 41, 59, 32, 114, 101, 116,
117, 114, 110, 32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 32,
43, 32, 105, 100, 120, 59, 10, 125, 10, 10, 10, 83, 84, 67, 95, 73,
78, 76, 73, 78, 69, 32, 95, 109, 95, 105, 116, 101, 114, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 98, 101, 103, 105, 110, 41, 40, 99, 111,
110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 41, 32, 123, 32, 10, 32, 32, 32, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 110, 32, 61, 32, 115, 101, 108, 102, 45, 62, 95,
108, 101, 110, 59, 32, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114,
110, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 95, 109, 95, 105,
116, 101, 114, 41, 123, 110, 32, 63, 32, 115, 101, 108, 102, 45, 62,
100, 97, 116, 97, 32, 58, 32, 78, 85, 76, 76, 44, 32, 115, 101, 108,
102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 110, 125, 59, 10, 125, 10,
10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 105, 116,
101, 114, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 110, 100, 41,
40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 41, 32, 10, 32, 32, 32, 32, 123, 32, 114, 101,
116, 117, 114, 110, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 95,
109, 95, 105, 116, 101, 114, 41, 123, 78, 85, 76, 76, 44, 32, 115,
101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 115, 101, 108,
102, 45, 62, 95, 108, 101, 110, 125, 59, 32, 125, 10, 10, 83, 84, 67,
95, 73, 78, 76, 73, 78, 69, 32, 118, 111, 105, 100, 32, 95, 99, 95,
77, 69, 77, 66, 40, 95, 110, 101, 120, 116, 41, 40, 95, 109, 95, 105,
116, 101, 114, 42, 32, 105, 116, 41, 32, 10, 32, 32, 32, 32, 123, 32,
105, 102, 32, 40, 43, 43, 105, 116, 45, 62, 114, 101, 102, 32, 61, 61,
32, 105, 116, 45, 62, 101, 110, 100, 41, 32, 105, 116, 45, 62, 114,
101, 102, 32, 61, 32, 78, 85, 76, 76, 59, 32, 125, 10, 10, 83, 84, 67,
95, 73, 78, 76, 73, 78, 69, 32, 95, 109, 95, 105, 116, 101, 114, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 97, 100, 118, 97, 110, 99, 101,
41, 40, 95, 109, 95, 105, 116, 101, 114, 32, 105, 116, 44, 32, 115,
105, 122, 101, 95, 116, 32, 110, 41, 10, 32, 32, 32, 32, 123, 32, 105,
102, 32, 40, 40, 105, 116, 46, 114, 101, 102, 32, 43, 61, 32, 110, 41,
32, 62, 61, 32, 105, 116, 46, 101, 110, 100, 41, 32, 105, 116, 46,
114, 101, 102, 32, 61, 32, 78, 85, 76, 76, 59, 32, 114, 101, 116, 117,
114, 110, 32, 105, 116, 59, 32, 125, 10, 10, 83, 84, 67, 95, 73, 78,
76, 73, 78, 69, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 95, 99,
95, 77, 69, 77, 66, 40, 95, 105, 110, 100, 101, 120, 41, 40, 99, 111,
110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 44, 32, 95, 109, 95, 105, 116, 101, 114, 32, 105, 116, 41, 32,
10, 32, 32, 32, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 40,
105, 116, 46, 114, 101, 102, 32, 45, 32, 115, 101, 108, 102, 45, 62,
100, 97, 116, 97, 41, 59, 32, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76,
73, 78, 69, 32, 118, 111, 105, 100, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 97, 100, 106, 117, 115, 116, 95, 101, 110, 100, 95, 41, 40,
105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 105,
110, 116, 112, 116, 114, 95, 116, 32, 110, 41, 10, 32, 32, 32, 32,
123, 32, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 32, 43, 61,
32, 110, 59, 32, 125, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105,
110, 101, 100, 32, 95, 105, 95, 104, 97, 115, 95, 101, 113, 32, 124,
124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95, 104, 97,
115, 95, 99, 109, 112, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32,
95, 109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40,
95, 102, 105, 110, 100, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95,
114, 97, 119, 32, 114, 97, 119, 41, 32, 123, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 102,
105, 110, 100, 95, 105, 110, 41, 40, 95, 99, 95, 77, 69, 77, 66, 40,
95, 98, 101, 103, 105, 110, 41, 40, 115, 101, 108, 102, 41, 44, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 101, 110, 100, 41, 40, 115, 101,
108, 102, 41, 44, 32, 114, 97, 119, 41, 59, 10, 125, 10, 10, 83, 84,
67, 95, 73, 78, 76, 73, 78, 69, 32, 99, 111, 110, 115, 116, 32, 95,
109, 95, 118, 97, 108, 117, 101, 42, 10, 95, 99, 95, 77, 69, 77, 66,
40, 95, 103, 101, 116, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 95, 109, 95,
114, 97, 119, 32, 114, 97, 119, 41, 32, 123, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 102,
105, 110, 100, 41, 40, 115, 101, 108, 102, 44, 32, 114, 97, 119, 41,
46, 114, 101, 102, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76,
73, 78, 69, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 10, 95, 99,
95, 77, 69, 77, 66, 40, 95, 103, 101, 116, 95, 109, 117, 116, 41, 40,
99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 95, 109, 95, 114, 97, 119, 32, 114, 97, 119,
41, 10, 32, 32, 32, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 40,
95, 109, 95, 118, 97, 108, 117, 101, 42, 41, 32, 95, 99, 95, 77, 69,
77, 66, 40, 95, 103, 101, 116, 41, 40, 115, 101, 108, 102, 44, 32,
114, 97, 119, 41, 59, 32, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76, 73,
78, 69, 32, 95, 66, 111, 111, 108, 10, 95, 99, 95, 77, 69, 77, 66, 40,
95, 101, 113, 41, 40, 99, 111, 110, 115, 116, 32, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110, 115, 116,
32, 105, 95, 116, 121, 112, 101, 42, 32, 111, 116, 104, 101, 114, 41,
32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108, 102, 45,
62, 95, 108, 101, 110, 32, 33, 61, 32, 111, 116, 104, 101, 114, 45,
62, 95, 108, 101, 110, 41, 32, 114, 101, 116, 117, 114, 110, 32, 48,
59, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110, 116, 112,
116, 114, 95, 116, 32, 105, 32, 61, 32, 48, 59, 32, 105, 32, 60, 32,
115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 59, 32, 43, 43, 105,
41, 32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111, 110, 115,
116, 32, 95, 109, 95, 114, 97, 119, 32, 95, 114, 120, 32, 61, 32, 105,
95, 107, 101, 121, 116, 111, 40, 115, 101, 108, 102, 45, 62, 100, 97,
116, 97, 43, 105, 41, 44, 32, 95, 114, 121, 32, 61, 32, 105, 95, 107,
101, 121, 116, 111, 40, 111, 116, 104, 101, 114, 45, 62, 100, 97, 116,
97, 43, 105, 41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32,
40, 33, 40, 105, 95, 101, 113, 40, 40, 38, 95, 114, 120, 41, 44, 32,
40, 38, 95, 114, 121, 41, 41, 41, 41, 32, 114, 101, 116, 117, 114,
110, 32, 48, 59, 10, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 49, 59, 10, 125, 10, 35, 101, 110, 100,
105, 102, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101, 100,
32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10, 83, 84, 67, 95,
65, 80, 73, 32, 105, 110, 116, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
118, 97, 108, 117, 101, 95, 99, 109, 112, 41, 40, 99, 111, 110, 115,
116, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 120, 44, 32, 99,
111, 110, 115, 116, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32,
121, 41, 59, 10, 10, 83, 84, 67, 95, 73, 78, 76, 73, 78, 69, 32, 118,
111, 105, 100, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 115, 111, 114,
116, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102,
41, 32, 123, 10, 32, 32, 32, 32, 113, 115, 111, 114, 116, 40, 115,
101, 108, 102, 45, 62, 100, 97, 116, 97, 44, 32, 40, 115, 105, 122,
101, 95, 116, 41, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 44,
32, 115, 105, 122, 101, 111, 102, 40, 95, 109, 95, 118, 97, 108, 117,
101, 41, 44, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 105, 110,
116, 40, 42, 41, 40, 99, 111, 110, 115, 116, 32, 118, 111, 105, 100,
42, 44, 32, 99, 111, 110, 115, 116, 32, 118, 111, 105, 100, 42, 41,
41, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117, 101, 95,
99, 109, 112, 41, 41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 73, 78, 76,
73, 78, 69, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 10, 95, 99,
95, 77, 69, 77, 66, 40, 95, 98, 115, 101, 97, 114, 99, 104, 41, 40,
99, 111, 110, 115, 116, 32, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 95, 109, 95, 118, 97, 108, 117, 101, 32, 107,
101, 121, 41, 32, 123, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114,
110, 32, 40, 95, 109, 95, 118, 97, 108, 117, 101, 42, 41, 98, 115,
101, 97, 114, 99, 104, 40, 38, 107, 101, 121, 44, 32, 115, 101, 108,
102, 45, 62, 100, 97, 116, 97, 44, 32, 40, 115, 105, 122, 101, 95,
116, 41, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 44, 32, 115,
105, 122, 101, 111, 102, 40, 95, 109, 95, 118, 97, 108, 117, 101, 41,
44, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 40, 105,
110, 116, 40, 42, 41, 40, 99, 111, 110, 115, 116, 32, 118, 111, 105,
100, 42, 44, 32, 99, 111, 110, 115, 116, 32, 118, 111, 105, 100, 42,
41, 41, 95, 99, 95, 77, 69, 77, 66, 40, 95, 118, 97, 108, 117, 101,
95, 99, 109, 112, 41, 41, 59, 10, 125, 10, 35, 101, 110, 100, 105,
102, 32, 47, 47, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10,
10, 47, 42, 32, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 32, 73, 77, 80,
76, 69, 77, 69, 78, 84, 65, 84, 73, 79, 78, 32, 45, 45, 45, 45, 45,
45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
45, 45, 45, 32, 42, 47, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 40, 105, 95, 105, 109, 112, 108, 101, 109, 101, 110, 116,
41, 32, 124, 124, 32, 100, 101, 102, 105, 110, 101, 100, 40, 105, 95,
115, 116, 97, 116, 105, 99, 41, 10, 10, 83, 84, 67, 95, 68, 69, 70,
32, 105, 95, 116, 121, 112, 101, 10, 95, 99, 95, 77, 69, 77, 66, 40,
95, 105, 110, 105, 116, 41, 40, 118, 111, 105, 100, 41, 32, 123, 10,
32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 99, 95, 76, 73, 84,
69, 82, 65, 76, 40, 105, 95, 116, 121, 112, 101, 41, 123, 78, 85, 76,
76, 125, 59, 10, 125, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 118,
111, 105, 100, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 101,
97, 114, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115, 101, 108,
102, 41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108,
102, 45, 62, 95, 99, 97, 112, 41, 32, 123, 10, 32, 32, 32, 32, 32, 32,
32, 32, 102, 111, 114, 32, 40, 95, 109, 95, 118, 97, 108, 117, 101,
32, 42, 112, 32, 61, 32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97,
44, 32, 42, 113, 32, 61, 32, 112, 32, 43, 32, 115, 101, 108, 102, 45,
62, 95, 108, 101, 110, 59, 32, 112, 32, 33, 61, 32, 113, 59, 32, 41,
32, 123, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 45, 45,
113, 59, 32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 40, 113, 41,
59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32, 32,
32, 32, 32, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 32, 61, 32,
48, 59, 10, 32, 32, 32, 32, 125, 10, 125, 10, 10, 83, 84, 67, 95, 68,
69, 70, 32, 118, 111, 105, 100, 10, 95, 99, 95, 77, 69, 77, 66, 40,
95, 100, 114, 111, 112, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32,
115, 101, 108, 102, 41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40,
115, 101, 108, 102, 45, 62, 95, 99, 97, 112, 32, 61, 61, 32, 48, 41,
10, 32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 59,
10, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 101,
97, 114, 41, 40, 115, 101, 108, 102, 41, 59, 10, 32, 32, 32, 32, 105,
95, 102, 114, 101, 101, 40, 115, 101, 108, 102, 45, 62, 100, 97, 116,
97, 44, 32, 115, 101, 108, 102, 45, 62, 95, 99, 97, 112, 42, 99, 95,
115, 105, 122, 101, 111, 102, 40, 42, 115, 101, 108, 102, 45, 62, 100,
97, 116, 97, 41, 41, 59, 10, 125, 10, 10, 83, 84, 67, 95, 68, 69, 70,
32, 95, 66, 111, 111, 108, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95,
114, 101, 115, 101, 114, 118, 101, 41, 40, 105, 95, 116, 121, 112,
101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110, 115, 116, 32,
105, 110, 116, 112, 116, 114, 95, 116, 32, 99, 97, 112, 41, 32, 123,
10, 32, 32, 32, 32, 105, 102, 32, 40, 99, 97, 112, 32, 62, 32, 115,
101, 108, 102, 45, 62, 95, 99, 97, 112, 32, 124, 124, 32, 40, 99, 97,
112, 32, 38, 38, 32, 99, 97, 112, 32, 61, 61, 32, 115, 101, 108, 102,
45, 62, 95, 108, 101, 110, 41, 41, 32, 123, 10, 32, 32, 32, 32, 32,
32, 32, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 100, 32, 61,
32, 40, 95, 109, 95, 118, 97, 108, 117, 101, 42, 41, 105, 95, 114,
101, 97, 108, 108, 111, 99, 40, 115, 101, 108, 102, 45, 62, 100, 97,
116, 97, 44, 32, 115, 101, 108, 102, 45, 62, 95, 99, 97, 112, 42, 99,
95, 115, 105, 122, 101, 111, 102, 32, 42, 100, 44, 10, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
99, 97, 112, 42, 99, 95, 115, 105, 122, 101, 111, 102, 32, 42, 100,
41, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 105, 102, 32, 40, 33, 100,
41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 114, 101, 116,
117, 114, 110, 32, 48, 59, 10, 32, 32, 32, 32, 32, 32, 32, 32, 115,
101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 61, 32, 100, 59, 10, 32,
32, 32, 32, 32, 32, 32, 32, 115, 101, 108, 102, 45, 62, 95, 99, 97,
112, 32, 61, 32, 99, 97, 112, 59, 10, 32, 32, 32, 32, 125, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 49, 59, 10, 125, 10, 10, 83,
84, 67, 95, 68, 69, 70, 32, 95, 66, 111, 111, 108, 10, 95, 99, 95, 77,
69, 77, 66, 40, 95, 114, 101, 115, 105, 122, 101, 41, 40, 105, 95,
116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110,
115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 108, 101,
110, 44, 32, 95, 109, 95, 118, 97, 108, 117, 101, 32, 110, 117, 108,
108, 41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 33, 95, 99,
95, 77, 69, 77, 66, 40, 95, 114, 101, 115, 101, 114, 118, 101, 41, 40,
115, 101, 108, 102, 44, 32, 108, 101, 110, 41, 41, 10, 32, 32, 32, 32,
32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 48, 59, 10, 32, 32,
32, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95,
116, 32, 110, 32, 61, 32, 115, 101, 108, 102, 45, 62, 95, 108, 101,
110, 59, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110, 116,
112, 116, 114, 95, 116, 32, 105, 32, 61, 32, 108, 101, 110, 59, 32,
105, 32, 60, 32, 110, 59, 32, 43, 43, 105, 41, 10, 32, 32, 32, 32, 32,
32, 32, 32, 123, 32, 105, 95, 107, 101, 121, 100, 114, 111, 112, 40,
40, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 105, 41,
41, 59, 32, 125, 10, 32, 32, 32, 32, 102, 111, 114, 32, 40, 105, 110,
116, 112, 116, 114, 95, 116, 32, 105, 32, 61, 32, 110, 59, 32, 105,
32, 60, 32, 108, 101, 110, 59, 32, 43, 43, 105, 41, 10, 32, 32, 32,
32, 32, 32, 32, 32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 91,
105, 93, 32, 61, 32, 110, 117, 108, 108, 59, 10, 32, 32, 32, 32, 115,
101, 108, 102, 45, 62, 95, 108, 101, 110, 32, 61, 32, 108, 101, 110,
59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 49, 59, 10,
125, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95, 105, 116,
101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 115, 101,
114, 116, 95, 117, 110, 105, 110, 105, 116, 41, 40, 105, 95, 116, 121,
112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111, 110, 115, 116,
32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 100, 120, 44, 32,
99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32,
110, 41, 32, 123, 10, 32, 32, 32, 32, 105, 102, 32, 40, 115, 101, 108,
102, 45, 62, 95, 108, 101, 110, 32, 43, 32, 110, 32, 62, 32, 115, 101,
108, 102, 45, 62, 95, 99, 97, 112, 41, 10, 32, 32, 32, 32, 32, 32, 32,
32, 105, 102, 32, 40, 33, 95, 99, 95, 77, 69, 77, 66, 40, 95, 114,
101, 115, 101, 114, 118, 101, 41, 40, 115, 101, 108, 102, 44, 32, 115,
101, 108, 102, 45, 62, 95, 108, 101, 110, 42, 51, 47, 50, 32, 43, 32,
110, 41, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 101,
110, 100, 41, 40, 115, 101, 108, 102, 41, 59, 10, 10, 32, 32, 32, 32,
95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 112, 111, 115, 32, 61,
32, 115, 101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 105,
100, 120, 59, 10, 32, 32, 32, 32, 99, 95, 109, 101, 109, 109, 111,
118, 101, 40, 112, 111, 115, 32, 43, 32, 110, 44, 32, 112, 111, 115,
44, 32, 40, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 32, 45, 32,
105, 100, 120, 41, 42, 99, 95, 115, 105, 122, 101, 111, 102, 32, 42,
112, 111, 115, 41, 59, 10, 32, 32, 32, 32, 115, 101, 108, 102, 45, 62,
95, 108, 101, 110, 32, 43, 61, 32, 110, 59, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40,
95, 109, 95, 105, 116, 101, 114, 41, 123, 112, 111, 115, 44, 32, 115,
101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 115, 101, 108,
102, 45, 62, 95, 108, 101, 110, 125, 59, 10, 125, 10, 10, 83, 84, 67,
95, 68, 69, 70, 32, 95, 109, 95, 105, 116, 101, 114, 10, 95, 99, 95,
77, 69, 77, 66, 40, 95, 101, 114, 97, 115, 101, 95, 110, 41, 40, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111,
110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105,
100, 120, 44, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116,
114, 95, 116, 32, 108, 101, 110, 41, 32, 123, 10, 32, 32, 32, 32, 95,
109, 95, 118, 97, 108, 117, 101, 42, 32, 100, 32, 61, 32, 115, 101,
108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 105, 100, 120, 44, 32,
42, 112, 32, 61, 32, 100, 44, 32, 42, 101, 110, 100, 32, 61, 32, 115,
101, 108, 102, 45, 62, 100, 97, 116, 97, 32, 43, 32, 115, 101, 108,
102, 45, 62, 95, 108, 101, 110, 59, 10, 32, 32, 32, 32, 102, 111, 114,
32, 40, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105, 32, 61, 32,
48, 59, 32, 105, 32, 60, 32, 108, 101, 110, 59, 32, 43, 43, 105, 44,
32, 43, 43, 112, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 123, 32, 105,
95, 107, 101, 121, 100, 114, 111, 112, 40, 112, 41, 59, 32, 125, 10,
32, 32, 32, 32, 99, 95, 109, 101, 109, 109, 111, 118, 101, 40, 100,
44, 32, 112, 44, 32, 40, 101, 110, 100, 32, 45, 32, 112, 41, 42, 99,
95, 115, 105, 122, 101, 111, 102, 32, 42, 100, 41, 59, 10, 32, 32, 32,
32, 115, 101, 108, 102, 45, 62, 95, 108, 101, 110, 32, 45, 61, 32,
108, 101, 110, 59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110,
32, 99, 95, 76, 73, 84, 69, 82, 65, 76, 40, 95, 109, 95, 105, 116,
101, 114, 41, 123, 112, 32, 61, 61, 32, 101, 110, 100, 32, 63, 32, 78,
85, 76, 76, 32, 58, 32, 100, 44, 32, 101, 110, 100, 32, 45, 32, 108,
101, 110, 125, 59, 10, 125, 10, 10, 35, 105, 102, 32, 33, 100, 101,
102, 105, 110, 101, 100, 32, 105, 95, 110, 111, 95, 99, 108, 111, 110,
101, 10, 83, 84, 67, 95, 68, 69, 70, 32, 105, 95, 116, 121, 112, 101,
10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 108, 111, 110, 101, 41,
40, 105, 95, 116, 121, 112, 101, 32, 99, 120, 41, 32, 123, 10, 32, 32,
32, 32, 105, 95, 116, 121, 112, 101, 32, 111, 117, 116, 32, 61, 32,
95, 99, 95, 77, 69, 77, 66, 40, 95, 105, 110, 105, 116, 41, 40, 41,
59, 10, 32, 32, 32, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 111,
112, 121, 95, 110, 41, 40, 38, 111, 117, 116, 44, 32, 48, 44, 32, 99,
120, 46, 100, 97, 116, 97, 44, 32, 99, 120, 46, 95, 108, 101, 110, 41,
59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 111, 117,
116, 59, 10, 125, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95, 109, 95,
105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95, 99, 111,
112, 121, 95, 110, 41, 40, 105, 95, 116, 121, 112, 101, 42, 32, 115,
101, 108, 102, 44, 32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112,
116, 114, 95, 116, 32, 105, 100, 120, 44, 10, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111, 110, 115, 116,
32, 95, 109, 95, 118, 97, 108, 117, 101, 32, 97, 114, 114, 91, 93, 44,
32, 99, 111, 110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116,
32, 110, 41, 32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 105, 116, 101,
114, 32, 105, 116, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66, 40, 95,
105, 110, 115, 101, 114, 116, 95, 117, 110, 105, 110, 105, 116, 41,
40, 115, 101, 108, 102, 44, 32, 105, 100, 120, 44, 32, 110, 41, 59,
10, 32, 32, 32, 32, 105, 102, 32, 40, 105, 116, 46, 114, 101, 102, 41,
10, 32, 32, 32, 32, 32, 32, 32, 32, 102, 111, 114, 32, 40, 95, 109,
95, 118, 97, 108, 117, 101, 42, 32, 112, 32, 61, 32, 105, 116, 46,
114, 101, 102, 44, 32, 42, 113, 32, 61, 32, 112, 32, 43, 32, 110, 59,
32, 112, 32, 33, 61, 32, 113, 59, 32, 43, 43, 97, 114, 114, 41, 10,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 42, 112, 43, 43, 32,
61, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 40, 40, 42,
97, 114, 114, 41, 41, 59, 10, 32, 32, 32, 32, 114, 101, 116, 117, 114,
110, 32, 105, 116, 59, 10, 125, 10, 35, 101, 110, 100, 105, 102, 32,
47, 47, 32, 33, 105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 10, 10,
35, 105, 102, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 105, 95,
110, 111, 95, 101, 109, 112, 108, 97, 99, 101, 10, 83, 84, 67, 95, 68,
69, 70, 32, 95, 109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69,
77, 66, 40, 95, 101, 109, 112, 108, 97, 99, 101, 95, 110, 41, 40, 105,
95, 116, 121, 112, 101, 42, 32, 115, 101, 108, 102, 44, 32, 99, 111,
110, 115, 116, 32, 105, 110, 116, 112, 116, 114, 95, 116, 32, 105,
100, 120, 44, 32, 99, 111, 110, 115, 116, 32, 95, 109, 95, 114, 97,
119, 32, 114, 97, 119, 91, 93, 44, 32, 105, 110, 116, 112, 116, 114,
95, 116, 32, 110, 41, 32, 123, 10, 32, 32, 32, 32, 95, 109, 95, 105,
116, 101, 114, 32, 105, 116, 32, 61, 32, 95, 99, 95, 77, 69, 77, 66,
40, 95, 105, 110, 115, 101, 114, 116, 95, 117, 110, 105, 110, 105,
116, 41, 40, 115, 101, 108, 102, 44, 32, 105, 100, 120, 44, 32, 110,
41, 59, 10, 32, 32, 32, 32, 105, 102, 32, 40, 105, 116, 46, 114, 101,
102, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 102, 111, 114, 32, 40,
95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 112, 32, 61, 32, 105,
116, 46, 114, 101, 102, 59, 32, 110, 45, 45, 59, 32, 43, 43, 114, 97,
119, 44, 32, 43, 43, 112, 41, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 42, 112, 32, 61, 32, 105, 95, 107, 101, 121, 102, 114,
111, 109, 40, 40, 42, 114, 97, 119, 41, 41, 59, 10, 32, 32, 32, 32,
114, 101, 116, 117, 114, 110, 32, 105, 116, 59, 10, 125, 10, 35, 101,
110, 100, 105, 102, 32, 47, 47, 32, 33, 105, 95, 110, 111, 95, 101,
109, 112, 108, 97, 99, 101, 10, 35, 105, 102, 32, 100, 101, 102, 105,
110, 101, 100, 32, 95, 105, 95, 104, 97, 115, 95, 101, 113, 32, 124,
124, 32, 100, 101, 102, 105, 110, 101, 100, 32, 95, 105, 95, 104, 97,
115, 95, 99, 109, 112, 10, 10, 83, 84, 67, 95, 68, 69, 70, 32, 95,
109, 95, 105, 116, 101, 114, 10, 95, 99, 95, 77, 69, 77, 66, 40, 95,
102, 105, 110, 100, 95, 105, 110, 41, 40, 95, 109, 95, 105, 116, 101,
114, 32, 105, 49, 44, 32, 95, 109, 95, 105, 116, 101, 114, 32, 105,
50, 44, 32, 95, 109, 95, 114, 97, 119, 32, 114, 97, 119, 41, 32, 123,
10, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 95, 109, 95, 118, 97,
108, 117, 101, 42, 32, 112, 50, 32, 61, 32, 95, 105, 116, 50, 95, 112,
116, 114, 40, 105, 49, 44, 32, 105, 50, 41, 59, 10, 32, 32, 32, 32,
102, 111, 114, 32, 40, 59, 32, 105, 49, 46, 114, 101, 102, 32, 33, 61,
32, 112, 50, 59, 32, 43, 43, 105, 49, 46, 114, 101, 102, 41, 32, 123,
10, 32, 32, 32, 32, 32, 32, 32, 32, 99, 111, 110, 115, 116, 32, 95,
109, 95, 114, 97, 119, 32, 114, 32, 61, 32, 105, 95, 107, 101, 121,
116, 111, 40, 105, 49, 46, 114, 101, 102, 41, 59, 10, 32, 32, 32, 32,
32, 32, 32, 32, 105, 102, 32, 40, 105, 95, 101, 113, 40, 40, 38, 114,
97, 119, 41, 44, 32, 40, 38, 114, 41, 41, 41, 10, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 114, 101, 116, 117, 114, 110, 32, 105, 49,
59, 10, 32, 32, 32, 32, 125, 10, 32, 32, 32, 32, 105, 50, 46, 114,
101, 102, 32, 61, 32, 78, 85, 76, 76, 59, 10, 32, 32, 32, 32, 114,
101, 116, 117, 114, 110, 32, 105, 50, 59, 10, 125, 10, 35, 101, 110,
100, 105, 102, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110, 101,
100, 32, 95, 105, 95, 104, 97, 115, 95, 99, 109, 112, 10, 83, 84, 67,
95, 68, 69, 70, 32, 105, 110, 116, 32, 95, 99, 95, 77, 69, 77, 66, 40,
95, 118, 97, 108, 117, 101, 95, 99, 109, 112, 41, 40, 99, 111, 110,
115, 116, 32, 95, 109, 95, 118, 97, 108, 117, 101, 42, 32, 120, 44,
32, 99, 111, 110, 115, 116, 32, 95, 109, 95, 118, 97, 108, 117, 101,
42, 32, 121, 41, 32, 123, 10, 32, 32, 32, 32, 99, 111, 110, 115, 116,
32, 95, 109, 95, 114, 97, 119, 32, 114, 120, 32, 61, 32, 105, 95, 107,
101, 121, 116, 111, 40, 120, 41, 59, 10, 32, 32, 32, 32, 99, 111, 110,
115, 116, 32, 95, 109, 95, 114, 97, 119, 32, 114, 121, 32, 61, 32,
105, 95, 107, 101, 121, 116, 111, 40, 121, 41, 59, 10, 32, 32, 32, 32,
114, 101, 116, 117, 114, 110, 32, 105, 95, 99, 109, 112, 40, 40, 38,
114, 120, 41, 44, 32, 40, 38, 114, 121, 41, 41, 59, 10, 125, 10, 35,
101, 110, 100, 105, 102, 32, 47, 47, 32, 95, 105, 95, 104, 97, 115,
95, 99, 109, 112, 10, 35, 101, 110, 100, 105, 102, 32, 47, 47, 32,
105, 95, 105, 109, 112, 108, 101, 109, 101, 110, 116, 10, 35, 100,
101, 102, 105, 110, 101, 32, 67, 86, 69, 67, 95, 72, 95, 73, 78, 67,
76, 85, 68, 69, 68, 10, 47, 47, 32, 35, 35, 35, 32, 66, 69, 71, 73,
78, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 116,
101, 109, 112, 108, 97, 116, 101, 50, 46, 104, 10, 35, 105, 102, 100,
101, 102, 32, 105, 95, 109, 111, 114, 101, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 109, 111, 114, 101, 10, 35, 101, 108, 115, 101, 10,
35, 117, 110, 100, 101, 102, 32, 105, 95, 116, 121, 112, 101, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 116, 97, 103, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 105, 109, 112, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 111, 112, 116, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 108, 101, 115, 115, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 99, 109, 112, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 101,
113, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 104, 97, 115, 104,
10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 99, 97, 112, 97, 99,
105, 116, 121, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 114, 97,
119, 95, 99, 108, 97, 115, 115, 10, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 118, 97, 108, 95, 115, 116, 114, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 95, 115, 115, 118, 10, 35, 117, 110, 100,
101, 102, 32, 105, 95, 118, 97, 108, 95, 97, 114, 99, 98, 111, 120,
10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108, 95, 99,
108, 97, 115, 115, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 118,
97, 108, 114, 97, 119, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95,
118, 97, 108, 99, 108, 111, 110, 101, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 118, 97, 108, 102, 114, 111, 109, 10, 35, 117, 110, 100,
101, 102, 32, 105, 95, 118, 97, 108, 116, 111, 10, 35, 117, 110, 100,
101, 102, 32, 105, 95, 118, 97, 108, 100, 114, 111, 112, 10, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 95, 115, 116, 114, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 95, 115, 115,
118, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 95,
97, 114, 99, 98, 111, 120, 10, 35, 117, 110, 100, 101, 102, 32, 105,
95, 107, 101, 121, 95, 99, 108, 97, 115, 115, 10, 35, 117, 110, 100,
101, 102, 32, 105, 95, 107, 101, 121, 114, 97, 119, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 99, 108, 111, 110, 101, 10,
35, 117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121, 102, 114,
111, 109, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121,
116, 111, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 107, 101, 121,
100, 114, 111, 112, 10, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95,
117, 115, 101, 95, 99, 109, 112, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 110, 111, 95, 104, 97, 115, 104, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 110, 111, 95, 99, 108, 111, 110, 101, 10, 35, 117,
110, 100, 101, 102, 32, 105, 95, 110, 111, 95, 101, 109, 112, 108, 97,
99, 101, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 105, 115, 95,
102, 111, 114, 119, 97, 114, 100, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 104, 97, 115, 95, 101, 109, 112, 108, 97, 99, 101, 10, 10,
35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 104, 97, 115, 95, 99,
109, 112, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95, 104, 97,
115, 95, 101, 113, 10, 35, 117, 110, 100, 101, 102, 32, 95, 105, 95,
112, 114, 101, 102, 105, 120, 10, 35, 117, 110, 100, 101, 102, 32, 95,
105, 95, 116, 101, 109, 112, 108, 97, 116, 101, 10, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 107, 101, 121, 99, 108, 97, 115, 115, 32,
47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93,
10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 118, 97, 108, 99, 108,
97, 115, 115, 32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97,
116, 101, 100, 93, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 114,
97, 119, 99, 108, 97, 115, 115, 32, 47, 47, 32, 91, 100, 101, 112,
114, 101, 99, 97, 116, 101, 100, 93, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 107, 101, 121, 98, 111, 120, 101, 100, 32, 47, 47, 32,
91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100, 93, 10, 35, 117,
110, 100, 101, 102, 32, 105, 95, 118, 97, 108, 98, 111, 120, 101, 100,
32, 47, 47, 32, 91, 100, 101, 112, 114, 101, 99, 97, 116, 101, 100,
93, 10, 35, 101, 110, 100, 105, 102, 10, 47, 47, 32, 35, 35, 35, 32,
69, 78, 68, 95, 70, 73, 76, 69, 95, 73, 78, 67, 76, 85, 68, 69, 58,
32, 116, 101, 109, 112, 108, 97, 116, 101, 50, 46, 104, 10, 47, 47,
32, 35, 35, 35, 32, 66, 69, 71, 73, 78, 95, 70, 73, 76, 69, 95, 73,
78, 67, 76, 85, 68, 69, 58, 32, 108, 105, 110, 107, 97, 103, 101, 50,
46, 104, 10, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 97, 108,
108, 111, 99, 97, 116, 111, 114, 10, 35, 117, 110, 100, 101, 102, 32,
105, 95, 109, 97, 108, 108, 111, 99, 10, 35, 117, 110, 100, 101, 102,
32, 105, 95, 99, 97, 108, 108, 111, 99, 10, 35, 117, 110, 100, 101,
102, 32, 105, 95, 114, 101, 97, 108, 108, 111, 99, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 102, 114, 101, 101, 10, 10, 35, 117, 110,
100, 101, 102, 32, 105, 95, 115, 116, 97, 116, 105, 99, 10, 35, 117,
110, 100, 101, 102, 32, 105, 95, 104, 101, 97, 100, 101, 114, 10, 35,
117, 110, 100, 101, 102, 32, 105, 95, 105, 109, 112, 108, 101, 109,
101, 110, 116, 10, 35, 117, 110, 100, 101, 102, 32, 105, 95, 105, 109,
112, 111, 114, 116, 10, 10, 35, 105, 102, 32, 100, 101, 102, 105, 110,
101, 100, 32, 95, 95, 99, 108, 97, 110, 103, 95, 95, 32, 38, 38, 32,
33, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 99, 112, 108, 117,
115, 112, 108, 117, 115, 10, 32, 32, 35, 112, 114, 97, 103, 109, 97,
32, 99, 108, 97, 110, 103, 32, 100, 105, 97, 103, 110, 111, 115, 116,
105, 99, 32, 112, 111, 112, 10, 35, 101, 108, 105, 102, 32, 100, 101,
102, 105, 110, 101, 100, 32, 95, 95, 71, 78, 85, 67, 95, 95, 32, 38,
38, 32, 33, 100, 101, 102, 105, 110, 101, 100, 32, 95, 95, 99, 112,
108, 117, 115, 112, 108, 117, 115, 10, 32, 32, 35, 112, 114, 97, 103,
109, 97, 32, 71, 67, 67, 32, 100, 105, 97, 103, 110, 111, 115, 116,
105, 99, 32, 112, 111, 112, 10, 35, 101, 110, 100, 105, 102, 10, 47,
47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73, 76, 69, 95, 73, 78,
67, 76, 85, 68, 69, 58, 32, 108, 105, 110, 107, 97, 103, 101, 50, 46,
104, 10, 47, 47, 32, 35, 35, 35, 32, 69, 78, 68, 95, 70, 73, 76, 69,
95, 73, 78, 67, 76, 85, 68, 69, 58, 32, 99, 118, 101, 99, 46, 104, 10,
10, 0, 35, 105, 102, 110, 100, 101, 102, 32, 95, 95, 83, 84, 68, 70,
76, 79, 65, 84, 95, 72, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95,
95, 83, 84, 68, 70, 76, 79, 65, 84, 95, 72, 10, 10, 35, 100, 101, 102,
105, 110, 101, 32, 68, 69, 67, 73, 77, 65, 76, 95, 68, 73, 71, 32, 50,
49, 10, 35, 100, 101, 102, 105, 110, 101, 32, 70, 76, 84, 95, 69, 86,
65, 76, 95, 77, 69, 84, 72, 79, 68, 32, 48, 32, 32, 47, 47, 32, 67,
49, 49, 32, 53, 46, 50, 46, 52, 46, 50, 46, 50, 112, 57, 10, 35, 100,
101, 102, 105, 110, 101, 32, 70, 76, 84, 95, 82, 65, 68, 73, 88, 32,
50, 10, 35, 100, 101, 102, 105, 110, 101, 32, 70, 76, 84, 95, 82, 79,
85, 78, 68, 83, 32, 49, 32, 32, 47, 47, 32, 67, 49, 49, 32, 53, 46,
50, 46, 52, 46, 50, 46, 50, 112, 56, 58, 32, 116, 111, 32, 110, 101,
97, 114, 101, 115, 116, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
70, 76, 84, 95, 68, 73, 71, 32, 54, 10, 35, 100, 101, 102, 105, 110,
101, 32, 70, 76, 84, 95, 69, 80, 83, 73, 76, 79, 78, 32, 48, 120, 49,
112, 45, 50, 51, 10, 35, 100, 101, 102, 105, 110, 101, 32, 70, 76, 84,
95, 77, 65, 78, 84, 95, 68, 73, 71, 32, 50, 52, 10, 35, 100, 101, 102,
105, 110, 101, 32, 70, 76, 84, 95, 77, 65, 88, 32, 48, 120, 49, 46,
102, 102, 102, 102, 102, 101, 112, 43, 49, 50, 55, 10, 35, 100, 101,
102, 105, 110, 101, 32, 70, 76, 84, 95, 77, 65, 88, 95, 49, 48, 95,
69, 88, 80, 32, 51, 56, 10, 35, 100, 101, 102, 105, 110, 101, 32, 70,
76, 84, 95, 77, 65, 88, 95, 69, 88, 80, 32, 49, 50, 56, 10, 35, 100,
101, 102, 105, 110, 101, 32, 70, 76, 84, 95, 77, 73, 78, 32, 48, 120,
49, 112, 45, 49, 50, 54, 10, 35, 100, 101, 102, 105, 110, 101, 32, 70,
76, 84, 95, 77, 73, 78, 95, 49, 48, 95, 69, 88, 80, 32, 45, 51, 55,
10, 35, 100, 101, 102, 105, 110, 101, 32, 70, 76, 84, 95, 77, 73, 78,
95, 69, 88, 80, 32, 45, 49, 50, 53, 10, 35, 100, 101, 102, 105, 110,
101, 32, 70, 76, 84, 95, 84, 82, 85, 69, 95, 77, 73, 78, 32, 48, 120,
49, 112, 45, 49, 52, 57, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32,
68, 66, 76, 95, 68, 73, 71, 32, 49, 53, 10, 35, 100, 101, 102, 105,
110, 101, 32, 68, 66, 76, 95, 69, 80, 83, 73, 76, 79, 78, 32, 48, 120,
49, 112, 45, 53, 50, 10, 35, 100, 101, 102, 105, 110, 101, 32, 68, 66,
76, 95, 77, 65, 78, 84, 95, 68, 73, 71, 32, 53, 51, 10, 35, 100, 101,
102, 105, 110, 101, 32, 68, 66, 76, 95, 77, 65, 88, 32, 48, 120, 49,
46, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102,
112, 43, 49, 48, 50, 51, 10, 35, 100, 101, 102, 105, 110, 101, 32, 68,
66, 76, 95, 77, 65, 88, 95, 49, 48, 95, 69, 88, 80, 32, 51, 48, 56,
10, 35, 100, 101, 102, 105, 110, 101, 32, 68, 66, 76, 95, 77, 65, 88,
95, 69, 88, 80, 32, 49, 48, 50, 52, 10, 35, 100, 101, 102, 105, 110,
101, 32, 68, 66, 76, 95, 77, 73, 78, 32, 48, 120, 49, 112, 45, 49, 48,
50, 50, 10, 35, 100, 101, 102, 105, 110, 101, 32, 68, 66, 76, 95, 77,
73, 78, 95, 49, 48, 95, 69, 88, 80, 32, 45, 51, 48, 55, 10, 35, 100,
101, 102, 105, 110, 101, 32, 68, 66, 76, 95, 77, 73, 78, 95, 69, 88,
80, 32, 45, 49, 48, 50, 49, 10, 35, 100, 101, 102, 105, 110, 101, 32,
68, 66, 76, 95, 84, 82, 85, 69, 95, 77, 73, 78, 32, 48, 120, 48, 46,
48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 112, 45, 49, 48,
50, 50, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 76, 68, 66, 76,
95, 68, 73, 71, 32, 49, 53, 10, 35, 100, 101, 102, 105, 110, 101, 32,
76, 68, 66, 76, 95, 69, 80, 83, 73, 76, 79, 78, 32, 48, 120, 49, 112,
45, 53, 50, 10, 35, 100, 101, 102, 105, 110, 101, 32, 76, 68, 66, 76,
95, 77, 65, 78, 84, 95, 68, 73, 71, 32, 53, 51, 10, 35, 100, 101, 102,
105, 110, 101, 32, 76, 68, 66, 76, 95, 77, 65, 88, 32, 48, 120, 49,
46, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102,
112, 43, 49, 48, 50, 51, 10, 35, 100, 101, 102, 105, 110, 101, 32, 76,
68, 66, 76, 95, 77, 65, 88, 95, 49, 48, 95, 69, 88, 80, 32, 51, 48,
56, 10, 35, 100, 101, 102, 105, 110, 101, 32, 76, 68, 66, 76, 95, 77,
65, 88, 95, 69, 88, 80, 32, 49, 48, 50, 52, 10, 35, 100, 101, 102,
105, 110, 101, 32, 76, 68, 66, 76, 95, 77, 73, 78, 32, 48, 120, 49,
112, 45, 49, 48, 50, 50, 10, 35, 100, 101, 102, 105, 110, 101, 32, 76,
68, 66, 76, 95, 77, 73, 78, 95, 49, 48, 95, 69, 88, 80, 32, 45, 51,
48, 55, 10, 35, 100, 101, 102, 105, 110, 101, 32, 76, 68, 66, 76, 95,
77, 73, 78, 95, 69, 88, 80, 32, 45, 49, 48, 50, 49, 10, 35, 100, 101,
102, 105, 110, 101, 32, 76, 68, 66, 76, 95, 84, 82, 85, 69, 95, 77,
73, 78, 32, 48, 120, 48, 46, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
48, 48, 49, 112, 45, 49, 48, 50, 50, 10, 10, 35, 101, 110, 100, 105,
102, 10, 0, 35, 105, 102, 110, 100, 101, 102, 32, 95, 95, 83, 84, 68,
65, 82, 71, 95, 72, 10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 95,
83, 84, 68, 65, 82, 71, 95, 72, 10, 10, 116, 121, 112, 101, 100, 101,
102, 32, 115, 116, 114, 117, 99, 116, 32, 123, 10, 32, 32, 117, 110,
115, 105, 103, 110, 101, 100, 32, 105, 110, 116, 32, 103, 112, 95,
111, 102, 102, 115, 101, 116, 59, 10, 32, 32, 117, 110, 115, 105, 103,
110, 101, 100, 32, 105, 110, 116, 32, 102, 112, 95, 111, 102, 102,
115, 101, 116, 59, 10, 32, 32, 118, 111, 105, 100, 42, 32, 111, 118,
101, 114, 102, 108, 111, 119, 95, 97, 114, 103, 95, 97, 114, 101, 97,
59, 10, 32, 32, 118, 111, 105, 100, 42, 32, 114, 101, 103, 95, 115,
97, 118, 101, 95, 97, 114, 101, 97, 59, 10, 125, 32, 95, 95, 118, 97,
95, 101, 108, 101, 109, 59, 10, 10, 116, 121, 112, 101, 100, 101, 102,
32, 95, 95, 118, 97, 95, 101, 108, 101, 109, 32, 118, 97, 95, 108,
105, 115, 116, 91, 49, 93, 59, 10, 10, 35, 100, 101, 102, 105, 110,
101, 32, 118, 97, 95, 115, 116, 97, 114, 116, 40, 97, 112, 44, 32,
108, 97, 115, 116, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
92, 10, 32, 32, 100, 111, 32, 123, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 92, 10, 32, 32, 32, 32, 42, 40, 97, 112, 41,
32, 61, 32, 42, 40, 95, 95, 118, 97, 95, 101, 108, 101, 109, 42, 41,
95, 95, 118, 97, 95, 97, 114, 101, 97, 95, 95, 59, 32, 92, 10, 32, 32,
125, 32, 119, 104, 105, 108, 101, 32, 40, 48, 41, 10, 10, 35, 100,
101, 102, 105, 110, 101, 32, 118, 97, 95, 101, 110, 100, 40, 97, 112,
41, 10, 10, 115, 116, 97, 116, 105, 99, 32, 118, 111, 105, 100, 42,
32, 95, 95, 118, 97, 95, 97, 114, 103, 95, 109, 101, 109, 40, 95, 95,
118, 97, 95, 101, 108, 101, 109, 42, 32, 97, 112, 44, 32, 105, 110,
116, 32, 115, 122, 44, 32, 105, 110, 116, 32, 97, 108, 105, 103, 110,
41, 32, 123, 10, 32, 32, 118, 111, 105, 100, 42, 32, 112, 32, 61, 32,
97, 112, 45, 62, 111, 118, 101, 114, 102, 108, 111, 119, 95, 97, 114,
103, 95, 97, 114, 101, 97, 59, 10, 32, 32, 105, 102, 32, 40, 97, 108,
105, 103, 110, 32, 62, 32, 56, 41, 10, 32, 32, 32, 32, 112, 32, 61,
32, 40, 112, 32, 43, 32, 49, 53, 41, 32, 47, 32, 49, 54, 32, 42, 32,
49, 54, 59, 10, 32, 32, 97, 112, 45, 62, 111, 118, 101, 114, 102, 108,
111, 119, 95, 97, 114, 103, 95, 97, 114, 101, 97, 32, 61, 32, 40, 40,
117, 110, 115, 105, 103, 110, 101, 100, 32, 108, 111, 110, 103, 41,
112, 32, 43, 32, 115, 122, 32, 43, 32, 55, 41, 32, 47, 32, 56, 32, 42,
32, 56, 59, 10, 32, 32, 114, 101, 116, 117, 114, 110, 32, 112, 59, 10,
125, 10, 10, 115, 116, 97, 116, 105, 99, 32, 118, 111, 105, 100, 42,
32, 95, 95, 118, 97, 95, 97, 114, 103, 95, 103, 112, 40, 95, 95, 118,
97, 95, 101, 108, 101, 109, 42, 32, 97, 112, 44, 32, 105, 110, 116,
32, 115, 122, 44, 32, 105, 110, 116, 32, 97, 108, 105, 103, 110, 41,
32, 123, 10, 32, 32, 105, 102, 32, 40, 97, 112, 45, 62, 103, 112, 95,
111, 102, 102, 115, 101, 116, 32, 62, 61, 32, 52, 56, 41, 10, 32, 32,
32, 32, 114, 101, 116, 117, 114, 110, 32, 95, 95, 118, 97, 95, 97,
114, 103, 95, 109, 101, 109, 40, 97, 112, 44, 32, 115, 122, 44, 32,
97, 108, 105, 103, 110, 41, 59, 10, 10, 32, 32, 118, 111, 105, 100,
42, 32, 114, 32, 61, 32, 97, 112, 45, 62, 114, 101, 103, 95, 115, 97,
118, 101, 95, 97, 114, 101, 97, 32, 43, 32, 97, 112, 45, 62, 103, 112,
95, 111, 102, 102, 115, 101, 116, 59, 10, 32, 32, 97, 112, 45, 62,
103, 112, 95, 111, 102, 102, 115, 101, 116, 32, 43, 61, 32, 56, 59,
10, 32, 32, 114, 101, 116, 117, 114, 110, 32, 114, 59, 10, 125, 10,
10, 115, 116, 97, 116, 105, 99, 32, 118, 111, 105, 100, 42, 32, 95,
95, 118, 97, 95, 97, 114, 103, 95, 102, 112, 40, 95, 95, 118, 97, 95,
101, 108, 101, 109, 42, 32, 97, 112, 44, 32, 105, 110, 116, 32, 115,
122, 44, 32, 105, 110, 116, 32, 97, 108, 105, 103, 110, 41, 32, 123,
10, 32, 32, 105, 102, 32, 40, 97, 112, 45, 62, 102, 112, 95, 111, 102,
102, 115, 101, 116, 32, 62, 61, 32, 49, 49, 50, 41, 10, 32, 32, 32,
32, 114, 101, 116, 117, 114, 110, 32, 95, 95, 118, 97, 95, 97, 114,
103, 95, 109, 101, 109, 40, 97, 112, 44, 32, 115, 122, 44, 32, 97,
108, 105, 103, 110, 41, 59, 10, 10, 32, 32, 118, 111, 105, 100, 42,
32, 114, 32, 61, 32, 97, 112, 45, 62, 114, 101, 103, 95, 115, 97, 118,
101, 95, 97, 114, 101, 97, 32, 43, 32, 97, 112, 45, 62, 102, 112, 95,
111, 102, 102, 115, 101, 116, 59, 10, 32, 32, 97, 112, 45, 62, 102,
112, 95, 111, 102, 102, 115, 101, 116, 32, 43, 61, 32, 56, 59, 10, 32,
32, 114, 101, 116, 117, 114, 110, 32, 114, 59, 10, 125, 10, 10, 35,
100, 101, 102, 105, 110, 101, 32, 118, 97, 95, 97, 114, 103, 40, 97,
112, 44, 32, 116, 121, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 92, 10,
32, 32, 40, 123, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 92, 10, 32,
32, 32, 32, 105, 110, 116, 32, 107, 108, 97, 115, 115, 32, 61, 32, 95,
95, 98, 117, 105, 108, 116, 105, 110, 95, 114, 101, 103, 95, 99, 108,
97, 115, 115, 40, 116, 121, 41, 59, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
92, 10, 32, 32, 32, 32, 42, 40, 116, 121, 42, 41, 40, 107, 108, 97,
115, 115, 32, 61, 61, 32, 48, 32, 63, 32, 95, 95, 118, 97, 95, 97,
114, 103, 95, 103, 112, 40, 97, 112, 44, 32, 115, 105, 122, 101, 111,
102, 40, 116, 121, 41, 44, 32, 95, 65, 108, 105, 103, 110, 111, 102,
40, 116, 121, 41, 41, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 92, 10, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 58, 32, 107, 108, 97,
115, 115, 32, 61, 61, 32, 49, 32, 63, 32, 95, 95, 118, 97, 95, 97,
114, 103, 95, 102, 112, 40, 97, 112, 44, 32, 115, 105, 122, 101, 111,
102, 40, 116, 121, 41, 44, 32, 95, 65, 108, 105, 103, 110, 111, 102,
40, 116, 121, 41, 41, 32, 32, 32, 32, 92, 10, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 58, 32, 95, 95, 118,
97, 95, 97, 114, 103, 95, 109, 101, 109, 40, 97, 112, 44, 32, 115,
105, 122, 101, 111, 102, 40, 116, 121, 41, 44, 32, 95, 65, 108, 105,
103, 110, 111, 102, 40, 116, 121, 41, 41, 41, 59, 32, 92, 10, 32, 32,
125, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 118, 97, 95,
99, 111, 112, 121, 40, 100, 101, 115, 116, 44, 32, 115, 114, 99, 41,
32, 40, 40, 100, 101, 115, 116, 41, 91, 48, 93, 32, 61, 32, 40, 115,
114, 99, 41, 91, 48, 93, 41, 10, 10, 35, 100, 101, 102, 105, 110, 101,
32, 95, 95, 71, 78, 85, 67, 95, 86, 65, 95, 76, 73, 83, 84, 32, 49,
10, 116, 121, 112, 101, 100, 101, 102, 32, 118, 97, 95, 108, 105, 115,
116, 32, 95, 95, 103, 110, 117, 99, 95, 118, 97, 95, 108, 105, 115,
116, 59, 10, 10, 35, 101, 110, 100, 105, 102, 10, 0, 35, 105, 102,
110, 100, 101, 102, 32, 95, 95, 83, 84, 68, 66, 79, 79, 76, 95, 72,
10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 83, 84, 68, 66, 79,
79, 76, 95, 72, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 98, 111,
111, 108, 32, 95, 66, 111, 111, 108, 10, 35, 100, 101, 102, 105, 110,
101, 32, 116, 114, 117, 101, 32, 49, 10, 35, 100, 101, 102, 105, 110,
101, 32, 102, 97, 108, 115, 101, 32, 48, 10, 35, 100, 101, 102, 105,
110, 101, 32, 95, 95, 98, 111, 111, 108, 95, 116, 114, 117, 101, 95,
102, 97, 108, 115, 101, 95, 97, 114, 101, 95, 100, 101, 102, 105, 110,
101, 100, 32, 49, 10, 10, 35, 101, 110, 100, 105, 102, 10, 0, 35, 105,
102, 110, 100, 101, 102, 32, 95, 95, 83, 84, 68, 68, 69, 70, 95, 72,
10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 83, 84, 68, 68, 69,
70, 95, 72, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 78, 85, 76,
76, 32, 40, 40, 118, 111, 105, 100, 42, 41, 48, 41, 10, 10, 116, 121,
112, 101, 100, 101, 102, 32, 117, 110, 115, 105, 103, 110, 101, 100,
32, 108, 111, 110, 103, 32, 115, 105, 122, 101, 95, 116, 59, 10, 116,
121, 112, 101, 100, 101, 102, 32, 108, 111, 110, 103, 32, 112, 116,
114, 100, 105, 102, 102, 95, 116, 59, 10, 116, 121, 112, 101, 100,
101, 102, 32, 117, 110, 115, 105, 103, 110, 101, 100, 32, 105, 110,
116, 32, 119, 99, 104, 97, 114, 95, 116, 59, 10, 116, 121, 112, 101,
100, 101, 102, 32, 108, 111, 110, 103, 32, 109, 97, 120, 95, 97, 108,
105, 103, 110, 95, 116, 59, 10, 10, 35, 100, 101, 102, 105, 110, 101,
32, 111, 102, 102, 115, 101, 116, 111, 102, 40, 116, 121, 112, 101,
44, 32, 109, 101, 109, 98, 101, 114, 41, 32, 40, 40, 115, 105, 122,
101, 95, 116, 41, 32, 38, 32, 40, 40, 40, 116, 121, 112, 101, 42, 41,
48, 41, 45, 62, 109, 101, 109, 98, 101, 114, 41, 41, 10, 10, 35, 101,
110, 100, 105, 102, 10, 0, 35, 105, 102, 110, 100, 101, 102, 32, 95,
95, 83, 84, 68, 78, 79, 82, 69, 84, 85, 82, 78, 95, 72, 10, 35, 100,
101, 102, 105, 110, 101, 32, 95, 95, 83, 84, 68, 78, 79, 82, 69, 84,
85, 82, 78, 95, 72, 10, 10, 35, 100, 101, 102, 105, 110, 101, 32, 110,
111, 114, 101, 116, 117, 114, 110, 32, 95, 78, 111, 114, 101, 116,
117, 114, 110, 10, 10, 35, 101, 110, 100, 105, 102, 10, 0, 35, 105,
102, 110, 100, 101, 102, 32, 95, 95, 83, 84, 68, 68, 69, 70, 95, 72,
10, 35, 100, 101, 102, 105, 110, 101, 32, 95, 95, 83, 84, 68, 68, 69,
70, 95, 72, 10, 10, 35, 105, 110, 99, 108, 117, 100, 101, 32, 60, 118,
99, 114, 117, 110, 116, 105, 109, 101, 46, 104, 62, 10, 10, 35, 100,
101, 102, 105, 110, 101, 32, 111, 102, 102, 115, 101, 116, 111, 102,
40, 116, 121, 112, 101, 44, 32, 109, 101, 109, 98, 101, 114, 41, 32,
40, 40, 115, 105, 122, 101, 95, 116, 41, 32, 38, 32, 40, 40, 40, 116,
121, 112, 101, 42, 41, 48, 41, 45, 62, 109, 101, 109, 98, 101, 114,
41, 41, 10, 10, 35, 101, 110, 100, 105, 102, 10, 0};
//
// END OF compincl.h
//
#undef C
#undef L
#undef VOID
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127)
#pragma warning(disable: 4242)
#pragma warning(disable: 4244)
#endif
//
// START OF ../../src/dynasm/dasm_proto.h
//
/*
** DynASM encoding engine prototypes.
** Copyright (C) 2005-2022 Mike Pall. All rights reserved.
** Released under the MIT license. See dynasm.lua for full copyright notice.
*/

#ifndef _DASM_PROTO_H
#define _DASM_PROTO_H

#include <stddef.h>
#include <stdarg.h>

#define DASM_IDENT	"DynASM 1.5.0"
#define DASM_VERSION	10500	/* 1.5.0 */

#ifndef Dst_DECL
#define Dst_DECL	dasm_State **Dst
#endif

#ifndef Dst_REF
#define Dst_REF		(*Dst)
#endif

#ifndef DASM_FDEF
#define DASM_FDEF	extern
#endif

#ifndef DASM_M_GROW
#define DASM_M_GROW(ctx, t, p, sz, need) \
  do { \
    size_t _sz = (sz), _need = (need); \
    if (_sz < _need) { \
      if (_sz < 16) _sz = 16; \
      while (_sz < _need) _sz += _sz; \
      (p) = (t *)realloc((p), _sz); \
      if ((p) == NULL) exit(1); \
      (sz) = _sz; \
    } \
  } while(0)
#endif

#ifndef DASM_M_FREE
#define DASM_M_FREE(ctx, p, sz)	free(p)
#endif

/* Internal DynASM encoder state. */
typedef struct dasm_State dasm_State;


/* Initialize and free DynASM state. */
static void dasm_init(Dst_DECL, int maxsection);
static void dasm_free(Dst_DECL);

/* Setup global array. Must be called before dasm_setup(). */
static void dasm_setupglobal(Dst_DECL, void **gl, unsigned int maxgl);

/* Grow PC label array. Can be called after dasm_setup(), too. */
static void dasm_growpc(Dst_DECL, unsigned int maxpc);

/* Setup encoder. */
static void dasm_setup(Dst_DECL, const void *actionlist);

/* Feed encoder with actions. Calls are generated by pre-processor. */
static void dasm_put(Dst_DECL, int start, ...);

/* Link sections and return the resulting size. */
static int dasm_link(Dst_DECL, size_t *szp);

/* Encode sections into buffer. */
static int dasm_encode(Dst_DECL, void *buffer);

/* Get PC label offset. */
static int dasm_getpclabel(Dst_DECL, unsigned int pc);

#ifdef DASM_CHECKS
/* Optional sanity checker to call between isolated encoding steps. */
static int dasm_checkstep(Dst_DECL, int secmatch);
#else
#define dasm_checkstep(a, b)	0
#endif


#endif /* _DASM_PROTO_H */
//
// END OF ../../src/dynasm/dasm_proto.h
//
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#undef C
#undef L
#undef VOID
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127)
#pragma warning(disable: 4242)
#pragma warning(disable: 4244)
#endif
//
// START OF ../../src/dynasm/dasm_x86.h
//
/*
** DynASM x86 encoding engine.
** Copyright (C) 2005-2022 Mike Pall. All rights reserved.
** Released under the MIT license. See dynasm.lua for full copyright notice.
*/

#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#define DASM_ARCH		"x86"

#ifndef DASM_EXTERN
#define DASM_EXTERN(a,b,c,d)	0
#endif

/* Action definitions. DASM_STOP must be 255. */
enum {
  DASM_DISP = 233,
  DASM_IMM_S, DASM_IMM_B, DASM_IMM_W, DASM_IMM_D, DASM_IMM_WB, DASM_IMM_DB,
  DASM_VREG, DASM_SPACE, DASM_SETLABEL, DASM_REL_A, DASM_REL_LG, DASM_REL_PC,
  DASM_IMM_LG, DASM_IMM_PC, DASM_LABEL_LG, DASM_LABEL_PC, DASM_ALIGN,
  DASM_EXTERN, DASM_ESC, DASM_MARK, DASM_SECTION, DASM_STOP
};

/* Maximum number of section buffer positions for a single dasm_put() call. */
#define DASM_MAXSECPOS		25

/* DynASM encoder status codes. Action list offset or number are or'ed in. */
#define DASM_S_OK		0x00000000
#define DASM_S_NOMEM		0x01000000
#define DASM_S_PHASE		0x02000000
#define DASM_S_MATCH_SEC	0x03000000
#define DASM_S_RANGE_I		0x11000000
#define DASM_S_RANGE_SEC	0x12000000
#define DASM_S_RANGE_LG		0x13000000
#define DASM_S_RANGE_PC		0x14000000
#define DASM_S_RANGE_VREG	0x15000000
#define DASM_S_UNDEF_L		0x21000000
#define DASM_S_UNDEF_PC		0x22000000

/* Macros to convert positions (8 bit section + 24 bit index). */
#define DASM_POS2IDX(pos)	((pos)&0x00ffffff)
#define DASM_POS2BIAS(pos)	((pos)&0xff000000)
#define DASM_SEC2POS(sec)	((sec)<<24)
#define DASM_POS2SEC(pos)	((pos)>>24)
#define DASM_POS2PTR(D, pos)	(D->sections[DASM_POS2SEC(pos)].rbuf + (pos))

/* Action list type. */
typedef const unsigned char *dasm_ActList;

/* Per-section structure. */
typedef struct dasm_Section {
  int *rbuf;		/* Biased buffer pointer (negative section bias). */
  int *buf;		/* True buffer pointer. */
  size_t bsize;		/* Buffer size in bytes. */
  int pos;		/* Biased buffer position. */
  int epos;		/* End of biased buffer position - max single put. */
  int ofs;		/* Byte offset into section. */
} dasm_Section;

/* Core structure holding the DynASM encoding state. */
struct dasm_State {
  size_t psize;			/* Allocated size of this structure. */
  dasm_ActList actionlist;	/* Current actionlist pointer. */
  int *lglabels;		/* Local/global chain/pos ptrs. */
  size_t lgsize;
  int *pclabels;		/* PC label chains/pos ptrs. */
  size_t pcsize;
  void **globals;		/* Array of globals (bias -10). */
  dasm_Section *section;	/* Pointer to active section. */
  size_t codesize;		/* Total size of all code sections. */
  int maxsection;		/* 0 <= sectionidx < maxsection. */
  int status;			/* Status code. */
  dasm_Section sections[1];	/* All sections. Alloc-extended. */
};

/* The size of the core structure depends on the max. number of sections. */
#define DASM_PSZ(ms)	(sizeof(dasm_State)+(ms-1)*sizeof(dasm_Section))


/* Initialize DynASM state. */
void dasm_init(Dst_DECL, int maxsection)
{
  dasm_State *D;
  size_t psz = 0;
  int i;
  Dst_REF = NULL;
  DASM_M_GROW(Dst, struct dasm_State, Dst_REF, psz, DASM_PSZ(maxsection));
  D = Dst_REF;
  D->psize = psz;
  D->lglabels = NULL;
  D->lgsize = 0;
  D->pclabels = NULL;
  D->pcsize = 0;
  D->globals = NULL;
  D->maxsection = maxsection;
  for (i = 0; i < maxsection; i++) {
    D->sections[i].buf = NULL;  /* Need this for pass3. */
    D->sections[i].rbuf = D->sections[i].buf - DASM_SEC2POS(i);
    D->sections[i].bsize = 0;
    D->sections[i].epos = 0;  /* Wrong, but is recalculated after resize. */
  }
}

/* Free DynASM state. */
void dasm_free(Dst_DECL)
{
  dasm_State *D = Dst_REF;
  int i;
  for (i = 0; i < D->maxsection; i++)
    if (D->sections[i].buf)
      DASM_M_FREE(Dst, D->sections[i].buf, D->sections[i].bsize);
  if (D->pclabels) DASM_M_FREE(Dst, D->pclabels, D->pcsize);
  if (D->lglabels) DASM_M_FREE(Dst, D->lglabels, D->lgsize);
  DASM_M_FREE(Dst, D, D->psize);
}

/* Setup global label array. Must be called before dasm_setup(). */
void dasm_setupglobal(Dst_DECL, void **gl, unsigned int maxgl)
{
  dasm_State *D = Dst_REF;
  D->globals = gl - 10;  /* Negative bias to compensate for locals. */
  DASM_M_GROW(Dst, int, D->lglabels, D->lgsize, (10+maxgl)*sizeof(int));
}

/* Grow PC label array. Can be called after dasm_setup(), too. */
void dasm_growpc(Dst_DECL, unsigned int maxpc)
{
  dasm_State *D = Dst_REF;
  size_t osz = D->pcsize;
  DASM_M_GROW(Dst, int, D->pclabels, D->pcsize, maxpc*sizeof(int));
  memset((void *)(((unsigned char *)D->pclabels)+osz), 0, D->pcsize-osz);
}

/* Setup encoder. */
void dasm_setup(Dst_DECL, const void *actionlist)
{
  dasm_State *D = Dst_REF;
  int i;
  D->actionlist = (dasm_ActList)actionlist;
  D->status = DASM_S_OK;
  D->section = &D->sections[0];
  memset((void *)D->lglabels, 0, D->lgsize);
  if (D->pclabels) memset((void *)D->pclabels, 0, D->pcsize);
  for (i = 0; i < D->maxsection; i++) {
    D->sections[i].pos = DASM_SEC2POS(i);
    D->sections[i].ofs = 0;
  }
}


#ifdef DASM_CHECKS
#define CK(x, st) \
  do { if (!(x)) { \
    D->status = DASM_S_##st|(int)(p-D->actionlist-1); return; } } while (0)
#define CKPL(kind, st) \
  do { if ((size_t)((char *)pl-(char *)D->kind##labels) >= D->kind##size) { \
    D->status=DASM_S_RANGE_##st|(int)(p-D->actionlist-1); return; } } while (0)
#else
#define CK(x, st)	((void)0)
#define CKPL(kind, st)	((void)0)
#endif

/* Pass 1: Store actions and args, link branches/labels, estimate offsets. */
void dasm_put(Dst_DECL, int start, ...)
{
  va_list ap;
  dasm_State *D = Dst_REF;
  dasm_ActList p = D->actionlist + start;
  dasm_Section *sec = D->section;
  int pos = sec->pos, ofs = sec->ofs, mrm = -1;
  int *b;

  if (pos >= sec->epos) {
    DASM_M_GROW(Dst, int, sec->buf, sec->bsize,
      sec->bsize + 2*DASM_MAXSECPOS*sizeof(int));
    sec->rbuf = sec->buf - DASM_POS2BIAS(pos);
    sec->epos = (int)sec->bsize/sizeof(int) - DASM_MAXSECPOS+DASM_POS2BIAS(pos);
  }

  b = sec->rbuf;
  b[pos++] = start;

  va_start(ap, start);
  while (1) {
    int action = *p++;
    if (action < DASM_DISP) {
      ofs++;
    } else if (action <= DASM_REL_A) {
      int n = va_arg(ap, int);
      b[pos++] = n;
      switch (action) {
      case DASM_DISP:
	if (n == 0) { if (mrm < 0) mrm = p[-2]; if ((mrm&7) != 5) break; }
	/* fallthrough */
      case DASM_IMM_DB: if (((n+128)&-256) == 0) goto ob; /* fallthrough */
      case DASM_REL_A: /* Assumes ptrdiff_t is int. !x64 */
      case DASM_IMM_D: ofs += 4; break;
      case DASM_IMM_S: CK(((n+128)&-256) == 0, RANGE_I); goto ob;
      case DASM_IMM_B: CK((n&-256) == 0, RANGE_I); ob: ofs++; break;
      case DASM_IMM_WB: if (((n+128)&-256) == 0) goto ob; /* fallthrough */
      case DASM_IMM_W: CK((n&-65536) == 0, RANGE_I); ofs += 2; break;
      case DASM_SPACE: p++; ofs += n; break;
      case DASM_SETLABEL: b[pos-2] = -0x40000000; break;  /* Neg. label ofs. */
      case DASM_VREG: CK((n&-16) == 0 && (n != 4 || (*p>>5) != 2), RANGE_VREG);
	if (*p < 0x40 && p[1] == DASM_DISP) mrm = n;
	if (*p < 0x20 && (n&7) == 4) ofs++;
	switch ((*p++ >> 3) & 3) {
	case 3: n |= b[pos-3]; /* fallthrough */
	case 2: n |= b[pos-2]; /* fallthrough */
	case 1: if (n <= 7) { b[pos-1] |= 0x10; ofs--; }
	}
	continue;
      }
      mrm = -1;
    } else {
      int *pl, n;
      switch (action) {
      case DASM_REL_LG:
      case DASM_IMM_LG:
	n = *p++; pl = D->lglabels + n;
	/* Bkwd rel or global. */
	if (n <= 246) { CK(n>=10||*pl<0, RANGE_LG); CKPL(lg, LG); goto putrel; }
	pl -= 246; n = *pl;
	if (n < 0) n = 0;  /* Start new chain for fwd rel if label exists. */
	goto linkrel;
      case DASM_REL_PC:
      case DASM_IMM_PC: pl = D->pclabels + va_arg(ap, int); CKPL(pc, PC);
      putrel:
	n = *pl;
	if (n < 0) {  /* Label exists. Get label pos and store it. */
	  b[pos] = -n;
	} else {
      linkrel:
	  b[pos] = n;  /* Else link to rel chain, anchored at label. */
	  *pl = pos;
	}
	pos++;
	ofs += 4;  /* Maximum offset needed. */
	if (action == DASM_REL_LG || action == DASM_REL_PC) {
	  b[pos++] = ofs;  /* Store pass1 offset estimate. */
	} else if (sizeof(ptrdiff_t) == 8) {
	  ofs += 4;
	}
	break;
      case DASM_LABEL_LG: pl = D->lglabels + *p++; CKPL(lg, LG); goto putlabel;
      case DASM_LABEL_PC: pl = D->pclabels + va_arg(ap, int); CKPL(pc, PC);
      putlabel:
	n = *pl;  /* n > 0: Collapse rel chain and replace with label pos. */
	while (n > 0) { int *pb = DASM_POS2PTR(D, n); n = *pb; *pb = pos; }
	*pl = -pos;  /* Label exists now. */
	b[pos++] = ofs;  /* Store pass1 offset estimate. */
	break;
      case DASM_ALIGN:
	ofs += *p++;  /* Maximum alignment needed (arg is 2**n-1). */
	b[pos++] = ofs;  /* Store pass1 offset estimate. */
	break;
      case DASM_EXTERN: p += 2; ofs += 4; break;
      case DASM_ESC: p++; ofs++; break;
      case DASM_MARK: mrm = p[-2]; break;
      case DASM_SECTION:
	n = *p; CK(n < D->maxsection, RANGE_SEC); D->section = &D->sections[n];
      case DASM_STOP: goto stop;
      }
    }
  }
stop:
  va_end(ap);
  sec->pos = pos;
  sec->ofs = ofs;
}
#undef CK

/* Pass 2: Link sections, shrink branches/aligns, fix label offsets. */
int dasm_link(Dst_DECL, size_t *szp)
{
  dasm_State *D = Dst_REF;
  int secnum;
  int ofs = 0;

#ifdef DASM_CHECKS
  *szp = 0;
  if (D->status != DASM_S_OK) return D->status;
  {
    int pc;
    for (pc = 0; pc*sizeof(int) < D->pcsize; pc++)
      if (D->pclabels[pc] > 0) return DASM_S_UNDEF_PC|pc;
  }
#endif

  { /* Handle globals not defined in this translation unit. */
    int idx;
    for (idx = 10; idx*sizeof(int) < D->lgsize; idx++) {
      int n = D->lglabels[idx];
      /* Undefined label: Collapse rel chain and replace with marker (< 0). */
      while (n > 0) { int *pb = DASM_POS2PTR(D, n); n = *pb; *pb = -idx; }
    }
  }

  /* Combine all code sections. No support for data sections (yet). */
  for (secnum = 0; secnum < D->maxsection; secnum++) {
    dasm_Section *sec = D->sections + secnum;
    int *b = sec->rbuf;
    int pos = DASM_SEC2POS(secnum);
    int lastpos = sec->pos;

    while (pos != lastpos) {
      dasm_ActList p = D->actionlist + b[pos++];
      int op = 0;
      while (1) {
	int action = *p++;
	switch (action) {
	case DASM_REL_LG: p++;
	  /* fallthrough */
	case DASM_REL_PC: {
	  int shrink = op == 0xe9 ? 3 : ((op&0xf0) == 0x80 ? 4 : 0);
	  if (shrink) {  /* Shrinkable branch opcode? */
	    int lofs, lpos = b[pos];
	    if (lpos < 0) goto noshrink;  /* Ext global? */
	    lofs = *DASM_POS2PTR(D, lpos);
	    if (lpos > pos) {  /* Fwd label: add cumulative section offsets. */
	      int i;
	      for (i = secnum; i < DASM_POS2SEC(lpos); i++)
		lofs += D->sections[i].ofs;
	    } else {
	      lofs -= ofs;  /* Bkwd label: unfix offset. */
	    }
	    lofs -= b[pos+1];  /* Short branch ok? */
	    if (lofs >= -128-shrink && lofs <= 127) ofs -= shrink;  /* Yes. */
	    else { noshrink: shrink = 0; }  /* No, cannot shrink op. */
	  }
	  b[pos+1] = shrink;
	  pos += 2;
	  break;
	}
	  /* fallthrough */
	case DASM_SPACE: case DASM_IMM_LG: case DASM_VREG: p++;
	  /* fallthrough */
	case DASM_DISP: case DASM_IMM_S: case DASM_IMM_B: case DASM_IMM_W:
	case DASM_IMM_D: case DASM_IMM_WB: case DASM_IMM_DB:
	case DASM_SETLABEL: case DASM_REL_A: case DASM_IMM_PC: pos++; break;
	case DASM_LABEL_LG: p++;
	  /* fallthrough */
	case DASM_LABEL_PC: b[pos++] += ofs; break; /* Fix label offset. */
	case DASM_ALIGN: ofs -= (b[pos++]+ofs)&*p++; break; /* Adjust ofs. */
	case DASM_EXTERN: p += 2; break;
	case DASM_ESC: op = *p++; break;
	case DASM_MARK: break;
	case DASM_SECTION: case DASM_STOP: goto stop;
	default: op = action; break;
	}
      }
      stop: (void)0;
    }
    ofs += sec->ofs;  /* Next section starts right after current section. */
  }

  D->codesize = ofs;  /* Total size of all code sections */
  *szp = ofs;
  return DASM_S_OK;
}

#define dasmb(x)	*cp++ = (unsigned char)(x)
#ifndef DASM_ALIGNED_WRITES
#define dasmw(x) \
  do { *((unsigned short *)cp) = (unsigned short)(x); cp+=2; } while (0)
#define dasmd(x) \
  do { *((unsigned int *)cp) = (unsigned int)(x); cp+=4; } while (0)
#define dasmq(x) \
  do { *((unsigned long long *)cp) = (unsigned long long)(x); cp+=8; } while (0)
#else
#define dasmw(x)	do { dasmb(x); dasmb((x)>>8); } while (0)
#define dasmd(x)	do { dasmw(x); dasmw((x)>>16); } while (0)
#define dasmq(x)	do { dasmd(x); dasmd((x)>>32); } while (0)
#endif
static unsigned char *dasma_(unsigned char *cp, ptrdiff_t x)
{
  if (sizeof(ptrdiff_t) == 8)
    dasmq((unsigned long long)x);
  else
    dasmd((unsigned int)x);
  return cp;
}
#define dasma(x)	(cp = dasma_(cp, (x)))

/* Pass 3: Encode sections. */
int dasm_encode(Dst_DECL, void *buffer)
{
  dasm_State *D = Dst_REF;
  unsigned char *base = (unsigned char *)buffer;
  unsigned char *cp = base;
  int secnum;

  /* Encode all code sections. No support for data sections (yet). */
  for (secnum = 0; secnum < D->maxsection; secnum++) {
    dasm_Section *sec = D->sections + secnum;
    int *b = sec->buf;
    int *endb = sec->rbuf + sec->pos;

    while (b != endb) {
      dasm_ActList p = D->actionlist + *b++;
      unsigned char *mark = NULL;
      while (1) {
	int action = *p++;
	int n = (action >= DASM_DISP && action <= DASM_ALIGN) ? *b++ : 0;
	switch (action) {
	case DASM_DISP: if (!mark) mark = cp; {
	  unsigned char *mm = mark;
	  if (*p != DASM_IMM_DB && *p != DASM_IMM_WB) mark = NULL;
	  if (n == 0) { int mrm = mm[-1]&7; if (mrm == 4) mrm = mm[0]&7;
	    if (mrm != 5) { mm[-1] -= 0x80; break; } }
	  if (((n+128) & -256) != 0) goto wd; else mm[-1] -= 0x40;
	}
	  /* fallthrough */
	case DASM_IMM_S: case DASM_IMM_B: wb: dasmb(n); break;
	case DASM_IMM_DB: if (((n+128)&-256) == 0) {
	    db: if (!mark) mark = cp; mark[-2] += 2; mark = NULL; goto wb;
	  } else mark = NULL;
	  /* fallthrough */
	case DASM_IMM_D: wd: dasmd(n); break;
	case DASM_IMM_WB: if (((n+128)&-256) == 0) goto db; else mark = NULL;
	  /* fallthrough */
	case DASM_IMM_W: dasmw(n); break;
	case DASM_VREG: {
	  int t = *p++;
	  unsigned char *ex = cp - (t&7);
	  if ((n & 8) && t < 0xa0) {
	    if (*ex & 0x80) ex[1] ^= 0x20 << (t>>6); else *ex ^= 1 << (t>>6);
	    n &= 7;
	  } else if (n & 0x10) {
	    if (*ex & 0x80) {
	      *ex = 0xc5; ex[1] = (ex[1] & 0x80) | ex[2]; ex += 2;
	    }
	    while (++ex < cp) ex[-1] = *ex;
	    if (mark) mark--;
	    cp--;
	    n &= 7;
	  }
	  if (t >= 0xc0) n <<= 4;
	  else if (t >= 0x40) n <<= 3;
	  else if (n == 4 && t < 0x20) { cp[-1] ^= n; *cp++ = 0x20; }
	  cp[-1] ^= n;
	  break;
	}
	case DASM_REL_LG: p++; if (n >= 0) goto rel_pc;
	  b++; n = (int)(ptrdiff_t)D->globals[-n];
	  /* fallthrough */
	case DASM_REL_A: rel_a:
	  n -= (unsigned int)(ptrdiff_t)(cp+4); goto wd; /* !x64 */
	case DASM_REL_PC: rel_pc: {
	  int shrink = *b++;
	  int *pb = DASM_POS2PTR(D, n); if (*pb < 0) { n = pb[1]; goto rel_a; }
	  n = *pb - ((int)(cp-base) + 4-shrink);
	  if (shrink == 0) goto wd;
	  if (shrink == 4) { cp--; cp[-1] = *cp-0x10; } else cp[-1] = 0xeb;
	  goto wb;
	}
	case DASM_IMM_LG:
	  p++;
	  if (n < 0) { dasma((ptrdiff_t)D->globals[-n]); break; }
	  /* fallthrough */
	case DASM_IMM_PC: {
	  int *pb = DASM_POS2PTR(D, n);
	  dasma(*pb < 0 ? (ptrdiff_t)pb[1] : (*pb + (ptrdiff_t)base));
	  break;
	}
	case DASM_LABEL_LG: {
	  int idx = *p++;
	  if (idx >= 10)
	    D->globals[idx] = (void *)(base + (*p == DASM_SETLABEL ? *b : n));
	  break;
	}
	case DASM_LABEL_PC: case DASM_SETLABEL: break;
	case DASM_SPACE: { int fill = *p++; while (n--) *cp++ = fill; break; }
	case DASM_ALIGN:
	  n = *p++;
	  while (((cp-base) & n)) *cp++ = 0x90; /* nop */
	  break;
	case DASM_EXTERN: n = DASM_EXTERN(Dst, cp, p[1], *p); p += 2; goto wd;
	case DASM_MARK: mark = cp; break;
	case DASM_ESC: action = *p++;
	  /* fallthrough */
	default: *cp++ = action; break;
	case DASM_SECTION: case DASM_STOP: goto stop;
	}
      }
      stop: (void)0;
    }
  }

  if (base + D->codesize != cp)  /* Check for phase errors. */
    return DASM_S_PHASE;
  return DASM_S_OK;
}

/* Get PC label offset. */
int dasm_getpclabel(Dst_DECL, unsigned int pc)
{
  dasm_State *D = Dst_REF;
  if (pc*sizeof(int) < D->pcsize) {
    int pos = D->pclabels[pc];
    if (pos < 0) return *DASM_POS2PTR(D, -pos);
    if (pos > 0) return -1;  /* Undefined. */
  }
  return -2;  /* Unused or out of range. */
}

#ifdef DASM_CHECKS
/* Optional sanity checker to call between isolated encoding steps. */
int dasm_checkstep(Dst_DECL, int secmatch)
{
  dasm_State *D = Dst_REF;
  if (D->status == DASM_S_OK) {
    int i;
    for (i = 1; i <= 9; i++) {
      if (D->lglabels[i] > 0) { D->status = DASM_S_UNDEF_L|i; break; }
      D->lglabels[i] = 0;
    }
  }
  if (D->status == DASM_S_OK && secmatch >= 0 &&
      D->section != &D->sections[secmatch])
    D->status = DASM_S_MATCH_SEC|(int)(D->section-D->sections);
  return D->status;
}
#endif

//
// END OF ../../src/dynasm/dasm_x86.h
//
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#undef C
#undef L
#undef VOID
//
// START OF ../../src/type.c
//

static Type* ty_void = &(Type){TY_VOID, 1, 1};
static Type* ty_bool = &(Type){TY_BOOL, 1, 1};

static Type* ty_char = &(Type){TY_CHAR, 1, 1};
static Type* ty_short = &(Type){TY_SHORT, 2, 2};
static Type* ty_int = &(Type){TY_INT, 4, 4};
static Type* ty_long = &(Type){TY_LONG, 8, 8};

static Type* ty_uchar = &(Type){TY_CHAR, 1, 1, true};
static Type* ty_ushort = &(Type){TY_SHORT, 2, 2, true};
static Type* ty_uint = &(Type){TY_INT, 4, 4, true};
static Type* ty_ulong = &(Type){TY_LONG, 8, 8, true};

static Type* ty_float = &(Type){TY_FLOAT, 4, 4};
static Type* ty_double = &(Type){TY_DOUBLE, 8, 8};
#if X64WIN
static Type* ty_ldouble = &(Type){TY_LDOUBLE, 8, 8};
#else
static Type* ty_ldouble = &(Type){TY_LDOUBLE, 16, 16};
#endif

static Type* new_type(TypeKind kind, int size, int align) {
  Type* ty = bumpcalloc(1, sizeof(Type), AL_Compile);
  ty->kind = kind;
  ty->size = size;
  ty->align = align;
  return ty;
}

static bool is_integer(Type* ty) {
  TypeKind k = ty->kind;
  return k == TY_BOOL || k == TY_CHAR || k == TY_SHORT || k == TY_INT || k == TY_LONG ||
         k == TY_ENUM;
}

static bool is_flonum(Type* ty) {
  return ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE || ty->kind == TY_LDOUBLE;
}

static bool is_numeric(Type* ty) {
  return is_integer(ty) || is_flonum(ty);
}

static bool is_void(Type* ty) {
  return ty->kind == TY_VOID;
}

static bool is_compatible(Type* t1, Type* t2) {
  if (t1 == t2)
    return true;

  if (t1->origin)
    return is_compatible(t1->origin, t2);

  if (t2->origin)
    return is_compatible(t1, t2->origin);

  if (t1->kind != t2->kind)
    return false;

  switch (t1->kind) {
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
    case TY_LONG:
      return t1->is_unsigned == t2->is_unsigned;
    case TY_FLOAT:
    case TY_DOUBLE:
#if !X64WIN
    case TY_LDOUBLE:
#endif
      return true;
    case TY_PTR:
      return is_compatible(t1->base, t2->base);
    case TY_FUNC: {
      if (!is_compatible(t1->return_ty, t2->return_ty))
        return false;
      if (t1->is_variadic != t2->is_variadic)
        return false;

      Type* p1 = t1->params;
      Type* p2 = t2->params;
      for (; p1 && p2; p1 = p1->next, p2 = p2->next)
        if (!is_compatible(p1, p2))
          return false;
      return p1 == NULL && p2 == NULL;
    }
    case TY_ARRAY:
      if (!is_compatible(t1->base, t2->base))
        return false;
      return t1->array_len < 0 && t2->array_len < 0 && t1->array_len == t2->array_len;
  }
  return false;
}

static Type* copy_type(Type* ty) {
  Type* ret = bumpcalloc(1, sizeof(Type), AL_Compile);
  *ret = *ty;
  ret->origin = ty;
  return ret;
}

static Type* pointer_to(Type* base) {
  Type* ty = new_type(TY_PTR, 8, 8);
  ty->base = base;
  ty->is_unsigned = true;
  return ty;
}

static Type* func_type(Type* return_ty) {
  // The C spec disallows sizeof(<function type>), but
  // GCC allows that and the expression is evaluated to 1.
  Type* ty = new_type(TY_FUNC, 1, 1);
  ty->return_ty = return_ty;
  return ty;
}

static Type* array_of(Type* base, int len, Token* err_tok) {
  int64_t arr_size = (int64_t)base->size * len;
  if (arr_size > INT_MAX)
    error_tok(err_tok, "array too large");
  Type* ty = new_type(TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->array_len = len;
  return ty;
}

static Type* vla_of(Type* base, Node* len) {
  Type* ty = new_type(TY_VLA, 8, 8);
  ty->base = base;
  ty->vla_len = len;
  return ty;
}

static Type* enum_type(void) {
  return new_type(TY_ENUM, 4, 4);
}

static Type* struct_type(void) {
  return new_type(TY_STRUCT, 0, 1);
}

static Type* get_common_type(Type* ty1, Type* ty2) {
  if (ty1->base)
    return pointer_to(ty1->base);

  if (ty1->kind == TY_FUNC)
    return pointer_to(ty1);
  if (ty2->kind == TY_FUNC)
    return pointer_to(ty2);

  if (ty1->kind == TY_LDOUBLE || ty2->kind == TY_LDOUBLE)
    return ty_ldouble;
  if (ty1->kind == TY_DOUBLE || ty2->kind == TY_DOUBLE)
    return ty_double;
  if (ty1->kind == TY_FLOAT || ty2->kind == TY_FLOAT)
    return ty_float;

  if (ty1->size < 4)
    ty1 = ty_int;
  if (ty2->size < 4)
    ty2 = ty_int;

  if (ty1->size != ty2->size)
    return (ty1->size < ty2->size) ? ty2 : ty1;

  if (ty2->is_unsigned)
    return ty2;
  return ty1;
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
static void usual_arith_conv(Node** lhs, Node** rhs) {
  Type* ty = get_common_type((*lhs)->ty, (*rhs)->ty);
  *lhs = new_cast(*lhs, ty);
  *rhs = new_cast(*rhs, ty);
}

static void add_type(Node* node) {
  if (!node || node->ty)
    return;

  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->cond);
  add_type(node->then);
  add_type(node->els);
  add_type(node->init);
  add_type(node->inc);

  for (Node* n = node->body; n; n = n->next)
    add_type(n);
  for (Node* n = node->args; n; n = n->next)
    add_type(n);

  switch (node->kind) {
    case ND_NUM:
      node->ty = ty_int;
      return;
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
      usual_arith_conv(&node->lhs, &node->rhs);
      node->ty = node->lhs->ty;
      return;
    case ND_NEG: {
      Type* ty = get_common_type(ty_int, node->lhs->ty);
      node->lhs = new_cast(node->lhs, ty);
      node->ty = ty;
      return;
    }
    case ND_ASSIGN:
      if (node->lhs->ty->kind == TY_ARRAY)
        error_tok(node->lhs->tok, "not an lvalue");
      if (node->lhs->ty->kind == TY_PTR &&
          (node->rhs->ty->kind == TY_STRUCT || node->rhs->ty->kind == TY_UNION)) {
        error_tok(node->lhs->tok, "value of type %.*s can't be assigned to a pointer",
                  node->rhs->ty->name->len, node->rhs->ty->name->loc);
      }
      if (node->lhs->ty->kind != TY_STRUCT) {
        node->rhs = new_cast(node->rhs, node->lhs->ty);
      } else {
        if (node->rhs->ty->kind != TY_STRUCT) {
          error_tok(node->lhs->tok, "cannot assign to struct");
        }
        if (node->lhs->ty->members != node->rhs->ty->members) {
          error_tok(node->lhs->tok, "cannot assign incompatible structs");
        }
      }
      node->ty = node->lhs->ty;
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      usual_arith_conv(&node->lhs, &node->rhs);
      node->ty = ty_int;
      return;
    case ND_FUNCALL:
      node->ty = node->func_ty->return_ty;
      return;
    case ND_NOT:
    case ND_LOGOR:
    case ND_LOGAND:
      node->ty = ty_int;
      return;
    case ND_BITNOT:
    case ND_SHL:
    case ND_SHR:
      node->ty = node->lhs->ty;
      return;
    case ND_VAR:
    case ND_VLA_PTR:
      if (!node->var)
        error_tok(node->tok, "non-constant value");
      node->ty = node->var->ty;
      return;
    case ND_COND:
      if (node->then->ty->kind == TY_VOID || node->els->ty->kind == TY_VOID) {
        node->ty = ty_void;
      } else {
        usual_arith_conv(&node->then, &node->els);
        node->ty = node->then->ty;
      }
      return;
    case ND_COMMA:
      node->ty = node->rhs->ty;
      return;
    case ND_MEMBER:
      node->ty = node->member->ty;
      return;
    case ND_ADDR: {
      Type* ty = node->lhs->ty;
      if (ty->kind == TY_ARRAY)
        node->ty = pointer_to(ty->base);
      else
        node->ty = pointer_to(ty);
      return;
    }
    case ND_DEREF:
      if (!node->lhs->ty->base)
        error_tok(node->tok, "invalid pointer dereference");
      if (node->lhs->ty->base->kind == TY_VOID)
        error_tok(node->tok, "dereferencing a void pointer");

      node->ty = node->lhs->ty->base;
      return;
    case ND_STMT_EXPR:
      if (node->body) {
        Node* stmt = node->body;
        while (stmt->next)
          stmt = stmt->next;
        if (stmt->kind == ND_EXPR_STMT) {
          node->ty = stmt->lhs->ty;
          return;
        }
      }
      error_tok(node->tok, "statement expression returning void is not supported");
      return;
    case ND_LABEL_VAL:
      node->ty = pointer_to(ty_void);
      return;
    case ND_CAS:
      add_type(node->cas_addr);
      add_type(node->cas_old);
      add_type(node->cas_new);
      node->ty = ty_bool;

      if (node->cas_addr->ty->kind != TY_PTR)
        error_tok(node->cas_addr->tok, "pointer expected");
      if (node->cas_old->ty->kind != TY_PTR)
        error_tok(node->cas_old->tok, "pointer expected");
      return;
    case ND_LOCKCE:
      add_type(node->cas_addr);
      add_type(node->cas_old);
      add_type(node->cas_new);
      node->ty = ty_bool;

      if (node->cas_addr->ty->kind != TY_PTR)
        error_tok(node->cas_addr->tok, "pointer expected");
      return;
    case ND_EXCH:
      if (node->lhs->ty->kind != TY_PTR)
        error_tok(node->cas_addr->tok, "pointer expected");
      node->ty = node->lhs->ty->base;
      return;
  }
}
//
// END OF ../../src/type.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/alloc.c
//

#if X64WIN
#include <windows.h>
#else
#include <sys/mman.h>
#endif

// MSVC chokes during preprocess on __has_feature().
#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define __SANITIZE_ADDRESS__ 1
#endif
#endif

#if defined(__SANITIZE_ADDRESS__)
void __asan_poison_memory_region(void const volatile* addr, size_t size);
void __asan_unpoison_memory_region(void const volatile* addr, size_t size);
#define ASAN_POISON_MEMORY_REGION(addr, size) __asan_poison_memory_region((addr), (size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) __asan_unpoison_memory_region((addr), (size))
#else
#define ASAN_POISON_MEMORY_REGION(addr, size) ((void)(addr), (void)(size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) ((void)(addr), (void)(size))
#endif

static UserContext* user_context;
static jmp_buf toplevel_update_jmpbuf;
static CompilerState compiler_state;
static LinkerState linker_state;

typedef struct HeapData {
  char* base;
  char* alloc_pointer;
  size_t size;
} HeapData;

static HeapData heap[NUM_BUMP_HEAPS] = {
    {NULL, NULL, 1024 << 20},  // AL_Compile
    {NULL, NULL, 128 << 20},   // AL_Temp
    {NULL, NULL, 128 << 20},   // AL_Link
    {NULL, NULL, 64 << 20},    // AL_UserContext
};

static void alloc_init(AllocLifetime lifetime) {
  assert(lifetime < NUM_BUMP_HEAPS);
  HeapData* hd = &heap[lifetime];

  hd->alloc_pointer = hd->base = allocate_writable_memory(hd->size);
  ASAN_POISON_MEMORY_REGION(hd->base, hd->size);
  if (lifetime == AL_Compile) {
    memset(&compiler_state, 0, sizeof(compiler_state));
  } else if (lifetime == AL_Link) {
    memset(&linker_state, 0, sizeof(linker_state));
  }
}

static void alloc_reset(AllocLifetime lifetime) {
  assert(lifetime < NUM_BUMP_HEAPS);
  HeapData* hd = &heap[lifetime];
  // We allow double resets because we may longjmp out during error handling,
  // and don't know which heaps are initialized at that point.
  if (hd->base) {
    ASAN_POISON_MEMORY_REGION(hd->base, hd->size);
    free_executable_memory(hd->base, hd->size);
    hd->alloc_pointer = NULL;
    hd->base = NULL;
  }
}

static void* bumpcalloc(size_t num, size_t size, AllocLifetime lifetime) {
  if (lifetime == AL_Manual) {
    return calloc(num, size);
  }

  size_t toalloc = align_to_u(num * size, 8);
  HeapData* hd = &heap[lifetime];
  char* ret = hd->alloc_pointer;
  hd->alloc_pointer += toalloc;
  if (hd->alloc_pointer > hd->base + hd->size) {
    error("heap exhausted");
  }
  ASAN_UNPOISON_MEMORY_REGION(ret, toalloc);
  memset(ret, 0, toalloc);
  return ret;
}

static void alloc_free(void* p, AllocLifetime lifetime) {
  (void)lifetime;
  assert(lifetime == AL_Manual);
  free(p);
}

static void* bumplamerealloc(void* old,
                                 size_t old_size,
                                 size_t new_size,
                                 AllocLifetime lifetime) {
  void* newptr = bumpcalloc(1, new_size, lifetime);
  memcpy(newptr, old, MIN(old_size, new_size));
  ASAN_POISON_MEMORY_REGION(old, old_size);
  return newptr;
}

static void* aligned_allocate(size_t size, size_t alignment) {
  size = align_to_u(size, alignment);
#if X64WIN
  return _aligned_malloc(size, alignment);
#else
  return aligned_alloc(alignment, size);
#endif
}

static void aligned_free(void* p) {
#if X64WIN
  _aligned_free(p);
#else
  free(p);
#endif
}

// Allocates RW memory of given size and returns a pointer to it. On failure,
// prints out the error and returns NULL. Unlike malloc, the memory is allocated
// on a page boundary so it's suitable for calling mprotect.
static void* allocate_writable_memory(size_t size) {
#if X64WIN
  void* p = VirtualAlloc(0, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  if (!p) {
    error("VirtualAlloc of %zu failed: 0x%x\n", size, GetLastError());
  }
  ASAN_UNPOISON_MEMORY_REGION(p, size);
  return p;
#else
  void* ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (ptr == (void*)-1) {
    perror("mmap");
    return NULL;
  }
  ASAN_UNPOISON_MEMORY_REGION(ptr, size);
  return ptr;
#endif
}

// Sets a RX permission on the given memory, which must be page-aligned. Returns
// 0 on success. On failure, prints out the error and returns -1.
static bool make_memory_readwrite(void* m, size_t size) {
#if X64WIN
  DWORD old_protect;
  if (!VirtualProtect(m, size, PAGE_READWRITE, &old_protect)) {
    error("VirtualProtect %p %zu failed: 0x%x\n", m, size, GetLastError());
  }
  return true;
#else
  if (mprotect(m, size, PROT_READ | PROT_WRITE) == -1) {
    perror("mprotect");
    return false;
  }
  return true;
#endif
}

// Sets a RX permission on the given memory, which must be page-aligned. Returns
// 0 on success. On failure, prints out the error and returns -1.
static bool make_memory_executable(void* m, size_t size) {
#if X64WIN
  DWORD old_protect;
  if (!VirtualProtect(m, size, PAGE_EXECUTE_READ, &old_protect)) {
    error("VirtualProtect %p %zu failed: 0x%x\n", m, size, GetLastError());
  }
  return true;
#else
  if (mprotect(m, size, PROT_READ | PROT_EXEC) == -1) {
    perror("mprotect");
    return false;
  }
  return true;
#endif
}

static void free_executable_memory(void* p, size_t size) {
#if X64WIN
  (void)size;  // If |size| is passed, free will fail.
  if (!VirtualFree(p, 0, MEM_RELEASE)) {
    error("VirtualFree %p %zu failed: 0x%x\n", p, size, GetLastError());
  }
  ASAN_POISON_MEMORY_REGION(p, size);
#else
  munmap(p, size);
  ASAN_POISON_MEMORY_REGION(p, size);
#endif
}
//
// END OF ../../src/alloc.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/hashmap.c
//
// This is an implementation of the open-addressing hash table.


// Initial hash bucket size
#define INIT_SIZE 16

// Rehash if the usage exceeds 70%.
#define HIGH_WATERMARK 70

// We'll keep the usage below 50% after rehashing.
#define LOW_WATERMARK 50

// Represents a deleted hash entry
#define TOMBSTONE ((void*)-1)

static uint64_t fnv_hash(char* s, int len) {
  uint64_t hash = 0xcbf29ce484222325;
  for (int i = 0; i < len; i++) {
    hash *= 0x100000001b3;
    hash ^= (unsigned char)s[i];
  }
  return hash;
}

// Make room for new entires in a given hashmap by removing
// tombstones and possibly extending the bucket size.
static void rehash(HashMap* map) {
  // Compute the size of the new hashmap.
  int nkeys = 0;
  for (int i = 0; i < map->capacity; i++)
    if (map->buckets[i].key && map->buckets[i].key != TOMBSTONE)
      nkeys++;

  int cap = map->capacity;
  while ((nkeys * 100) / cap >= LOW_WATERMARK)
    cap = cap * 2;
  assert(cap > 0);

  // Create a new hashmap and copy all key-values.
  HashMap map2 = {0};
  map2.buckets = bumpcalloc(cap, sizeof(HashEntry), map->alloc_lifetime);
  map2.capacity = cap;
  map2.alloc_lifetime = map->alloc_lifetime;

  for (int i = 0; i < map->capacity; i++) {
    HashEntry* ent = &map->buckets[i];
    if (ent->key && ent->key != TOMBSTONE) {
      hashmap_put2(&map2, ent->key, ent->keylen, ent->val);
    }
  }

  assert(map2.used == nkeys);
  if (map->alloc_lifetime == AL_Manual) {
    alloc_free(map->buckets, map->alloc_lifetime);
  }
  *map = map2;
}

static bool match(HashEntry* ent, char* key, int keylen) {
  return ent->key && ent->key != TOMBSTONE && ent->keylen == keylen &&
         memcmp(ent->key, key, keylen) == 0;
}

static HashEntry* get_entry(HashMap* map, char* key, int keylen) {
  if (!map->buckets)
    return NULL;

  uint64_t hash = fnv_hash(key, keylen);

  for (int i = 0; i < map->capacity; i++) {
    HashEntry* ent = &map->buckets[(hash + i) % map->capacity];
    if (match(ent, key, keylen))
      return ent;
    if (ent->key == NULL)
      return NULL;
  }
  unreachable();
}

static HashEntry* get_or_insert_entry(HashMap* map, char* key, int keylen) {
  if (!map->buckets) {
    map->buckets = bumpcalloc(INIT_SIZE, sizeof(HashEntry), map->alloc_lifetime);
    map->capacity = INIT_SIZE;
  } else if ((map->used * 100) / map->capacity >= HIGH_WATERMARK) {
    rehash(map);
  }

  uint64_t hash = fnv_hash(key, keylen);

  for (int i = 0; i < map->capacity; i++) {
    HashEntry* ent = &map->buckets[(hash + i) % map->capacity];

    if (match(ent, key, keylen)) {
      if (map->alloc_lifetime == AL_Manual) {
        free(ent->key);
      }
      ent->key = key;
      return ent;
    }

    // It is tempting to allow a TOMBSTONE entry to be reused here, but they
    // cannot be, see: https://github.com/rui314/chibicc/issues/135.

    if (ent->key == NULL) {
      ent->key = key;
      ent->keylen = keylen;
      map->used++;
      return ent;
    }
  }
  unreachable();
}

static void* hashmap_get(HashMap* map, char* key) {
  return hashmap_get2(map, key, (int)strlen(key));
}

static void* hashmap_get2(HashMap* map, char* key, int keylen) {
  HashEntry* ent = get_entry(map, key, keylen);
  return ent ? ent->val : NULL;
}

static void hashmap_put(HashMap* map, char* key, void* val) {
  hashmap_put2(map, key, (int)strlen(key), val);
}

static void hashmap_put2(HashMap* map, char* key, int keylen, void* val) {
  HashEntry* ent = get_or_insert_entry(map, key, keylen);
  ent->val = val;
}

static void hashmap_delete(HashMap* map, char* key) {
  hashmap_delete2(map, key, (int)strlen(key));
}

static void hashmap_delete2(HashMap* map, char* key, int keylen) {
  HashEntry* ent = get_entry(map, key, keylen);
  if (ent) {
    if (map->alloc_lifetime == AL_Manual) {
      free(ent->key);
    }
    ent->key = TOMBSTONE;
  }
}

// keys strdup'd with AL_Manual, and values that are the data segment
// allocations allocated by aligned_allocate.
static void hashmap_clear_manual_key_owned_value_owned_aligned(HashMap* map) {
  assert(map->alloc_lifetime == AL_Manual);
  for (int i = 0; i < map->capacity; i++) {
    HashEntry* ent = &map->buckets[i];
    if (ent->key && ent->key != TOMBSTONE) {
      alloc_free(ent->key, map->alloc_lifetime);
      aligned_free(ent->val);
    }
  }
  alloc_free(map->buckets, map->alloc_lifetime);
  map->buckets = NULL;
  map->used = 0;
  map->capacity = 0;
}

// keys strdup'd with AL_Manual, and values that point into the codeseg, so
// aren't freed.
static void hashmap_clear_manual_key_owned_value_unowned(HashMap* map) {
  assert(map->alloc_lifetime == AL_Manual);
  for (int i = 0; i < map->capacity; i++) {
    HashEntry* ent = &map->buckets[i];
    if (ent->key && ent->key != TOMBSTONE) {
      alloc_free(ent->key, map->alloc_lifetime);
      // ent->val points into codeseg, not to be freed here.
    }
  }
  alloc_free(map->buckets, map->alloc_lifetime);
  map->buckets = NULL;
  map->used = 0;
  map->capacity = 0;
}
//
// END OF ../../src/hashmap.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/link.c
//

#if X64WIN
#include <direct.h>
#include <io.h>
#include <malloc.h>
#include <math.h>
#include <process.h>
#include <windows.h>
#define alloca _alloca
#else
#include <dlfcn.h>
#include <unistd.h>
#endif

#define L(x) linker_state.link__##x

#if X64WIN
static void Unimplemented(void) {
  ABORT("unimplemented function");
}

extern int __chkstk(void);

static void Xstosb(PBYTE Destination, BYTE Value, SIZE_T Count) {
  (void)Destination;
  (void)Value;
  (void)Count;
  ABORT("unimplemented __stosb");
}

static void XReadWriteBarrier(void) {
  // I think this is probably a sufficient implementation in our compiler.
}

static void* get_standard_runtime_function(char* name) {
  if (L(runtime_function_map).capacity == 0) {
    L(runtime_function_map).alloc_lifetime = AL_Link;
#define X(func) hashmap_put(&L(runtime_function_map), #func, (void*)&func)
#define Y(name, func) hashmap_put(&L(runtime_function_map), name, (void*)&func)
    X(AreFileApisANSI);
    X(CharUpperW);
    X(CloseHandle);
    X(CreateFileA);
    X(CreateFileMappingW);
    X(CreateFileW);
    X(CreateMutexW);
    X(CreateThread);
    X(DebugBreak);
    X(DeleteCriticalSection);
    X(DeleteFileA);
    X(DeleteFileW);
    X(EnterCriticalSection);
    X(FindClose);
    X(FindFirstFileW);
    X(FlushFileBuffers);
    X(FlushViewOfFile);
    X(FormatMessageA);
    X(FormatMessageW);
    X(FreeLibrary);
    X(GetConsoleScreenBufferInfo);
    X(GetCurrentProcess);
    X(GetCurrentProcessId);
    X(GetCurrentThreadId);
    X(GetDiskFreeSpaceA);
    X(GetDiskFreeSpaceW);
    X(GetEnvironmentVariableA);
    X(GetFileAttributesA);
    X(GetFileAttributesExW);
    X(GetFileAttributesW);
    X(GetFileSize);
    X(GetFullPathNameA);
    X(GetFullPathNameW);
    X(GetLastError);
    X(GetProcAddress);
    X(GetProcessHeap);
    X(GetStdHandle);
    X(GetSystemInfo);
    X(GetSystemTime);
    X(GetSystemTimeAsFileTime);
    X(GetTempPathA);
    X(GetTempPathW);
    X(GetTickCount);
    X(HeapAlloc);
    X(HeapCompact);
    X(HeapCreate);
    X(HeapDestroy);
    X(HeapFree);
    X(HeapReAlloc);
    X(HeapSize);
    X(HeapValidate);
    X(InitializeCriticalSection);
    X(LeaveCriticalSection);
    X(LoadLibraryA);
    X(LoadLibraryW);
    X(LocalFree);
    X(LockFile);
    X(LockFileEx);
    X(MapViewOfFile);
    X(MapViewOfFileNuma2);
    X(MessageBoxA);
    X(MultiByteToWideChar);
    X(OutputDebugStringA);
    X(OutputDebugStringW);
    X(QueryPerformanceCounter);
    X(ReadFile);
    X(SetConsoleCtrlHandler);
    X(SetConsoleTextAttribute);
    X(SetCurrentDirectoryW);
    X(SetEndOfFile);
    X(SetFilePointer);
    X(SetFileTime);
    X(SetProcessDPIAware);
    X(Sleep);
    X(SystemTimeToFileTime);
    X(SystemTimeToFileTime);
    X(TryEnterCriticalSection);
    X(UnlockFile);
    X(UnlockFileEx);
    X(UnmapViewOfFile);
    X(WaitForSingleObject);
    X(WaitForSingleObjectEx);
    X(WideCharToMultiByte);
    X(WriteFile);
    X(__acrt_iob_func);
    X(__chkstk);
    X(__pctype_func);
    X(__stdio_common_vfprintf);
    X(__stdio_common_vfprintf_p);
    X(__stdio_common_vfprintf_s);
    X(__stdio_common_vfscanf);
    X(__stdio_common_vfwprintf);
    X(__stdio_common_vfwprintf_p);
    X(__stdio_common_vfwprintf_s);
    X(__stdio_common_vfwscanf);
    X(__stdio_common_vsnprintf_s);
    X(__stdio_common_vsnwprintf_s);
    X(__stdio_common_vsprintf);
    X(__stdio_common_vsprintf_p);
    X(__stdio_common_vsprintf_s);
    X(__stdio_common_vsscanf);
    X(__stdio_common_vswprintf);
    X(__stdio_common_vswprintf_p);
    X(__stdio_common_vswprintf_s);
    X(__stdio_common_vswscanf);
    X(_access);
    X(_beginthreadex);
    X(_byteswap_ulong);
    X(_byteswap_ushort);
    X(_chgsign);
    X(_chmod);
    X(_chmod);
    X(_copysign);
    X(_ctime64);
    X(_ctime64_s);
    X(_difftime64);
    X(_endthreadex);
    X(_errno);
    X(_fileno);
    X(_fileno);
    X(_fileno);
    X(_findclose);
    X(_findfirst64i32);
    X(_findnext64i32);
    X(_findnext64i32);
    X(_findnext64i32);
    X(_fstat64i32);
    X(_gmtime64);
    X(_gmtime64_s);
    X(_hypot);
    X(_hypotf);
    X(_invalid_parameter_noinfo);
    X(_isatty);
    X(_isctype_l);
    X(_localtime64);
    X(_localtime64_s);
    X(_mkdir);
    X(_mkdir);
    X(_mkgmtime64);
    X(_mktime64);
    X(_msize);
    X(_pclose);
    X(_popen);
    X(_setmode);
    X(_setmode);
    X(_snprintf);
    X(_stat64i32);
    X(_stat64i32);
    X(_strdup);
    X(_time64);
    X(_timespec64_get);
    X(_unlink);
    X(_wcsicmp);
    X(_wctime64);
    X(_wctime64_s);
    X(_wunlink);
    X(acos);
    X(asin);
    X(atan);
    X(atan2);
    X(atoi);
    X(calloc);
    X(ceil);
    X(cos);
    X(cosh);
    X(exit);
    X(exp);
    X(fabs);
    X(fclose);
    X(fflush);
    X(fgetc);
    X(fgets);
    X(floor);
    X(fmod);
    X(fopen);
    X(fprintf);
    X(fputc);
    X(fputs);
    X(fread);
    X(free);
    X(frexp);
    X(fseek);
    X(ftell);
    X(fwrite);
    X(getenv);
    X(isalnum);
    X(isalpha);
    X(isdigit);
    X(isprint);
    X(isspace);
    X(ldexp);
    X(log);
    X(log10);
    X(lstrcmpW);
    X(lstrcmpiW);
    X(lstrlenW);
    X(malloc);
    X(memcmp);
    X(memcpy);
    X(memmove);
    X(memset);
    X(modf);
    X(pow);
    X(printf);
    X(putc);
    X(realloc);
    X(rewind);
    X(setvbuf);
    X(sin);
    X(sinh);
    X(sprintf);
    X(sqrt);
    X(sscanf);
    X(strchr);
    X(strcmp);
    X(strcpy);
    X(strcspn);
    X(strlen);
    X(strncmp);
    X(strncpy);
    X(strncpy);
    X(strncpy);
    X(strncpy);
    X(strnlen);
    X(strrchr);
    X(strstr);
    X(strtol);
    X(system);
    X(tan);
    X(tanh);
    X(tolower);
    X(uaw_lstrcmpW);
    X(uaw_lstrcmpiW);
    X(uaw_lstrlenW);
    X(uaw_wcschr);
    X(uaw_wcscpy);
    X(uaw_wcsicmp);
    X(uaw_wcslen);
    X(uaw_wcsrchr);
    X(vfprintf);
    X(vsprintf);
    X(wcschr);
    X(wcscpy);
    X(wcscpy_s);
    X(wcslen);
    X(wcsnlen);
    X(wcsrchr);
    X(wcstok);

    Y("__stosb", Xstosb);
    Y("_ReadWriteBarrier", XReadWriteBarrier);

#ifndef NDEBUG
    X(_wassert);
#endif

#undef X
  }

  void* ret = hashmap_get(&L(runtime_function_map), name);
  if (ret == NULL) {
    if (strcmp(name, "uaw_CharUpperW") == 0 ||             //
        strcmp(name, "__readgsqword") == 0 ||              //
        strcmp(name, "__readgsdword") == 0 ||              //
        strcmp(name, "__readgsword") == 0 ||               //
        strcmp(name, "__readgsbyte") == 0 ||               //
        strcmp(name, "__stosb") == 0 ||                    //
        strcmp(name, "_ReadWriteBarrier") == 0 ||          //
        strcmp(name, "_umul128") == 0 ||                   //
        strcmp(name, "_mul128") == 0 ||                    //
        strcmp(name, "__shiftright128") == 0 ||            //
        strcmp(name, "_InterlockedExchangeAdd64") == 0 ||  //
        strcmp(name, "_InterlockedExchangeAdd") == 0       //
    ) {
      return (void*)Unimplemented;
    }
  }
  return ret;
}
#endif

static void* symbol_lookup(char* name) {
  if (user_context->get_function_address) {
    void* f = user_context->get_function_address(name);
    if (f) {
      return f;
    }
  }

#if X64WIN
  void* f = get_standard_runtime_function(name);
  if (f) {
    return f;
  }
  return (void*)GetProcAddress(GetModuleHandle(NULL), name);
#else
  return dlsym(NULL, name);
#endif
}

static bool link_all_files(void) {
  UserContext* uc = user_context;

  if (uc->num_files == 0)
    return false;

  // Process fixups.
  for (size_t i = 0; i < uc->num_files; ++i) {
    FileLinkData* fld = &uc->files[i];

    if (!make_memory_readwrite(fld->codeseg_base_address, fld->codeseg_size)) {
      outaf("failed to make %p size %zu readwrite\n", fld->codeseg_base_address, fld->codeseg_size);
      return false;
    }

    for (int j = 0; j < fld->flen; ++j) {
      void* fixup_address = fld->fixups[j].at;
      char* name = fld->fixups[j].name;
      int addend = fld->fixups[j].addend;

      void* target_address = hashmap_get(&uc->global_data[i], name);
      if (!target_address) {
        target_address = hashmap_get(&uc->exports[i], name);
        if (!target_address) {
          target_address = hashmap_get(&uc->global_data[uc->num_files], name);
          if (!target_address) {
            target_address = hashmap_get(&uc->exports[uc->num_files], name);
            if (!target_address) {
              target_address = symbol_lookup(name);
              if (!target_address) {
                outaf("undefined symbol: %s\n", name);
                return false;
              }
            }
          }
        }
      }

      *((uintptr_t*)fixup_address) = (uintptr_t)target_address + addend;
    }

    if (!make_memory_executable(fld->codeseg_base_address, fld->codeseg_size)) {
      outaf("failed to make %p size %zu executable\n", fld->codeseg_base_address,
            fld->codeseg_size);
      return false;
    }
  }

  return true;
}
//
// END OF ../../src/link.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/main.c
//

#if X64WIN
#include <direct.h>
#endif

#define C(x) compiler_state.main__##x
#define L(x) linker_state.main__##x

#if 0  // for -E call after preprocess().
static void print_tokens(Token* tok) {
  int line = 1;
  for (; tok->kind != TK_EOF; tok = tok->next) {
    if (line > 1 && tok->at_bol)
      printf("\n");
    if (tok->has_space && !tok->at_bol)
      printf(" ");
    printf("%.*s", tok->len, tok->loc);
    line++;
  }
  printf("\n");
}
#endif

static int default_output_fn(const char* fmt, va_list ap) {
  int ret = vfprintf(stdout, fmt, ap);
  return ret;
}

static bool default_load_file_fn(const char* path, char** contents, size_t* size) {
  FILE* fp = fopen(path, "rb");
  if (!fp) {
    return false;
  }

  fseek(fp, 0, SEEK_END);
  *size = ftell(fp);
  rewind(fp);
  *contents = malloc(*size);
  fread(*contents, 1, *size, fp);
  fclose(fp);
  return true;
}

DyibiccContext* dyibicc_set_environment(DyibiccEnviromentData* env_data) {
  // Set this up with a temporary value early, mostly so we can ABORT() below
  // with output if necessary.
  UserContext tmp_user_context = {0};
  tmp_user_context.output_function = default_output_fn;
  tmp_user_context.load_file_contents = default_load_file_fn;
  user_context = &tmp_user_context;

  alloc_init(AL_Temp);

  // Clone env_data into allocated ctx

  size_t total_include_paths_len = 0;
  size_t num_include_paths = 0;
  for (const char** p = env_data->include_paths; *p; ++p) {
    total_include_paths_len += strlen(*p) + 1;
    ++num_include_paths;
  }

  StringArray sys_inc_paths = {0};
#if X64WIN
  strarray_push(&sys_inc_paths, "__include__/win", AL_Temp);
  strarray_push(&sys_inc_paths, "__include__/all", AL_Temp);

#define GET_ENV_VAR(x)                          \
  char* env_##x = getenv(#x);                   \
  if (!env_##x) {                               \
    ABORT("environment variable " #x " unset"); \
  }

  GET_ENV_VAR(WindowsSdkDir);
  GET_ENV_VAR(WindowsSdkLibVersion);
  GET_ENV_VAR(VcToolsInstallDir);
#undef GET_ENV_VAR

  strarray_push(&sys_inc_paths,
                format(AL_Temp, "%sInclude\\%sucrt", env_WindowsSdkDir, env_WindowsSdkLibVersion),
                AL_Temp);
  strarray_push(&sys_inc_paths,
                format(AL_Temp, "%sInclude\\%sum", env_WindowsSdkDir, env_WindowsSdkLibVersion),
                AL_Temp);
  strarray_push(&sys_inc_paths,
                format(AL_Temp, "%sInclude\\%sshared", env_WindowsSdkDir, env_WindowsSdkLibVersion),
                AL_Temp);
  strarray_push(&sys_inc_paths, format(AL_Temp, "%sinclude", env_VcToolsInstallDir), AL_Temp);

#else
  strarray_push(&sys_inc_paths, "__include__/linux", AL_Temp);
  strarray_push(&sys_inc_paths, "__include__/all", AL_Temp);

  strarray_push(&sys_inc_paths, "/usr/local/include", AL_Temp);
  strarray_push(&sys_inc_paths, "/usr/include/x86_64-linux-gnu", AL_Temp);
  strarray_push(&sys_inc_paths, "/usr/include", AL_Temp);
#endif

  for (int i = 0; i < sys_inc_paths.len; ++i) {
    total_include_paths_len += strlen(sys_inc_paths.data[i]) + 1;
    ++num_include_paths;
  }

  // Don't currently need dyibicc_include_dir once sys_inc_paths are added to.

  size_t total_source_files_len = 0;
  size_t num_files = 0;
  for (const char** p = env_data->files; *p; ++p) {
    total_source_files_len += strlen(*p) + 1;
    ++num_files;
  }

  size_t total_size =
      sizeof(UserContext) +                       // base structure
      (num_include_paths * sizeof(char*)) +       // array in base structure
      (num_files * sizeof(FileLinkData)) +        // array in base structure
      (total_include_paths_len * sizeof(char)) +  // pointed to by include_paths
      (total_source_files_len * sizeof(char)) +   // pointed to by FileLinkData.source_name
      ((num_files + 1) * sizeof(HashMap)) +       // +1 beyond num_files for fully global dataseg
      ((num_files + 1) * sizeof(HashMap))         // +1 beyond num_files for fully global exports
      ;

  UserContext* data = calloc(1, total_size);

  data->load_file_contents = env_data->load_file_contents;
  if (!data->load_file_contents) {
    data->load_file_contents = default_load_file_fn;
  }
  data->get_function_address = env_data->get_function_address;
  data->output_function = env_data->output_function;
  if (!data->output_function) {
    data->output_function = default_output_fn;
  }
  data->use_ansi_codes = env_data->use_ansi_codes;
  data->generate_debug_symbols = env_data->generate_debug_symbols;

  char* d = (char*)(&data[1]);

  data->num_include_paths = num_include_paths;
  data->include_paths = (char**)d;
  d += sizeof(char*) * num_include_paths;

  data->num_files = num_files;
  data->files = (FileLinkData*)d;
  d += sizeof(FileLinkData) * num_files;

  data->global_data = (HashMap*)d;
  d += sizeof(HashMap) * (num_files + 1);

  data->exports = (HashMap*)d;
  d += sizeof(HashMap) * (num_files + 1);

  int i = 0;
  for (const char** p = env_data->include_paths; *p; ++p) {
    data->include_paths[i++] = d;
    strcpy(d, *p);
    d += strlen(*p) + 1;
  }
  for (int j = 0; j < sys_inc_paths.len; ++j) {
    data->include_paths[i++] = d;
    strcpy(d, sys_inc_paths.data[j]);
    d += strlen(sys_inc_paths.data[j]) + 1;
  }

  i = 0;
  for (const char** p = env_data->files; *p; ++p) {
    FileLinkData* dld = &data->files[i++];
    dld->source_name = d;
    strcpy(dld->source_name, *p);
    d += strlen(*p) + 1;
  }

  // These maps store an arbitrary number of symbols, and they must persist
  // beyond AL_Link (to be saved for relink updates) so they must be manually
  // managed.
  for (size_t j = 0; j < num_files + 1; ++j) {
    data->global_data[j].alloc_lifetime = AL_Manual;
    data->exports[j].alloc_lifetime = AL_Manual;
  }
  data->reflect_types.alloc_lifetime = AL_UserContext;

  if ((size_t)(d - (char*)data) != total_size) {
    ABORT("incorrect size calculation");
  }

  user_context = data;
  alloc_reset(AL_Temp);
  alloc_init(AL_UserContext);
  return (DyibiccContext*)data;
}

void dyibicc_free(DyibiccContext* context) {
  UserContext* ctx = (UserContext*)context;
  assert(ctx == user_context && "only one context currently supported");
  for (size_t i = 0; i < ctx->num_files + 1; ++i) {
    hashmap_clear_manual_key_owned_value_owned_aligned(&ctx->global_data[i]);
    hashmap_clear_manual_key_owned_value_unowned(&ctx->exports[i]);
  }
  alloc_reset(AL_UserContext);

  for (size_t i = 0; i < ctx->num_files; ++i) {
    free_link_fixups(&ctx->files[i]);
  }
#if X64WIN
  unregister_and_free_function_table_data(ctx);
#endif
  free(ctx);
  user_context = NULL;
}

bool dyibicc_update(DyibiccContext* context, char* filename, char* contents) {
  if (setjmp(toplevel_update_jmpbuf) != 0) {
    codegen_free();
    alloc_reset(AL_Compile);
    alloc_reset(AL_Temp);
    alloc_reset(AL_Link);
    memset(&compiler_state, 0, sizeof(compiler_state));
    memset(&linker_state, 0, sizeof(linker_state));
    return false;
  }

  UserContext* ctx = (UserContext*)context;
  bool link_result = true;

  assert(ctx == user_context && "only one context currently supported");

  bool compiled_any = false;
  {
    for (size_t i = 0; i < ctx->num_files; ++i) {
      FileLinkData* dld = &ctx->files[i];

      if (filename && strcmp(dld->source_name, filename) != 0) {
        // If a specific update is provided, we only compile that one.
        continue;
      }

      {
        alloc_init(AL_Compile);

        init_macros();
        C(base_file) = dld->source_name;
        Token* tok;
        if (filename) {
          tok = tokenize_filecontents(filename, contents);
        } else {
          tok = tokenize_file(C(base_file));
        }
        if (!tok)
          error("%s: %s", C(base_file), strerror(errno));
        tok = preprocess(tok);
        tok = add_container_instantiations(tok);

        codegen_init();  // Initializes dynasm so that parse() can assign labels.

        Obj* prog = parse(tok);
        codegen(prog, i);

        compiled_any = true;

        alloc_reset(AL_Compile);
      }
    }

    if (compiled_any) {
      alloc_init(AL_Link);

      link_result = link_all_files();

      alloc_reset(AL_Link);
    }
  }

  return link_result;
}

void* dyibicc_find_export(DyibiccContext* context, char* name) {
  UserContext* ctx = (UserContext*)context;
  return hashmap_get(&ctx->exports[ctx->num_files], name);
}
//
// END OF ../../src/main.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/parse.c
//
// This file contains a recursive descent parser for C.
//
// Most functions in this file are named after the symbols they are
// supposed to read from an input token list. For example, stmt() is
// responsible for reading a statement from a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens. Since C doesn't support
// multiple return values, the remaining tokens are returned to the
// caller via a pointer argument.
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this
// parser.



#define C(x) compiler_state.parse__##x

// Scope for local variables, global variables, typedefs
// or enum constants
typedef struct {
  Obj* var;
  Type* type_def;
  Type* enum_ty;
  int enum_val;
} VarScope;

// Variable attributes such as typedef or extern.
typedef struct {
  bool is_typedef;
  bool is_static;
  bool is_extern;
  bool is_inline;
  bool is_tls;
  int align;
} VarAttr;

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
typedef struct Initializer Initializer;
struct Initializer {
  Initializer* next;
  Type* ty;
  Token* tok;
  bool is_flexible;

  // If it's not an aggregate type and has an initializer,
  // `expr` has an initialization expression.
  Node* expr;

  // If it's an initializer for an aggregate type (e.g. array or struct),
  // `children` has initializers for its children.
  Initializer** children;

  // Only one member can be initialized for a union.
  // `mem` is used to clarify which member is initialized.
  Member* mem;
};

// For local variable initializer.
typedef struct InitDesg InitDesg;
struct InitDesg {
  InitDesg* next;
  int idx;
  Member* member;
  Obj* var;
};

static bool is_typename(Token* tok);
static Type* declspec(Token** rest, Token* tok, VarAttr* attr);
static Type* typename(Token** rest, Token* tok);
static Type* enum_specifier(Token** rest, Token* tok);
static Type* typeof_specifier(Token** rest, Token* tok);
static Type* type_suffix(Token** rest, Token* tok, Type* ty);
static Type* declarator(Token** rest, Token* tok, Type* ty);
static Node* declaration(Token** rest, Token* tok, Type* basety, VarAttr* attr);
static void array_initializer2(Token** rest, Token* tok, Initializer* init, int i);
static void struct_initializer2(Token** rest, Token* tok, Initializer* init, Member* mem);
static void initializer2(Token** rest, Token* tok, Initializer* init);
static Initializer* initializer(Token** rest, Token* tok, Type* ty, Type** new_ty);
static Node* lvar_initializer(Token** rest, Token* tok, Obj* var);
static void gvar_initializer(Token** rest, Token* tok, Obj* var);
static Node* compound_stmt(Token** rest, Token* tok);
static Node* stmt(Token** rest, Token* tok);
static Node* expr_stmt(Token** rest, Token* tok);
static Node* expr(Token** rest, Token* tok);
static int64_t eval(Node* node);
static int64_t eval2(Node* node, char*** label, int** pclabel);
static int64_t eval_rval(Node* node, char*** label, int** pclabel);
static int64_t const_expr(Token** rest, Token* tok);
static bool is_const_expr(Node* node);
static Node* assign(Token** rest, Token* tok);
static Node* logor(Token** rest, Token* tok);
static double eval_double(Node* node);
static Node* conditional(Token** rest, Token* tok);
static Node* logand(Token** rest, Token* tok);
static Node* bitor (Token * *rest, Token* tok);
static Node* bitxor(Token** rest, Token* tok);
static Node*bitand(Token** rest, Token* tok);
static Node* equality(Token** rest, Token* tok);
static Node* relational(Token** rest, Token* tok);
static Node* shift(Token** rest, Token* tok);
static Node* add(Token** rest, Token* tok);
static Node* new_add(Node* lhs, Node* rhs, Token* tok);
static Node* new_sub(Node* lhs, Node* rhs, Token* tok);
static Node* mul(Token** rest, Token* tok);
static Node* cast(Token** rest, Token* tok);
static Member* get_struct_member(Type* ty, Token* tok);
static Type* struct_decl(Token** rest, Token* tok);
static Type* union_decl(Token** rest, Token* tok);
static Node* postfix(Token** rest, Token* tok);
static Node* funcall(Token** rest, Token* tok, Node* node, Node* injected_self);
static Node* unary(Token** rest, Token* tok);
static Node* primary(Token** rest, Token* tok);
static Token* parse_typedef(Token* tok, Type* basety);
static bool is_function(Token* tok);
static Token* function(Token* tok, Type* basety, VarAttr* attr);
static Token* global_variable(Token* tok, Type* basety, VarAttr* attr);

static int align_down(int n, int align) {
  return (int)align_to_s(n - align + 1, align);
}

static void enter_scope(void) {
  Scope* sc = bumpcalloc(1, sizeof(Scope), AL_Compile);
  sc->next = C(scope);
  C(scope) = sc;
}

static void leave_scope(void) {
  C(scope) = C(scope)->next;
}

// Find a variable by name.
static VarScope* find_var(Token* tok) {
  for (Scope* sc = C(scope); sc; sc = sc->next) {
    VarScope* sc2 = hashmap_get2(&sc->vars, tok->loc, tok->len);
    if (sc2)
      return sc2;
  }
  return NULL;
}

static Type* find_tag(Token* tok) {
  for (Scope* sc = C(scope); sc; sc = sc->next) {
    Type* ty = hashmap_get2(&sc->tags, tok->loc, tok->len);
    if (ty)
      return ty;
  }
  return NULL;
}

static Node* new_node(NodeKind kind, Token* tok) {
  Node* node = bumpcalloc(1, sizeof(Node), AL_Compile);
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Node* new_binary(NodeKind kind, Node* lhs, Node* rhs, Token* tok) {
  Node* node = new_node(kind, tok);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node* new_unary(NodeKind kind, Node* expr, Token* tok) {
  Node* node = new_node(kind, tok);
  node->lhs = expr;
  return node;
}

static Node* new_num(int64_t val, Token* tok) {
  Node* node = new_node(ND_NUM, tok);
  node->val = val;
  return node;
}

static Node* new_long(int64_t val, Token* tok) {
  Node* node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_long;
  return node;
}

static Node* new_ulong(long val, Token* tok) {
  Node* node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_ulong;
  return node;
}

static Node* new_var_node(Obj* var, Token* tok) {
  Node* node = new_node(ND_VAR, tok);
  node->var = var;
  return node;
}

static Node* new_vla_ptr(Obj* var, Token* tok) {
  Node* node = new_node(ND_VLA_PTR, tok);
  node->var = var;
  return node;
}

static Node* new_reflect_type_ptr(_ReflectType* rty, Token* tok) {
  Node* node = new_node(ND_REFLECT_TYPE_PTR, tok);
  node->reflect_ty = (uintptr_t)rty;
  return node;
}

static Node* new_cast(Node* expr, Type* ty) {
  add_type(expr);

  Node* node = bumpcalloc(1, sizeof(Node), AL_Compile);
  node->kind = ND_CAST;
  node->tok = expr->tok;
  node->lhs = expr;
  node->ty = copy_type(ty);
  return node;
}

static VarScope* push_scope(char* name) {
  VarScope* sc = bumpcalloc(1, sizeof(VarScope), AL_Compile);
  hashmap_put(&C(scope)->vars, name, sc);
  return sc;
}

static Initializer* new_initializer(Type* ty, bool is_flexible, Token* err_tok) {
  Initializer* init = bumpcalloc(1, sizeof(Initializer), AL_Compile);
  init->ty = ty;

  if (ty->kind == TY_ARRAY) {
    if (is_flexible && ty->size < 0) {
      init->is_flexible = true;
      return init;
    }

    if (ty->array_len < 0) {
      error_tok(err_tok, "array has incomplete element type");
    }

    init->children = bumpcalloc(ty->array_len, sizeof(Initializer*), AL_Compile);
    for (int i = 0; i < ty->array_len; i++)
      init->children[i] = new_initializer(ty->base, false, err_tok);
    return init;
  }

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    // Count the number of struct members.
    int len = 0;
    for (Member* mem = ty->members; mem; mem = mem->next)
      len++;

    init->children = bumpcalloc(len, sizeof(Initializer*), AL_Compile);

    for (Member* mem = ty->members; mem; mem = mem->next) {
      if (is_flexible && ty->is_flexible && !mem->next) {
        Initializer* child = bumpcalloc(1, sizeof(Initializer), AL_Compile);
        child->ty = mem->ty;
        child->is_flexible = true;
        init->children[mem->idx] = child;
      } else {
        init->children[mem->idx] = new_initializer(mem->ty, false, err_tok);
      }
    }
    return init;
  }

  return init;
}

static Obj* new_var(char* name, Type* ty) {
  Obj* var = bumpcalloc(1, sizeof(Obj), AL_Compile);
  var->name = name;
  var->ty = ty;
  var->align = ty->align;
  push_scope(name)->var = var;
  return var;
}

static Obj* new_lvar(char* name, Type* ty) {
  Obj* var = new_var(name, ty);
  var->is_local = true;
  var->next = C(locals);
  C(locals) = var;
  return var;
}

static Obj* new_gvar(char* name, Type* ty) {
  Obj* var = new_var(name, ty);
  var->next = C(globals);
  var->is_static = true;
  var->is_definition = true;
  C(globals) = var;
  return var;
}

static char* new_unique_name(void) {
  return format(AL_Compile, "L..%d", C(unique_name_id)++);
}

static Obj* new_anon_gvar(Type* ty) {
  return new_gvar(new_unique_name(), ty);
}

static Obj* new_string_literal(char* p, Type* ty) {
  Obj* var = new_anon_gvar(ty);
  var->init_data = p;
  var->is_rodata = true;
  return var;
}

static char* get_ident(Token* tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected an identifier");
  return bumpstrndup(tok->loc, tok->len, AL_Compile);
}

static Type* find_typedef(Token* tok) {
  if (tok->kind == TK_IDENT) {
    VarScope* sc = find_var(tok);
    if (sc)
      return sc->type_def;
  }
  return NULL;
}

static void push_tag_scope(Token* tok, Type* ty) {
  hashmap_put2(&C(scope)->tags, tok->loc, tok->len, ty);
}

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//             | "typedef" | "static" | "extern" | "inline"
//             | "_Thread_local" | "__thread"
//             | "signed" | "unsigned"
//             | struct-decl | union-decl | typedef-name
//             | enum-specifier | typeof-specifier
//             | "const" | "volatile" | "auto" | "register" | "restrict"
//             | "__restrict" | "__restrict__" | "_Noreturn")+
//
// The order of typenames in a type-specifier doesn't matter. For
// example, `int long static` means the same as `static long int`.
// That can also be written as `static long` because you can omit
// `int` if `long` or `short` are specified. However, something like
// `char int` is not a valid type specifier. We have to accept only a
// limited combinations of the typenames.
//
// In this function, we count the number of occurrences of each typename
// while keeping the "current" type object that the typenames up
// until that point represent. When we reach a non-typename token,
// we returns the current type object.
static Type* declspec(Token** rest, Token* tok, VarAttr* attr) {
  // We use a single integer as counters for all typenames.
  // For example, bits 0 and 1 represents how many times we saw the
  // keyword "void" so far. With this, we can use a switch statement
  // as you can see below.
  enum {
    VOID = 1 << 0,
    BOOL = 1 << 2,
    CHAR = 1 << 4,
    SHORT = 1 << 6,
    INT = 1 << 8,
    LONG = 1 << 10,
#if X64WIN
    INT64 = 1 << 12,
#endif
    FLOAT = 1 << 14,
    DOUBLE = 1 << 16,
    OTHER = 1 << 18,
    SIGNED = 1 << 19,
    UNSIGNED = 1 << 20,
  };

  Type* ty = ty_int;
  int counter = 0;
  bool is_atomic = false;

  while (is_typename(tok)) {
    // Handle storage class specifiers.
    if (equal(tok, "typedef") || equal(tok, "static") || equal(tok, "extern") ||
        equal(tok, "inline") || equal(tok, "_Thread_local") || equal(tok, "__thread")) {
      if (!attr)
        error_tok(tok, "storage class specifier is not allowed in this context");

      if (equal(tok, "typedef"))
        attr->is_typedef = true;
      else if (equal(tok, "static"))
        attr->is_static = true;
      else if (equal(tok, "extern"))
        attr->is_extern = true;
      else if (equal(tok, "inline"))
        attr->is_inline = true;
      else
        attr->is_tls = true;

      if (attr->is_typedef &&
          attr->is_static + attr->is_extern + attr->is_inline + attr->is_tls > 1)
        error_tok(tok,
                  "typedef may not be used together with static,"
                  " extern, inline, __thread or _Thread_local");
      tok = tok->next;
      continue;
    }

    // These keywords are recognized but ignored.
    if (consume(&tok, tok, "const") || consume(&tok, tok, "volatile") ||
        consume(&tok, tok, "auto") || consume(&tok, tok, "register") ||
        consume(&tok, tok, "restrict") || consume(&tok, tok, "__restrict") ||
        consume(&tok, tok, "__restrict__") || consume(&tok, tok, "_Noreturn"))
      continue;

    if (equal(tok, "_Atomic")) {
      tok = tok->next;
      if (equal(tok, "(")) {
        ty = typename(&tok, tok->next);
        tok = skip(tok, ")");
      }
      is_atomic = true;
      continue;
    }

    if (equal(tok, "_Alignas")) {
      if (!attr)
        error_tok(tok, "_Alignas is not allowed in this context");
      tok = skip(tok->next, "(");

      if (is_typename(tok))
        attr->align = typename(&tok, tok)->align;
      else
        attr->align = (int)const_expr(&tok, tok);
      tok = skip(tok, ")");
      continue;
    }

    // Handle user-defined types.
    Type* ty2 = find_typedef(tok);
    if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") || equal(tok, "typeof") ||
        ty2) {
      if (counter)
        break;

      if (equal(tok, "struct")) {
        ty = struct_decl(&tok, tok->next);
      } else if (equal(tok, "union")) {
        ty = union_decl(&tok, tok->next);
      } else if (equal(tok, "enum")) {
        ty = enum_specifier(&tok, tok->next);
      } else if (equal(tok, "typeof")) {
        ty = typeof_specifier(&tok, tok->next);
      } else {
        ty = ty2;
        tok = tok->next;
      }

      counter += OTHER;
      continue;
    }

    // Handle built-in types.
    if (equal(tok, "void"))
      counter += VOID;
    else if (equal(tok, "_Bool"))
      counter += BOOL;
    else if (equal(tok, "char"))
      counter += CHAR;
    else if (equal(tok, "short"))
      counter += SHORT;
    else if (equal(tok, "int"))
      counter += INT;
    else if (equal(tok, "long"))
      counter += LONG;
#if X64WIN
    else if (equal(tok, "__int64"))
      counter += INT64;
#endif
    else if (equal(tok, "float"))
      counter += FLOAT;
    else if (equal(tok, "double"))
      counter += DOUBLE;
    else if (equal(tok, "signed"))
      counter |= SIGNED;
    else if (equal(tok, "unsigned"))
      counter |= UNSIGNED;
    else
      unreachable();

    switch (counter) {
      case VOID:
        ty = ty_void;
        break;
      case BOOL:
        ty = ty_bool;
        break;
      case CHAR:
      case SIGNED + CHAR:
        ty = ty_char;
        break;
      case UNSIGNED + CHAR:
        ty = ty_uchar;
        break;
      case SHORT:
      case SHORT + INT:
      case SIGNED + SHORT:
      case SIGNED + SHORT + INT:
        ty = ty_short;
        break;
      case UNSIGNED + SHORT:
      case UNSIGNED + SHORT + INT:
        ty = ty_ushort;
        break;
#if X64WIN
      case INT:
      case SIGNED:
      case SIGNED + INT:
      case LONG:
      case LONG + INT:
      case SIGNED + LONG:
      case SIGNED + LONG + INT:
        ty = ty_int;
        break;
      case UNSIGNED:
      case UNSIGNED + INT:
      case UNSIGNED + LONG:
      case UNSIGNED + LONG + INT:
        ty = ty_uint;
        break;
      case LONG + LONG:
      case LONG + LONG + INT:
      case SIGNED + LONG + LONG:
      case SIGNED + LONG + LONG + INT:
      case INT64:
      case SIGNED + INT64:
        ty = ty_long;
        break;
      case UNSIGNED + LONG + LONG:
      case UNSIGNED + LONG + LONG + INT:
      case UNSIGNED + INT64:
        ty = ty_ulong;
        break;
#else
      case INT:
      case SIGNED:
      case SIGNED + INT:
        ty = ty_int;
        break;
      case UNSIGNED:
      case UNSIGNED + INT:
        ty = ty_uint;
        break;
      case LONG:
      case LONG + INT:
      case LONG + LONG:
      case LONG + LONG + INT:
      case SIGNED + LONG:
      case SIGNED + LONG + INT:
      case SIGNED + LONG + LONG:
      case SIGNED + LONG + LONG + INT:
        ty = ty_long;
        break;
      case UNSIGNED + LONG:
      case UNSIGNED + LONG + INT:
      case UNSIGNED + LONG + LONG:
      case UNSIGNED + LONG + LONG + INT:
        ty = ty_ulong;
        break;
#endif
      case FLOAT:
        ty = ty_float;
        break;
      case DOUBLE:
        ty = ty_double;
        break;
      case LONG + DOUBLE:
        ty = ty_ldouble;
        break;
      default:
        error_tok(tok, "invalid type");
    }

    tok = tok->next;
  }

  if (is_atomic) {
    ty = copy_type(ty);
    ty->is_atomic = true;
  }

  *rest = tok;
  return ty;
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
static Type* func_params(Token** rest, Token* tok, Type* ty) {
  if (equal(tok, "void") && equal(tok->next, ")")) {
    *rest = tok->next->next;
    return func_type(ty);
  }

  Type head = {0};
  Type* cur = &head;
  bool is_variadic = false;

  while (!equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");

    if (equal(tok, "...")) {
      is_variadic = true;
      tok = tok->next;
      skip(tok, ")");
      break;
    }

    Type* ty2 = declspec(&tok, tok, NULL);
    ty2 = declarator(&tok, tok, ty2);

    Token* name = ty2->name;
    Token* name_pos = ty2->name_pos;

    if (ty2->kind == TY_ARRAY) {
      // "array of T" is converted to "pointer to T" only in the parameter
      // context. For example, *argv[] is converted to **argv by this.
      ty2 = pointer_to(ty2->base);
      ty2->name = name;
      ty2->name_pos = name_pos;
    } else if (ty2->kind == TY_FUNC) {
      // Likewise, a function is converted to a pointer to a function
      // only in the parameter context.
      ty2 = pointer_to(ty2);
      ty2->name = name;
      ty2->name_pos = name_pos;
    }

    cur = cur->next = copy_type(ty2);
  }

  if (cur == &head)
    is_variadic = true;

  ty = func_type(ty);
  ty->params = head.next;
  ty->is_variadic = is_variadic;
  *rest = tok->next;
  return ty;
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
static Type* array_dimensions(Token** rest, Token* tok, Type* ty) {
  while (equal(tok, "static") || equal(tok, "restrict"))
    tok = tok->next;

  if (equal(tok, "]")) {
    ty = type_suffix(rest, tok->next, ty);
    return array_of(ty, -1, tok);
  }

  Node* expr = conditional(&tok, tok);
  tok = skip(tok, "]");
  ty = type_suffix(rest, tok, ty);

  if (ty->kind == TY_VLA || !is_const_expr(expr))
    return vla_of(ty, expr);
  int dim = (int)eval(expr);
  if (dim < 0) {
    error_tok(expr->tok, "array declared with negative bounds");
  }
  return array_of(ty, dim, tok);
}

// type-suffix = "(" func-params
//             | "[" array-dimensions
//             | ε
static Type* type_suffix(Token** rest, Token* tok, Type* ty) {
  if (equal(tok, "("))
    return func_params(rest, tok->next, ty);

  if (equal(tok, "["))
    return array_dimensions(rest, tok->next, ty);

  *rest = tok;
  return ty;
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
static Type* pointers(Token** rest, Token* tok, Type* ty) {
  while (consume(&tok, tok, "*")) {
    ty = pointer_to(ty);
    while (equal(tok, "const") || equal(tok, "volatile") || equal(tok, "restrict") ||
           equal(tok, "__restrict") || equal(tok, "__restrict__"))
      tok = tok->next;
  }
  *rest = tok;
  return ty;
}

// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
static Type* declarator(Token** rest, Token* tok, Type* ty) {
  ty = pointers(&tok, tok, ty);

  if (equal(tok, "(")) {
    Token* start = tok;
    Type dummy = {0};
    declarator(&tok, start->next, &dummy);
    tok = skip(tok, ")");
    ty = type_suffix(rest, tok, ty);
    return declarator(&tok, start->next, ty);
  }

  Token* name = NULL;
  Token* name_pos = tok;

  if (tok->kind == TK_IDENT) {
    name = tok;
    tok = tok->next;
  }

  ty = type_suffix(rest, tok, ty);
  ty->name = name;
  ty->name_pos = name_pos;
  return ty;
}

// abstract-declarator = pointers ("(" abstract-declarator ")")? type-suffix
static Type* abstract_declarator(Token** rest, Token* tok, Type* ty) {
  ty = pointers(&tok, tok, ty);

  if (equal(tok, "(")) {
    Token* start = tok;
    Type dummy = {0};
    abstract_declarator(&tok, start->next, &dummy);
    tok = skip(tok, ")");
    ty = type_suffix(rest, tok, ty);
    return abstract_declarator(&tok, start->next, ty);
  }

  return type_suffix(rest, tok, ty);
}

// type-name = declspec abstract-declarator
static Type* typename(Token** rest, Token* tok) {
  Type* ty = declspec(&tok, tok, NULL);
  return abstract_declarator(rest, tok, ty);
}

static bool is_end(Token* tok) {
  return equal(tok, "}") || (equal(tok, ",") && equal(tok->next, "}"));
}

static bool consume_end(Token** rest, Token* tok) {
  if (equal(tok, "}")) {
    *rest = tok->next;
    return true;
  }

  if (equal(tok, ",") && equal(tok->next, "}")) {
    *rest = tok->next->next;
    return true;
  }

  return false;
}

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
//
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
static Type* enum_specifier(Token** rest, Token* tok) {
  Type* ty = enum_type();

  // Read a struct tag.
  Token* tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    Type* ty2 = find_tag(tag);
    if (!ty2)
      error_tok(tag, "unknown enum type");
    if (ty2->kind != TY_ENUM)
      error_tok(tag, "not an enum tag");
    *rest = tok;
    return ty2;
  }

  tok = skip(tok, "{");

  // Read an enum-list.
  int i = 0;
  int val = 0;
  while (!consume_end(rest, tok)) {
    if (i++ > 0)
      tok = skip(tok, ",");

    char* name = get_ident(tok);
    tok = tok->next;

    if (equal(tok, "="))
      val = (int)const_expr(&tok, tok->next);

    VarScope* sc = push_scope(name);
    sc->enum_ty = ty;
    sc->enum_val = val++;
  }

  if (tag)
    push_tag_scope(tag, ty);
  return ty;
}

// typeof-specifier = "(" (expr | typename) ")"
static Type* typeof_specifier(Token** rest, Token* tok) {
  tok = skip(tok, "(");

  Type* ty;
  if (is_typename(tok)) {
    ty = typename(&tok, tok);
  } else {
    Node* node = expr(&tok, tok);
    add_type(node);
    ty = node->ty;
  }
  *rest = skip(tok, ")");
  return ty;
}

// Generate code for computing a VLA size.
static Node* compute_vla_size(Type* ty, Token* tok) {
  Node* node = new_node(ND_NULL_EXPR, tok);
  if (ty->base)
    node = new_binary(ND_COMMA, node, compute_vla_size(ty->base, tok), tok);

  if (ty->kind != TY_VLA)
    return node;

  Node* base_sz;
  if (ty->base->kind == TY_VLA)
    base_sz = new_var_node(ty->base->vla_size, tok);
  else
    base_sz = new_num(ty->base->size, tok);

  ty->vla_size = new_lvar("", ty_ulong);
  Node* expr = new_binary(ND_ASSIGN, new_var_node(ty->vla_size, tok),
                          new_binary(ND_MUL, ty->vla_len, base_sz, tok), tok);
  return new_binary(ND_COMMA, node, expr, tok);
}

static Node* new_alloca(Node* sz) {
  Node* node = new_unary(ND_FUNCALL, new_var_node(C(builtin_alloca), sz->tok), sz->tok);
  node->func_ty = C(builtin_alloca)->ty;
  node->ty = C(builtin_alloca)->ty->return_ty;
  node->args = sz;
  add_type(sz);
  return node;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node* declaration(Token** rest, Token* tok, Type* basety, VarAttr* attr) {
  Node head = {0};
  Node* cur = &head;
  int i = 0;

  while (!equal(tok, ";")) {
    if (i++ > 0)
      tok = skip(tok, ",");

    Type* ty = declarator(&tok, tok, basety);
    if (ty->kind == TY_VOID)
      error_tok(tok, "variable declared void");
    if (!ty->name)
      error_tok(ty->name_pos, "variable name omitted");

    if (attr && attr->is_static) {
      // static local variable
      Obj* var = new_anon_gvar(ty);
      push_scope(get_ident(ty->name))->var = var;
      if (equal(tok, "="))
        gvar_initializer(&tok, tok->next, var);
      continue;
    }

    // Generate code for computing a VLA size. We need to do this
    // even if ty is not VLA because ty may be a pointer to VLA
    // (e.g. int (*foo)[n][m] where n and m are variables.)
    cur = cur->next = new_unary(ND_EXPR_STMT, compute_vla_size(ty, tok), tok);

    if (ty->kind == TY_VLA) {
      if (equal(tok, "="))
        error_tok(tok, "variable-sized object may not be initialized");

      // Variable length arrays (VLAs) are translated to alloca() calls.
      // For example, `int x[n+2]` is translated to `tmp = n + 2,
      // x = alloca(tmp)`.
      Obj* var = new_lvar(get_ident(ty->name), ty);
      Token* tok2 = ty->name;
      Node* expr = new_binary(ND_ASSIGN, new_vla_ptr(var, tok2),
                              new_alloca(new_var_node(ty->vla_size, tok2)), tok2);

      cur = cur->next = new_unary(ND_EXPR_STMT, expr, tok2);
      continue;
    }

    Obj* var = new_lvar(get_ident(ty->name), ty);
    if (attr && attr->align)
      var->align = attr->align;

    if (equal(tok, "=")) {
      Node* expr = lvar_initializer(&tok, tok->next, var);
      cur = cur->next = new_unary(ND_EXPR_STMT, expr, tok);
    }

    if (var->ty->size < 0)
      error_tok(ty->name, "variable has incomplete type");
    if (var->ty->kind == TY_VOID)
      error_tok(ty->name, "variable declared void");
  }

  Node* node = new_node(ND_BLOCK, tok);
  node->body = head.next;
  *rest = tok->next;
  return node;
}

static Token* skip_excess_element(Token* tok) {
  if (equal(tok, "{")) {
    tok = skip_excess_element(tok->next);
    return skip(tok, "}");
  }

  assign(&tok, tok);
  return tok;
}

// string-initializer = string-literal
static void string_initializer(Token** rest, Token* tok, Initializer* init) {
  if (init->is_flexible)
    *init = *new_initializer(array_of(init->ty->base, tok->ty->array_len, tok), false, tok);

  int len = MIN(init->ty->array_len, tok->ty->array_len);

  switch (init->ty->base->size) {
    case 1: {
      char* str = tok->str;
      for (int i = 0; i < len; i++)
        init->children[i]->expr = new_num(str[i], tok);
      break;
    }
    case 2: {
      uint16_t* str = (uint16_t*)tok->str;
      for (int i = 0; i < len; i++)
        init->children[i]->expr = new_num(str[i], tok);
      break;
    }
    case 4: {
      uint32_t* str = (uint32_t*)tok->str;
      for (int i = 0; i < len; i++)
        init->children[i]->expr = new_num(str[i], tok);
      break;
    }
    default:
      unreachable();
  }

  *rest = tok->next;
}

// array-designator = "[" const-expr "]"
//
// C99 added the designated initializer to the language, which allows
// programmers to move the "cursor" of an initializer to any element.
// The syntax looks like this:
//
//   int x[10] = { 1, 2, [5]=3, 4, 5, 6, 7 };
//
// `[5]` moves the cursor to the 5th element, so the 5th element of x
// is set to 3. Initialization then continues forward in order, so
// 6th, 7th, 8th and 9th elements are initialized with 4, 5, 6 and 7,
// respectively. Unspecified elements (in this case, 3rd and 4th
// elements) are initialized with zero.
//
// Nesting is allowed, so the following initializer is valid:
//
//   int x[5][10] = { [5][8]=1, 2, 3 };
//
// It sets x[5][8], x[5][9] and x[6][0] to 1, 2 and 3, respectively.
//
// Use `.fieldname` to move the cursor for a struct initializer. E.g.
//
//   struct { int a, b, c; } x = { .c=5 };
//
// The above initializer sets x.c to 5.
static void array_designator(Token** rest, Token* tok, Type* ty, int* begin, int* end) {
  *begin = (int)const_expr(&tok, tok->next);
  if (*begin >= ty->array_len || *begin < 0)
    error_tok(tok, "array designator index exceeds array bounds");

  if (equal(tok, "...")) {
    *end = (int)const_expr(&tok, tok->next);
    if (*end >= ty->array_len || *end < 0)
      error_tok(tok, "array designator index exceeds array bounds");
    if (*end < *begin)
      error_tok(tok, "array designator range [%d, %d] is empty", *begin, *end);
  } else {
    *end = *begin;
  }

  *rest = skip(tok, "]");
}

// struct-designator = "." ident
static Member* struct_designator(Token** rest, Token* tok, Type* ty) {
  Token* start = tok;
  tok = skip(tok, ".");
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected a field designator");

  for (Member* mem = ty->members; mem; mem = mem->next) {
    // Anonymous struct member
    if (!mem->name) {
      if (mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION) {
        if (get_struct_member(mem->ty, tok)) {
          *rest = start;
          return mem;
        }
      }
      continue;
    }

    // Regular struct member
    if (mem->name->len == tok->len && !strncmp(mem->name->loc, tok->loc, tok->len)) {
      *rest = tok->next;
      return mem;
    }
  }

  error_tok(tok, "struct has no such member");
}

// designation = ("[" const-expr "]" | "." ident)* "="? initializer
static void designation(Token** rest, Token* tok, Initializer* init) {
  if (equal(tok, "[")) {
    if (init->ty->kind != TY_ARRAY)
      error_tok(tok, "array index in non-array initializer");

    int begin, end;
    array_designator(&tok, tok, init->ty, &begin, &end);

    Token* tok2 = NULL;
    for (int i = begin; i <= end; i++) {
      if (!init->children) {
        error_tok(tok, "incomplete array element type");
      }
      designation(&tok2, tok, init->children[i]);
    }
    array_initializer2(rest, tok2, init, begin + 1);
    return;
  }

  if (equal(tok, ".") && init->ty->kind == TY_STRUCT) {
    Member* mem = struct_designator(&tok, tok, init->ty);
    designation(&tok, tok, init->children[mem->idx]);
    init->expr = NULL;
    struct_initializer2(rest, tok, init, mem->next);
    return;
  }

  if (equal(tok, ".") && init->ty->kind == TY_UNION) {
    Member* mem = struct_designator(&tok, tok, init->ty);
    init->mem = mem;
    designation(rest, tok, init->children[mem->idx]);
    return;
  }

  if (equal(tok, "."))
    error_tok(tok, "field name not in struct or union initializer");

  if (equal(tok, "="))
    tok = tok->next;
  initializer2(rest, tok, init);
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
static int count_array_init_elements(Token* tok, Type* ty) {
  bool first = true;
  Initializer* dummy = new_initializer(ty->base, true, tok);

  int i = 0, max = 0;

  while (!consume_end(&tok, tok)) {
    if (!first)
      tok = skip(tok, ",");
    first = false;

    if (equal(tok, "[")) {
      i = (int)const_expr(&tok, tok->next);
      if (equal(tok, "..."))
        i = (int)const_expr(&tok, tok->next);
      tok = skip(tok, "]");
      designation(&tok, tok, dummy);
    } else {
      initializer2(&tok, tok, dummy);
    }

    i++;
    max = MAX(max, i);
  }
  return max;
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void array_initializer1(Token** rest, Token* tok, Initializer* init) {
  tok = skip(tok, "{");

  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(array_of(init->ty->base, len, tok), false, tok);
  }

  bool first = true;

  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(array_of(init->ty->base, len, tok), false, tok);
  }

  for (int i = 0; !consume_end(rest, tok); i++) {
    if (!first)
      tok = skip(tok, ",");
    first = false;

    if (equal(tok, "[")) {
      int begin, end;
      array_designator(&tok, tok, init->ty, &begin, &end);

      Token* tok2 = NULL;
      for (int j = begin; j <= end; j++)
        designation(&tok2, tok, init->children[j]);
      tok = tok2;
      i = end;
      continue;
    }

    if (i < init->ty->array_len)
      initializer2(&tok, tok, init->children[i]);
    else
      tok = skip_excess_element(tok);
  }
}

// array-initializer2 = initializer ("," initializer)*
static void array_initializer2(Token** rest, Token* tok, Initializer* init, int i) {
  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(array_of(init->ty->base, len, tok), false, tok);
  }

  for (; i < init->ty->array_len && !is_end(tok); i++) {
    Token* start = tok;
    if (i > 0)
      tok = skip(tok, ",");

    if (equal(tok, "[") || equal(tok, ".")) {
      *rest = start;
      return;
    }

    initializer2(&tok, tok, init->children[i]);
  }
  *rest = tok;
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void struct_initializer1(Token** rest, Token* tok, Initializer* init) {
  tok = skip(tok, "{");

  Member* mem = init->ty->members;
  bool first = true;

  while (!consume_end(rest, tok)) {
    if (!first)
      tok = skip(tok, ",");
    first = false;

    if (equal(tok, ".")) {
      mem = struct_designator(&tok, tok, init->ty);
      designation(&tok, tok, init->children[mem->idx]);
      mem = mem->next;
      continue;
    }

    if (mem) {
      initializer2(&tok, tok, init->children[mem->idx]);
      mem = mem->next;
    } else {
      tok = skip_excess_element(tok);
    }
  }
}

// struct-initializer2 = initializer ("," initializer)*
static void struct_initializer2(Token** rest, Token* tok, Initializer* init, Member* mem) {
  bool first = true;

  for (; mem && !is_end(tok); mem = mem->next) {
    Token* start = tok;

    if (!first)
      tok = skip(tok, ",");
    first = false;

    if (equal(tok, "[") || equal(tok, ".")) {
      *rest = start;
      return;
    }

    initializer2(&tok, tok, init->children[mem->idx]);
  }
  *rest = tok;
}

static void union_initializer(Token** rest, Token* tok, Initializer* init) {
  // Unlike structs, union initializers take only one initializer,
  // and that initializes the first union member by default.
  // You can initialize other member using a designated initializer.
  if (equal(tok, "{") && equal(tok->next, ".")) {
    Member* mem = struct_designator(&tok, tok->next, init->ty);
    init->mem = mem;
    designation(&tok, tok, init->children[mem->idx]);
    *rest = skip(tok, "}");
    return;
  }

  init->mem = init->ty->members;

  if (equal(tok, "{")) {
    initializer2(&tok, tok->next, init->children[0]);
    consume(&tok, tok, ",");
    *rest = skip(tok, "}");
  } else {
    initializer2(rest, tok, init->children[0]);
  }
}

// initializer = string-initializer | array-initializer
//             | struct-initializer | union-initializer
//             | assign
static void initializer2(Token** rest, Token* tok, Initializer* init) {
  if (init->ty->kind == TY_ARRAY && tok->kind == TK_STR) {
    string_initializer(rest, tok, init);
    return;
  }

  if (init->ty->kind == TY_ARRAY) {
    if (equal(tok, "{")) {
      if (init->ty->base->kind == TY_CHAR && tok->next->kind == TK_STR) {
        // A string initializer for a char array can be surrounded by braces.
        // E.g. `char str[] = {"foo"};`.
        initializer2(&tok, tok->next, init);
        *rest = skip(tok, "}");
        return;
      }
      array_initializer1(rest, tok, init);
      return;
    }
    array_initializer2(rest, tok, init, 0);
    return;
  }

  if (init->ty->kind == TY_STRUCT) {
    if (equal(tok, "{")) {
      struct_initializer1(rest, tok, init);
      return;
    }

    // A struct can be initialized with another struct. E.g.
    // `struct T x = y;` where y is a variable of type `struct T`.
    // Handle that case first.
    Node* expr = assign(rest, tok);
    add_type(expr);
    if (expr->ty->kind == TY_STRUCT) {
      init->expr = expr;
      return;
    }

    struct_initializer2(rest, tok, init, init->ty->members);
    return;
  }

  if (init->ty->kind == TY_UNION) {
    union_initializer(rest, tok, init);
    return;
  }

  if (equal(tok, "{")) {
    // An initializer for a scalar variable can be surrounded by
    // braces. E.g. `int x = {3};`. Handle that case.
    initializer2(&tok, tok->next, init);
    *rest = skip(tok, "}");
    return;
  }

  init->expr = assign(rest, tok);
}

static Type* copy_struct_type(Type* ty) {
  ty = copy_type(ty);

  Member head = {0};
  Member* cur = &head;
  for (Member* mem = ty->members; mem; mem = mem->next) {
    Member* m = bumpcalloc(1, sizeof(Member), AL_Compile);
    *m = *mem;
    cur = cur->next = m;
  }

  ty->members = head.next;
  return ty;
}

static Initializer* initializer(Token** rest, Token* tok, Type* ty, Type** new_ty) {
  Initializer* init = new_initializer(ty, true, tok);
  initializer2(rest, tok, init);

  if ((ty->kind == TY_STRUCT || ty->kind == TY_UNION) && ty->is_flexible) {
    ty = copy_struct_type(ty);

    Member* mem = ty->members;
    while (mem->next)
      mem = mem->next;
    mem->ty = init->children[mem->idx]->ty;
    ty->size += mem->ty->size;

    *new_ty = ty;
    return init;
  }

  *new_ty = init->ty;
  return init;
}

static Node* init_desg_expr(InitDesg* desg, Token* tok) {
  if (desg->var)
    return new_var_node(desg->var, tok);

  if (desg->member) {
    Node* node = new_unary(ND_MEMBER, init_desg_expr(desg->next, tok), tok);
    node->member = desg->member;
    return node;
  }

  Node* lhs = init_desg_expr(desg->next, tok);
  Node* rhs = new_num(desg->idx, tok);
  return new_unary(ND_DEREF, new_add(lhs, rhs, tok), tok);
}

static Node* create_lvar_init(Initializer* init, Type* ty, InitDesg* desg, Token* tok) {
  if (ty->kind == TY_ARRAY) {
    Node* node = new_node(ND_NULL_EXPR, tok);
    for (int i = 0; i < ty->array_len; i++) {
      InitDesg desg2 = {desg, i};
      Node* rhs = create_lvar_init(init->children[i], ty->base, &desg2, tok);
      node = new_binary(ND_COMMA, node, rhs, tok);
    }
    return node;
  }

  if (ty->kind == TY_STRUCT && !init->expr) {
    Node* node = new_node(ND_NULL_EXPR, tok);

    for (Member* mem = ty->members; mem; mem = mem->next) {
      InitDesg desg2 = {desg, 0, mem};
      Node* rhs = create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
      node = new_binary(ND_COMMA, node, rhs, tok);
    }
    return node;
  }

  if (ty->kind == TY_UNION) {
    Member* mem = init->mem ? init->mem : ty->members;
    InitDesg desg2 = {desg, 0, mem};
    return create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
  }

  if (!init->expr)
    return new_node(ND_NULL_EXPR, tok);

  Node* lhs = init_desg_expr(desg, tok);
  return new_binary(ND_ASSIGN, lhs, init->expr, tok);
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//   x[0][0] = 6;
//   x[0][1] = 7;
//   x[1][0] = 8;
//   x[1][1] = 9;
static Node* lvar_initializer(Token** rest, Token* tok, Obj* var) {
  Initializer* init = initializer(rest, tok, var->ty, &var->ty);
  InitDesg desg = {NULL, 0, NULL, var};

  // If a partial initializer list is given, the standard requires
  // that unspecified elements are set to 0. Here, we simply
  // zero-initialize the entire memory region of a variable before
  // initializing it with user-supplied values.
  Node* lhs = new_node(ND_MEMZERO, tok);
  lhs->var = var;

  Node* rhs = create_lvar_init(init, var->ty, &desg, tok);
  return new_binary(ND_COMMA, lhs, rhs, tok);
}

static uint64_t read_buf(char* buf, int sz) {
  if (sz == 1)
    return *buf;
  if (sz == 2)
    return *(uint16_t*)buf;
  if (sz == 4)
    return *(uint32_t*)buf;
  if (sz == 8)
    return *(uint64_t*)buf;
  unreachable();
}

static void write_buf(char* buf, uint64_t val, int sz) {
  if (sz == 1)
    *buf = (char)val;
  else if (sz == 2)
    *(uint16_t*)buf = (uint16_t)val;
  else if (sz == 4)
    *(uint32_t*)buf = (uint32_t)val;
  else if (sz == 8)
    *(uint64_t*)buf = (uint64_t)val;
  else
    unreachable();
}

static Relocation* write_gvar_data(Relocation* cur,
                                   Initializer* init,
                                   Type* ty,
                                   char* buf,
                                   int offset) {
  if (ty->kind == TY_ARRAY) {
    int sz = ty->base->size;
    for (int i = 0; i < ty->array_len; i++)
      cur = write_gvar_data(cur, init->children[i], ty->base, buf, offset + sz * i);
    return cur;
  }

  if (ty->kind == TY_STRUCT) {
    for (Member* mem = ty->members; mem; mem = mem->next) {
      if (mem->is_bitfield) {
        Node* expr = init->children[mem->idx]->expr;
        if (!expr)
          break;

        char* loc = buf + offset + mem->offset;
        uint64_t oldval = read_buf(loc, mem->ty->size);
        uint64_t newval = eval(expr);
        uint64_t mask = (1L << mem->bit_width) - 1;
        uint64_t combined = oldval | ((newval & mask) << mem->bit_offset);
        write_buf(loc, combined, mem->ty->size);
      } else {
        cur = write_gvar_data(cur, init->children[mem->idx], mem->ty, buf, offset + mem->offset);
      }
    }
    return cur;
  }

  if (ty->kind == TY_UNION) {
    if (!init->mem)
      return cur;
    return write_gvar_data(cur, init->children[init->mem->idx], init->mem->ty, buf, offset);
  }

  if (!init->expr)
    return cur;

  if (ty->kind == TY_FLOAT) {
    *(float*)(buf + offset) = (float)eval_double(init->expr);
    return cur;
  }

  if (ty->kind == TY_DOUBLE) {
    *(double*)(buf + offset) = eval_double(init->expr);
    return cur;
  }

  char** label = NULL;
  int* pc_label = NULL;
  uint64_t val = eval2(init->expr, &label, &pc_label);

  if (!label && !pc_label) {
    write_buf(buf + offset, val, ty->size);
    return cur;
  }

  Relocation* rel = bumpcalloc(1, sizeof(Relocation), AL_Compile);
  assert(!(label && pc_label));  // Both shouldn't be set.
  rel->offset = offset;
  rel->string_label = label;
  rel->internal_code_label = pc_label;
  rel->addend = (long)val;
  cur->next = rel;
  return cur->next;
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
static void gvar_initializer(Token** rest, Token* tok, Obj* var) {
  Initializer* init = initializer(rest, tok, var->ty, &var->ty);

  Relocation head = {0};
  char* buf = bumpcalloc(1, var->ty->size, AL_Compile);
  write_gvar_data(&head, init, var->ty, buf, 0);
  var->init_data = buf;
  var->rel = head.next;
}

// Returns true if a given token represents a type.
static bool is_typename(Token* tok) {
  if (C(typename_map).capacity == 0) {
    static char* kw[] = {
      "void",
      "_Bool",
      "char",
      "short",
      "int",
      "long",
      "struct",
      "union",
      "typedef",
      "enum",
      "static",
      "extern",
      "_Alignas",
      "signed",
      "unsigned",
      "const",
      "volatile",
      "auto",
      "register",
      "restrict",
      "__restrict",
      "__restrict__",
      "_Noreturn",
      "float",
      "double",
      "typeof",
      "inline",
      "_Thread_local",
      "__thread",
      "_Atomic",
#if X64WIN
      "__int64",
#endif
    };

    for (size_t i = 0; i < sizeof(kw) / sizeof(*kw); i++)
      hashmap_put(&C(typename_map), kw[i], (void*)1);
  }

  return hashmap_get2(&C(typename_map), tok->loc, tok->len) || find_typedef(tok);
}

// asm-stmt = "asm" ("volatile" | "inline")* "(" string-literal ")"
static Node* asm_stmt(Token** rest, Token* tok) {
  Node* node = new_node(ND_ASM, tok);
  tok = tok->next;

  while (equal(tok, "volatile") || equal(tok, "inline"))
    tok = tok->next;

  tok = skip(tok, "(");
  if (tok->kind != TK_STR || tok->ty->base->kind != TY_CHAR)
    error_tok(tok, "expected string literal");
  node->asm_str = tok->str;
  *rest = skip(tok->next, ")");
  return node;
}

// stmt = "return" expr? ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ("..." const-expr)? ":" stmt
//      | "default" ":" stmt
//      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "do" stmt "while" "(" expr ")" ";"
//      | "asm" asm-stmt
//      | "goto" (ident | "*" expr) ";"
//      | "break" ";"
//      | "continue" ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr-stmt
static Node* stmt(Token** rest, Token* tok) {
  if (equal(tok, "return")) {
    Node* node = new_node(ND_RETURN, tok);
    if (consume(rest, tok->next, ";"))
      return node;

    Node* exp = expr(&tok, tok->next);
    *rest = skip(tok, ";");

    add_type(exp);
    Type* ty = C(current_fn)->ty->return_ty;
    if (ty->kind != TY_STRUCT && ty->kind != TY_UNION)
      exp = new_cast(exp, C(current_fn)->ty->return_ty);

    node->lhs = exp;
    return node;
  }

  if (equal(tok, "if")) {
    Node* node = new_node(ND_IF, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    node->then = stmt(&tok, tok);
    if (equal(tok, "else"))
      node->els = stmt(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (equal(tok, "switch")) {
    Node* node = new_node(ND_SWITCH, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");

    Node* sw = C(current_switch);
    C(current_switch) = node;

    int brk_pc = C(brk_pc_label);
    C(brk_pc_label) = node->brk_pc_label = codegen_pclabel();

    node->then = stmt(rest, tok);

    C(current_switch) = sw;

    C(brk_pc_label) = brk_pc;

    return node;
  }

  if (equal(tok, "case")) {
    if (!C(current_switch))
      error_tok(tok, "stray case");

    Node* node = new_node(ND_CASE, tok);
    int begin = (int)const_expr(&tok, tok->next);
    int end;

    if (equal(tok, "...")) {
      // [GNU] Case ranges, e.g. "case 1 ... 5:"
      end = (int)const_expr(&tok, tok->next);
      if (end < begin)
        error_tok(tok, "empty case range specified");
    } else {
      end = begin;
    }

    tok = skip(tok, ":");
    node->label = new_unique_name();
    node->pc_label = codegen_pclabel();
    node->lhs = stmt(rest, tok);
    node->begin = begin;
    node->end = end;
    node->case_next = C(current_switch)->case_next;
    C(current_switch)->case_next = node;
    return node;
  }

  if (equal(tok, "default")) {
    if (!C(current_switch))
      error_tok(tok, "stray default");

    Node* node = new_node(ND_CASE, tok);
    tok = skip(tok->next, ":");
    node->label = new_unique_name();
    node->pc_label = codegen_pclabel();
    node->lhs = stmt(rest, tok);
    C(current_switch)->default_case = node;
    return node;
  }

  if (equal(tok, "for")) {
    Node* node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");

    enter_scope();

    int brk_pc = C(brk_pc_label);
    int cont_pc = C(cont_pc_label);
    C(brk_pc_label) = node->brk_pc_label = codegen_pclabel();
    C(cont_pc_label) = node->cont_pc_label = codegen_pclabel();

    if (is_typename(tok)) {
      Type* basety = declspec(&tok, tok, NULL);
      node->init = declaration(&tok, tok, basety, NULL);
    } else {
      node->init = expr_stmt(&tok, tok);
    }

    if (!equal(tok, ";"))
      node->cond = expr(&tok, tok);
    tok = skip(tok, ";");

    if (!equal(tok, ")"))
      node->inc = expr(&tok, tok);
    tok = skip(tok, ")");

    node->then = stmt(rest, tok);

    leave_scope();

    C(brk_pc_label) = brk_pc;
    C(cont_pc_label) = cont_pc;

    return node;
  }

  if (equal(tok, "while")) {
    Node* node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");

    int brk_pc = C(brk_pc_label);
    int cont_pc = C(cont_pc_label);
    C(brk_pc_label) = node->brk_pc_label = codegen_pclabel();
    C(cont_pc_label) = node->cont_pc_label = codegen_pclabel();

    node->then = stmt(rest, tok);

    C(brk_pc_label) = brk_pc;
    C(cont_pc_label) = cont_pc;
    return node;
  }

  if (equal(tok, "do")) {
    Node* node = new_node(ND_DO, tok);

    int brk_pc = C(brk_pc_label);
    int cont_pc = C(cont_pc_label);
    C(brk_pc_label) = node->brk_pc_label = codegen_pclabel();
    C(cont_pc_label) = node->cont_pc_label = codegen_pclabel();

    node->then = stmt(&tok, tok->next);

    C(brk_pc_label) = brk_pc;
    C(cont_pc_label) = cont_pc;

    tok = skip(tok, "while");
    tok = skip(tok, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    *rest = skip(tok, ";");
    return node;
  }

  if (equal(tok, "asm"))
    return asm_stmt(rest, tok);

  if (equal(tok, "goto")) {
    if (equal(tok->next, "*")) {
      // [GNU] `goto *ptr` jumps to the address specified by `ptr`.
      Node* node = new_node(ND_GOTO_EXPR, tok);
      node->lhs = expr(&tok, tok->next->next);
      *rest = skip(tok, ";");
      return node;
    }

    Node* node = new_node(ND_GOTO, tok);
    node->label = get_ident(tok->next);
    node->goto_next = C(gotos);
    C(gotos) = node;
    *rest = skip(tok->next->next, ";");
    return node;
  }

  if (equal(tok, "break")) {
    if (!C(brk_pc_label))
      error_tok(tok, "stray break");
    Node* node = new_node(ND_GOTO, tok);
    node->pc_label = C(brk_pc_label);
    *rest = skip(tok->next, ";");
    return node;
  }

  if (equal(tok, "continue")) {
    if (!C(cont_pc_label))
      error_tok(tok, "stray continue");
    Node* node = new_node(ND_GOTO, tok);
    node->pc_label = C(cont_pc_label);
    *rest = skip(tok->next, ";");
    return node;
  }

  if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
    Node* node = new_node(ND_LABEL, tok);
    node->label = bumpstrndup(tok->loc, tok->len, AL_Compile);
    node->pc_label = codegen_pclabel();
    node->lhs = stmt(rest, tok->next->next);
    node->goto_next = C(labels);
    C(labels) = node;
    return node;
  }

  if (equal(tok, "{"))
    return compound_stmt(rest, tok->next);

  return expr_stmt(rest, tok);
}

// compound-stmt = (typedef | declaration | stmt)* "}"
static Node* compound_stmt(Token** rest, Token* tok) {
  Node* node = new_node(ND_BLOCK, tok);
  Node head = {0};
  Node* cur = &head;

  enter_scope();

  while (!equal(tok, "}")) {
    if (is_typename(tok) && !equal(tok->next, ":")) {
      VarAttr attr = {0};
      Type* basety = declspec(&tok, tok, &attr);

      if (attr.is_typedef) {
        tok = parse_typedef(tok, basety);
        continue;
      }

      if (is_function(tok)) {
        tok = function(tok, basety, &attr);
        continue;
      }

      if (attr.is_extern) {
        tok = global_variable(tok, basety, &attr);
        continue;
      }

      cur = cur->next = declaration(&tok, tok, basety, &attr);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }
    add_type(cur);
  }

  leave_scope();

  node->body = head.next;
  *rest = tok->next;
  return node;
}

// expr-stmt = expr? ";"
static Node* expr_stmt(Token** rest, Token* tok) {
  if (equal(tok, ";")) {
    *rest = tok->next;
    return new_node(ND_BLOCK, tok);
  }

  Node* node = new_node(ND_EXPR_STMT, tok);
  node->lhs = expr(&tok, tok);
  *rest = skip(tok, ";");
  return node;
}

// expr = assign ("," expr)?
static Node* expr(Token** rest, Token* tok) {
  Node* node = assign(&tok, tok);

  if (equal(tok, ","))
    return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);

  *rest = tok;
  return node;
}

static int64_t eval(Node* node) {
  return eval2(node, NULL, NULL);
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a positive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
static int64_t eval2(Node* node, char*** label, int** pclabel) {
  add_type(node);

  if (is_flonum(node->ty))
    return (int64_t)eval_double(node);

  switch (node->kind) {
    case ND_ADD:
      return eval2(node->lhs, label, pclabel) + eval(node->rhs);
    case ND_SUB:
      return eval2(node->lhs, label, pclabel) - eval(node->rhs);
    case ND_MUL:
      return eval(node->lhs) * eval(node->rhs);
    case ND_DIV: {
      int64_t divisor = eval(node->rhs);
      if (divisor == 0)
        error_tok(node->tok, "division by zero");
      if (node->ty->is_unsigned)
        return (uint64_t)eval(node->lhs) / divisor;
      return eval(node->lhs) / divisor;
    }
    case ND_NEG:
      return -eval(node->lhs);
    case ND_MOD: {
      int64_t divisor = eval(node->rhs);
      if (divisor == 0)
        error_tok(node->tok, "division by zero");
      if (node->ty->is_unsigned)
        return (uint64_t)eval(node->lhs) % divisor;
      return eval(node->lhs) % divisor;
    }
    case ND_BITAND:
      return eval(node->lhs) & eval(node->rhs);
    case ND_BITOR:
      return eval(node->lhs) | eval(node->rhs);
    case ND_BITXOR:
      return eval(node->lhs) ^ eval(node->rhs);
    case ND_SHL:
      return eval(node->lhs) << eval(node->rhs);
    case ND_SHR:
      if (node->ty->is_unsigned && node->ty->size == 8)
        return (uint64_t)eval(node->lhs) >> eval(node->rhs);
      return eval(node->lhs) >> eval(node->rhs);
    case ND_EQ:
      return eval(node->lhs) == eval(node->rhs);
    case ND_NE:
      return eval(node->lhs) != eval(node->rhs);
    case ND_LT:
      if (node->lhs->ty->is_unsigned)
        return (int64_t)(uint64_t)eval(node->lhs) < eval(node->rhs);
      return eval(node->lhs) < eval(node->rhs);
    case ND_LE:
      if (node->lhs->ty->is_unsigned)
        return (int64_t)((uint64_t)eval(node->lhs) <= (uint64_t)eval(node->rhs));
      return (int64_t)(eval(node->lhs) <= eval(node->rhs));
    case ND_COND:
      return eval(node->cond) ? eval2(node->then, label, pclabel)
                              : eval2(node->els, label, pclabel);
    case ND_COMMA:
      return eval2(node->rhs, label, pclabel);
    case ND_NOT:
      return !eval(node->lhs);
    case ND_BITNOT:
      return ~eval(node->lhs);
    case ND_LOGAND:
      return eval(node->lhs) && eval(node->rhs);
    case ND_LOGOR:
      return eval(node->lhs) || eval(node->rhs);
    case ND_CAST: {
      int64_t val = eval2(node->lhs, label, pclabel);
      if (is_integer(node->ty)) {
        switch (node->ty->size) {
          case 1:
            return node->ty->is_unsigned ? (uint8_t)val : (int8_t)val;
          case 2:
            return node->ty->is_unsigned ? (uint16_t)val : (int16_t)val;
          case 4:
            if (node->ty->is_unsigned)
              return (uint32_t)val;
            else
              return (int32_t)val;
        }
      }
      return val;
    }
    case ND_ADDR:
      return eval_rval(node->lhs, label, pclabel);
    case ND_LABEL_VAL:
      if (!pclabel)
        error_tok(node->tok, "not a compile-time constant");
      *pclabel = &node->pc_label;
      return 0;
    case ND_MEMBER:
      if (!label)
        error_tok(node->tok, "not a compile-time constant");
      if (node->ty->kind != TY_ARRAY)
        error_tok(node->tok, "invalid initializer");
      return eval_rval(node->lhs, label, pclabel) + node->member->offset;
    case ND_VAR:
      if (!label)
        error_tok(node->tok, "not a compile-time constant (data)");
      if (!pclabel)
        error_tok(node->tok, "not a compile-time constant (code)");
      if (node->var->ty->kind != TY_ARRAY && node->var->ty->kind != TY_FUNC)
        error_tok(node->tok, "invalid initializer");
      *label = &node->var->name;
      return 0;
    case ND_NUM:
      return node->val;
  }

  error_tok(node->tok, "not a compile-time constant");
}

static int64_t eval_rval(Node* node, char*** label, int** pclabel) {
  switch (node->kind) {
    case ND_VAR:
      if (node->var->is_local || !label)
        error_tok(node->tok, "not a compile-time constant");
      *label = &node->var->name;
      return 0;
    case ND_DEREF:
      return eval2(node->lhs, label, pclabel);
    case ND_MEMBER:
      return eval_rval(node->lhs, label, pclabel) + node->member->offset;
  }

  error_tok(node->tok, "invalid initializer");
}

static bool is_const_expr(Node* node) {
  add_type(node);

  switch (node->kind) {
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
    case ND_SHL:
    case ND_SHR:
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_LOGAND:
    case ND_LOGOR:
      return is_const_expr(node->lhs) && is_const_expr(node->rhs);
    case ND_COND:
      if (!is_const_expr(node->cond))
        return false;
      return is_const_expr(eval(node->cond) ? node->then : node->els);
    case ND_COMMA:
      return is_const_expr(node->rhs);
    case ND_NEG:
    case ND_NOT:
    case ND_BITNOT:
    case ND_CAST:
      return is_const_expr(node->lhs);
    case ND_NUM:
      return true;
  }

  return false;
}

static int64_t const_expr(Token** rest, Token* tok) {
  Node* node = conditional(rest, tok);
  return eval(node);
}

static int64_t pp_const_expr(Token** rest, Token* tok) {
  C(evaluating_pp_const) = true;
  Node* node = conditional(rest, tok);
  C(evaluating_pp_const) = false;
  return eval(node);
}

static double eval_double(Node* node) {
  add_type(node);

  if (is_integer(node->ty)) {
    if (node->ty->is_unsigned)
      return (unsigned long)eval(node);
    return (double)eval(node);
  }

  switch (node->kind) {
    case ND_ADD:
      return eval_double(node->lhs) + eval_double(node->rhs);
    case ND_SUB:
      return eval_double(node->lhs) - eval_double(node->rhs);
    case ND_MUL:
      return eval_double(node->lhs) * eval_double(node->rhs);
    case ND_DIV:
      return eval_double(node->lhs) / eval_double(node->rhs);
    case ND_NEG:
      return -eval_double(node->lhs);
    case ND_COND:
      return eval_double(node->cond) ? eval_double(node->then) : eval_double(node->els);
    case ND_COMMA:
      return eval_double(node->rhs);
    case ND_CAST:
      if (is_flonum(node->lhs->ty))
        return eval_double(node->lhs);
      return (double)eval(node->lhs);
    case ND_NUM:
      return (double)node->fval;
  }

  error_tok(node->tok, "not a compile-time constant");
}

// Convert op= operators to expressions containing an assignment.
//
// In general, `A op= C` is converted to ``tmp = &A, *tmp = *tmp op B`.
// However, if a given expression is of form `A.x op= C`, the input is
// converted to `tmp = &A, (*tmp).x = (*tmp).x op C` to handle assignments
// to bitfields.
static Node* to_assign(Node* binary) {
  add_type(binary->lhs);
  add_type(binary->rhs);
  Token* tok = binary->tok;

  if (is_void(binary->lhs->ty) || is_void(binary->rhs->ty)) {
    error_tok(tok, "%.*s expression with type void", tok->len, tok->loc);
  }

  // Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`.
  if (binary->lhs->kind == ND_MEMBER) {
    Obj* var = new_lvar("", pointer_to(binary->lhs->lhs->ty));

    Node* expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
                             new_unary(ND_ADDR, binary->lhs->lhs, tok), tok);

    Node* expr2 = new_unary(ND_MEMBER, new_unary(ND_DEREF, new_var_node(var, tok), tok), tok);
    expr2->member = binary->lhs->member;

    Node* expr3 = new_unary(ND_MEMBER, new_unary(ND_DEREF, new_var_node(var, tok), tok), tok);
    expr3->member = binary->lhs->member;

    Node* expr4 =
        new_binary(ND_ASSIGN, expr2, new_binary(binary->kind, expr3, binary->rhs, tok), tok);

    return new_binary(ND_COMMA, expr1, expr4, tok);
  }

  // If A is an atomic type, Convert `A op= B` to
  //
  // ({
  //   T1 *addr = &A; T2 val = (B); T1 old = *addr; T1 new;
  //   do {
  //    new = old op val;
  //   } while (!atomic_compare_exchange_strong(addr, &old, new));
  //   new;
  // })
  if (binary->lhs->ty->is_atomic) {
    Node head = {0};
    Node* cur = &head;

    Obj* addr = new_lvar("", pointer_to(binary->lhs->ty));
    Obj* val = new_lvar("", binary->rhs->ty);
    Obj* old = new_lvar("", binary->lhs->ty);
    Obj* new = new_lvar("", binary->lhs->ty);

    cur = cur->next = new_unary(
        ND_EXPR_STMT,
        new_binary(ND_ASSIGN, new_var_node(addr, tok), new_unary(ND_ADDR, binary->lhs, tok), tok),
        tok);

    cur = cur->next = new_unary(
        ND_EXPR_STMT, new_binary(ND_ASSIGN, new_var_node(val, tok), binary->rhs, tok), tok);

    cur = cur->next = new_unary(ND_EXPR_STMT,
                                new_binary(ND_ASSIGN, new_var_node(old, tok),
                                           new_unary(ND_DEREF, new_var_node(addr, tok), tok), tok),
                                tok);

    Node* loop = new_node(ND_DO, tok);

    Node* body = new_binary(
        ND_ASSIGN, new_var_node(new, tok),
        new_binary(binary->kind, new_var_node(old, tok), new_var_node(val, tok), tok), tok);

    loop->then = new_node(ND_BLOCK, tok);
    loop->then->body = new_unary(ND_EXPR_STMT, body, tok);

    Node* cas = new_node(ND_CAS, tok);
    cas->cas_addr = new_var_node(addr, tok);
    cas->cas_old = new_unary(ND_ADDR, new_var_node(old, tok), tok);
    cas->cas_new = new_var_node(new, tok);
    loop->cond = new_unary(ND_NOT, cas, tok);

    cur = cur->next = loop;
    cur = cur->next = new_unary(ND_EXPR_STMT, new_var_node(new, tok), tok);

    Node* node = new_node(ND_STMT_EXPR, tok);
    node->body = head.next;
    return node;
  }

  // Convert `A op= B` to ``tmp = &A, *tmp = *tmp op B`.
  Obj* var = new_lvar("", pointer_to(binary->lhs->ty));

  Node* expr1 =
      new_binary(ND_ASSIGN, new_var_node(var, tok), new_unary(ND_ADDR, binary->lhs, tok), tok);

  Node* expr2 = new_binary(
      ND_ASSIGN, new_unary(ND_DEREF, new_var_node(var, tok), tok),
      new_binary(binary->kind, new_unary(ND_DEREF, new_var_node(var, tok), tok), binary->rhs, tok),
      tok);

  return new_binary(ND_COMMA, expr1, expr2, tok);
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
static Node* assign(Token** rest, Token* tok) {
  Node* node = conditional(&tok, tok);

  if (equal(tok, "="))
    return new_binary(ND_ASSIGN, node, assign(rest, tok->next), tok);

  if (equal(tok, "+="))
    return to_assign(new_add(node, assign(rest, tok->next), tok));

  if (equal(tok, "-="))
    return to_assign(new_sub(node, assign(rest, tok->next), tok));

  if (equal(tok, "*="))
    return to_assign(new_binary(ND_MUL, node, assign(rest, tok->next), tok));

  if (equal(tok, "/="))
    return to_assign(new_binary(ND_DIV, node, assign(rest, tok->next), tok));

  if (equal(tok, "%="))
    return to_assign(new_binary(ND_MOD, node, assign(rest, tok->next), tok));

  if (equal(tok, "&="))
    return to_assign(new_binary(ND_BITAND, node, assign(rest, tok->next), tok));

  if (equal(tok, "|="))
    return to_assign(new_binary(ND_BITOR, node, assign(rest, tok->next), tok));

  if (equal(tok, "^="))
    return to_assign(new_binary(ND_BITXOR, node, assign(rest, tok->next), tok));

  if (equal(tok, "<<="))
    return to_assign(new_binary(ND_SHL, node, assign(rest, tok->next), tok));

  if (equal(tok, ">>="))
    return to_assign(new_binary(ND_SHR, node, assign(rest, tok->next), tok));

  *rest = tok;
  return node;
}

// conditional = logor ("?" expr? ":" conditional)?
static Node* conditional(Token** rest, Token* tok) {
  Node* cond = logor(&tok, tok);

  if (!equal(tok, "?")) {
    *rest = tok;
    return cond;
  }

  if (equal(tok->next, ":")) {
    // [GNU] Compile `a ?: b` as `tmp = a, tmp ? tmp : b`.
    add_type(cond);
    Obj* var = new_lvar("", cond->ty);
    Node* lhs = new_binary(ND_ASSIGN, new_var_node(var, tok), cond, tok);
    Node* rhs = new_node(ND_COND, tok);
    rhs->cond = new_var_node(var, tok);
    rhs->then = new_var_node(var, tok);
    rhs->els = conditional(rest, tok->next->next);
    return new_binary(ND_COMMA, lhs, rhs, tok);
  }

  Node* node = new_node(ND_COND, tok);
  node->cond = cond;
  node->then = expr(&tok, tok->next);
  tok = skip(tok, ":");
  node->els = conditional(rest, tok);
  return node;
}

// logor = logand ("||" logand)*
static Node* logor(Token** rest, Token* tok) {
  Node* node = logand(&tok, tok);
  while (equal(tok, "||")) {
    Token* start = tok;
    node = new_binary(ND_LOGOR, node, logand(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// logand = bitor ("&&" bitor)*
static Node* logand(Token** rest, Token* tok) {
  Node* node = bitor (&tok, tok);
  while (equal(tok, "&&")) {
    Token* start = tok;
    node = new_binary(ND_LOGAND, node, bitor (&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitor = bitxor ("|" bitxor)*
static Node* bitor (Token * *rest, Token* tok) {
  Node* node = bitxor(&tok, tok);
  while (equal(tok, "|")) {
    Token* start = tok;
    node = new_binary(ND_BITOR, node, bitxor(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitxor = bitand ("^" bitand)*
static Node* bitxor(Token** rest, Token* tok) {
  Node* node = bitand(&tok, tok);
  while (equal(tok, "^")) {
    Token* start = tok;
    node = new_binary(ND_BITXOR, node, bitand(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitand = equality ("&" equality)*
static Node*bitand(Token** rest, Token* tok) {
  Node* node = equality(&tok, tok);
  while (equal(tok, "&")) {
    Token* start = tok;
    node = new_binary(ND_BITAND, node, equality(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node* equality(Token** rest, Token* tok) {
  Node* node = relational(&tok, tok);

  for (;;) {
    Token* start = tok;

    if (equal(tok, "==")) {
      node = new_binary(ND_EQ, node, relational(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "!=")) {
      node = new_binary(ND_NE, node, relational(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
static Node* relational(Token** rest, Token* tok) {
  Node* node = shift(&tok, tok);

  for (;;) {
    Token* start = tok;

    if (equal(tok, "<")) {
      node = new_binary(ND_LT, node, shift(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "<=")) {
      node = new_binary(ND_LE, node, shift(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">")) {
      node = new_binary(ND_LT, shift(&tok, tok->next), node, start);
      continue;
    }

    if (equal(tok, ">=")) {
      node = new_binary(ND_LE, shift(&tok, tok->next), node, start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// shift = add ("<<" add | ">>" add)*
static Node* shift(Token** rest, Token* tok) {
  Node* node = add(&tok, tok);

  for (;;) {
    Token* start = tok;

    if (equal(tok, "<<")) {
      node = new_binary(ND_SHL, node, add(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">>")) {
      node = new_binary(ND_SHR, node, add(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
static Node* new_add(Node* lhs, Node* rhs, Token* tok) {
  add_type(lhs);
  add_type(rhs);

  if (is_void(lhs->ty) || is_void(rhs->ty)) {
    error_tok(tok, "%.*s expression with type void", tok->len, tok->loc);
  }

  // num + num
  if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
    return new_binary(ND_ADD, lhs, rhs, tok);

  if (lhs->ty->base && rhs->ty->base)
    error_tok(tok, "invalid operands");

  // Canonicalize `num + ptr` to `ptr + num`.
  if (!lhs->ty->base && rhs->ty->base) {
    Node* tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  // VLA + num
  if (lhs->ty->base && lhs->ty->base->kind == TY_VLA) {
    rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  // ptr + num
  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  error_tok(tok, "invalid operands");
}

// Like `+`, `-` is overloaded for the pointer type.
static Node* new_sub(Node* lhs, Node* rhs, Token* tok) {
  add_type(lhs);
  add_type(rhs);

  if (is_void(lhs->ty) || is_void(rhs->ty)) {
    error_tok(tok, "%.*s expression with type void", tok->len, tok->loc);
  }

  // num - num
  if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
    return new_binary(ND_SUB, lhs, rhs, tok);

  // VLA + num
  if (lhs->ty->base && lhs->ty->base->kind == TY_VLA) {
    rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
    add_type(rhs);
    Node* node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = lhs->ty;
    return node;
  }

  // ptr - num
  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
    add_type(rhs);
    Node* node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = lhs->ty;
    return node;
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base) {
    Node* node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = ty_long;
    return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
  }

  error_tok(tok, "invalid operands");
}

// add = mul ("+" mul | "-" mul)*
static Node* add(Token** rest, Token* tok) {
  Node* node = mul(&tok, tok);

  for (;;) {
    Token* start = tok;

    if (equal(tok, "+")) {
      node = new_add(node, mul(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "-")) {
      node = new_sub(node, mul(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = cast ("*" cast | "/" cast | "%" cast)*
static Node* mul(Token** rest, Token* tok) {
  Node* node = cast(&tok, tok);

  for (;;) {
    Token* start = tok;

    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, cast(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, cast(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "%")) {
      node = new_binary(ND_MOD, node, cast(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// cast = "(" type-name ")" cast | unary
static Node* cast(Token** rest, Token* tok) {
  if (equal(tok, "(") && is_typename(tok->next)) {
    Token* start = tok;
    Type* ty = typename(&tok, tok->next);
    tok = skip(tok, ")");

    // compound literal
    if (equal(tok, "{"))
      return unary(rest, start);

    // type cast
    Node* node = new_cast(cast(rest, tok), ty);
    node->tok = start;
    return node;
  }

  return unary(rest, tok);
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | "&&" ident
//       | postfix
static Node* unary(Token** rest, Token* tok) {
  if (equal(tok, "+"))
    return cast(rest, tok->next);

  if (equal(tok, "-"))
    return new_unary(ND_NEG, cast(rest, tok->next), tok);

  if (equal(tok, "&")) {
    Node* lhs = cast(rest, tok->next);
    add_type(lhs);
    if (lhs->kind == ND_MEMBER && lhs->member->is_bitfield)
      error_tok(tok, "cannot take address of bitfield");
    return new_unary(ND_ADDR, lhs, tok);
  }

  if (equal(tok, "*")) {
    // [https://www.sigbus.info/n1570#6.5.3.2p4] This is an oddity
    // in the C spec, but dereferencing a function shouldn't do
    // anything. If foo is a function, `*foo`, `**foo` or `*****foo`
    // are all equivalent to just `foo`.
    Node* node = cast(rest, tok->next);
    add_type(node);
    if (node->ty->kind == TY_FUNC)
      return node;
    return new_unary(ND_DEREF, node, tok);
  }

  if (equal(tok, "!"))
    return new_unary(ND_NOT, cast(rest, tok->next), tok);

  if (equal(tok, "~"))
    return new_unary(ND_BITNOT, cast(rest, tok->next), tok);

  // Read ++i as i+=1
  if (equal(tok, "++")) {
    if (C(evaluating_pp_const))
      error_tok(tok, "invalid preprocessor token");
    return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));
  }

  // Read --i as i-=1
  if (equal(tok, "--")) {
    if (C(evaluating_pp_const))
      error_tok(tok, "invalid preprocessor token");
    return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));
  }

  // [GNU] labels-as-values
  if (equal(tok, "&&")) {
    Node* node = new_node(ND_LABEL_VAL, tok);
    node->label = get_ident(tok->next);
    node->goto_next = C(gotos);
    C(gotos) = node;
    *rest = tok->next->next;
    return node;
  }

  return postfix(rest, tok);
}

// struct-members = (declspec declarator (","  declarator)* ";")*
static void struct_members(Token** rest, Token* tok, Type* ty) {
  Member head = {0};
  Member* cur = &head;
  int idx = 0;

  while (!equal(tok, "}")) {
    VarAttr attr = {0};
    Type* basety = declspec(&tok, tok, &attr);
    bool first = true;

    // Anonymous struct member
    if ((basety->kind == TY_STRUCT || basety->kind == TY_UNION) && consume(&tok, tok, ";")) {
      Member* mem = bumpcalloc(1, sizeof(Member), AL_Compile);
      mem->ty = basety;
      mem->idx = idx++;
      mem->align = attr.align ? attr.align : mem->ty->align;
      cur = cur->next = mem;
      continue;
    }

    // Regular struct members
    while (!consume(&tok, tok, ";")) {
      if (!first)
        tok = skip(tok, ",");
      first = false;

      Member* mem = bumpcalloc(1, sizeof(Member), AL_Compile);
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      mem->idx = idx++;
      mem->align = attr.align ? attr.align : mem->ty->align;

      if (consume(&tok, tok, ":")) {
        mem->is_bitfield = true;
        mem->bit_width = (int)const_expr(&tok, tok);
      }

      cur = cur->next = mem;
    }
  }

  // If the last element is an array of incomplete type, it's
  // called a "flexible array member". It should behave as if
  // if were a zero-sized array.
  if (cur != &head && cur->ty->kind == TY_ARRAY && cur->ty->array_len < 0) {
    cur->ty = array_of(cur->ty->base, 0, tok);
    ty->is_flexible = true;
  }

  *rest = tok->next;
  ty->members = head.next;
}

// attribute = ("__attribute__" "(" "(" "packed" ")" ")")*
//           = ("__attribute__" "(" "(" "aligned" "(" N ")" ")" ")")*
//           = ("__attribute__" "(" "(" "methodcall" "(" prefix ")" ")" ")")*
static Token* attribute_list(Token* tok, Type* ty) {
  while (consume(&tok, tok, "__attribute__")) {
    tok = skip(tok, "(");
    tok = skip(tok, "(");

    bool first = true;

    while (!consume(&tok, tok, ")")) {
      if (!first)
        tok = skip(tok, ",");
      first = false;

      if (consume(&tok, tok, "packed")) {
        ty->is_packed = true;
        continue;
      }

      if (consume(&tok, tok, "methodcall")) {
        tok = skip(tok, "(");
        ty->methodcall_prefix = tok;
        tok = skip(tok->next, ")");
        continue;
      }

      if (consume(&tok, tok, "aligned")) {
        tok = skip(tok, "(");
        ty->align = (int)const_expr(&tok, tok);
        tok = skip(tok, ")");
        continue;
      }

      error_tok(tok, "unknown attribute");
    }

    tok = skip(tok, ")");
  }

  return tok;
}

// struct-union-decl = attribute? ident? ("{" struct-members)?
static Type* struct_union_decl(Token** rest, Token* tok) {
  Type* ty = struct_type();
  tok = attribute_list(tok, ty);

  // Read a tag.
  Token* tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    *rest = tok;

    Type* ty2 = find_tag(tag);
    if (ty2)
      return ty2;

    ty->size = -1;
    push_tag_scope(tag, ty);
    return ty;
  }

  tok = skip(tok, "{");

  // Construct a struct object.
  struct_members(&tok, tok, ty);
  *rest = attribute_list(tok, ty);

  ty->name = tag;

  if (tag) {
    // If this is a redefinition, overwrite a previous type.
    // Otherwise, register the struct type.
    Type* ty2 = hashmap_get2(&C(scope)->tags, tag->loc, tag->len);
    if (ty2) {
      if (ty2->size >= 0)
        error_tok(tag, "redefinition of type");
      *ty2 = *ty;
      return ty2;
    }

    push_tag_scope(tag, ty);
  }

  return ty;
}

// struct-decl = struct-union-decl
static Type* struct_decl(Token** rest, Token* tok) {
  Type* ty = struct_union_decl(rest, tok);
  ty->kind = TY_STRUCT;

  if (ty->size < 0)
    return ty;

  // Assign offsets within the struct to members.
  int bits = 0;

  for (Member* mem = ty->members; mem; mem = mem->next) {
    if (mem->is_bitfield && mem->bit_width == 0) {
      // Zero-width anonymous bitfield has a special meaning.
      // It affects only alignment.
      bits = (int)align_to_s(bits, mem->ty->size * 8);
    } else if (mem->is_bitfield) {
      int sz = mem->ty->size;
      if (bits / (sz * 8) != (bits + mem->bit_width - 1) / (sz * 8))
        bits = (int)align_to_s(bits, sz * 8);

      mem->offset = align_down(bits / 8, sz);
      mem->bit_offset = bits % (sz * 8);
      bits += mem->bit_width;
    } else {
      if (!ty->is_packed)
        bits = (int)align_to_s(bits, mem->align * 8);
      mem->offset = bits / 8;
      bits += mem->ty->size * 8;
    }

    if (!ty->is_packed && ty->align < mem->align)
      ty->align = mem->align;
  }

  ty->size = (int)align_to_s(bits, ty->align * 8) / 8;
  return ty;
}

// union-decl = struct-union-decl
static Type* union_decl(Token** rest, Token* tok) {
  Type* ty = struct_union_decl(rest, tok);
  ty->kind = TY_UNION;

  if (ty->size < 0)
    return ty;

  // If union, we don't have to assign offsets because they
  // are already initialized to zero. We need to compute the
  // alignment and the size though.
  for (Member* mem = ty->members; mem; mem = mem->next) {
    if (ty->align < mem->align)
      ty->align = mem->align;
    if (ty->size < mem->ty->size)
      ty->size = mem->ty->size;
  }
  ty->size = (int)align_to_s(ty->size, ty->align);
  return ty;
}

// Find a struct member by name.
static Member* get_struct_member(Type* ty, Token* tok) {
  for (Member* mem = ty->members; mem; mem = mem->next) {
    // Anonymous struct member
    if (!mem->name) {
      if (mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION)
        if (get_struct_member(mem->ty, tok))
          return mem;
      continue;
    }

    // Regular struct member
    if (mem->name->len == tok->len && !strncmp(mem->name->loc, tok->loc, tok->len))
      return mem;
  }
  return NULL;
}

// Create a node representing a struct member access, such as foo.bar
// where foo is a struct and bar is a member name.
//
// C has a feature called "anonymous struct" which allows a struct to
// have another unnamed struct as a member like this:
//
//   struct { struct { int a; }; int b; } x;
//
// The members of an anonymous struct belong to the outer struct's
// member namespace. Therefore, in the above example, you can access
// member "a" of the anonymous struct as "x.a".
//
// This function takes care of anonymous structs.
static Node* struct_ref(Node* node, Token* tok) {
  add_type(node);
  if (node->ty->kind != TY_STRUCT && node->ty->kind != TY_UNION)
    error_tok(node->tok, "neither a struct nor a union");

  Type* ty = node->ty;

  for (;;) {
    Member* mem = get_struct_member(ty, tok);
    if (!mem)
      error_tok(tok, "no such member");
    node = new_unary(ND_MEMBER, node, tok);
    node->member = mem;
    if (mem->name)
      break;
    ty = mem->ty;
  }
  return node;
}

static Node* methodcall_ref(Token** rest, Token* tok, Node* node) {
  add_type(node);
  if (node->ty->kind == TY_PTR) {
    node = new_unary(ND_DEREF, node, tok);
    return methodcall_ref(rest, tok, node);
  }

  add_type(node);
  if (node->ty->kind != TY_STRUCT && node->ty->kind != TY_UNION)
    error_tok(node->tok, "neither a struct nor a union");
  if (!node->ty->methodcall_prefix)
    error_tok(node->tok, "not an __attribute__((methodcall(prefix))) type");

  Token* built_prefix = tokenize(
      new_file(tok->file->name, format(AL_Compile, "%.*s%.*s", node->ty->methodcall_prefix->len,
                                       node->ty->methodcall_prefix->loc, tok->len, tok->loc)));
  built_prefix->line_no = tok->line_no;

  Token* unused;
  Node* funcnode = primary(&unused, built_prefix);

  Node* self = new_unary(ND_ADDR, node, tok);

  *rest = tok->next->next;
  return funcall(rest, tok->next->next, funcnode, self);
}

// Convert A++ to `(typeof A)((A += 1) - 1)`
static Node* new_inc_dec(Node* node, Token* tok, int addend) {
  add_type(node);
  return new_cast(
      new_add(to_assign(new_add(node, new_num(addend, tok), tok)), new_num(-addend, tok), tok),
      node->ty);
}

// postfix = "(" type-name ")" "{" initializer-list "}"
//         = ident "(" func-args ")" postfix-tail*
//         | primary postfix-tail*
//
// postfix-tail = "[" expr "]"
//              | "(" func-args ")"
//              | "." ident
//              | "->" ident
//              | "++"
//              | "--"
static Node* postfix(Token** rest, Token* tok) {
  if (equal(tok, "(") && is_typename(tok->next)) {
    // Compound literal
    Token* start = tok;
    Type* ty = typename(&tok, tok->next);
    tok = skip(tok, ")");

    if (C(scope)->next == NULL) {
      Obj* var = new_anon_gvar(ty);
      gvar_initializer(rest, tok, var);
      return new_var_node(var, start);
    }

    Obj* var = new_lvar("", ty);
    Node* lhs = lvar_initializer(rest, tok, var);
    Node* rhs = new_var_node(var, tok);
    return new_binary(ND_COMMA, lhs, rhs, start);
  }

  Node* node = primary(&tok, tok);

  for (;;) {
    if (equal(tok, "(")) {
      node = funcall(&tok, tok->next, node, NULL);
      continue;
    }

    if (equal(tok, "[")) {
      // x[y] is short for *(x+y)
      Token* start = tok;
      Node* idx = expr(&tok, tok->next);
      tok = skip(tok, "]");
      node = new_unary(ND_DEREF, new_add(node, idx, start), start);
      continue;
    }

    if (equal(tok, ".")) {
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "..")) {
      // v..func(...) is short for methodcall_prefix##func(&v, ...)
      node = methodcall_ref(&tok, tok->next, node);
      continue;
    }

    if (equal(tok, "->")) {
      // x->y is short for (*x).y
      node = new_unary(ND_DEREF, node, tok);
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "++")) {
      if (C(evaluating_pp_const))
        error_tok(tok, "invalid token in preprocessor expression");
      node = new_inc_dec(node, tok, 1);
      tok = tok->next;
      continue;
    }

    if (equal(tok, "--")) {
      if (C(evaluating_pp_const))
        error_tok(tok, "invalid token in preprocessor expression");
      node = new_inc_dec(node, tok, -1);
      tok = tok->next;
      continue;
    }

    *rest = tok;
    return node;
  }
}

// funcall = (assign ("," assign)*)? ")"
static Node* funcall(Token** rest, Token* tok, Node* fn, Node* injected_self) {
  add_type(fn);

  if (fn->ty->kind != TY_FUNC && (fn->ty->kind != TY_PTR || fn->ty->base->kind != TY_FUNC))
    error_tok(tok, "not a function");

  Type* ty = (fn->ty->kind == TY_FUNC) ? fn->ty : fn->ty->base;
  Type* param_ty = ty->params;

  Node head = {0};
  Node* cur = &head;

  while (!equal(tok, ")") || injected_self) {
    if (cur != &head) {
      if (injected_self) {
        injected_self = NULL;
        if (equal(tok, ")"))
          break;
      } else {
        tok = skip(tok, ",");
      }
    }

    Node* arg;

    if (injected_self) {
      arg = injected_self;
      // cleared on next loop, instead of skipping comma above.
    } else {
      arg = assign(&tok, tok);
    }
    add_type(arg);

    if (!param_ty && !ty->is_variadic)
      error_tok(tok, "too many arguments");

    if (param_ty) {
      if (param_ty->kind != TY_STRUCT && param_ty->kind != TY_UNION)
        arg = new_cast(arg, param_ty);
      param_ty = param_ty->next;
    } else if (arg->ty->kind == TY_FLOAT) {
      // If parameter type is omitted (e.g. in "..."), float
      // arguments are promoted to double.
      arg = new_cast(arg, ty_double);
    }

    cur = cur->next = arg;
  }

  if (param_ty)
    error_tok(tok, "too few arguments");

  *rest = skip(tok, ")");

  Node* node = new_unary(ND_FUNCALL, fn, tok);
  node->func_ty = ty;
  node->ty = ty->return_ty;
  node->args = head.next;

  // If a function returns a struct, it is caller's responsibility
  // to allocate a space for the return value.
  if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION)
    node->ret_buffer = new_lvar("", node->ty);
  return node;
}

// generic-selection = "(" assign "," generic-assoc ("," generic-assoc)* ")"
//
// generic-assoc = type-name ":" assign
//               | "default" ":" assign
static Node* generic_selection(Token** rest, Token* tok) {
  Token* start = tok;
  tok = skip(tok, "(");

  Node* ctrl = assign(&tok, tok);
  add_type(ctrl);

  Type* t1 = ctrl->ty;
  if (t1->kind == TY_FUNC)
    t1 = pointer_to(t1);
  else if (t1->kind == TY_ARRAY)
    t1 = pointer_to(t1->base);

  Node* ret = NULL;

  while (!consume(rest, tok, ")")) {
    tok = skip(tok, ",");

    if (equal(tok, "default")) {
      tok = skip(tok->next, ":");
      Node* node = assign(&tok, tok);
      if (!ret)
        ret = node;
      continue;
    }

    Type* t2 = typename(&tok, tok);
    tok = skip(tok, ":");
    Node* node = assign(&tok, tok);
    if (is_compatible(t1, t2))
      ret = node;
  }

  if (!ret)
    error_tok(start,
              "controlling expression type not compatible with"
              " any generic association type");
  return ret;
}

static _ReflectType build_reflect_base_fields(Type* ty) {
  // Create an init_data by combining the flat data in _ReflectType plus the
  // relocation that's saved the correct offset to have it point at the name.
  _ReflectType rtype = {0};
  rtype.size = ty->size;
  rtype.align = ty->align;
  rtype.kind = ty->kind;  // TODO: manual map, don't assume the same
  rtype.flags |= ty->is_unsigned ? _REFLECT_TYPEFLAG_UNSIGNED : 0;
  rtype.flags |= ty->is_atomic ? _REFLECT_TYPEFLAG_ATOMIC : 0;
  rtype.flags |= ty->is_flexible ? _REFLECT_TYPEFLAG_FLEXIBLE : 0;
  rtype.flags |= ty->is_packed ? _REFLECT_TYPEFLAG_PACKED : 0;
  rtype.flags |= ty->is_variadic ? _REFLECT_TYPEFLAG_VARIADIC : 0;
  return rtype;
}

static char* get_reflect_builtin_mangled_name(Type* ty) {
  switch (ty->kind) {
    case TY_VOID:
      return "v";
    case TY_BOOL:
      return "b";
    case TY_CHAR:
      return "c";  // TODO: char vs signed char vs unsigned char
    case TY_SHORT:
      return ty->is_unsigned ? "t" : "s";
    case TY_INT:
      return ty->is_unsigned ? "j" : "i";
    case TY_LONG:
      return ty->is_unsigned ? "m" : "l";
    case TY_FLOAT:
      return "f";
    case TY_DOUBLE:
      return "d";
#if !X64WIN
    case TY_LDOUBLE:
      return "e";
#endif
    default:
      ABORT("not a builtin type");
  }
}

static char* get_reflect_builtin_user_name_impl(Type* ty) {
  switch (ty->kind) {
    case TY_VOID:
      return "void";
    case TY_BOOL:
      return "bool";
    case TY_CHAR:
      return "char";  // TODO: char vs signed char vs unsigned char
    case TY_SHORT:
      return ty->is_unsigned ? "unsigned short" : "short";
    case TY_INT:
      return ty->is_unsigned ? "unsigned int" : "int";
#if X64WIN
    case TY_LONG:
      return ty->is_unsigned ? "unsigned long long" : "long long";
#else
    case TY_LONG:
      return ty->is_unsigned ? "unsigned long" : "long";
#endif
    case TY_FLOAT:
      return "float";
    case TY_DOUBLE:
      return "double";
#if !X64WIN
    case TY_LDOUBLE:
      return "long double";
#endif
    default:
      ABORT("not a builtin type");
  }
}

// Returns mangled name for the given type, string is either rodata or
// AL_Compile.
static char* build_reflect_mangled_name(Type* ty) {
  if (ty->kind <= TY_LDOUBLE) {
    return get_reflect_builtin_mangled_name(ty);
  }

  if (ty->kind == TY_PTR) {
    return format(AL_Compile, "P%s", build_reflect_mangled_name(ty->base));
  }

  if (ty->kind == TY_ARRAY) {
    // TODO: format for flexible?
    return format(AL_Compile, "A%d_%s", ty->array_len, build_reflect_mangled_name(ty->base));
  }

  if (ty->kind == TY_FUNC) {
    char* cur = format(AL_Compile, "F%s", build_reflect_mangled_name(ty->return_ty));
    if (!ty->params) {
      return format(AL_Compile, "%svE", cur);
    }

    Type* param = ty->params;
    while (param) {
      cur = format(AL_Compile, "%s%s", cur, build_reflect_mangled_name(param));
      param = param->next;
    }
    return format(AL_Compile, "%sE", cur);
  }

  if (ty->kind == TY_STRUCT) {
    return format(AL_Compile, "%d%.*s", ty->name->len, ty->name->len, ty->name->loc);
  }

  ABORT("todo");
}

static char* build_reflect_user_name_left(Type* ty) {
  if (ty->kind <= TY_LDOUBLE) {
    return get_reflect_builtin_user_name_impl(ty);
  }

  if (ty->kind == TY_PTR) {
    char* ret = build_reflect_user_name_left(ty->base);
    if (ty->base->kind == TY_ARRAY) {
      return format(AL_Compile, "%s (*", ret);
    } else if (ty->base->kind == TY_FUNC) {
      return format(AL_Compile, "%s(*", ret);
    } else {
      return format(AL_Compile, "%s*", ret);
    }
  }

  if (ty->kind == TY_ARRAY) {
    return build_reflect_user_name_left(ty->base);
  }

  if (ty->kind == TY_FUNC) {
    char* ret = build_reflect_user_name_left(ty->return_ty);
    return format(AL_Compile, "%s ", ret);
  }

  if (ty->kind == TY_STRUCT) {
    return format(AL_Compile, "%.*s", ty->name->len, ty->name->loc);
  }

  ABORT("todo");
}

static char* build_reflect_user_name_right(Type* ty, bool multi_array) {
  if (ty->kind == TY_PTR) {
    char* ret = "";
    if (ty->base->kind == TY_ARRAY || ty->base->kind == TY_FUNC) {
      ret = ")";
    }
    ret = format(AL_Compile, "%s%s", ret, build_reflect_user_name_right(ty->base, false));
    return ret;
  }

  if (ty->kind == TY_ARRAY) {
    // TODO: not sure about this multi_array hack.
    // TODO: zero sized array
    return format(AL_Compile, "%s[%d]%s", multi_array ? "" : " ", ty->array_len,
                  build_reflect_user_name_right(ty->base, true));
  }

  if (ty->kind == TY_FUNC) {
    char* ret = "(";
    if (!ty->params) {
      ret = format(AL_Compile, "%svoid", ret);
    } else {
      bool first = true;
      for (Type* param = ty->params; param; param = param->next) {
        char* p_left = build_reflect_user_name_left(param);
        char* p_right = build_reflect_user_name_right(param, false);
        ret = format(AL_Compile, "%s%s%s%s", ret, first ? "" : ", ", p_left, p_right);
        first = false;
      }
    }
    return format(AL_Compile, "%s)%s", ret, build_reflect_user_name_right(ty->return_ty, false));
  }

  return "";
}

static char* build_reflect_user_name(Type* ty) {
  char* left = build_reflect_user_name_left(ty);
  char* right = build_reflect_user_name_right(ty, false);
  return format(AL_Compile, "%s%s", left, right);
}

static _ReflectType* get_reflect_type(Type* ty) {
  char* mangled = build_reflect_mangled_name(ty);
  void* prev = hashmap_get(&user_context->reflect_types, mangled);
  if (prev) {
    return prev;
  }

  // Otherwise, it actually needs to be created.
  // XXX figure out invalidation on update().
  // Possibly need to write real Obj* with Relocation list, rather than
  // smuggling it through to runtime in UserContext. See old commits on
  // 'typedesc' branch.

  _ReflectType rtype = build_reflect_base_fields(ty);
  if (ty->kind <= TY_LDOUBLE) {
    rtype.name = get_reflect_builtin_user_name_impl(ty);
  } else if (ty->kind == TY_PTR) {
    rtype.name = bumpstrdup(build_reflect_user_name(ty), AL_UserContext);
    rtype.ptr.base = get_reflect_type(ty->base);
  } else if (ty->kind == TY_ARRAY) {
    rtype.name = bumpstrdup(build_reflect_user_name(ty), AL_UserContext);
    rtype.arr.base = get_reflect_type(ty->base);
    rtype.arr.len = ty->array_len;
  } else if (ty->kind == TY_FUNC) {
    rtype.name = bumpstrdup(build_reflect_user_name(ty), AL_UserContext);
    rtype.func.return_ty = get_reflect_type(ty->return_ty);
    rtype.func.num_params = 0;
    for (Type* param = ty->params; param; param = param->next) {
      rtype.func.num_params++;
    }
    // This one has to be done specially because of the flexible params array.
    _ReflectType* rtp = bumpcalloc(1, sizeof(rtype) + rtype.func.num_params * sizeof(_ReflectType*),
                                   AL_UserContext);
    memcpy(rtp, &rtype, sizeof(rtype));
    int i = 0;
    for (Type* param = ty->params; param; param = param->next) {
      rtp->func.params[i++] = get_reflect_type(param);
    }
    hashmap_put(&user_context->reflect_types, bumpstrdup(mangled, AL_UserContext), rtp);
    return rtp;
  } else if (ty->kind == TY_STRUCT) {
    rtype.name = bumpstrdup(build_reflect_user_name(ty), AL_UserContext);
    rtype.su.num_members = 0;
    for (Member* mem = ty->members; mem; mem = mem->next) {
      rtype.su.num_members++;
    }
    // This one has to be done specially because of the flexible members array.
    _ReflectType* rtp = bumpcalloc(
        1, sizeof(rtype) + rtype.su.num_members * sizeof(_ReflectTypeMember), AL_UserContext);
    memcpy(rtp, &rtype, sizeof(rtype));
    hashmap_put(&user_context->reflect_types, bumpstrdup(mangled, AL_UserContext), rtp);
    int i = 0;
    for (Member* mem = ty->members; mem; mem = mem->next) {
      _ReflectTypeMember* rtm = &rtp->su.members[i++];
      rtm->type = get_reflect_type(mem->ty);
      rtm->name = bumpstrndup(mem->name->loc, mem->name->len, AL_UserContext);
      rtm->align = mem->align;
      rtm->offset = mem->offset;
      if (mem->idx != i - 1)
        ABORT("idx doesn't match expected index");
      rtm->bit_width = mem->is_bitfield ? mem->bit_width : -1;
      rtm->bit_offset = mem->is_bitfield ? mem->bit_offset : -1;
    }
    hashmap_put(&user_context->reflect_types, bumpstrdup(mangled, AL_UserContext), rtp);
    return rtp;
  } else {
    ABORT("todo");
  }

  void* p = bumpcalloc(1, sizeof(rtype), AL_UserContext);
  memcpy(p, &rtype, sizeof(rtype));
  hashmap_put(&user_context->reflect_types, bumpstrdup(mangled, AL_UserContext), p);
  return p;
}

// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | "_Alignof" "(" type-name ")"
//         | "_Alignof" unary
//         | "_Generic" generic-selection
//         | "__builtin_types_compatible_p" "(" type-name, type-name, ")"
//         | "__builtin_reg_class" "(" type-name ")"
//         | ident
//         | str
//         | num
static Node* primary(Token** rest, Token* tok) {
  Token* start = tok;

  if (equal(tok, "(") && equal(tok->next, "{")) {
    // This is a GNU statement expresssion.
    Node* node = new_node(ND_STMT_EXPR, tok);
    node->body = compound_stmt(&tok, tok->next->next)->body;
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "(")) {
    Node* node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "sizeof") && equal(tok->next, "(") && is_typename(tok->next->next)) {
    Type* ty = typename(&tok, tok->next->next);
    *rest = skip(tok, ")");

    if (ty->kind == TY_VLA) {
      if (ty->vla_size)
        return new_var_node(ty->vla_size, tok);

      Node* lhs = compute_vla_size(ty, tok);
      Node* rhs = new_var_node(ty->vla_size, tok);
      return new_binary(ND_COMMA, lhs, rhs, tok);
    }

    return new_ulong(ty->size, start);
  }

  if (equal(tok, "_ReflectTypeOf") && equal(tok->next, "(")) {
    tok = skip(tok->next, "(");

    Type* ty;
    if (is_typename(tok)) {
      ty = typename(&tok, tok);
    } else {
      Node* node = expr(&tok, tok);
      add_type(node);
      ty = node->ty;
    }
    *rest = skip(tok, ")");
    Node* ret = new_reflect_type_ptr(get_reflect_type(ty), tok);
    ret->ty = pointer_to(ty_void);
    return ret;
  }

  if (equal(tok, "sizeof")) {
    Node* node = unary(rest, tok->next);
    add_type(node);
    if (node->ty->kind == TY_VLA)
      return new_var_node(node->ty->vla_size, tok);
    return new_ulong(node->ty->size, tok);
  }

  if (equal(tok, "_Alignof") && equal(tok->next, "(") && is_typename(tok->next->next)) {
    Type* ty = typename(&tok, tok->next->next);
    *rest = skip(tok, ")");
    return new_ulong(ty->align, tok);
  }

  if (equal(tok, "_Alignof")) {
    Node* node = unary(rest, tok->next);
    add_type(node);
    return new_ulong(node->ty->align, tok);
  }

  if (equal(tok, "_Generic"))
    return generic_selection(rest, tok->next);

  if (equal(tok, "__builtin_types_compatible_p")) {
    tok = skip(tok->next, "(");
    Type* t1 = typename(&tok, tok);
    tok = skip(tok, ",");
    Type* t2 = typename(&tok, tok);
    *rest = skip(tok, ")");
    return new_num(is_compatible(t1, t2), start);
  }

  if (equal(tok, "__builtin_reg_class")) {
    tok = skip(tok->next, "(");
    Type* ty = typename(&tok, tok);
    *rest = skip(tok, ")");

    if (is_integer(ty) || ty->kind == TY_PTR)
      return new_num(0, start);
    if (is_flonum(ty))
      return new_num(1, start);
    return new_num(2, start);
  }

  if (equal(tok, "__builtin_compare_and_swap")) {
    Node* node = new_node(ND_CAS, tok);
    tok = skip(tok->next, "(");
    node->cas_addr = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas_old = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas_new = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "_InterlockedCompareExchange")) {
    Node* node = new_node(ND_LOCKCE, tok);
    tok = skip(tok->next, "(");
    node->cas_addr = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas_new = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas_old = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_atomic_exchange")) {
    Node* node = new_node(ND_EXCH, tok);
    tok = skip(tok->next, "(");
    node->lhs = assign(&tok, tok);
    tok = skip(tok, ",");
    node->rhs = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (tok->kind == TK_IDENT) {
    // Variable or enum constant
    VarScope* sc = find_var(tok);
    *rest = tok->next;

    // For "static inline" function
    if (sc && sc->var && sc->var->is_function) {
      if (C(current_fn))
        strarray_push(&C(current_fn)->refs, sc->var->name, AL_Compile);
      else
        sc->var->is_root = true;
    }

    if (sc) {
      if (sc->var)
        return new_var_node(sc->var, tok);
      if (sc->enum_ty)
        return new_num(sc->enum_val, tok);
    }

    if (equal(tok->next, "("))
      error_tok(tok, "implicit declaration of a function");
    error_tok(tok, "undefined variable");
  }

  if (tok->kind == TK_STR) {
    if (C(evaluating_pp_const)) {
      error_tok(tok, "invalid token in preprocessor expression");
    }
    Obj* var = new_string_literal(tok->str, tok->ty);
    *rest = tok->next;
    return new_var_node(var, tok);
  }

  if (tok->kind == TK_NUM) {
    Node* node;
    if (is_flonum(tok->ty)) {
      node = new_node(ND_NUM, tok);
      node->fval = tok->fval;
    } else {
      node = new_num(tok->val, tok);
    }

    node->ty = tok->ty;
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

static Token* parse_typedef(Token* tok, Type* basety) {
  bool first = true;

  while (!consume(&tok, tok, ";")) {
    if (!first)
      tok = skip(tok, ",");
    first = false;

    Type* ty = declarator(&tok, tok, basety);
    if (!ty->name)
      error_tok(ty->name_pos, "typedef name omitted");
    push_scope(get_ident(ty->name))->type_def = ty;
  }
  return tok;
}

static void create_param_lvars(Type* param) {
  if (param) {
    create_param_lvars(param->next);
    if (!param->name)
      error_tok(param->name_pos, "parameter name omitted");
    Obj* p = new_lvar(get_ident(param->name), param);
#if X64WIN
    if (!type_passed_in_register(param)) {
      p->is_param_passed_by_reference = true;
    }
#else
    (void)p;
#endif
  }
}

// This function matches gotos or labels-as-values with labels.
//
// We cannot resolve gotos as we parse a function because gotos
// can refer a label that appears later in the function.
// So, we need to do this after we parse the entire function.
static void resolve_goto_labels(void) {
  for (Node* x = C(gotos); x; x = x->goto_next) {
    for (Node* y = C(labels); y; y = y->goto_next) {
      if (!strcmp(x->label, y->label)) {
        x->pc_label = y->pc_label;
        break;
      }
    }

    if (x->pc_label == 0)
      error_tok(x->tok->next, "use of undeclared label");
  }

  C(gotos) = C(labels) = NULL;
}

static Obj* find_func(char* name) {
  Scope* sc = C(scope);
  while (sc->next)
    sc = sc->next;

  VarScope* sc2 = hashmap_get(&sc->vars, name);
  if (sc2 && sc2->var && sc2->var->is_function)
    return sc2->var;
  return NULL;
}

static void mark_live(Obj* var) {
  if (!var->is_function || var->is_live)
    return;
  var->is_live = true;

  for (int i = 0; i < var->refs.len; i++) {
    Obj* fn = find_func(var->refs.data[i]);
    if (fn)
      mark_live(fn);
  }
}

static Token* function(Token* tok, Type* basety, VarAttr* attr) {
  Type* ty = declarator(&tok, tok, basety);
  if (!ty->name)
    error_tok(ty->name_pos, "function name omitted");
  char* name_str = get_ident(ty->name);

  Obj* fn = find_func(name_str);
  if (fn) {
    // Redeclaration
    if (!fn->is_function)
      error_tok(tok, "redeclared as a different kind of symbol");
    if (fn->is_definition && equal(tok, "{"))
      error_tok(tok, "redefinition of %s", name_str);
    if (!fn->is_static && attr->is_static)
      error_tok(tok, "static declaration follows a non-static declaration");
    fn->is_definition = fn->is_definition || equal(tok, "{");
  } else {
    fn = new_gvar(name_str, ty);
    fn->is_function = true;
    fn->is_definition = equal(tok, "{");
    fn->is_static = attr->is_static || (attr->is_inline && !attr->is_extern);
    fn->is_inline = attr->is_inline;
  }

  fn->is_root = !(fn->is_static && fn->is_inline);

  if (consume(&tok, tok, ";"))
    return tok;

  C(current_fn) = fn;
  C(locals) = NULL;
  enter_scope();
  create_param_lvars(ty->params);

  // A buffer for a struct/union return value is passed
  // as the hidden first parameter.
  Type* rty = ty->return_ty;
  if ((rty->kind == TY_STRUCT || rty->kind == TY_UNION) &&
#if X64WIN
      !type_passed_in_register(rty)
#else
      rty->size > 16
#endif
  ) {
    new_lvar("", pointer_to(rty));
  }

  fn->params = C(locals);

#if !X64WIN
  if (ty->is_variadic)
    fn->va_area = new_lvar("__va_area__", array_of(ty_char, 136, NULL));
#endif
  fn->alloca_bottom = new_lvar("__alloca_size__", pointer_to(ty_char));

  tok = skip(tok, "{");

  // [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
  // automatically defined as a local variable containing the
  // current function name.
  push_scope("__func__")->var =
      new_string_literal(fn->name, array_of(ty_char, (int)strlen(fn->name) + 1, NULL));

  // [GNU] __FUNCTION__ is yet another name of __func__.
  push_scope("__FUNCTION__")->var =
      new_string_literal(fn->name, array_of(ty_char, (int)strlen(fn->name) + 1, NULL));

  fn->body = compound_stmt(&tok, tok);
  fn->locals = C(locals);
  leave_scope();
  resolve_goto_labels();
  return tok;
}

static Token* global_variable(Token* tok, Type* basety, VarAttr* attr) {
  bool first = true;

  while (!consume(&tok, tok, ";")) {
    if (!first)
      tok = skip(tok, ",");
    first = false;

    Token* start = tok;

    Type* ty = declarator(&tok, tok, basety);
    if (!ty->name)
      error_tok(ty->name_pos, "variable name omitted");

    Obj* var = new_gvar(get_ident(ty->name), ty);
    var->is_definition = !attr->is_extern;
    var->is_static = attr->is_static;
    var->is_tls = attr->is_tls;
    if (attr->align)
      var->align = attr->align;

    if (equal(tok, "="))
      gvar_initializer(&tok, tok->next, var);
    else if (!attr->is_extern && !attr->is_tls)
      var->is_tentative = true;

    if (!attr->is_extern && var->ty->kind == TY_ARRAY && var->ty->size < 0)
      error_tok(start, "incomplete type for array");
  }
  return tok;
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
static bool is_function(Token* tok) {
  if (equal(tok, ";"))
    return false;

  Type dummy = {0};
  Type* ty = declarator(&tok, tok, &dummy);
  return ty->kind == TY_FUNC;
}

// Remove redundant tentative definitions.
static void scan_globals(void) {
  Obj head;
  Obj* cur = &head;

  for (Obj* var = C(globals); var; var = var->next) {
    if (!var->is_tentative) {
      cur = cur->next = var;
      continue;
    }

    // Find another definition of the same identifier.
    Obj* var2 = C(globals);
    for (; var2; var2 = var2->next)
      if (var != var2 && var2->is_definition && !strcmp(var->name, var2->name))
        break;

    // If there's another definition, the tentative definition
    // is redundant
    if (!var2)
      cur = cur->next = var;
  }

  cur->next = NULL;
  C(globals) = head.next;
}

static void declare_builtin_functions(void) {
  Type* ty = func_type(pointer_to(ty_void));
  ty->params = copy_type(ty_int);
  C(builtin_alloca) = new_gvar("alloca", ty);
  C(builtin_alloca)->is_definition = false;
}

// program = (typedef | function-definition | global-variable)*
static Obj* parse(Token* tok) {
  C(scope) = &C(empty_scope);

  declare_builtin_functions();
  C(globals) = NULL;

  while (tok->kind != TK_EOF) {
    // logerr("%s:%d\n", tok->filename, tok->line_no);
    VarAttr attr = {0};
    Type* basety = declspec(&tok, tok, &attr);

    // Typedef
    if (attr.is_typedef) {
      tok = parse_typedef(tok, basety);
      continue;
    }

    // Function
    if (is_function(tok)) {
      tok = function(tok, basety, &attr);
      continue;
    }

    // Global variable
    tok = global_variable(tok, basety, &attr);
  }

  for (Obj* var = C(globals); var; var = var->next)
    if (var->is_root)
      mark_live(var);

  // Remove redundant tentative definitions.
  scan_globals();
  return C(globals);
}
//
// END OF ../../src/parse.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/preprocess.c
//
// This file implements the C preprocessor.
//
// The preprocessor takes a list of tokens as an input and returns a
// new list of tokens as an output.
//
// The preprocessing language is designed in such a way that that's
// guaranteed to stop even if there is a recursive macro.
// Informally speaking, a macro is applied only once for each token.
// That is, if a macro token T appears in a result of direct or
// indirect macro expansion of T, T won't be expanded any further.
// For example, if T is defined as U, and U is defined as T, then
// token T is expanded to U and then to T and the macro expansion
// stops at that point.
//
// To achieve the above behavior, we attach for each token a set of
// macro names from which the token is expanded. The set is called
// "hideset". Hideset is initially empty, and every time we expand a
// macro, the macro name is added to the resulting tokens' hidesets.
//
// The above macro expansion algorithm is explained in this document
// written by Dave Prossor, which is used as a basis for the
// standard's wording:
// https://github.com/rui314/chibicc/wiki/cpp.algo.pdf



#define C(x) compiler_state.preprocess__##x

typedef struct MacroParam MacroParam;
struct MacroParam {
  MacroParam* next;
  char* name;
};

typedef struct MacroArg MacroArg;
struct MacroArg {
  MacroArg* next;
  char* name;
  bool is_va_args;
  Token* tok;
};

typedef struct Macro Macro;

typedef Token* macro_handler_fn(Macro* mac, Token*);

struct Macro {
  char* name;
  bool is_objlike;  // Object-like or function-like
  MacroParam* params;
  char* va_args_name;
  Token* body;
  macro_handler_fn* handler;
  bool handler_advances;
};

// `#if` can be nested, so we use a stack to manage nested `#if`s.
struct CondIncl {
  CondIncl* next;
  enum { IN_THEN, IN_ELIF, IN_ELSE } ctx;
  Token* tok;
  bool included;
};

typedef struct Hideset Hideset;
struct Hideset {
  Hideset* next;
  char* name;
};

static Token* preprocess2(Token* tok);
static Macro* find_macro(Token* tok);

static bool is_hash(Token* tok) {
  return tok->at_bol && equal(tok, "#");
}

// Some preprocessor directives such as #include allow extraneous
// tokens before newline. This function skips such tokens.
static Token* skip_line(Token* tok) {
  if (tok->at_bol)
    return tok;
  warn_tok(tok, "extra token");
  while (tok->at_bol)
    tok = tok->next;
  return tok;
}

static Token* copy_token(Token* tok) {
  Token* t = bumpcalloc(1, sizeof(Token), AL_Compile);
  *t = *tok;
  t->next = NULL;
  return t;
}

static Token* new_eof(Token* tok) {
  Token* t = copy_token(tok);
  t->kind = TK_EOF;
  t->len = 0;
  return t;
}

static Hideset* new_hideset(char* name) {
  Hideset* hs = bumpcalloc(1, sizeof(Hideset), AL_Compile);
  hs->name = name;
  return hs;
}

static Hideset* hideset_union(Hideset* hs1, Hideset* hs2) {
  Hideset head = {0};
  Hideset* cur = &head;

  for (; hs1; hs1 = hs1->next)
    cur = cur->next = new_hideset(hs1->name);
  cur->next = hs2;
  return head.next;
}

static bool hideset_contains(Hideset* hs, char* s, int len) {
  for (; hs; hs = hs->next)
    if (strlen(hs->name) == (size_t)len && !strncmp(hs->name, s, len))
      return true;
  return false;
}

static Hideset* hideset_intersection(Hideset* hs1, Hideset* hs2) {
  Hideset head = {0};
  Hideset* cur = &head;

  for (; hs1; hs1 = hs1->next)
    if (hideset_contains(hs2, hs1->name, (int)strlen(hs1->name)))
      cur = cur->next = new_hideset(hs1->name);
  return head.next;
}

static Token* add_hideset(Token* tok, Hideset* hs) {
  Token head = {0};
  Token* cur = &head;

  for (; tok; tok = tok->next) {
    Token* t = copy_token(tok);
    t->hideset = hideset_union(t->hideset, hs);
    cur = cur->next = t;
  }
  return head.next;
}

// Append tok2 to the end of tok1.
static Token* append(Token* tok1, Token* tok2) {
  if (tok1->kind == TK_EOF)
    return tok2;

  Token head = {0};
  Token* cur = &head;

  for (; tok1->kind != TK_EOF; tok1 = tok1->next)
    cur = cur->next = copy_token(tok1);
  cur->next = tok2;
  return head.next;
}

static Token* skip_cond_incl2(Token* tok) {
  while (tok->kind != TK_EOF) {
    if (is_hash(tok) &&
        (equal(tok->next, "if") || equal(tok->next, "ifdef") || equal(tok->next, "ifndef"))) {
      tok = skip_cond_incl2(tok->next->next);
      continue;
    }
    if (is_hash(tok) && equal(tok->next, "endif"))
      return tok->next->next;
    tok = tok->next;
  }
  return tok;
}

// Skip until next `#else`, `#elif` or `#endif`.
// Nested `#if` and `#endif` are skipped.
static Token* skip_cond_incl(Token* tok) {
  while (tok->kind != TK_EOF) {
    if (is_hash(tok) &&
        (equal(tok->next, "if") || equal(tok->next, "ifdef") || equal(tok->next, "ifndef"))) {
      tok = skip_cond_incl2(tok->next->next);
      continue;
    }

    if (is_hash(tok) &&
        (equal(tok->next, "elif") || equal(tok->next, "else") || equal(tok->next, "endif")))
      break;
    tok = tok->next;
  }
  return tok;
}

// Double-quote a given string and returns it.
static char* quote_string(char* str) {
  int bufsize = 3;
  for (int i = 0; str[i]; i++) {
    if (str[i] == '\\' || str[i] == '"')
      bufsize++;
    bufsize++;
  }

  char* buf = bumpcalloc(1, bufsize, AL_Compile);
  char* p = buf;
  *p++ = '"';
  for (int i = 0; str[i]; i++) {
    if (str[i] == '\\' || str[i] == '"')
      *p++ = '\\';
    *p++ = str[i];
  }
  *p++ = '"';
  *p++ = '\0';
  return buf;
}

static Token* new_str_token(char* str, Token* tmpl) {
  char* buf = quote_string(str);
  return tokenize(new_file(tmpl->file->name, buf));
}

// Copy all tokens until the next newline, terminate them with
// an EOF token and then returns them. This function is used to
// create a new list of tokens for `#if` arguments.
static Token* copy_line(Token** rest, Token* tok) {
  Token head = {0};
  Token* cur = &head;

  for (; tok->kind != TK_EOF && !tok->at_bol; tok = tok->next)
    cur = cur->next = copy_token(tok);

  cur->next = new_eof(tok);
  *rest = tok;
  return head.next;
}

static Token* new_num_token(int val, Token* tmpl) {
  char* buf = format(AL_Compile, "%d\n", val);
  return tokenize(new_file(tmpl->file->name, buf));
}

static Token* read_const_expr(Token** rest, Token* tok) {
  tok = copy_line(rest, tok);

  Token head = {0};
  Token* cur = &head;

  while (tok->kind != TK_EOF) {
    // "defined(foo)" or "defined foo" becomes "1" if macro "foo"
    // is defined. Otherwise "0".
    if (equal(tok, "defined")) {
      Token* start = tok;
      bool has_paren = consume(&tok, tok->next, "(");

      if (tok->kind != TK_IDENT)
        error_tok(start, "macro name must be an identifier");
      Macro* m = find_macro(tok);
      tok = tok->next;

      if (has_paren)
        tok = skip(tok, ")");

      cur = cur->next = new_num_token(m ? 1 : 0, start);
      continue;
    }

    cur = cur->next = tok;
    tok = tok->next;
  }

  cur->next = tok;
  return head.next;
}

// Read and evaluate a constant expression.
static long eval_const_expr(Token** rest, Token* tok) {
  Token* start = tok;
  Token* expr = read_const_expr(rest, tok->next);
  expr = preprocess2(expr);

  if (expr->kind == TK_EOF)
    error_tok(start, "no expression");

  // [https://www.sigbus.info/n1570#6.10.1p4] The standard requires
  // we replace remaining non-macro identifiers with "0" before
  // evaluating a constant expression. For example, `#if foo` is
  // equivalent to `#if 0` if foo is not defined.
  for (Token* t = expr; t->kind != TK_EOF; t = t->next) {
    if (t->kind == TK_IDENT) {
      Token* next = t->next;
      *t = *new_num_token(0, t);
      t->next = next;
    }
  }

  // Convert pp-numbers to regular numbers
  convert_pp_tokens(expr);

  Token* rest2;
  long val = (long)pp_const_expr(&rest2, expr);
  if (rest2->kind != TK_EOF)
    error_tok(rest2, "extra token");
  return val;
}

static CondIncl* push_cond_incl(Token* tok, bool included) {
  CondIncl* ci = bumpcalloc(1, sizeof(CondIncl), AL_Compile);
  ci->next = C(cond_incl);
  ci->ctx = IN_THEN;
  ci->tok = tok;
  ci->included = included;
  C(cond_incl) = ci;
  return ci;
}

static Macro* find_macro(Token* tok) {
  if (tok->kind != TK_IDENT)
    return NULL;
  return hashmap_get2(&C(macros), tok->loc, tok->len);
}

static Macro* add_macro(char* name, bool is_objlike, Token* body) {
  Macro* m = bumpcalloc(1, sizeof(Macro), AL_Compile);
  m->name = name;
  m->is_objlike = is_objlike;
  m->body = body;
  hashmap_put(&C(macros), name, m);
  return m;
}

static MacroParam* read_macro_params(Token** rest, Token* tok, char** va_args_name) {
  MacroParam head = {0};
  MacroParam* cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");

    if (equal(tok, "...")) {
      *va_args_name = "__VA_ARGS__";
      *rest = skip(tok->next, ")");
      return head.next;
    }

    if (tok->kind != TK_IDENT)
      error_tok(tok, "expected an identifier");

    if (equal(tok->next, "...")) {
      *va_args_name = bumpstrndup(tok->loc, tok->len, AL_Compile);
      *rest = skip(tok->next->next, ")");
      return head.next;
    }

    MacroParam* m = bumpcalloc(1, sizeof(MacroParam), AL_Compile);
    m->name = bumpstrndup(tok->loc, tok->len, AL_Compile);
    cur = cur->next = m;
    tok = tok->next;
  }

  *rest = tok->next;
  return head.next;
}

static Macro* read_macro_definition(Token** rest, Token* tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "macro name must be an identifier");
  char* name = bumpstrndup(tok->loc, tok->len, AL_Compile);
  tok = tok->next;

  if (!tok->has_space && equal(tok, "(")) {
    // Function-like macro
    char* va_args_name = NULL;
    MacroParam* params = read_macro_params(&tok, tok->next, &va_args_name);

    Macro* m = add_macro(name, false, copy_line(rest, tok));
    m->params = params;
    m->va_args_name = va_args_name;
    return m;
  } else {
    // Object-like macro
    return add_macro(name, true, copy_line(rest, tok));
  }
}

static MacroArg* read_macro_arg_one(Token** rest, Token* tok, bool read_rest) {
  Token head = {0};
  Token* cur = &head;
  int level = 0;

  for (;;) {
    if (level == 0 && equal(tok, ")"))
      break;
    if (level == 0 && !read_rest && equal(tok, ","))
      break;

    if (tok->kind == TK_EOF)
      error_tok(tok, "premature end of input");

    if (equal(tok, "("))
      level++;
    else if (equal(tok, ")"))
      level--;

    cur = cur->next = copy_token(tok);
    tok = tok->next;
  }

  cur->next = new_eof(tok);

  MacroArg* arg = bumpcalloc(1, sizeof(MacroArg), AL_Compile);
  arg->tok = head.next;
  *rest = tok;
  return arg;
}

static MacroArg* read_macro_args(Token** rest, Token* tok, MacroParam* params, char* va_args_name) {
  Token* start = tok;
  tok = tok->next->next;

  MacroArg head = {0};
  MacroArg* cur = &head;

  MacroParam* pp = params;
  for (; pp; pp = pp->next) {
    if (cur != &head)
      tok = skip(tok, ",");
    cur = cur->next = read_macro_arg_one(&tok, tok, false);
    cur->name = pp->name;
  }

  if (va_args_name) {
    MacroArg* arg;
    if (equal(tok, ")")) {
      arg = bumpcalloc(1, sizeof(MacroArg), AL_Compile);
      arg->tok = new_eof(tok);
    } else {
      if (pp != params)
        tok = skip(tok, ",");
      arg = read_macro_arg_one(&tok, tok, true);
    }
    arg->name = va_args_name;
    ;
    arg->is_va_args = true;
    cur = cur->next = arg;
  } else if (pp) {
    error_tok(start, "too many arguments");
  }

  skip(tok, ")");
  *rest = tok;
  return head.next;
}

static MacroArg* find_arg(MacroArg* args, Token* tok) {
  for (MacroArg* ap = args; ap; ap = ap->next)
    if ((size_t)tok->len == strlen(ap->name) && !strncmp(tok->loc, ap->name, tok->len))
      return ap;
  return NULL;
}

// Concatenates all tokens in `tok` and returns a new string.
static char* join_tokens(Token* tok, Token* end) {
  // Compute the length of the resulting token.
  int len = 1;
  for (Token* t = tok; t != end && t->kind != TK_EOF; t = t->next) {
    if (t != tok && t->has_space)
      len++;
    len += t->len;
  }

  char* buf = bumpcalloc(1, len, AL_Compile);

  // Copy token texts.
  int pos = 0;
  for (Token* t = tok; t != end && t->kind != TK_EOF; t = t->next) {
    if (t != tok && t->has_space)
      buf[pos++] = ' ';
    strncpy(buf + pos, t->loc, t->len);
    pos += t->len;
  }
  buf[pos] = '\0';
  return buf;
}

// Concatenates all tokens in `arg` and returns a new string token.
// This function is used for the stringizing operator (#).
static Token* stringize(Token* hash, Token* arg) {
  // Create a new string token. We need to set some value to its
  // source location for error reporting function, so we use a macro
  // name token as a template.
  char* s = join_tokens(arg, NULL);
  return new_str_token(s, hash);
}

// Concatenate two tokens to create a new token.
static Token* paste(Token* lhs, Token* rhs) {
  // Paste the two tokens.
  char* buf = format(AL_Compile, "%.*s%.*s", lhs->len, lhs->loc, rhs->len, rhs->loc);

  // Tokenize the resulting string.
  Token* tok = tokenize(new_file(lhs->file->name, buf));
  if (tok->next->kind != TK_EOF)
    error_tok(lhs, "pasting forms '%s', an invalid token", buf);
  return tok;
}

static bool has_varargs(MacroArg* args) {
  for (MacroArg* ap = args; ap; ap = ap->next)
    if (!strcmp(ap->name, "__VA_ARGS__"))
      return ap->tok->kind != TK_EOF;
  return false;
}

// Replace func-like macro parameters with given arguments.
static Token* subst(Token* tok, MacroArg* args) {
  Token head = {0};
  Token* cur = &head;

  while (tok->kind != TK_EOF) {
    // "#" followed by a parameter is replaced with stringized actuals.
    if (equal(tok, "#")) {
      MacroArg* arg = find_arg(args, tok->next);
      if (!arg)
        error_tok(tok->next, "'#' is not followed by a macro parameter");
      cur = cur->next = stringize(tok, arg->tok);
      tok = tok->next->next;
      continue;
    }

    // [GNU] If __VA_ARG__ is empty, `,##__VA_ARGS__` is expanded
    // to the empty token list. Otherwise, its expaned to `,` and
    // __VA_ARGS__.
    if (equal(tok, ",") && equal(tok->next, "##")) {
      MacroArg* arg = find_arg(args, tok->next->next);
      if (arg && arg->is_va_args) {
        if (arg->tok->kind == TK_EOF) {
          tok = tok->next->next->next;
        } else {
          cur = cur->next = copy_token(tok);
          tok = tok->next->next;
        }
        continue;
      }
    }

    if (equal(tok, "##")) {
      if (cur == &head)
        error_tok(tok, "'##' cannot appear at start of macro expansion");

      if (tok->next->kind == TK_EOF)
        error_tok(tok, "'##' cannot appear at end of macro expansion");

      MacroArg* arg = find_arg(args, tok->next);
      if (arg) {
        if (arg->tok->kind != TK_EOF) {
          *cur = *paste(cur, arg->tok);
          for (Token* t = arg->tok->next; t->kind != TK_EOF; t = t->next)
            cur = cur->next = copy_token(t);
        }
        tok = tok->next->next;
        continue;
      }

      *cur = *paste(cur, tok->next);
      tok = tok->next->next;
      continue;
    }

    MacroArg* arg = find_arg(args, tok);

    if (arg && equal(tok->next, "##")) {
      Token* rhs = tok->next->next;

      if (arg->tok->kind == TK_EOF) {
        MacroArg* arg2 = find_arg(args, rhs);
        if (arg2) {
          for (Token* t = arg2->tok; t->kind != TK_EOF; t = t->next)
            cur = cur->next = copy_token(t);
        } else {
          cur = cur->next = copy_token(rhs);
        }
        tok = rhs->next;
        continue;
      }

      for (Token* t = arg->tok; t->kind != TK_EOF; t = t->next)
        cur = cur->next = copy_token(t);
      tok = tok->next;
      continue;
    }

    // If __VA_ARG__ is empty, __VA_OPT__(x) is expanded to the
    // empty token list. Otherwise, __VA_OPT__(x) is expanded to x.
    if (equal(tok, "__VA_OPT__") && equal(tok->next, "(")) {
      MacroArg* arg2 = read_macro_arg_one(&tok, tok->next->next, true);
      if (has_varargs(args))
        for (Token* t = arg2->tok; t->kind != TK_EOF; t = t->next)
          cur = cur->next = t;
      tok = skip(tok, ")");
      continue;
    }

    // Handle a macro token. Macro arguments are completely macro-expanded
    // before they are substituted into a macro body.
    if (arg) {
      Token* t = preprocess2(arg->tok);
      t->at_bol = tok->at_bol;
      t->has_space = tok->has_space;
      for (; t->kind != TK_EOF; t = t->next)
        cur = cur->next = copy_token(t);
      tok = tok->next;
      continue;
    }

    // Handle a non-macro token.
    cur = cur->next = copy_token(tok);
    tok = tok->next;
    continue;
  }

  cur->next = tok;
  return head.next;
}

static bool file_exists_in_builtins(char* path) {
  if (C(builtin_includes_map).capacity == 0) {
    for (size_t i = 0; i < sizeof(compiler_includes) / sizeof(compiler_includes[0]); ++i) {
      hashmap_put(&C(builtin_includes_map), compiler_includes[i].path,
                  (void*)&compiler_include_blob[compiler_includes[i].offset]);
    }
  }

  return hashmap_get(&C(builtin_includes_map), path) != NULL;
}

static bool file_exists(char* path) {
  struct stat st;

  if (file_exists_in_builtins(path))
    return true;

  return !stat(path, &st);
}

// If tok is a macro, expand it and return true.
// Otherwise, do nothing and return false.
static bool expand_macro(Token** rest, Token* tok) {
  if (hideset_contains(tok->hideset, tok->loc, tok->len))
    return false;

  Macro* m = find_macro(tok);
  if (!m)
    return false;

  // Built-in dynamic macro application such as __LINE__
  if (m->handler) {
    *rest = m->handler(m, tok);
    if (!m->handler_advances) {
      (*rest)->next = tok->next;
    }
    return true;
  }

  // Object-like macro application
  if (m->is_objlike) {
    Hideset* hs = hideset_union(tok->hideset, new_hideset(m->name));
    Token* body = add_hideset(m->body, hs);
    for (Token* t = body; t->kind != TK_EOF; t = t->next)
      t->origin = tok;
    *rest = append(body, tok->next);
    (*rest)->at_bol = tok->at_bol;
    (*rest)->has_space = tok->has_space;
    return true;
  }

  // If a funclike macro token is not followed by an argument list,
  // treat it as a normal identifier.
  if (!equal(tok->next, "("))
    return false;

  // Function-like macro application
  Token* macro_token = tok;
  MacroArg* args = read_macro_args(&tok, tok, m->params, m->va_args_name);
  Token* rparen = tok;

  // Tokens that consist a func-like macro invocation may have different
  // hidesets, and if that's the case, it's not clear what the hideset
  // for the new tokens should be. We take the interesection of the
  // macro token and the closing parenthesis and use it as a new hideset
  // as explained in the Dave Prossor's algorithm.
  Hideset* hs = hideset_intersection(macro_token->hideset, rparen->hideset);
  hs = hideset_union(hs, new_hideset(m->name));

  Token* body = subst(m->body, args);
  body = add_hideset(body, hs);
  for (Token* t = body; t->kind != TK_EOF; t = t->next)
    t->origin = macro_token;
  *rest = append(body, tok->next);
  (*rest)->at_bol = macro_token->at_bol;
  (*rest)->has_space = macro_token->has_space;
  return true;
}

static char* search_include_paths(char* filename) {
  if (filename[0] == '/')
    return filename;

  char* cached = hashmap_get(&C(include_path_cache), filename);
  if (cached)
    return cached;

  // Search a file from the include paths.
  for (int i = 0; i < (int)user_context->num_include_paths; i++) {
    char* path = format(AL_Compile, "%s/%s", user_context->include_paths[i], filename);
    if (!file_exists(path))
      continue;
    hashmap_put(&C(include_path_cache), filename, path);
    C(include_next_idx) = i + 1;
    return path;
  }
  return NULL;
}

static char* search_include_next(char* filename) {
  for (; C(include_next_idx) < (int)user_context->num_include_paths; C(include_next_idx)++) {
    char* path =
        format(AL_Compile, "%s/%s", user_context->include_paths[C(include_next_idx)], filename);
    if (file_exists(path))
      return path;
  }
  return NULL;
}

// Read an #include argument.
static char* read_include_filename(Token** rest, Token* tok, bool* is_dquote) {
  // Pattern 1: #include "foo.h"
  if (tok->kind == TK_STR) {
    // A double-quoted filename for #include is a special kind of
    // token, and we don't want to interpret any escape sequences in it.
    // For example, "\f" in "C:\foo" is not a formfeed character but
    // just two non-control characters, backslash and f.
    // So we don't want to use token->str.
    *is_dquote = true;
    *rest = skip_line(tok->next);
    return bumpstrndup(tok->loc + 1, tok->len - 2, AL_Compile);
  }

  // Pattern 2: #include <foo.h>
  if (equal(tok, "<")) {
    // Reconstruct a filename from a sequence of tokens between
    // "<" and ">".
    Token* start = tok;

    // Find closing ">".
    for (; !equal(tok, ">"); tok = tok->next)
      if (tok->at_bol || tok->kind == TK_EOF)
        error_tok(tok, "expected '>'");

    *is_dquote = false;
    *rest = skip_line(tok->next);
    return join_tokens(start->next, tok);
  }

  // Pattern 3: #include FOO
  // In this case FOO must be macro-expanded to either
  // a single string token or a sequence of "<" ... ">".
  if (tok->kind == TK_IDENT) {
    Token* tok2 = preprocess2(copy_line(rest, tok));
    return read_include_filename(&tok2, tok2, is_dquote);
  }

  error_tok(tok, "expected a filename");
}

// Detect the following "include guard" pattern.
//
//   #ifndef FOO_H
//   #define FOO_H
//   ...
//   #endif
static char* detect_include_guard(Token* tok) {
  // Detect the first two lines.
  if (!is_hash(tok) || !equal(tok->next, "ifndef"))
    return NULL;
  tok = tok->next->next;

  if (tok->kind != TK_IDENT)
    return NULL;

  char* macro = bumpstrndup(tok->loc, tok->len, AL_Compile);
  tok = tok->next;

  if (!is_hash(tok) || !equal(tok->next, "define") || !equal(tok->next->next, macro))
    return NULL;

  // Read until the end of the file.
  while (tok->kind != TK_EOF) {
    if (!is_hash(tok)) {
      tok = tok->next;
      continue;
    }

    if (equal(tok->next, "endif") && tok->next->next->kind == TK_EOF)
      return macro;

    if (equal(tok, "if") || equal(tok, "ifdef") || equal(tok, "ifndef"))
      tok = skip_cond_incl(tok->next);
    else
      tok = tok->next;
  }
  return NULL;
}

static Token* include_file(Token* tok, char* path, Token* filename_tok) {
  // Check for "#pragma once"
  if (hashmap_get(&C(pragma_once), path))
    return tok;

  // If we read the same file before, and if the file was guarded
  // by the usual #ifndef ... #endif pattern, we may be able to
  // skip the file without opening it.
  char* guard_name = hashmap_get(&C(include_guards), path);
  if (guard_name && hashmap_get(&C(macros), guard_name))
    return tok;

  Token* tok2;
  char* builtin_include_contents = hashmap_get(&C(builtin_includes_map), path);
  if (builtin_include_contents) {
    tok2 = tokenize_filecontents(path, builtin_include_contents);
  } else {
    tok2 = tokenize_file(path);
  }
  if (!tok2)
    error_tok(filename_tok, "%s: cannot open file: %s", path, strerror(errno));

  guard_name = detect_include_guard(tok2);
  if (guard_name)
    hashmap_put(&C(include_guards), path, guard_name);

  return append(tok2, tok);
}

// Read #line arguments
static void read_line_marker(Token** rest, Token* tok) {
  Token* start = tok;
  tok = preprocess(copy_line(rest, tok));

  if (tok->kind != TK_NUM || tok->ty->kind != TY_INT)
    error_tok(tok, "invalid line marker");
  start->file->line_delta = (int)(tok->val - start->line_no);

  tok = tok->next;
  if (tok->kind == TK_EOF)
    return;

  if (tok->kind != TK_STR)
    error_tok(tok, "filename expected");
  start->file->display_name = tok->str;
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
static Token* preprocess2(Token* tok) {
  Token head = {0};
  Token* cur = &head;

  while (tok->kind != TK_EOF) {
    // If it is a macro, expand it.
    if (expand_macro(&tok, tok))
      continue;

    // Pass through if it is not a "#".
    if (!is_hash(tok)) {
      tok->line_delta = tok->file->line_delta;
      tok->filename = tok->file->display_name;
      cur = cur->next = tok;
      tok = tok->next;
      continue;
    }

    Token* start = tok;
    tok = tok->next;

    if (equal(tok, "include")) {
      bool is_dquote;
      char* filename = read_include_filename(&tok, tok->next, &is_dquote);

      if (filename[0] != '/' && is_dquote) {
        char* path = format(AL_Compile, "%s/%s", dirname(bumpstrdup(start->file->name, AL_Compile)),
                            filename);
        if (file_exists(path)) {
          tok = include_file(tok, path, start->next->next);
          continue;
        }
      }

      char* path = search_include_paths(filename);
      tok = include_file(tok, path ? path : filename, start->next->next);
      continue;
    }

    if (equal(tok, "include_next")) {
      bool ignore;
      char* filename = read_include_filename(&tok, tok->next, &ignore);
      char* path = search_include_next(filename);
      tok = include_file(tok, path ? path : filename, start->next->next);
      continue;
    }

    if (equal(tok, "define")) {
      read_macro_definition(&tok, tok->next);
      continue;
    }

    if (equal(tok, "undef")) {
      tok = tok->next;
      if (tok->kind != TK_IDENT)
        error_tok(tok, "macro name must be an identifier");
      undef_macro(bumpstrndup(tok->loc, tok->len, AL_Compile));
      tok = skip_line(tok->next);
      continue;
    }

    if (equal(tok, "if")) {
      long val = eval_const_expr(&tok, tok);
      push_cond_incl(start, val);
      if (!val)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "ifdef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, defined);
      if (tok->next->kind == TK_EOF) {
        error_tok(tok, "unterminated #ifdef");
      }
      tok = skip_line(tok->next->next);
      if (!defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "ifndef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, !defined);
      if (tok->next->kind == TK_EOF) {
        error_tok(tok, "unterminated #ifndef");
      }
      tok = skip_line(tok->next->next);
      if (defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "elif")) {
      if (!C(cond_incl) || C(cond_incl)->ctx == IN_ELSE)
        error_tok(start, "stray #elif");
      C(cond_incl)->ctx = IN_ELIF;

      if (!C(cond_incl)->included && eval_const_expr(&tok, tok))
        C(cond_incl)->included = true;
      else
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "else")) {
      if (!C(cond_incl) || C(cond_incl)->ctx == IN_ELSE)
        error_tok(start, "stray #else");
      C(cond_incl)->ctx = IN_ELSE;
      tok = skip_line(tok->next);

      if (C(cond_incl)->included)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "endif")) {
      if (!C(cond_incl))
        error_tok(start, "stray #endif");
      C(cond_incl) = C(cond_incl)->next;
      tok = skip_line(tok->next);
      continue;
    }

    if (equal(tok, "line")) {
      read_line_marker(&tok, tok->next);
      continue;
    }

    if (tok->kind == TK_PP_NUM) {
      read_line_marker(&tok, tok);
      continue;
    }

    if (equal(tok, "pragma") && equal(tok->next, "once")) {
      hashmap_put(&C(pragma_once), tok->file->name, (void*)1);
      tok = skip_line(tok->next->next);
      continue;
    }

    if (equal(tok, "pragma")) {
      do {
        tok = tok->next;
      } while (!tok->at_bol);
      continue;
    }

    if (equal(tok, "error"))
      error_tok(tok, "error");

    // `#`-only line is legal. It's called a null directive.
    if (tok->at_bol)
      continue;

    error_tok(tok, "invalid preprocessor directive");
  }

  cur->next = tok;
  return head.next;
}

static void define_macro(char* name, char* buf) {
  Token* tok = tokenize(new_file("<built-in>", buf));
  add_macro(name, true, tok);
}

static void undef_macro(char* name) {
  hashmap_delete(&C(macros), name);
}

static void define_function_macro(char* buf, macro_handler_fn* fn) {
  Token* tok = tokenize(new_file("<built-in>", buf));
  Token* rest = tok;
  Macro* m = read_macro_definition(&rest, tok);
  m->handler = fn;
  m->handler_advances = true;
}

static Macro* add_builtin(char* name, macro_handler_fn* fn) {
  Macro* m = add_macro(name, true, NULL);
  m->handler = fn;
  return m;
}

static Token* file_macro(Macro* m, Token* tmpl) {
  (void)m;
  while (tmpl->origin)
    tmpl = tmpl->origin;
  return new_str_token(tmpl->file->display_name, tmpl);
}

static Token* line_macro(Macro* m, Token* tmpl) {
  (void)m;
  while (tmpl->origin)
    tmpl = tmpl->origin;
  int i = tmpl->line_no + tmpl->file->line_delta;
  return new_num_token(i, tmpl);
}

// __COUNTER__ is expanded to serial values starting from 0.
static Token* counter_macro(Macro* m, Token* tmpl) {
  (void)m;
  return new_num_token(C(counter_macro_i)++, tmpl);
}

// __TIMESTAMP__ is expanded to a string describing the last
// modification time of the current file. E.g.
// "Fri Jul 24 01:32:50 2020"
static Token* timestamp_macro(Macro* m, Token* tmpl) {
  (void)m;
  return new_str_token("Mon May 02 01:23:45 1977", tmpl);
}

static Token* base_file_macro(Macro* m, Token* tmpl) {
  (void)m;
  return new_str_token(compiler_state.main__base_file, tmpl);
}

// __DATE__ is expanded to the current date, e.g. "May 17 2020".
static char* format_date(struct tm* tm) {
  (void)tm;
  return "\"May 02 1977\"";
}

// __TIME__ is expanded to the current time, e.g. "13:34:03".
static char* format_time(struct tm* tm) {
  (void)tm;
  return "\"01:23:45\"";
}

static void append_to_container_tokens(Token* to_add) {
  // Must be maintained in order that the files were instantiated because (e.g.)
  // header guards will cause different structs to be defined, and they might be
  // later referenced by a reinclude.
  tokenptrarray_push(&C(container_tokens), to_add, AL_Compile);
}

static char* format_container_type_as_ident(Token* ma) {
  if (ma->kind == TK_EOF) {
    return "";
  } else if (ma->kind == TK_IDENT) {
    return format(AL_Compile, "%.*s%s", ma->len, ma->loc, format_container_type_as_ident(ma->next));
  } else if (ma->kind == TK_PUNCT) {
    assert(ma->loc[0] == '*' && ma->len == 1);
    return format(AL_Compile, "$STAR$%s", format_container_type_as_ident(ma->next));
  } else {
    unreachable();
  }
}

static char* format_container_type_as_template_arg(Token* ma) {
  if (ma->kind == TK_EOF) {
    return "";
  } else if (ma->kind == TK_IDENT) {
    return format(AL_Compile, "%.*s%s", ma->len, ma->loc,
                  format_container_type_as_template_arg(ma->next));
  } else if (ma->kind == TK_PUNCT) {
    assert(ma->loc[0] == '*' && ma->len == 1);
    return format(AL_Compile, "*%s", format_container_type_as_template_arg(ma->next));
  } else {
    unreachable();
  }
}

static Token* container_vec_setup(Macro* m, Token* tok) {
  Token* rparen;
  MacroArg* args = read_macro_args(&rparen, tok, m->params, m->va_args_name);

  char* key_as_arg = format_container_type_as_template_arg(args->tok);
  char* key_as_ident = format_container_type_as_ident(args->tok);

  char* key = format(AL_Compile, "type:vec,arg:%s", key_as_ident);
  if (!hashmap_get(&C(container_included), key)) {
    append_to_container_tokens(preprocess(
        tokenize(new_file(tok->file->name, format(AL_Compile,
                                                  "#define __dyibicc_internal_include__ 1\n"
                                                  "#define i_key %s\n"
                                                  "#define i_type _Vec$%s\n"
                                                  "#include <_vec.h>\n"
                                                  "#undef __dyibicc_internal_include__\n",
                                                  key_as_arg, key_as_ident)))));

    hashmap_put(&C(container_included), key, (void*)1);
  }

  Token* ret = tokenize(new_file(tok->file->name, format(AL_Compile, "_Vec$%s", key_as_ident)));
  ret->next = rparen->next;
  return ret;
}

static Token* container_map_setup(Macro* m, Token* tok) {
  Token* rparen;
  MacroArg* args = read_macro_args(&rparen, tok, m->params, m->va_args_name);

  char* key_as_arg = format_container_type_as_template_arg(args->tok);
  char* key_as_ident = format_container_type_as_ident(args->tok);
  char* val_as_arg = format_container_type_as_template_arg(args->next->tok);
  char* val_as_ident = format_container_type_as_ident(args->next->tok);

  char* key = format(AL_Compile, "type:map,arg:%s,arg:%s", key_as_ident, val_as_ident);

  if (!hashmap_get(&C(container_included), key)) {
    append_to_container_tokens(preprocess(tokenize(
        new_file(tok->file->name, format(AL_Compile,
                                         "#define __dyibicc_internal_include__ 1\n"
                                         "#define i_key %s\n"
                                         "#define i_val %s\n"
                                         "#define i_type _Map$%s$%s\n"
                                         "#include <_map.h>\n"
                                         "#undef __dyibicc_internal_include__\n",
                                         key_as_arg, val_as_arg, key_as_ident, val_as_ident)))));

    hashmap_put(&C(container_included), key, (void*)1);
  }

  Token* ret = tokenize(
      new_file(tok->file->name, format(AL_Compile, "_Map$%s$%s", key_as_ident, val_as_ident)));
  ret->next = rparen->next;
  return ret;
}

static void init_macros(void) {
  // Define predefined macros
  define_macro("_LP64", "1");
  define_macro("__C99_MACRO_WITH_VA_ARGS", "1");
  define_macro("__LP64__", "1");
  define_macro("__SIZEOF_DOUBLE__", "8");
  define_macro("__SIZEOF_FLOAT__", "4");
  define_macro("__SIZEOF_INT__", "4");
  define_macro("__SIZEOF_LONG_LONG__", "8");
  define_macro("__SIZEOF_POINTER__", "8");
  define_macro("__SIZEOF_PTRDIFF_T__", "8");
  define_macro("__SIZEOF_SHORT__", "2");
  define_macro("__SIZEOF_SIZE_T__", "8");
  define_macro("__SIZE_TYPE__", "unsigned long");
  define_macro("__STDC_HOSTED__", "1");
  define_macro("__STDC_NO_COMPLEX__", "1");
  define_macro("__STDC_UTF_16__", "1");
  define_macro("__STDC_UTF_32__", "1");
  define_macro("__STDC_VERSION__", "201112L");
  define_macro("__STDC__", "1");
  define_macro("__USER_LABEL_PREFIX__", "");
  define_macro("__alignof__", "_Alignof");
  define_macro("__amd64", "1");
  define_macro("__amd64__", "1");
  define_macro("__const__", "const");
  define_macro("__dyibicc__", "1");
  define_macro("__inline__", "inline");
  define_macro("__signed__", "signed");
  define_macro("__typeof__", "typeof");
  define_macro("__volatile__", "volatile");
  define_macro("__x86_64", "1");
  define_macro("__x86_64__", "1");
#if X64WIN
  define_macro("__SIZEOF_LONG__", "4");
  define_macro("__SIZEOF_LONG_DOUBLE__", "8");
  define_macro("__cdecl", "");
  define_macro("__stdcall", "");
  define_macro("__inline", "");
  define_macro("__forceinline", "");
  define_macro("__unaligned", "");
  define_macro("__alignof", "_Alignof");
  define_macro("__int8", "char");
  define_macro("__int16", "short");
  define_macro("__int32", "int");
  define_macro("__ptr32", "");  // Possibly wrong and needs to truncate?
  define_macro("__ptr64", "");
  define_macro("_M_X64", "100");
  define_macro("_M_AMD64", "100");
  define_macro("_AMD64_", "1");
  define_macro("_WIN32", "1");
  define_macro("WIN32", "1");
  define_macro("_WIN64", "1");
  // Without this, structs like OVERLAPPED don't use anon unions, so most user
  // code will break.
  define_macro("_MSC_EXTENSIONS", "1");
  // VS2008, arbitrarily. Has to be defined to something as a lot of code uses
  // it to indicate "is_windows", and 2008 was one of my favourites.
  define_macro("_MSC_VER", "1500");
  define_macro("_NO_CRT_STDIO_INLINE", "1");
  define_macro("_CRT_DECLARE_NONSTDC_NAMES", "1");
  define_macro("__WINT_TYPE__", "unsigned short");
  define_function_macro("__pragma(_)\n", NULL);
  define_function_macro("__declspec(_)\n", NULL);
#else
  define_macro("__SIZEOF_LONG__", "8");
  define_macro("__SIZEOF_LONG_DOUBLE__", "16");
  define_macro("__ELF__", "1");
  define_macro("linux", "1");
  define_macro("unix", "1");
  define_macro("__unix", "1");
  define_macro("__unix__", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");
#endif

  add_builtin("__FILE__", file_macro);
  add_builtin("__LINE__", line_macro);
  add_builtin("__COUNTER__", counter_macro);
  add_builtin("__TIMESTAMP__", timestamp_macro);
  add_builtin("__BASE_FILE__", base_file_macro);

  define_function_macro("$vec(T)", container_vec_setup);
  define_function_macro("$map(K,V)", container_map_setup);

  time_t now = time(NULL);
  struct tm* tm = localtime(&now);
  define_macro("__DATE__", format_date(tm));
  define_macro("__TIME__", format_time(tm));
}

typedef enum {
  STR_NONE,
  STR_UTF8,
  STR_UTF16,
  STR_UTF32,
  STR_WIDE,
} StringKind;

static StringKind get_string_kind(Token* tok) {
  if (!strcmp(tok->loc, "u8"))
    return STR_UTF8;

  switch (tok->loc[0]) {
    case '"':
      return STR_NONE;
    case 'u':
      return STR_UTF16;
    case 'U':
      return STR_UTF32;
    case 'L':
      return STR_WIDE;
  }
  unreachable();
}

// Concatenate adjacent string literals into a single string literal
// as per the C spec.
static void join_adjacent_string_literals(Token* tok) {
  // First pass: If regular string literals are adjacent to wide
  // string literals, regular string literals are converted to a wide
  // type before concatenation. In this pass, we do the conversion.
  for (Token* tok1 = tok; tok1->kind != TK_EOF;) {
    if (tok1->kind != TK_STR || tok1->next->kind != TK_STR) {
      tok1 = tok1->next;
      continue;
    }

    StringKind kind = get_string_kind(tok1);
    Type* basety = tok1->ty->base;

    for (Token* t = tok1->next; t->kind == TK_STR; t = t->next) {
      StringKind k = get_string_kind(t);
      if (kind == STR_NONE) {
        kind = k;
        basety = t->ty->base;
      } else if (k != STR_NONE && kind != k) {
        error_tok(t, "unsupported non-standard concatenation of string literals");
      }
    }

    if (basety->size > 1)
      for (Token* t = tok1; t->kind == TK_STR; t = t->next)
        if (t->ty->base->size == 1)
          *t = *tokenize_string_literal(t, basety);

    while (tok1->kind == TK_STR)
      tok1 = tok1->next;
  }

  // Second pass: concatenate adjacent string literals.
  for (Token* tok1 = tok; tok1->kind != TK_EOF;) {
    if (tok1->kind != TK_STR || tok1->next->kind != TK_STR) {
      tok1 = tok1->next;
      continue;
    }

    Token* tok2 = tok1->next;
    while (tok2->kind == TK_STR)
      tok2 = tok2->next;

    int len = tok1->ty->array_len;
    for (Token* t = tok1->next; t != tok2; t = t->next)
      len = len + t->ty->array_len - 1;

    char* buf = bumpcalloc(tok1->ty->base->size, len, AL_Compile);

    int i = 0;
    for (Token* t = tok1; t != tok2; t = t->next) {
      memcpy(buf + i, t->str, t->ty->size);
      i = i + t->ty->size - t->ty->base->size;
    }

    *tok1 = *copy_token(tok1);
    tok1->ty = array_of(tok1->ty->base, len, tok1);
    tok1->str = buf;
    tok1->next = tok2;
    tok1 = tok2;
  }
}

// Entry point function of the preprocessor.
static Token* preprocess(Token* tok) {
  tok = preprocess2(tok);
  if (C(cond_incl))
    error_tok(C(cond_incl)->tok, "unterminated conditional directive");
  convert_pp_tokens(tok);
  join_adjacent_string_literals(tok);

  for (Token* t = tok; t; t = t->next)
    t->line_no += t->line_delta;
  return tok;
}

static Token* add_container_instantiations(Token* tok) {
  // Reverse order is important. They were appended as included, so we need to
  // maintain that order here.
  for (int i = C(container_tokens).len - 1; i >= 0; --i) {
    Token* to_add = C(container_tokens).data[i];
    Token* cur = to_add;
    while (cur->next->kind != TK_EOF)
      cur = cur->next;
    cur->next = tok;
    tok = to_add;
  }
  return tok;
}
//
// END OF ../../src/preprocess.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/tokenize.c
//

#if X64WIN
#define strncasecmp _strnicmp
#endif

#define C(x) compiler_state.tokenize__##x

// Consumes the current token if it matches `op`.
static bool equal(Token* tok, char* op) {
  return strncmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

// Ensure that the current token is `op`.
static Token* skip(Token* tok, char* op) {
  if (!equal(tok, op))
    error_tok(tok, "expected '%s'", op);
  return tok->next;
}

static bool consume(Token** rest, Token* tok, char* str) {
  if (equal(tok, str)) {
    *rest = tok->next;
    return true;
  }
  *rest = tok;
  return false;
}

// Create a new token.
static Token* new_token(TokenKind kind, char* start, char* end) {
  Token* tok = bumpcalloc(1, sizeof(Token), AL_Compile);
  tok->kind = kind;
  tok->loc = start;
  tok->len = (int)(end - start);
  tok->file = C(current_file);
  tok->filename = C(current_file)->display_name;
  tok->at_bol = C(at_bol);
  tok->has_space = C(has_space);

  C(at_bol) = C(has_space) = false;
  return tok;
}

static bool startswith(char* p, char* q) {
  return strncmp(p, q, strlen(q)) == 0;
}

// Read an identifier and returns the length of it.
// If p does not point to a valid identifier, 0 is returned.
static int read_ident(char* start) {
  char* p = start;
  uint32_t c = decode_utf8(&p, p);

  if (!is_ident1(c))
    return 0;

  for (;;) {
    char* q;
    c = decode_utf8(&q, p);
    if (!is_ident2(c))
      return (int)(p - start);
    p = q;
  }
}

static int from_hex(char c) {
  if ('0' <= c && c <= '9')
    return c - '0';
  if ('a' <= c && c <= 'f')
    return c - 'a' + 10;
  return c - 'A' + 10;
}

// Read a punctuator token from p and returns its length.
static int read_punct(char* p) {
  static char* kw[] = {
      "<<=", ">>=", "...", "..", "==", "!=", "<=", ">=", "->", "+=", "-=", "*=",
      "/=",  "++",  "--", "%=",  "&=", "|=", "^=", "&&", "||", "<<", ">>", "##",
  };

  for (size_t i = 0; i < sizeof(kw) / sizeof(*kw); i++)
    if (startswith(p, kw[i]))
      return (int)strlen(kw[i]);

  return ispunct(*p) ? 1 : 0;
}

static bool is_keyword(Token* tok) {
  if (C(keyword_map).capacity == 0) {
    static char* kw[] = {
      "return",
      "if",
      "else",
      "for",
      "while",
      "int",
      "sizeof",
      "char",
      "struct",
      "union",
      "short",
      "long",
      "void",
      "typedef",
      "_Bool",
      "enum",
      "static",
      "goto",
      "break",
      "continue",
      "switch",
      "case",
      "default",
      "extern",
      "_Alignof",
      "_Alignas",
      "do",
      "signed",
      "unsigned",
      "const",
      "volatile",
      "auto",
      "register",
      "restrict",
      "__restrict",
      "__restrict__",
      "_Noreturn",
      "float",
      "double",
      "typeof",
      "asm",
      "_Thread_local",
      "__thread",
      "_Atomic",
      "__attribute__",

#if X64WIN
      "__int64",
#endif
    };

    for (size_t i = 0; i < sizeof(kw) / sizeof(*kw); i++)
      hashmap_put(&C(keyword_map), kw[i], (void*)1);
  }

  return hashmap_get2(&C(keyword_map), tok->loc, tok->len);
}

static int read_escaped_char(char** new_pos, char* p) {
  if ('0' <= *p && *p <= '7') {
    // Read an octal number.
    int c = *p++ - '0';
    if ('0' <= *p && *p <= '7') {
      c = (c << 3) + (*p++ - '0');
      if ('0' <= *p && *p <= '7')
        c = (c << 3) + (*p++ - '0');
    }
    *new_pos = p;
    return c;
  }

  if (*p == 'x') {
    // Read a hexadecimal number.
    p++;
    if (!isxdigit(*p))
      error_at(p, "invalid hex escape sequence");

    int c = 0;
    for (; isxdigit(*p); p++)
      c = (c << 4) + from_hex(*p);
    *new_pos = p;
    return c;
  }

  *new_pos = p + 1;

  // Escape sequences are defined using themselves here. E.g.
  // '\n' is implemented using '\n'. This tautological definition
  // works because the compiler that compiles our compiler knows
  // what '\n' actually is. In other words, we "inherit" the ASCII
  // code of '\n' from the compiler that compiles our compiler,
  // so we don't have to teach the actual code here.
  //
  // This fact has huge implications not only for the correctness
  // of the compiler but also for the security of the generated code.
  // For more info, read "Reflections on Trusting Trust" by Ken Thompson.
  // https://github.com/rui314/chibicc/wiki/thompson1984.pdf
  switch (*p) {
    case 'a':
      return '\a';
    case 'b':
      return '\b';
    case 't':
      return '\t';
    case 'n':
      return '\n';
    case 'v':
      return '\v';
    case 'f':
      return '\f';
    case 'r':
      return '\r';
    // [GNU] \e for the ASCII escape character is a GNU C extension.
    case 'e':
      return 27;
    default:
      return *p;
  }
}

// Find a closing double-quote.
static char* string_literal_end(char* p) {
  char* start = p;
  for (; *p != '"'; p++) {
    if (*p == '\n' || *p == '\0')
      error_at(start, "unclosed string literal");
    if (*p == '\\')
      p++;
  }
  return p;
}

static Token* read_string_literal(char* start, char* quote) {
  char* end = string_literal_end(quote + 1);
  char* buf = bumpcalloc(1, end - quote, AL_Compile);
  int len = 0;

  for (char* p = quote + 1; p < end;) {
    if (*p == '\\')
      buf[len++] = (char)read_escaped_char(&p, p + 1);
    else
      buf[len++] = *p++;
  }

  Token* tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty_char, len + 1, NULL);
  tok->str = buf;
  return tok;
}

// Read a UTF-8-encoded string literal and transcode it in UTF-16.
//
// UTF-16 is yet another variable-width encoding for Unicode. Code
// points smaller than U+10000 are encoded in 2 bytes. Code points
// equal to or larger than that are encoded in 4 bytes. Each 2 bytes
// in the 4 byte sequence is called "surrogate", and a 4 byte sequence
// is called a "surrogate pair".
static Token* read_utf16_string_literal(char* start, char* quote) {
  char* end = string_literal_end(quote + 1);
  uint16_t* buf = bumpcalloc(2, end - start, AL_Compile);
  int len = 0;

  for (char* p = quote + 1; p < end;) {
    if (*p == '\\') {
      buf[len++] = (uint16_t)read_escaped_char(&p, p + 1);
      continue;
    }

    uint32_t c = decode_utf8(&p, p);
    if (c < 0x10000) {
      // Encode a code point in 2 bytes.
      buf[len++] = (uint16_t)c;
    } else {
      // Encode a code point in 4 bytes.
      c -= 0x10000;
      buf[len++] = 0xd800 + ((c >> 10) & 0x3ff);
      buf[len++] = 0xdc00 + (c & 0x3ff);
    }
  }

  Token* tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty_ushort, len + 1, NULL);
  tok->str = (char*)buf;
  return tok;
}

// Read a UTF-8-encoded string literal and transcode it in UTF-32.
//
// UTF-32 is a fixed-width encoding for Unicode. Each code point is
// encoded in 4 bytes.
static Token* read_utf32_string_literal(char* start, char* quote, Type* ty) {
  char* end = string_literal_end(quote + 1);
  uint32_t* buf = bumpcalloc(4, end - quote, AL_Compile);
  int len = 0;

  for (char* p = quote + 1; p < end;) {
    if (*p == '\\')
      buf[len++] = read_escaped_char(&p, p + 1);
    else
      buf[len++] = decode_utf8(&p, p);
  }

  Token* tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty, len + 1, NULL);
  tok->str = (char*)buf;
  return tok;
}

static Token* read_char_literal(char* start, char* quote, Type* ty) {
  char* p = quote + 1;
  if (*p == '\0')
    error_at(start, "unclosed char literal");

  int c;
  if (*p == '\\')
    c = read_escaped_char(&p, p + 1);
  else
    c = decode_utf8(&p, p);

  char* end = strchr(p, '\'');
  if (!end)
    error_at(p, "unclosed char literal");

  Token* tok = new_token(TK_NUM, start, end + 1);
  tok->val = c;
  tok->ty = ty;
  return tok;
}

static bool convert_pp_int(Token* tok) {
  char* p = tok->loc;

  // Read a binary, octal, decimal or hexadecimal number.
  int base = 10;
  if (!strncasecmp(p, "0x", 2) && isxdigit(p[2])) {
    p += 2;
    base = 16;
  } else if (!strncasecmp(p, "0b", 2) && (p[2] == '0' || p[2] == '1')) {
    p += 2;
    base = 2;
  } else if (*p == '0') {
    base = 8;
  }

  int64_t val = strtoull(p, &p, base);

  // Read U, L or LL suffixes.
  bool l = false;
  bool u = false;

  if (startswith(p, "LLU") || startswith(p, "LLu") || startswith(p, "llU") ||
      startswith(p, "llu") || startswith(p, "ULL") || startswith(p, "Ull") ||
      startswith(p, "uLL") || startswith(p, "ull")) {
    p += 3;
    l = u = true;
#if X64WIN
  } else if (startswith(p, "i64")) {
    p += 3;
    l = true;
  } else if (startswith(p, "ui64")) {
    p += 4;
    l = u = true;
#endif
  } else if (!strncasecmp(p, "lu", 2) || !strncasecmp(p, "ul", 2)) {
    p += 2;
    l = u = true;
  } else if (startswith(p, "LL") || startswith(p, "ll")) {
    p += 2;
    l = true;
  } else if (*p == 'L' || *p == 'l') {
    p++;
    l = true;
  } else if (*p == 'U' || *p == 'u') {
    p++;
    u = true;
  }

  if (p != tok->loc + tok->len)
    return false;

  // Infer a type.
  Type* ty;
  if (base == 10) {
    if (l && u)
      ty = ty_ulong;
    else if (l)
      ty = ty_long;
    else if (u)
      ty = (val >> 32) ? ty_ulong : ty_uint;
    else
      ty = (val >> 31) ? ty_long : ty_int;
  } else {
    if (l && u)
      ty = ty_ulong;
    else if (l)
      ty = (val >> 63) ? ty_ulong : ty_long;
    else if (u)
      ty = (val >> 32) ? ty_ulong : ty_uint;
    else if (val >> 63)
      ty = ty_ulong;
    else if (val >> 32)
      ty = ty_long;
    else if (val >> 31)
      ty = ty_uint;
    else
      ty = ty_int;
  }

  tok->kind = TK_NUM;
  tok->val = val;
  tok->ty = ty;
  return true;
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
//
// This function converts a pp-number token to a regular number token.
static void convert_pp_number(Token* tok) {
  // Try to parse as an integer constant.
  if (convert_pp_int(tok))
    return;

  // If it's not an integer, it must be a floating point constant.
  char* end;
  long double val = strtold(tok->loc, &end);

  Type* ty;
  if (*end == 'f' || *end == 'F') {
    ty = ty_float;
    end++;
  } else if (*end == 'l' || *end == 'L') {
    ty = ty_ldouble;
    end++;
  } else {
    ty = ty_double;
  }

  if (tok->loc + tok->len != end)
    error_tok(tok, "invalid numeric constant");

  tok->kind = TK_NUM;
  tok->fval = val;
  tok->ty = ty;
}

static void convert_pp_tokens(Token* tok) {
  for (Token* t = tok; t->kind != TK_EOF; t = t->next) {
    if (t->kind == TK_IDENT && is_keyword(t))
      t->kind = TK_KEYWORD;
    else if (t->kind == TK_PP_NUM)
      convert_pp_number(t);
  }
}

// Initialize line info for all tokens.
static void add_line_numbers(Token* tok) {
  char* p = C(current_file)->contents;
  int n = 1;

  do {
    if (p == tok->loc) {
      tok->line_no = n;
      tok = tok->next;
    }
    if (*p == '\n')
      n++;
  } while (*p++);
}

Token* tokenize_string_literal(Token* tok, Type* basety) {
  Token* t;
  if (basety->size == 2)
    t = read_utf16_string_literal(tok->loc, tok->loc);
  else
    t = read_utf32_string_literal(tok->loc, tok->loc, basety);
  t->next = tok->next;
  return t;
}

// Tokenize a given string and returns new tokens.
Token* tokenize(File* file) {
  C(current_file) = file;

  char* p = file->contents;
  Token head = {0};
  Token* cur = &head;

  C(at_bol) = true;
  C(has_space) = false;

  while (*p) {
    // Skip line comments.
    if (p[0] == '/' && p[1] == '/') {
      p += 2;
      while (*p && *p != '\n')
        p++;
      C(has_space) = true;
      continue;
    }

    // Skip block comments.
    if (p[0] == '/' && p[1] == '*') {
      char* q = strstr(p + 2, "*/");
      if (!q)
        error_at(p, "unclosed block comment");
      p = q + 2;
      C(has_space) = true;
      continue;
    }

    // Skip newline.
    if (*p == '\n') {
      p++;
      C(at_bol) = true;
      C(has_space) = false;
      continue;
    }

    // Skip whitespace characters.
    if (isspace((unsigned char)*p)) {
      p++;
      C(has_space) = true;
      continue;
    }

    // Numeric literal
    if (isdigit((unsigned char)*p) || (*p == '.' && isdigit((unsigned char)p[1]))) {
      char* q = p++;
      for (;;) {
        if (p[0] && p[1] && strchr("eEpP", p[0]) && strchr("+-", p[1]))
          p += 2;
        else if (isalnum((unsigned char)*p) || *p == '.')
          p++;
        else
          break;
      }
      cur = cur->next = new_token(TK_PP_NUM, q, p);
      continue;
    }

    // String literal
    if (*p == '"') {
      cur = cur->next = read_string_literal(p, p);
      p += cur->len;
      continue;
    }

    // UTF-8 string literal
    if (startswith(p, "u8\"")) {
      cur = cur->next = read_string_literal(p, p + 2);
      p += cur->len;
      continue;
    }

    // UTF-16 string literal
    if (startswith(p, "u\"")) {
      cur = cur->next = read_utf16_string_literal(p, p + 1);
      p += cur->len;
      continue;
    }

    // Wide string literal
    if (startswith(p, "L\"")) {
      cur = cur->next = read_utf32_string_literal(p, p + 1, ty_int);
      p += cur->len;
      continue;
    }

    // UTF-32 string literal
    if (startswith(p, "U\"")) {
      cur = cur->next = read_utf32_string_literal(p, p + 1, ty_uint);
      p += cur->len;
      continue;
    }

    // Character literal
    if (*p == '\'') {
      cur = cur->next = read_char_literal(p, p, ty_int);
      cur->val = (char)cur->val;
      p += cur->len;
      continue;
    }

    // UTF-16 character literal
    if (startswith(p, "u'")) {
      cur = cur->next = read_char_literal(p, p + 1, ty_ushort);
      cur->val &= 0xffff;
      p += cur->len;
      continue;
    }

    // Wide character literal
    if (startswith(p, "L'")) {
      cur = cur->next = read_char_literal(p, p + 1, ty_int);
      p += cur->len;
      continue;
    }

    // UTF-32 character literal
    if (startswith(p, "U'")) {
      cur = cur->next = read_char_literal(p, p + 1, ty_uint);
      p += cur->len;
      continue;
    }

    // Identifier or keyword
    int ident_len = read_ident(p);
    if (ident_len) {
      cur = cur->next = new_token(TK_IDENT, p, p + ident_len);
      p += cur->len;
      continue;
    }

    // Punctuators
    int punct_len = read_punct(p);
    if (punct_len) {
      cur = cur->next = new_token(TK_PUNCT, p, p + punct_len);
      p += cur->len;
      continue;
    }

    error_at(p, "invalid token");
  }

  cur = cur->next = new_token(TK_EOF, p, p);
  add_line_numbers(head.next);
  return head.next;
}

static File* new_file(char* name, char* contents) {
  File* file = bumpcalloc(1, sizeof(File), AL_Compile);
  file->name = name;
  file->display_name = name;
  file->contents = contents;
  return file;
}

// Replaces \r or \r\n with \n.
static void canonicalize_newline(char* p) {
  int i = 0, j = 0;

  while (p[i]) {
    if (p[i] == '\r' && p[i + 1] == '\n') {
      i += 2;
      p[j++] = '\n';
    } else if (p[i] == '\r') {
      i++;
      p[j++] = '\n';
    } else {
      p[j++] = p[i++];
    }
  }

  p[j] = '\0';
}

// Removes backslashes followed by a newline.
static void remove_backslash_newline(char* p) {
  int i = 0, j = 0;

  // We want to keep the number of newline characters so that
  // the logical line number matches the physical one.
  // This counter maintain the number of newlines we have removed.
  int n = 0;

  while (p[i]) {
    if (p[i] == '\\' && p[i + 1] == '\n') {
      i += 2;
      n++;
    } else if (p[i] == '\n') {
      p[j++] = p[i++];
      for (; n > 0; n--)
        p[j++] = '\n';
    } else {
      p[j++] = p[i++];
    }
  }

  for (; n > 0; n--)
    p[j++] = '\n';
  p[j] = '\0';
}

static uint32_t read_universal_char(char* p, int len) {
  uint32_t c = 0;
  for (int i = 0; i < len; i++) {
    if (!isxdigit(p[i]))
      return 0;
    c = (c << 4) | from_hex(p[i]);
  }
  return c;
}

// Replace \u or \U escape sequences with corresponding UTF-8 bytes.
static void convert_universal_chars(char* p) {
  char* q = p;

  while (*p) {
    if (p[0] == '\\' && p[1] == 'u') {
      uint32_t c = read_universal_char(p + 2, 4);
      if (c) {
        p += 6;
        q += encode_utf8(q, c);
      } else {
        *q++ = *p++;
      }
    } else if (p[0] == '\\' && p[1] == 'U') {
      uint32_t c = read_universal_char(p + 2, 8);
      if (c) {
        p += 10;
        q += encode_utf8(q, c);
      } else {
        *q++ = *p++;
      }
    } else if (p[0] == '\\') {
      *q++ = *p++;
      if (!*p)
        break;
      *q++ = *p++;
    } else {
      *q++ = *p++;
    }
  }

  *q = '\0';
}

Token* tokenize_filecontents(char* path, char* p) {
  // UTF-8 texts may start with a 3-byte "BOM" marker sequence.
  // If exists, just skip them because they are useless bytes.
  // (It is actually not recommended to add BOM markers to UTF-8
  // texts, but it's not uncommon particularly on Windows.)
  if (!memcmp(p, "\xef\xbb\xbf", 3))
    p += 3;

  canonicalize_newline(p);
  remove_backslash_newline(p);
  convert_universal_chars(p);

  File* file = new_file(path, p);
  file->file_no = C(all_tokenized_files).len;
  fileptrarray_push(&C(all_tokenized_files), file, AL_Compile);
  return tokenize(file);
}

Token* tokenize_file(char* path) {
  char* p = read_file_wrap_user(path, AL_Compile);
  if (!p)
    return NULL;
  return tokenize_filecontents(path, p);
}
//
// END OF ../../src/tokenize.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/unicode.c
//

// Encode a given character in UTF-8.
static int encode_utf8(char* buf, uint32_t c) {
  if (c <= 0x7F) {
    buf[0] = (char)c;
    return 1;
  }

  if (c <= 0x7FF) {
    buf[0] = (char)(0b11000000 | (c >> 6));
    buf[1] = (char)(0b10000000 | (c & 0b00111111));
    return 2;
  }

  if (c <= 0xFFFF) {
    buf[0] = (char)(0b11100000 | (c >> 12));
    buf[1] = (char)(0b10000000 | ((c >> 6) & 0b00111111));
    buf[2] = (char)(0b10000000 | (c & 0b00111111));
    return 3;
  }

  buf[0] = (char)(0b11110000 | (c >> 18));
  buf[1] = (char)(0b10000000 | ((c >> 12) & 0b00111111));
  buf[2] = (char)(0b10000000 | ((c >> 6) & 0b00111111));
  buf[3] = (char)(0b10000000 | (c & 0b00111111));
  return 4;
}

// Read a UTF-8-encoded Unicode code point from a source file.
// We assume that source files are always in UTF-8.
//
// UTF-8 is a variable-width encoding in which one code point is
// encoded in one to four bytes. One byte UTF-8 code points are
// identical to ASCII. Non-ASCII characters are encoded using more
// than one byte.
static uint32_t decode_utf8(char** new_pos, char* p) {
  if ((unsigned char)*p < 128) {
    *new_pos = p + 1;
    return *p;
  }

  char* start = p;
  int len;
  uint32_t c;

  if ((unsigned char)*p >= 0b11110000) {
    len = 4;
    c = *p & 0b111;
  } else if ((unsigned char)*p >= 0b11100000) {
    len = 3;
    c = *p & 0b1111;
  } else if ((unsigned char)*p >= 0b11000000) {
    len = 2;
    c = *p & 0b11111;
  } else {
    error_at(start, "invalid UTF-8 sequence");
  }

  for (int i = 1; i < len; i++) {
    if ((unsigned char)p[i] >> 6 != 0b10)
      error_at(start, "invalid UTF-8 sequence");
    c = (c << 6) | (p[i] & 0b111111);
  }

  *new_pos = p + len;
  return c;
}

static bool in_range(uint32_t* range, uint32_t c) {
  for (uint32_t i = 0; range[i] != (uint32_t)-1; i += 2)
    if (range[i] <= c && c <= range[i + 1])
      return true;
  return false;
}

// [https://www.sigbus.info/n1570#D] C11 allows not only ASCII but
// some multibyte characters in certan Unicode ranges to be used in an
// identifier.
//
// This function returns true if a given character is acceptable as
// the first character of an identifier.
//
// For example, ¾ (U+00BE) is a valid identifier because characters in
// 0x00BE-0x00C0 are allowed, while neither ⟘ (U+27D8) nor '　'
// (U+3000, full-width space) are allowed because they are out of range.
static bool is_ident1(uint32_t c) {
  // Slight performance improvement to early out before full test.
  if (c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    return true;

  static uint32_t range[] = {
      // '_',     '_',     'a',     'z',     'A',     'Z',
      '$',          '$',     0x00A8,  0x00A8,  0x00AA,  0x00AA,  0x00AD,  0x00AD,  0x00AF,  0x00AF,
      0x00B2,       0x00B5,  0x00B7,  0x00BA,  0x00BC,  0x00BE,  0x00C0,  0x00D6,  0x00D8,  0x00F6,
      0x00F8,       0x00FF,  0x0100,  0x02FF,  0x0370,  0x167F,  0x1681,  0x180D,  0x180F,  0x1DBF,
      0x1E00,       0x1FFF,  0x200B,  0x200D,  0x202A,  0x202E,  0x203F,  0x2040,  0x2054,  0x2054,
      0x2060,       0x206F,  0x2070,  0x20CF,  0x2100,  0x218F,  0x2460,  0x24FF,  0x2776,  0x2793,
      0x2C00,       0x2DFF,  0x2E80,  0x2FFF,  0x3004,  0x3007,  0x3021,  0x302F,  0x3031,  0x303F,
      0x3040,       0xD7FF,  0xF900,  0xFD3D,  0xFD40,  0xFDCF,  0xFDF0,  0xFE1F,  0xFE30,  0xFE44,
      0xFE47,       0xFFFD,  0x10000, 0x1FFFD, 0x20000, 0x2FFFD, 0x30000, 0x3FFFD, 0x40000, 0x4FFFD,
      0x50000,      0x5FFFD, 0x60000, 0x6FFFD, 0x70000, 0x7FFFD, 0x80000, 0x8FFFD, 0x90000, 0x9FFFD,
      0xA0000,      0xAFFFD, 0xB0000, 0xBFFFD, 0xC0000, 0xCFFFD, 0xD0000, 0xDFFFD, 0xE0000, 0xEFFFD,
      (uint32_t)-1,
  };

  return in_range(range, c);
}

// Returns true if a given character is acceptable as a non-first
// character of an identifier.
static bool is_ident2(uint32_t c) {
  // Slight performance improvement to early out before full test.
  if (c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
    return true;

  static uint32_t range[] = {
      '0',    '9',    '$',    '$',    0x0300, 0x036F,       0x1DC0,
      0x1DFF, 0x20D0, 0x20FF, 0xFE20, 0xFE2F, (uint32_t)-1,
  };

  return is_ident1(c) || in_range(range, c);
}

// Returns the number of columns needed to display a given
// character in a fixed-width font.
//
// Based on https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
static int char_width(uint32_t c) {
  static uint32_t range1[] = {
      0x0000,  0x001F,  0x007f,  0x00a0,  0x0300,  0x036F,  0x0483,  0x0486,  0x0488,       0x0489,
      0x0591,  0x05BD,  0x05BF,  0x05BF,  0x05C1,  0x05C2,  0x05C4,  0x05C5,  0x05C7,       0x05C7,
      0x0600,  0x0603,  0x0610,  0x0615,  0x064B,  0x065E,  0x0670,  0x0670,  0x06D6,       0x06E4,
      0x06E7,  0x06E8,  0x06EA,  0x06ED,  0x070F,  0x070F,  0x0711,  0x0711,  0x0730,       0x074A,
      0x07A6,  0x07B0,  0x07EB,  0x07F3,  0x0901,  0x0902,  0x093C,  0x093C,  0x0941,       0x0948,
      0x094D,  0x094D,  0x0951,  0x0954,  0x0962,  0x0963,  0x0981,  0x0981,  0x09BC,       0x09BC,
      0x09C1,  0x09C4,  0x09CD,  0x09CD,  0x09E2,  0x09E3,  0x0A01,  0x0A02,  0x0A3C,       0x0A3C,
      0x0A41,  0x0A42,  0x0A47,  0x0A48,  0x0A4B,  0x0A4D,  0x0A70,  0x0A71,  0x0A81,       0x0A82,
      0x0ABC,  0x0ABC,  0x0AC1,  0x0AC5,  0x0AC7,  0x0AC8,  0x0ACD,  0x0ACD,  0x0AE2,       0x0AE3,
      0x0B01,  0x0B01,  0x0B3C,  0x0B3C,  0x0B3F,  0x0B3F,  0x0B41,  0x0B43,  0x0B4D,       0x0B4D,
      0x0B56,  0x0B56,  0x0B82,  0x0B82,  0x0BC0,  0x0BC0,  0x0BCD,  0x0BCD,  0x0C3E,       0x0C40,
      0x0C46,  0x0C48,  0x0C4A,  0x0C4D,  0x0C55,  0x0C56,  0x0CBC,  0x0CBC,  0x0CBF,       0x0CBF,
      0x0CC6,  0x0CC6,  0x0CCC,  0x0CCD,  0x0CE2,  0x0CE3,  0x0D41,  0x0D43,  0x0D4D,       0x0D4D,
      0x0DCA,  0x0DCA,  0x0DD2,  0x0DD4,  0x0DD6,  0x0DD6,  0x0E31,  0x0E31,  0x0E34,       0x0E3A,
      0x0E47,  0x0E4E,  0x0EB1,  0x0EB1,  0x0EB4,  0x0EB9,  0x0EBB,  0x0EBC,  0x0EC8,       0x0ECD,
      0x0F18,  0x0F19,  0x0F35,  0x0F35,  0x0F37,  0x0F37,  0x0F39,  0x0F39,  0x0F71,       0x0F7E,
      0x0F80,  0x0F84,  0x0F86,  0x0F87,  0x0F90,  0x0F97,  0x0F99,  0x0FBC,  0x0FC6,       0x0FC6,
      0x102D,  0x1030,  0x1032,  0x1032,  0x1036,  0x1037,  0x1039,  0x1039,  0x1058,       0x1059,
      0x1160,  0x11FF,  0x135F,  0x135F,  0x1712,  0x1714,  0x1732,  0x1734,  0x1752,       0x1753,
      0x1772,  0x1773,  0x17B4,  0x17B5,  0x17B7,  0x17BD,  0x17C6,  0x17C6,  0x17C9,       0x17D3,
      0x17DD,  0x17DD,  0x180B,  0x180D,  0x18A9,  0x18A9,  0x1920,  0x1922,  0x1927,       0x1928,
      0x1932,  0x1932,  0x1939,  0x193B,  0x1A17,  0x1A18,  0x1B00,  0x1B03,  0x1B34,       0x1B34,
      0x1B36,  0x1B3A,  0x1B3C,  0x1B3C,  0x1B42,  0x1B42,  0x1B6B,  0x1B73,  0x1DC0,       0x1DCA,
      0x1DFE,  0x1DFF,  0x200B,  0x200F,  0x202A,  0x202E,  0x2060,  0x2063,  0x206A,       0x206F,
      0x20D0,  0x20EF,  0x302A,  0x302F,  0x3099,  0x309A,  0xA806,  0xA806,  0xA80B,       0xA80B,
      0xA825,  0xA826,  0xFB1E,  0xFB1E,  0xFE00,  0xFE0F,  0xFE20,  0xFE23,  0xFEFF,       0xFEFF,
      0xFFF9,  0xFFFB,  0x10A01, 0x10A03, 0x10A05, 0x10A06, 0x10A0C, 0x10A0F, 0x10A38,      0x10A3A,
      0x10A3F, 0x10A3F, 0x1D167, 0x1D169, 0x1D173, 0x1D182, 0x1D185, 0x1D18B, 0x1D1AA,      0x1D1AD,
      0x1D242, 0x1D244, 0xE0001, 0xE0001, 0xE0020, 0xE007F, 0xE0100, 0xE01EF, (uint32_t)-1,
  };

  if (in_range(range1, c))
    return 0;

  static uint32_t range2[] = {
      0x1100, 0x115F, 0x2329,  0x2329,  0x232A,  0x232A,  0x2E80,  0x303E,  0x3040,       0xA4CF,
      0xAC00, 0xD7A3, 0xF900,  0xFAFF,  0xFE10,  0xFE19,  0xFE30,  0xFE6F,  0xFF00,       0xFF60,
      0xFFE0, 0xFFE6, 0x1F000, 0x1F644, 0x20000, 0x2FFFD, 0x30000, 0x3FFFD, (uint32_t)-1,
  };

  if (in_range(range2, c))
    return 2;
  return 1;
}

// Returns the number of columns needed to display a given
// string in a fixed-width font.
static int display_width(char* p, int len) {
  char* start = p;
  int w = 0;
  while (p - start < len) {
    uint32_t c = decode_utf8(&p, p);
    w += char_width(c);
  }
  return w;
}
//
// END OF ../../src/unicode.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/util.c
//

#ifdef _WIN64
#include <windows.h>
#else
#include <errno.h>
#include <sys/stat.h>
#endif

static char* bumpstrndup(const char* s, size_t n, AllocLifetime lifetime) {
  size_t l = strnlen(s, n);
  char* d = bumpcalloc(1, l + 1, lifetime);
  if (!d)
    return NULL;
  memcpy(d, s, l);
  d[l] = 0;
  return d;
}

static char* bumpstrdup(const char* s, AllocLifetime lifetime) {
  size_t l = strlen(s);
  char* d = bumpcalloc(1, l + 1, lifetime);
  if (!d)
    return NULL;
  memcpy(d, s, l);
  d[l] = 0;
  return d;
}

static char* dirname(char* s) {
  size_t i;
  if (!s || !*s)
    return ".";
  i = strlen(s) - 1;
  for (; s[i] == '/' || s[i] == '\\'; i--)
    if (!i)
      return "/";
  for (; s[i] != '/' || s[i] == '\\'; i--)
    if (!i)
      return ".";
  for (; s[i] == '/' || s[i] == '\\'; i--)
    if (!i)
      return "/";
  s[i + 1] = 0;
  return s;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
static uint64_t align_to_u(uint64_t n, uint64_t align) {
  return (n + align - 1) / align * align;
}

static int64_t align_to_s(int64_t n, int64_t align) {
  return (n + align - 1) / align * align;
}

static unsigned int get_page_size(void) {
#if X64WIN
  SYSTEM_INFO system_info;
  GetSystemInfo(&system_info);
  return system_info.dwPageSize;
#else
  return sysconf(_SC_PAGESIZE);
#endif
}

static void strarray_push(StringArray* arr, char* s, AllocLifetime lifetime) {
  if (!arr->data) {
    arr->data = bumpcalloc(8, sizeof(char*), lifetime);
    arr->capacity = 8;
  }

  if (arr->capacity == arr->len) {
    arr->data = bumplamerealloc(arr->data, sizeof(char*) * arr->capacity,
                                sizeof(char*) * arr->capacity * 2, lifetime);
    arr->capacity *= 2;
    for (int i = arr->len; i < arr->capacity; i++)
      arr->data[i] = NULL;
  }

  arr->data[arr->len++] = s;
}

static void strintarray_push(StringIntArray* arr, StringInt item, AllocLifetime lifetime) {
  if (!arr->data) {
    arr->data = bumpcalloc(8, sizeof(StringInt), lifetime);
    arr->capacity = 8;
  }

  if (arr->capacity == arr->len) {
    arr->data = bumplamerealloc(arr->data, sizeof(StringInt) * arr->capacity,
                                sizeof(StringInt) * arr->capacity * 2, lifetime);
    arr->capacity *= 2;
    for (int i = arr->len; i < arr->capacity; i++)
      arr->data[i] = (StringInt){NULL, -1};
  }

  arr->data[arr->len++] = item;
}

static void fileptrarray_push(FilePtrArray* arr, File* item, AllocLifetime lifetime) {
  if (!arr->data) {
    arr->data = bumpcalloc(8, sizeof(File*), lifetime);
    arr->capacity = 8;
  }

  if (arr->capacity == arr->len) {
    arr->data = bumplamerealloc(arr->data, sizeof(File*) * arr->capacity,
                                sizeof(File*) * arr->capacity * 2, lifetime);
    arr->capacity *= 2;
    for (int i = arr->len; i < arr->capacity; i++)
      arr->data[i] = NULL;
  }

  arr->data[arr->len++] = item;
}

static void tokenptrarray_push(TokenPtrArray* arr, Token* item, AllocLifetime lifetime) {
  if (!arr->data) {
    arr->data = bumpcalloc(8, sizeof(Token*), lifetime);
    arr->capacity = 8;
  }

  if (arr->capacity == arr->len) {
    arr->data = bumplamerealloc(arr->data, sizeof(Token*) * arr->capacity,
                                sizeof(Token*) * arr->capacity * 2, lifetime);
    arr->capacity *= 2;
    for (int i = arr->len; i < arr->capacity; i++)
      arr->data[i] = NULL;
  }

  arr->data[arr->len++] = item;
}

#if X64WIN
static void intintintarray_push(IntIntIntArray* arr, IntIntInt item, AllocLifetime lifetime) {
  if (!arr->data) {
    arr->data = bumpcalloc(8, sizeof(IntIntInt), lifetime);
    arr->capacity = 8;
  }

  if (arr->capacity == arr->len) {
    arr->data = bumplamerealloc(arr->data, sizeof(IntIntInt) * arr->capacity,
                                sizeof(IntIntInt) * arr->capacity * 2, lifetime);
    arr->capacity *= 2;
    for (int i = arr->len; i < arr->capacity; i++)
      arr->data[i] = (IntIntInt){-1, -1, -1};
  }

  arr->data[arr->len++] = item;
}
#endif

// Returns the contents of a given file. Doesn't support '-' for reading from
// stdin.
static char* read_file_wrap_user(char* path, AllocLifetime lifetime) {
  char* contents;
  size_t size;
  if (!user_context->load_file_contents(path, &contents, &size))
    return NULL;

  char* buf = bumpcalloc(1, size + 1, lifetime);  // TODO: doesn't really need a calloc
  memcpy(buf, contents, size);
  free(contents);
  buf[size] = 0;
  return buf;
}

// Takes a printf-style format string and returns a formatted string.
static char* format(AllocLifetime lifetime, char* fmt, ...) {
  char buf[4096];

  va_list ap;
  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
  va_end(ap);
  return bumpstrdup(buf, lifetime);
}

static int outaf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ret = user_context->output_function(fmt, ap);
  va_end(ap);
  return ret;
}

#define ANSI_WHITE "\033[1;37m"
#define ANSI_GREEN "\033[1;32m"
#define ANSI_RED "\033[1;31m"
#define ANSI_RESET "\033[0m"

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(char* filename, char* input, int line_no, char* loc, char* fmt, va_list ap) {
  // Find a line containing `loc`.
  char* line = loc;
  while (input < line && line[-1] != '\n')
    line--;

  char* end = loc;
  while (*end && *end != '\n')
    end++;

  // Print out the line.
  if (user_context->use_ansi_codes)
    outaf(ANSI_WHITE);

  int indent = outaf("%s:%d: ", filename, line_no);

  if (user_context->use_ansi_codes)
    outaf(ANSI_RESET);

  outaf("%.*s\n", (int)(end - line), line);

  // Show the error message.
  int pos = display_width(line, (int)(loc - line)) + indent;

  outaf("%*s", pos, "");  // print pos spaces.

  if (user_context->use_ansi_codes)
    outaf("%s^ %serror: %s", ANSI_GREEN, ANSI_RED, ANSI_WHITE);
  else
    outaf("^ error: ");

  user_context->output_function(fmt, ap);

  outaf("\n");
  if (user_context->use_ansi_codes)
    outaf("%s", ANSI_RESET);
}

static void error_at(char* loc, char* fmt, ...) {
  File* cf = compiler_state.tokenize__current_file;

  int line_no = 1;
  for (char* p = cf->contents; p < loc; p++)
    if (*p == '\n')
      line_no++;

  va_list ap;
  va_start(ap, fmt);
  verror_at(cf->name, cf->contents, line_no, loc, fmt, ap);
  longjmp(toplevel_update_jmpbuf, 1);
}

static void error_tok(Token* tok, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
  longjmp(toplevel_update_jmpbuf, 1);
}

static void warn_tok(Token* tok, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
  va_end(ap);
}

// Reports an error and exit update.
static void error(char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  if (!user_context || !user_context->output_function) {
    vfprintf(stderr, fmt, ap);
  } else {
    user_context->output_function(fmt, ap);
    outaf("\n");
  }
  longjmp(toplevel_update_jmpbuf, 1);
}

static void error_internal(char* file, int line, char* msg) {
  outaf("%sinternal error at %s:%d: %s%s\n%s", ANSI_RED, file, line, ANSI_WHITE, msg, ANSI_RESET);
  longjmp(toplevel_update_jmpbuf, 1);
}

#if X64WIN
static void register_function_table_data(UserContext* ctx, int func_count, char* base_addr) {
  if (ctx->generate_debug_symbols) {
    // We don't use RtlAddFunctionTable/RtlDeleteFunctionTable if using a pdb
    // dll, as the tables are already in the .pdata/.xdata section.
    return;
  }
  if (!RtlAddFunctionTable((RUNTIME_FUNCTION*)ctx->function_table_data, func_count,
                           (DWORD64)base_addr)) {
    error("failed to RtlAddFunctionTable");
  }
}

static void unregister_and_free_function_table_data(UserContext* ctx) {
  if (ctx->function_table_data) {
    // We don't use RtlAddFunctionTable/RtlDeleteFunctionTable if using a pdb
    // dll, as the tables are already in the .pdata/.xdata section.
    if (!ctx->generate_debug_symbols) {
      if (!RtlDeleteFunctionTable((RUNTIME_FUNCTION*)ctx->function_table_data)) {
        error("failed to RtlDeleteFunctionTable");
      }
    }
    free(ctx->function_table_data);
    ctx->function_table_data = NULL;
  }
}

static char* get_temp_pdb_filename(AllocLifetime lifetime) {
  char name_template[1024] = "dyibicc-XXXXXX";
  if (_mktemp_s(name_template, strlen(name_template) + 1) < 0) {
    error("failed to _mktemp_s");
  }
  strcat(name_template, ".pdb");
  return bumpstrdup(name_template, lifetime);
}

#define DYN_BASIC_PDB_IMPLEMENTATION

#endif
//
// END OF ../../src/util.c
//
#undef C
#undef L
#undef VOID
//
// START OF ../../src/dyn_basic_pdb.h
//
#ifndef INCLUDED_DYN_BASIC_PDB_H
#define INCLUDED_DYN_BASIC_PDB_H

// In exactly *one* C file:
//
//   #define DYN_BASIC_PDB_IMPLEMENTATION
//   #include "dyn_basic_pdb.h"
//
// then include and use dyn_basic_pdb.h in other files as usual.
//
// See dbp_example/dyn_basic_pdb_example.c for sample usage.
//
// This implementation only outputs function symbols and line mappings, not full
// type information, though it could be extended to do so with a bunch more
// futzing around.
//
// Only one module is supported (equivalent to one .obj file), because in my
// jit's implementation, all code is generated into a single code segment.
//
// Normally, a .pdb is referenced by another PE (exe/dll) or .dmp, and that's
// how VS locates and decides to load the PDB. Because there's no PE in the case
// of a JIT, in addition to writing a viable pdb, dbp_ready_to_execute() also
// does some goofy hacking to encourage the VS IDE to find and load the
// generated .pdb.

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

typedef struct DbpContext DbpContext;
typedef struct DbpFunctionSymbol DbpFunctionSymbol;
typedef struct DbpExceptionTables DbpExceptionTables;

// Allocates |image_size| bytes for JITing code into. |image_size| must be an
// even multiple of PAGE_SIZE (== 4096). |output_pdb_name| names the .pdb that
// will be generated, and the stub dll is based on the pdb name. The base
// address for generating code into can be retrieved by calling
// dbp_get_image_base().
DbpContext* dbp_create(size_t image_size, const char* output_pdb_name);

// Gets the base of the image, length is |image_size| as passed to dbp_create().
void* dbp_get_image_base(DbpContext* dbp);

// Create a global symbol |name| with the given |filename|. Visual Studio tends
// to work better if |filename| is an absolute path, but it's not required, and
// |filename| is used as-is. |address| should be relative to the base returned
// by dbp_get_image_base(). |length| is in bytes.
DbpFunctionSymbol* dbp_add_function_symbol(DbpContext* ctx,
                                           const char* name,
                                           const char* filename,
                                           unsigned int address,
                                           unsigned int length);

// Add a single debug line mapping to a function. |address| is the first of the
// instructions for the line of code, and should be relative to the base address
// as retrieved by dbp_get_image_base(). |line| is the one-based file line
// number in the source code.
void dbp_add_line_mapping(DbpContext* ctx,
                          DbpFunctionSymbol* fs,
                          unsigned int address,
                          unsigned int line);

// Called when all line information has been written to generate and load the
// .pdb.
//
// exception_tables can be NULL, but stack traces and exceptions will not work
// correctly (see RtlAddFunctionTable() on MSDN for more information). If
// provided, .pdata and UNWIND_INFO will be included in the synthetic DLL, and
// will be equivalent to calling RtlAddFunctionTable(). However, when the
// addresses for a dynamically provided table with RtlAddFunctionTable() overlap
// with the address space for a DLL, the static information in the DLL takes
// precedence and the dynamic information is ignored.
int dbp_ready_to_execute(DbpContext* ctx, DbpExceptionTables* exception_tables);

// Free |ctx| and all associated memory, including the stub dll and image.
void dbp_free(DbpContext* ctx);

// This is stored in CodeView records, default is "dyn_basic_pdb writer 1.0.0.0" if not set.
void dbp_set_compiler_information(DbpContext* ctx,
                                  const char* compiler_version_string,
                                  unsigned short major,
                                  unsigned short minor,
                                  unsigned short build,
                                  unsigned short qfe);

// Same as winnt.h RUNTIME_FUNCTION, we just want to avoid including windows.h
// in the interface header.
typedef struct DbpRUNTIME_FUNCTION {
  unsigned int begin_address;
  unsigned int end_address;
  unsigned int unwind_data;
} DbpRUNTIME_FUNCTION;

// pdata entries will be written to a .pdata section in the dll, with the RVA of
// .unwind_data fixed up to be relative to where unwind_info is stored.
// unwind_data==0 should correspond to &unwind_info[0].
struct DbpExceptionTables {
  DbpRUNTIME_FUNCTION* pdata;
  size_t num_pdata_entries;

  unsigned char* unwind_info;
  size_t unwind_info_byte_length;
};

#ifdef __cplusplus
}  // extern "C"
#endif

#endif  // INCLUDED_DYN_BASIC_PDB_H

#ifdef DYN_BASIC_PDB_IMPLEMENTATION

#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4201)  // non-standard extension: nameless struct/union
#pragma warning(disable : 4668)  // 'X' is not defined as a preprocessor macro, replacing with '0'
                                 // for '#if/#elif'
#pragma warning(disable : 4820)  // 'X' bytes padding added after data member 'Y'
#pragma warning(disable : 5045)  // Compiler will insert Spectre mitigation for memory load if
                                 // /Qspectre switch specified
#pragma comment(lib, "rpcrt4")

#ifdef __clang__
#pragma clang diagnostic ignored "-Wcast-align"
#pragma clang diagnostic ignored "-Wdeclaration-after-statement"
#pragma clang diagnostic ignored "-Wimplicit-fallthrough"
#pragma clang diagnostic ignored "-Wunsafe-buffer-usage"
#endif

#include <Windows.h>
#include <assert.h>
#include <malloc.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef unsigned int u32;
typedef signed int i32;
typedef unsigned short u16;
typedef unsigned long long u64;

typedef struct StreamData StreamData;
typedef struct SuperBlock SuperBlock;
typedef struct NmtAlikeHashTable NmtAlikeHashTable;

struct DbpContext {
  char* base_addr;    // This is the VirtualAlloc base
  void* image_addr;   // and this is the address returned to the user,
  size_t image_size;  // The user has this much accessible, and the allocation is 0x1000 larger.
  char* output_pdb_name;
  char* output_dll_name;
  HMODULE dll_module;

  DbpFunctionSymbol** func_syms;
  size_t func_syms_len;
  size_t func_syms_cap;

  UUID unique_id;

  NmtAlikeHashTable* names_nmt;

  char* compiler_version_string;
  u16 version_major;
  u16 version_minor;
  u16 version_build;
  u16 version_qfe;

  HANDLE file;
  char* data;
  size_t file_size;

  SuperBlock* superblock;

  StreamData** stream_data;
  size_t stream_data_len;
  size_t stream_data_cap;

  u32 next_potential_block;
  u32 padding;
};

typedef struct LineInfo {
  unsigned int address;
  unsigned int line;
} LineInfo;

struct DbpFunctionSymbol {
  char* name;
  char* filename;
  unsigned int address;             // Offset into image_addr where function is.
  unsigned int length;              // Number of bytes long.
  unsigned int module_info_offset;  // Location into modi where the full symbol info can be found.

  LineInfo* lines;
  size_t lines_len;
  size_t lines_cap;
};

#define PUSH_BACK(vec, item)                            \
  do {                                                  \
    if (!vec) {                                         \
      vec = calloc(8, sizeof(*vec));                    \
      vec##_len = 0;                                    \
      vec##_cap = 8;                                    \
    }                                                   \
                                                        \
    if (vec##_cap == vec##_len) {                       \
      vec = realloc(vec, sizeof(*vec) * vec##_cap * 2); \
      vec##_cap *= 2;                                   \
    }                                                   \
                                                        \
    vec[vec##_len++] = item;                            \
  } while (0)

DbpContext* dbp_create(size_t image_size, const char* output_pdb_name) {
  DbpContext* ctx = calloc(1, sizeof(DbpContext));
  // Allocate with an extra page for the DLL header.
  char* base_addr =
      VirtualAlloc(NULL, image_size + 0x1000, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  ctx->base_addr = base_addr;
  ctx->image_addr = base_addr + 0x1000;
  ctx->image_size = image_size;
  char full_pdb_name[MAX_PATH];
  GetFullPathName(output_pdb_name, sizeof(full_pdb_name), full_pdb_name, NULL);
  ctx->output_pdb_name = _strdup(full_pdb_name);
  size_t len = strlen(full_pdb_name);
  static char suffix[] = ".synthetic.dll";
  ctx->output_dll_name = malloc(len + sizeof(suffix));
  memcpy(ctx->output_dll_name, full_pdb_name, len);
  memcpy(ctx->output_dll_name + len, suffix, sizeof(suffix));
  if (UuidCreate(&ctx->unique_id) != RPC_S_OK) {
    fprintf(stderr, "UuidCreate failed\n");
    return NULL;
  }
  dbp_set_compiler_information(ctx, "dyn_basic_pdb writer 1.0.0.0", 1, 0, 0, 0);
  return ctx;
}

void* dbp_get_image_base(DbpContext* ctx) {
  return ctx->image_addr;
}

DbpFunctionSymbol* dbp_add_function_symbol(DbpContext* ctx,
                                           const char* name,
                                           const char* filename,
                                           unsigned int address,
                                           unsigned int length) {
  DbpFunctionSymbol* fs = calloc(1, sizeof(*fs));
  fs->name = _strdup(name);
  fs->filename = _strdup(filename);
  fs->address = address;
  fs->length = length;
  PUSH_BACK(ctx->func_syms, fs);
  return fs;
}

void dbp_set_compiler_information(DbpContext* ctx,
                                  const char* compiler_version_string,
                                  unsigned short major,
                                  unsigned short minor,
                                  unsigned short build,
                                  unsigned short qfe) {
  if (ctx->compiler_version_string)
    free(ctx->compiler_version_string);
  ctx->compiler_version_string = _strdup(compiler_version_string);
  ctx->version_major = major;
  ctx->version_minor = minor;
  ctx->version_build = build;
  ctx->version_qfe = qfe;
}

void dbp_add_line_mapping(DbpContext* ctx,
                          DbpFunctionSymbol* sym,
                          unsigned int address,
                          unsigned int line) {
  (void)ctx;
  assert(address >= sym->address);
  LineInfo line_info = {.address = address, .line = line};
  PUSH_BACK(sym->lines, line_info);
}

static const char big_hdr_magic[0x1e] = "Microsoft C/C++ MSF 7.00\r\n\x1a\x44\x53";

#define ENSURE(x, want)                                                               \
  do {                                                                                \
    if (want != x) {                                                                  \
      fprintf(stderr, "%s:%d: failed %s wasn't %s\n", __FILE__, __LINE__, #x, #want); \
      return 0;                                                                       \
    }                                                                                 \
  } while (0)

#define ENSURE_NE(x, bad)                                                         \
  do {                                                                            \
    if (bad == x) {                                                               \
      fprintf(stderr, "%s:%d: failed %s was %s\n", __FILE__, __LINE__, #x, #bad); \
      return 0;                                                                   \
    }                                                                             \
  } while (0)

struct SuperBlock {
  char file_magic[0x1e];
  char padding[2];
  u32 block_size;
  u32 free_block_map_block;
  u32 num_blocks;
  u32 num_directory_bytes;
  u32 unknown;
  u32 block_map_addr;
};

#define STUB_RDATA_SIZE 4096

#define BLOCK_SIZE 4096
#define DEFAULT_NUM_BLOCKS 256
#define PAGE_TO_WORD(pn) (pn >> 6)
#define PAGE_MASK(pn) (1ULL << (pn & ((sizeof(u64) * CHAR_BIT) - 1)))
static const char synthetic_obj_name[] = "dyn_basic_pdb-synthetic-for-jit.obj";

static void mark_block_used(DbpContext* ctx, u32 pn) {
  u64* map2 = (u64*)&ctx->data[BLOCK_SIZE * 2];
  map2[PAGE_TO_WORD(pn)] &= ~PAGE_MASK(pn);
}

static int block_is_free(DbpContext* ctx, u32 pn) {
  u64* map2 = (u64*)&ctx->data[BLOCK_SIZE * 2];
  return !!(map2[PAGE_TO_WORD(pn)] & PAGE_MASK(pn));
}

static void* get_block_ptr(DbpContext* ctx, u32 i) {
  return &ctx->data[BLOCK_SIZE * i];
}

static u32 alloc_block(DbpContext* ctx) {
  for (;;) {
    if (block_is_free(ctx, ctx->next_potential_block)) {
      mark_block_used(ctx, ctx->next_potential_block);
      return ctx->next_potential_block++;
    }
    ctx->next_potential_block++;
  }
}

struct StreamData {
  u32 stream_index;

  u32 data_length;

  char* cur_write;

  u32* blocks;
  size_t blocks_len;
  size_t blocks_cap;
};

static void stream_write_block(DbpContext* ctx, StreamData* stream, const void* data, size_t len) {
  if (!stream->cur_write) {
    u32 block_id = alloc_block(ctx);
    PUSH_BACK(stream->blocks, block_id);
    stream->cur_write = get_block_ptr(ctx, block_id);
  }

  u32 cur_block_filled = stream->data_length % BLOCK_SIZE;
  u32 max_remaining_this_block = BLOCK_SIZE - cur_block_filled;
  if (max_remaining_this_block >= len) {
    memcpy(stream->cur_write, data, len);
    stream->cur_write += len;
    stream->data_length += (u32)len;
    if (max_remaining_this_block == len) {
      stream->cur_write = NULL;
    }
  } else {
    memcpy(stream->cur_write, data, max_remaining_this_block);
    stream->cur_write += max_remaining_this_block;
    stream->data_length += max_remaining_this_block;
    stream->cur_write = NULL;
    stream_write_block(ctx, stream, (const char*)data + max_remaining_this_block,
                       len - max_remaining_this_block);
  }
}

static void stream_ensure_init(DbpContext* ctx, StreamData* stream) {
  // Hack for fixup capture if the block pointer hasn't been allocated yet
  // before the macro wants to capture it.
  unsigned char none;
  stream_write_block(ctx, stream, &none, 0);
}

#define SW_BLOCK(x, len) stream_write_block(ctx, stream, x, len)
#define SW_U32(x)                                   \
  do {                                              \
    u32 _ = (x);                                    \
    stream_write_block(ctx, stream, &_, sizeof(_)); \
  } while (0)
#define SW_I32(x)                                   \
  do {                                              \
    i32 _ = (x);                                    \
    stream_write_block(ctx, stream, &_, sizeof(_)); \
  } while (0)
#define SW_U16(x)                                   \
  do {                                              \
    u16 _ = (x);                                    \
    stream_write_block(ctx, stream, &_, sizeof(_)); \
  } while (0)
#define SW_ALIGN(to)                        \
  do {                                      \
    while (stream->data_length % to != 0) { \
      SW_BLOCK("", 1);                      \
    }                                       \
  } while (0)

typedef char* SwFixup;
#define SW_CAPTURE_FIXUP(strukt, field) \
  (stream_ensure_init(ctx, stream), stream->cur_write + offsetof(strukt, field))

#define SW_WRITE_FIXUP_FOR_LOCATION_U32(swfixup) \
  do {                                           \
    *(u32*)swfixup = stream->data_length;        \
  } while (0)

typedef u32 SwDelta;
#define SW_CAPTURE_DELTA_START() (stream->data_length)

#define SW_WRITE_DELTA_FIXUP(swfixup, delta)      \
  do {                                            \
    *(u32*)swfixup = stream->data_length - delta; \
  } while (0)

static void write_superblock(DbpContext* ctx) {
  SuperBlock* sb = (SuperBlock*)ctx->data;
  ctx->superblock = sb;
  memcpy(sb->file_magic, big_hdr_magic, sizeof(big_hdr_magic));
  sb->padding[0] = '\0';
  sb->padding[1] = '\0';
  sb->block_size = BLOCK_SIZE;
  sb->free_block_map_block = 2;  // We never use map 1.
  sb->num_blocks = DEFAULT_NUM_BLOCKS;
  // num_directory_bytes filled in later once we've written everything else.
  sb->unknown = 0;
  sb->block_map_addr = 3;

  // Mark all pages as free, then mark the first four in use:
  // 0 is super block, 1 is FPM1, 2 is FPM2, 3 is the block map.
  memset(&ctx->data[BLOCK_SIZE], 0xff, BLOCK_SIZE * 2);
  for (u32 i = 0; i <= 3; ++i)
    mark_block_used(ctx, i);
}

static StreamData* add_stream(DbpContext* ctx) {
  StreamData* stream = calloc(1, sizeof(StreamData));
  stream->stream_index = (u32)ctx->stream_data_len;
  PUSH_BACK(ctx->stream_data, stream);
  return stream;
}

static u32 align_to(u32 val, u32 align) {
  return (val + align - 1) / align * align;
}

typedef struct PdbStreamHeader {
  u32 Version;
  u32 Signature;
  u32 Age;
  UUID UniqueId;
} PdbStreamHeader;

static int write_pdb_info_stream(DbpContext* ctx, StreamData* stream, u32 names_stream) {
  PdbStreamHeader psh = {
      .Version = 20000404, /* VC70 */
      .Signature = (u32)time(NULL),
      .Age = 1,
  };
  memcpy(&psh.UniqueId, &ctx->unique_id, sizeof(UUID));
  SW_BLOCK(&psh, sizeof(psh));

  // Named Stream Map.

  // The LLVM docs are something that would be nice to refer to here:
  //
  //   https://llvm.org/docs/PDB/HashTable.html
  //
  // But unfortunately, this specific page is quite misleading (unlike the rest
  // of the PDB docs which are quite helpful). The microsoft-pdb repo is,
  // uh, "dense", but has the benefit of being correct by definition:
  //
  // https://github.com/microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/nmtni.h#L77-L95
  // https://github.com/microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/map.h#L474-L508
  //
  // Someone naturally already figured this out, as LLVM writes the correct
  // data, just the docs are wrong. (LLVM's patch for docs setup seems a bit
  // convoluted which is why I'm whining in a buried comment instead of just
  // fixing it...)

  // Starts with the string buffer (which we pad to % 4, even though that's not
  // actually required). We don't bother with actually building and updating a
  // map as the only named stream we need is /names (TBD: possibly /LinkInfo?).
  static const char string_data[] = "/names\0";
  SW_U32(sizeof(string_data));
  SW_BLOCK(string_data, sizeof(string_data));

  // Then hash size, and capacity.
  SW_U32(1);  // Size
  SW_U32(1);  // Capacity
  // Then two bit vectors, first for "present":
  SW_U32(0x01);  // Present length (1 word follows)
  SW_U32(0x01);  // 0b0000`0001    (only bucket occupied)
  // Then for "deleted" (we don't write any).
  SW_U32(0);
  // Now, the maps: mapping "/names" at offset 0 above to given names stream.
  SW_U32(0);
  SW_U32(names_stream);
  // This is "niMac", which is the last index allocated. We don't need it.
  SW_U32(0);

  // Finally, feature codes, which indicate that we're somewhat modern.
  SW_U32(20140508); /* VC140 */

  return 1;
}

typedef struct TpiStreamHeader {
  u32 version;
  u32 header_size;
  u32 type_index_begin;
  u32 type_index_end;
  u32 type_record_bytes;

  u16 hash_stream_index;
  u16 hash_aux_stream_index;
  u32 hash_key_size;
  u32 num_hash_buckets;

  i32 hash_value_buffer_offset;
  u32 hash_value_buffer_length;

  i32 index_offset_buffer_offset;
  u32 index_offset_buffer_length;

  i32 hash_adj_buffer_offset;
  u32 hash_adj_buffer_length;
} TpiStreamHeader;

static int write_empty_tpi_ipi_stream(DbpContext* ctx, StreamData* stream) {
  // This is an "empty" TPI/IPI stream, we do not emit any user-defined types
  // currently.
  TpiStreamHeader tsh = {
      .version = 20040203, /* V80 */
      .header_size = sizeof(TpiStreamHeader),
      .type_index_begin = 0x1000,
      .type_index_end = 0x1000,
      .type_record_bytes = 0,
      .hash_stream_index = 0xffff,
      .hash_aux_stream_index = 0xffff,
      .hash_key_size = 4,
      .num_hash_buckets = 0x3ffff,
      .hash_value_buffer_offset = 0,
      .hash_value_buffer_length = 0,
      .index_offset_buffer_offset = 0,
      .index_offset_buffer_length = 0,
      .hash_adj_buffer_offset = 0,
      .hash_adj_buffer_length = 0,
  };
  SW_BLOCK(&tsh, sizeof(tsh));
  return 1;
}

// Copied from:
// https://github.com/microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/misc.h#L15
// with minor type adaptations. It needs to match that implementation to make
// serialized hashes match up.
static u32 calc_hash(char* pb, size_t cb) {
  u32 ulHash = 0;

  // hash leading dwords using Duff's Device
  size_t cl = cb >> 2;
  u32* pul = (u32*)pb;
  u32* pulMac = pul + cl;
  size_t dcul = cl & 7;

  switch (dcul) {
    do {
      dcul = 8;
      ulHash ^= pul[7];
      case 7:
        ulHash ^= pul[6];
      case 6:
        ulHash ^= pul[5];
      case 5:
        ulHash ^= pul[4];
      case 4:
        ulHash ^= pul[3];
      case 3:
        ulHash ^= pul[2];
      case 2:
        ulHash ^= pul[1];
      case 1:
        ulHash ^= pul[0];
      case 0:;
    } while ((pul += dcul) < pulMac);
  }

  pb = (char*)(pul);

  // hash possible odd word
  if (cb & 2) {
    ulHash ^= *(unsigned short*)pb;
    pb = (char*)((unsigned short*)(pb) + 1);
  }

  // hash possible odd byte
  if (cb & 1) {
    ulHash ^= (u32) * (pb++);
  }

  const u32 toLowerMask = 0x20202020;
  ulHash |= toLowerMask;
  ulHash ^= (ulHash >> 11);

  return (ulHash ^ (ulHash >> 16));
}

// A hash table that emulates the microsoft-pdb nmt.h as required by the /names
// stream.
typedef struct NmtAlikeHashTable {
  char* strings;  // This is a "\0bunch\0of\0strings\0" always starting with \0,
                  // so that 0 is an invalid index.
  size_t strings_len;
  size_t strings_cap;

  u32* hash;  // hash[hashed_value % hash_len] = name_index, which is an index
              // into strings to get the actual value.
  size_t hash_len;

  u32 num_names;
} NmtAlikeHashTable;

#define NMT_INVALID (0u)

static NmtAlikeHashTable* nmtalike_create(void) {
  NmtAlikeHashTable* ret = calloc(1, sizeof(NmtAlikeHashTable));
  PUSH_BACK(ret->strings, '\0');
  ret->hash = calloc(1, sizeof(u32));
  ret->hash_len = 1;
  return ret;
}

static char* nmtalike__string_for_name_index(NmtAlikeHashTable* nmt, u32 name_index) {
  if (name_index >= nmt->strings_len)
    return NULL;
  return &nmt->strings[name_index];
}

// If |str| already exists, return 1 with *out_name_index set to its name_index.
// Else, return 0 and *out_slot is where a new name_index should be stored for
// |str|.
static void nmtalike__find(NmtAlikeHashTable* nmt, char* str, u32* out_name_index, u32* out_slot) {
  assert(nmt->strings_len > 0);
  assert(nmt->hash_len > 0);

  size_t len = strlen(str);
  u32 slot = calc_hash(str, len) % nmt->hash_len;
  u32 name_index = NMT_INVALID;
  for (;;) {
    name_index = nmt->hash[slot];
    if (name_index == NMT_INVALID)
      break;

    if (strcmp(str, nmtalike__string_for_name_index(nmt, name_index)) == 0)
      break;

    ++slot;
    if (slot >= nmt->hash_len)
      slot = 0;
  }

  *out_slot = slot;
  *out_name_index = name_index;
}

// Returns new name index.
static u32 nmtalike__append_to_string_buffer(NmtAlikeHashTable* nmt, char* str) {
  size_t len = strlen(str) + 1;

  while (nmt->strings_cap < nmt->strings_len + len) {
    nmt->strings = realloc(nmt->strings, sizeof(*nmt->strings) * nmt->strings_cap * 2);
    nmt->strings_cap *= 2;
  }

  char* start = &nmt->strings[nmt->strings_len];
  memcpy(start, str, len);
  nmt->strings_len += len;
  return (u32)(start - nmt->strings);
}

static void nmtalike__rehash(NmtAlikeHashTable* nmt, u32 new_count) {
  size_t new_hash_byte_len = sizeof(u32) * new_count;
  u32* new_hash = malloc(new_hash_byte_len);
  size_t new_hash_len = new_count;

  memset(new_hash, 0, new_hash_byte_len);

  for (u32 i = 0; i < nmt->hash_len; ++i) {
    u32 name_index = nmt->hash[i];
    if (name_index != NMT_INVALID) {
      char* str = nmtalike__string_for_name_index(nmt, name_index);
      u32 j = calc_hash(str, strlen(str)) % new_count;
      for (;;) {
        if (new_hash[j] == NMT_INVALID)
          break;
        ++j;
        if (j == new_count)
          j = 0;
      }
      new_hash[j] = name_index;
    }
  }

  free(nmt->hash);
  nmt->hash = new_hash;
  nmt->hash_len = new_hash_len;
}

static void nmtalike__grow(NmtAlikeHashTable* nmt) {
  ++nmt->num_names;

  // These growth factors have to match so that the buckets line up as expected
  // when serialized.
  if (nmt->hash_len * 3 / 4 < nmt->num_names) {
    nmtalike__rehash(nmt, (u32)(nmt->hash_len * 3 / 2 + 1));
  }
}

static u32 nmtalike_add_string(NmtAlikeHashTable* nmt, char* str) {
  u32 name_index = NMT_INVALID;
  u32 insert_location;
  nmtalike__find(nmt, str, &name_index, &insert_location);
  if (name_index != NMT_INVALID)
    return name_index;

  name_index = nmtalike__append_to_string_buffer(nmt, str);
  nmt->hash[insert_location] = name_index;
  nmtalike__grow(nmt);
  return name_index;
}

static u32 nmtalike_name_index_for_string(NmtAlikeHashTable* nmt, char* str) {
  u32 name_index = NMT_INVALID;
  u32 slot_unused;
  nmtalike__find(nmt, str, &name_index, &slot_unused);
  return name_index;  // either NMT_INVALID or the slot
}

typedef struct NmtAlikeEnum {
  NmtAlikeHashTable* nmt;
  u32 i;
} NmtAlikeEnum;

static NmtAlikeEnum nmtalike_enum_begin(NmtAlikeHashTable* nmt) {
  return (NmtAlikeEnum){.nmt = nmt, .i = (u32)-1};
}

static int nmtalike_enum_next(NmtAlikeEnum* it) {
  while (++it->i < it->nmt->hash_len) {
    if (it->nmt->hash[it->i] != NMT_INVALID)
      return 1;
  }
  return 0;
}

static void nmtalike_enum_get(NmtAlikeEnum* it, u32* name_index, char** str) {
  *name_index = it->nmt->hash[it->i];
  *str = nmtalike__string_for_name_index(it->nmt, *name_index);
}

static int write_names_stream(DbpContext* ctx, StreamData* stream) {
  NmtAlikeHashTable* nmt = ctx->names_nmt = nmtalike_create();

  for (size_t i = 0; i < ctx->func_syms_len; ++i) {
    nmtalike_add_string(nmt, ctx->func_syms[i]->filename);
  }

  // "/names" is:
  //
  // header
  // string bufer
  // hash table
  // number of names in the table
  //
  // Most of the 'magic' is in NmtAlikeHashTable, specifically in how it's
  // grown.

  SW_U32(0xeffeeffe);               // Header
  SW_U32(1);                        // verLongHash
  SW_U32((u32)(nmt->strings_len));  // Size of string buffer
  SW_BLOCK(nmt->strings, nmt->strings_len);

  SW_U32((u32)(nmt->hash_len));                      // Number of buckets
  SW_BLOCK(nmt->hash, nmt->hash_len * sizeof(u32));  // Hash buckets

  SW_U32(nmt->num_names);  // Number of names in the hash

  return 1;
}

typedef struct DbiStreamHeader {
  i32 version_signature;
  u32 version_header;
  u32 age;
  u16 global_stream_index;
  u16 build_number;
  u16 public_stream_index;
  u16 pdb_dll_version;
  u16 sym_record_stream;
  u16 pdb_dll_rbld;
  i32 mod_info_size;
  i32 section_contribution_size;
  i32 section_map_size;
  i32 source_info_size;
  i32 type_server_map_size;
  u32 mfc_type_server_index;
  i32 optional_dbg_header_size;
  i32 ec_substream_size;
  u16 flags;
  u16 machine;
  u32 padding;
} DbiStreamHeader;

// Part of ModInfo
typedef struct SectionContribEntry {
  u16 section;
  char padding1[2];
  i32 offset;
  i32 size;
  u32 characteristics;
  u16 module_index;
  char padding2[2];
  u32 data_crc;
  u32 reloc_crc;
} SectionContribEntry;

typedef struct ModInfo {
  u32 unused1;
  SectionContribEntry section_contr;
  u16 flags;
  u16 module_sym_stream;
  u32 sym_byte_size;
  u32 c11_byte_size;
  u32 c13_byte_size;
  u16 source_file_count;
  char padding[2];
  u32 unused2;
  u32 source_file_name_index;
  u32 pdb_file_path_name_index;
  // char module_name[];
  // char obj_file_name[];
} ModInfo;

typedef struct SectionMapHeader {
  u16 count;      // Number of segment descriptors
  u16 log_count;  // Number of logical segment descriptors
} SectionMapHeader;

typedef struct SectionMapEntry {
  u16 flags;  // See the SectionMapEntryFlags enum below.
  u16 ovl;    // Logical overlay number
  u16 group;  // Group index into descriptor array.
  u16 frame;
  u16 section_name;  // Byte index of segment / group name in string table, or 0xFFFF.
  u16 class_name;    // Byte index of class in string table, or 0xFFFF.
  u32 offset;        // Byte offset of the logical segment within physical segment. If group is set
                     // in flags, this is the offset of the group.
  u32 section_length;  // Byte count of the segment or group.
} SectionMapEntry;

enum SectionMapEntryFlags {
  SMEF_Read = 1 << 0,               // Segment is readable.
  SMEF_Write = 1 << 1,              // Segment is writable.
  SMEF_Execute = 1 << 2,            // Segment is executable.
  SMEF_AddressIs32Bit = 1 << 3,     // Descriptor describes a 32-bit linear address.
  SMEF_IsSelector = 1 << 8,         // Frame represents a selector.
  SMEF_IsAbsoluteAddress = 1 << 9,  // Frame represents an absolute address.
  SMEF_IsGroup = 1 << 10            // If set, descriptor represents a group.
};

typedef struct FileInfoSubstreamHeader {
  u16 num_modules;
  u16 num_source_files;

  // u16 mod_indices[num_modules];
  // u16 mod_file_counts[num_modules];
  // u32 file_name_offsets[num_source_files];
  // char names_buffer[][num_source_files];
} FileInfoSubstreamHeader;

typedef struct GsiData {
  u32 global_symbol_stream;
  u32 public_symbol_stream;
  u32 sym_record_stream;
} GsiData;

typedef struct DbiWriteData {
  GsiData gsi_data;
  u32 section_header_stream;
  u32 module_sym_stream;
  u32 module_symbols_byte_size;
  u32 module_c13_byte_size;
  u32 num_source_files;

  SwFixup fixup_mod_info_size;
  SwFixup fixup_section_contribution_size;
  SwFixup fixup_section_map_size;
  SwFixup fixup_source_info_size;
  SwFixup fixup_optional_dbg_header_size;
  SwFixup fixup_ec_substream_size;
} DbiWriteData;

static void write_dbi_stream_header(DbpContext* ctx, StreamData* stream, DbiWriteData* dwd) {
  DbiStreamHeader dsh = {
      .version_signature = -1,
      .version_header = 19990903, /* V70 */
      .age = 1,
      .global_stream_index = (u16)dwd->gsi_data.global_symbol_stream,
      .build_number = 0x8eb, /* 14.11 "new format" */
      .public_stream_index = (u16)dwd->gsi_data.public_symbol_stream,
      .pdb_dll_version = 0,
      .sym_record_stream = (u16)dwd->gsi_data.sym_record_stream,
      .pdb_dll_rbld = 0,
      .type_server_map_size = 0,
      .mfc_type_server_index = 0,
      .flags = 0,
      .machine = 0x8664, /* x64 */
      .padding = 0,
  };

  dwd->fixup_mod_info_size = SW_CAPTURE_FIXUP(DbiStreamHeader, mod_info_size);
  dwd->fixup_section_contribution_size =
      SW_CAPTURE_FIXUP(DbiStreamHeader, section_contribution_size);
  dwd->fixup_section_map_size = SW_CAPTURE_FIXUP(DbiStreamHeader, section_map_size);
  dwd->fixup_source_info_size = SW_CAPTURE_FIXUP(DbiStreamHeader, source_info_size);
  dwd->fixup_optional_dbg_header_size = SW_CAPTURE_FIXUP(DbiStreamHeader, optional_dbg_header_size);
  dwd->fixup_ec_substream_size = SW_CAPTURE_FIXUP(DbiStreamHeader, ec_substream_size);
  SW_BLOCK(&dsh, sizeof(dsh));
}

static void write_dbi_stream_modinfo(DbpContext* ctx, StreamData* stream, DbiWriteData* dwd) {
  SwDelta block_start = SW_CAPTURE_DELTA_START();

  // Module Info Substream. We output a single module with a single section for
  // the whole jit blob.
  ModInfo mod = {
      .unused1 = 0,
      .section_contr =
          {
              .section = 1,
              .padding1 = {0, 0},
              .offset = 0,
              .size = (i32)ctx->image_size,
              .characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_ALIGN_16BYTES |
                                 IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ,
              .module_index = 0,
              .padding2 = {0, 0},
              .data_crc = 0,
              .reloc_crc = 0,
          },
      .flags = 0,
      .module_sym_stream = (u16)dwd->module_sym_stream,
      .sym_byte_size = dwd->module_symbols_byte_size,
      .c11_byte_size = 0,
      .c13_byte_size = dwd->module_c13_byte_size,
      .source_file_count = (u16)dwd->num_source_files,
      .padding = {0, 0},
      .unused2 = 0,
      .source_file_name_index = 0,
      .pdb_file_path_name_index = 0,
  };
  SW_BLOCK(&mod, sizeof(mod));
  // Intentionally twice for two index fields.
  SW_BLOCK(synthetic_obj_name, sizeof(synthetic_obj_name));
  SW_BLOCK(synthetic_obj_name, sizeof(synthetic_obj_name));
  SW_ALIGN(4);
  SW_WRITE_DELTA_FIXUP(dwd->fixup_mod_info_size, block_start);
}

static void write_dbi_stream_section_contribution(DbpContext* ctx,
                                                  StreamData* stream,
                                                  DbiWriteData* dwd) {
  SwDelta block_start = SW_CAPTURE_DELTA_START();

  // We only write a single section, one big for .text.
  SW_U32(0xf12eba2d);  // Ver60
  SectionContribEntry text_section = {
      .section = 1,
      .padding1 = {0, 0},
      .offset = 0,
      .size = (i32)ctx->image_size,
      .characteristics =
          IMAGE_SCN_CNT_CODE | IMAGE_SCN_ALIGN_16BYTES | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ,
      .module_index = 0,
      .padding2 = {0, 0},
      .data_crc = 0,
      .reloc_crc = 0,
  };
  SW_BLOCK(&text_section, sizeof(text_section));
  SW_WRITE_DELTA_FIXUP(dwd->fixup_section_contribution_size, block_start);
}

static void write_dbi_stream_section_map(DbpContext* ctx, StreamData* stream, DbiWriteData* dwd) {
  // This pretends to look sensible, but it doesn't make a lot of sense to me at
  // the moment. A single SectionMapEntry for just .text that maps causes all
  // functions to not have an RVA, and so line numbers don't get found. (Don't)
  // ask me how many hours of trial-and-error that took to figure out!
  //
  // Making a second section seems to make it work. We make it .rdata and one
  // page large, which matches the fake dll that we write later.

  SwDelta block_start = SW_CAPTURE_DELTA_START();

  SectionMapHeader header = {.count = 2, .log_count = 2};
  SW_BLOCK(&header, sizeof(header));

  SectionMapEntry text = {
      .flags = SMEF_Read | SMEF_Execute | SMEF_AddressIs32Bit | SMEF_IsSelector,
      .ovl = 0,               // ?
      .group = 0,             // ?
      .frame = 1,             // 1-based section number
      .section_name = 0xfff,  // ?
      .class_name = 0xffff,   // ?
      .offset = 0,            // This seems to be added to the RVA, but defaults to 0x1000.
      .section_length = (u32)ctx->image_size,
  };
  SW_BLOCK(&text, sizeof(text));

  SectionMapEntry rdata = {
      .flags = SMEF_Read | SMEF_AddressIs32Bit,
      .ovl = 0,               // ?
      .group = 0,             // ?
      .frame = 2,             // 1-based section number
      .section_name = 0xfff,  // ?
      .class_name = 0xffff,   // ?
      .offset = 0,            // This seems to be added to the RVA.
      .section_length = STUB_RDATA_SIZE,
  };
  SW_BLOCK(&rdata, sizeof(rdata));

  SW_WRITE_DELTA_FIXUP(dwd->fixup_section_map_size, block_start);
}

static void write_dbi_stream_file_info(DbpContext* ctx, StreamData* stream, DbiWriteData* dwd) {
  SwDelta block_start = SW_CAPTURE_DELTA_START();

  // File Info Substream
  FileInfoSubstreamHeader fish = {
      .num_modules = 1,       // This is always 1 for us.
      .num_source_files = 0,  // This is unused.
  };
  SW_BLOCK(&fish, sizeof(fish));

  SW_U16(0);                               // [mod_indices], unused.
  SW_U16((u16)ctx->names_nmt->num_names);  // [num_source_files]

  // First, write array of offset to files.
  NmtAlikeEnum it = nmtalike_enum_begin(ctx->names_nmt);
  while (nmtalike_enum_next(&it)) {
    u32 name_index;
    char* str;
    nmtalike_enum_get(&it, &name_index, &str);
    SW_U32(name_index);
  }

  // Then the strings buffer.
  SW_BLOCK(ctx->names_nmt->strings, ctx->names_nmt->strings_len);

  SW_ALIGN(4);
  SW_WRITE_DELTA_FIXUP(dwd->fixup_source_info_size, block_start);
}

static void write_dbi_stream_ec_substream(DbpContext* ctx, StreamData* stream, DbiWriteData* dwd) {
  SwDelta block_start = SW_CAPTURE_DELTA_START();

  // llvm-pdbutil tries to load a pdb name from the ECSubstream. Emit a single
  // nul byte, as we only refer to index 0. (This is an NMT if it needs to be
  // fully written with more data.)
  static unsigned char empty_nmt[] = {
      0xfe, 0xef, 0xfe, 0xef,  // Header
      0x01, 0x00, 0x00, 0x00,  // verLongHash
      0x01, 0x00, 0x00, 0x00,  // Size
      0x00,                    // Single \0 string.
      0x01, 0x00, 0x00, 0x00,  // One element in array
      0x00, 0x00, 0x00, 0x00,  // Entry 0 which is ""
      0x00, 0x00, 0x00, 0x00,  // Number of names in hash table
                               // Doesn't include initial nul which is always in the table.
  };
  SW_BLOCK(&empty_nmt, sizeof(empty_nmt));

  // I don't think this one's supposed to be aligned /shruggie.

  SW_WRITE_DELTA_FIXUP(dwd->fixup_ec_substream_size, block_start);
}

static void write_dbi_stream_optional_dbg_header(DbpContext* ctx,
                                                 StreamData* stream,
                                                 DbiWriteData* dwd) {
  SwDelta block_start = SW_CAPTURE_DELTA_START();

  // Index 5 points to the section header stream, which is theoretically
  // optional, but llvm-pdbutil doesn't like it if it's not there, so I'm
  // guessing that various microsoft things don't either. The stream it points
  // at is empty, but that seems to be sufficient.
  for (int i = 0; i < 5; ++i)
    SW_U16(0xffff);
  SW_U16((u16)dwd->section_header_stream);
  for (int i = 0; i < 5; ++i)
    SW_U16(0xffff);

  SW_WRITE_DELTA_FIXUP(dwd->fixup_optional_dbg_header_size, block_start);
}

static int write_dbi_stream(DbpContext* ctx, StreamData* stream, DbiWriteData* dwd) {
  write_dbi_stream_header(ctx, stream, dwd);
  write_dbi_stream_modinfo(ctx, stream, dwd);
  write_dbi_stream_section_contribution(ctx, stream, dwd);
  write_dbi_stream_section_map(ctx, stream, dwd);
  write_dbi_stream_file_info(ctx, stream, dwd);
  // No type server map.
  // No MFC type server map.
  write_dbi_stream_ec_substream(ctx, stream, dwd);
  write_dbi_stream_optional_dbg_header(ctx, stream, dwd);

  return 1;
}

#define IPHR_HASH 4096

typedef struct HashSym {
  char* name;
  u32 offset;
  u32 hash_bucket;  // Must be % IPHR_HASH
} HashSym;

typedef struct HRFile {
  u32 off;
  u32 cref;
} HRFile;

typedef struct GsiHashBuilder {
  HashSym* sym;
  size_t sym_len;
  size_t sym_cap;

  HRFile* hash_records;
  size_t hash_records_len;

  u32* hash_buckets;
  size_t hash_buckets_len;
  size_t hash_buckets_cap;

  u32 hash_bitmap[(IPHR_HASH + 32) / 32];
} GsiHashBuilder;

typedef struct GsiBuilder {
  StreamData* public_hash_stream;
  StreamData* global_hash_stream;
  StreamData* sym_record_stream;

  GsiHashBuilder publics;
  GsiHashBuilder globals;
} GsiBuilder;

// The CodeView structs are all smooshed.
#pragma pack(push, 1)

#define CV_SYM_HEADER \
  u16 record_len;     \
  u16 record_type

typedef enum CV_S_PUB32_FLAGS {
  CVSPF_None = 0x00,
  CVSPF_Code = 0x01,
  CVSPF_Function = 0x02,
  CVSPF_Managed = 0x04,
  CVSPF_MSIL = 0x08,
} CV_S_PUB32_FLAGS;

typedef struct CV_S_PUB32 {
  CV_SYM_HEADER;
  u32 flags;
  u32 offset_into_codeseg;
  u16 segment;
  // unsigned char name[];
} CV_S_PUB32;

typedef struct CV_S_PROCREF {
  CV_SYM_HEADER;
  u32 sum_name;
  u32 offset_into_module_data;
  u16 segment;
  // unsigned char name[];
} CV_S_PROCREF;

typedef struct CV_S_OBJNAME {
  CV_SYM_HEADER;
  u32 signature;
  // unsigned char name[];
} CV_S_OBJNAME;

typedef struct CV_S_COMPILE3 {
  CV_SYM_HEADER;

  struct {
    u32 language : 8;         // language index
    u32 ec : 1;               // compiled for E/C
    u32 no_dbg_info : 1;      // not compiled with debug info
    u32 ltcg : 1;             // compiled with LTCG
    u32 no_data_align : 1;    // compiled with -Bzalign
    u32 managed_present : 1;  // managed code/data present
    u32 security_checks : 1;  // compiled with /GS
    u32 hot_patch : 1;        // compiled with /hotpatch
    u32 cvtcil : 1;           // converted with CVTCIL
    u32 msil_module : 1;      // MSIL netmodule
    u32 sdl : 1;              // compiled with /sdl
    u32 pgo : 1;              // compiled with /ltcg:pgo or pgu
    u32 exp : 1;              // .exp module
    u32 pad : 12;             // reserved, must be 0
  } flags;
  u16 machine;
  u16 ver_fe_major;
  u16 ver_fe_minor;
  u16 ver_fe_build;
  u16 ver_fe_qfe;
  u16 ver_be_major;
  u16 ver_be_minor;
  u16 ver_be_build;
  u16 ver_be_qfe;

  // unsigned char version[];
} CV_S_COMPILE3;

typedef struct CV_S_PROCFLAGS {
  union {
    unsigned char all;
    struct {
      unsigned char CV_PFLAG_NOFPO : 1;       // frame pointer present
      unsigned char CV_PFLAG_INT : 1;         // interrupt return
      unsigned char CV_PFLAG_FAR : 1;         // far return
      unsigned char CV_PFLAG_NEVER : 1;       // function does not return
      unsigned char CV_PFLAG_NOTREACHED : 1;  // label isn't fallen into
      unsigned char CV_PFLAG_CUST_CALL : 1;   // custom calling convention
      unsigned char CV_PFLAG_NOINLINE : 1;    // function marked as noinline
      unsigned char CV_PFLAG_OPTDBGINFO : 1;  // function has debug information for optimized code
    };
  };
} CV_S_PROCFLAGS;

typedef struct CV_S_GPROC32 {
  CV_SYM_HEADER;

  u32 parent;
  u32 end;
  u32 next;
  u32 len;
  u32 dbg_start;
  u32 dbg_end;
  u32 type_index;
  u32 offset;
  u16 seg;
  CV_S_PROCFLAGS flags;  // Proc flags

  // unsigned char name[];
} CV_S_GPROC32;

typedef enum CV_LineFlags {
  CF_LF_None = 0,
  CF_LF_HaveColumns = 1,
} CV_LineFlags;

typedef struct CV_LineFragmentHeader {
  u32 reloc_offset;
  u16 reloc_segment;
  u16 flags;  // CV_LineFlags
  u32 code_size;
} CV_LineFragmentHeader;

typedef struct CV_LineBlockFragmentHeader {
  u32 checksum_block_offset;  // Offset of file_checksum entry in file checksums buffer. The
                              // checksum entry then contains another offset into the string table
                              // of the actual name.
  u32 num_lines;
  u32 block_size;
  // CV_LineNumberEntry lines[num_lines];
  // Columns array goes here too, but we don't currently support that.
} CV_LineBlockFragmentHeader;

typedef struct CV_LineNumberEntry {
  u32 offset;               // Offset to start of code bytes for line number.
  u32 line_num_start : 24;  // Line where statement/expression starts.
  u32 delta_line_end : 7;   // Delta to line where statement ends (optional).
  u32 is_statement : 1;     // true if statement, false if expression.
} CV_LineNumberEntry;

typedef struct CV_S_NODATA {
  CV_SYM_HEADER;
} CV_S_NODATA;

typedef enum CV_FileChecksumKind {
  CV_FCSK_None,
  CV_FCSK_MD5,
  CV_FCSK_SHA1,
  CV_FCSK_SHA256,
} CV_FileChecksumKind;

typedef struct CV_FileChecksumEntryHeader {
  u32 filename_offset;
  unsigned char checksum_size;
  unsigned char checksum_kind;
} CV_FileChecksumEntryHeader;

typedef enum CV_DebugSubsectionKind {
  CV_DSF_Symbols = 0xf1,
  CV_DSF_Lines = 0xf2,
  CV_DSF_StringTable = 0xf3,
  CV_DSF_FileChecksums = 0xf4,
  // There are also others that we don't need.
} CV_DebugSubsectionKind;

typedef struct CV_DebugSubsectionHeader {
  u32 kind;    // CV_DebugSubsectionKind enum
  u32 length;  // includes data after, but not this struct.
} CV_DebugSubsectionHeader;

#define SW_CV_SYM_TRAILING_NAME(sym, name)                                                    \
  do {                                                                                        \
    u16 name_len = (u16)strlen(name) + 1; /* trailing \0 seems required in (most?) records */ \
    u16 record_len = (u16)align_to((u32)name_len + sizeof(sym), 4) -                          \
                     sizeof(u16) /* length field not included in length count */;             \
    sym.record_len = record_len;                                                              \
    assert(sym.record_type);                                                                  \
    SW_BLOCK(&sym, sizeof(sym));                                                              \
    SW_BLOCK(name, name_len);                                                                 \
    SW_ALIGN(4);                                                                              \
  } while (0)

#define SW_CV_SYM(sym)                                            \
  do {                                                            \
    u16 record_len = (u16)align_to(sizeof(sym), 4) - sizeof(u16); \
    sym.record_len = record_len;                                  \
    assert(sym.record_type);                                      \
    SW_BLOCK(&sym, sizeof(sym));                                  \
    /* No need to align because we should already be. */          \
  } while (0)

#pragma pack(pop)

static void gsi_builder_add_public(DbpContext* ctx,
                                   GsiBuilder* builder,
                                   CV_S_PUB32_FLAGS flags,
                                   u32 offset_into_codeseg,
                                   char* name) {
  StreamData* stream = builder->sym_record_stream;

  HashSym sym = {_strdup(name), stream->data_length, calc_hash(name, strlen(name)) % IPHR_HASH};
  PUSH_BACK(builder->publics.sym, sym);

  CV_S_PUB32 pub = {
      .record_type = 0x110e,
      .flags = (u32)flags,
      .offset_into_codeseg = offset_into_codeseg,
      .segment = 1  // segment is always 1 for us
  };
  SW_CV_SYM_TRAILING_NAME(pub, name);
}

static void gsi_builder_add_procref(DbpContext* ctx,
                                    GsiBuilder* builder,
                                    u32 offset_into_module_data,
                                    char* name) {
  StreamData* stream = builder->sym_record_stream;

  HashSym sym = {_strdup(name), stream->data_length, calc_hash(name, strlen(name)) % IPHR_HASH};
  PUSH_BACK(builder->globals.sym, sym);

  CV_S_PROCREF procref = {
      .record_type = 0x1125,
      .sum_name = 0,
      .offset_into_module_data = offset_into_module_data,
      .segment = 1,  // segment is always 1 for us
  };
  SW_CV_SYM_TRAILING_NAME(procref, name);
}

static int is_ascii_string(char* s) {
  for (unsigned char* p = (unsigned char*)s; *p; ++p) {
    if (*p >= 0x80)
      return 0;
  }
  return 1;
}

static int gsi_record_cmp(char* s1, char* s2) {
  // Not-at-all-Accidentally Quadratic, but rather Wantonly. :/
  size_t ls = strlen(s1);
  size_t rs = strlen(s2);
  if (ls != rs) {
    return (ls > rs) - (ls < rs);
  }

  // Non-ascii: memcmp.
  if (!is_ascii_string(s1) || !is_ascii_string(s2)) {
    return memcmp(s1, s2, ls);
  }

  // Otherwise case-insensitive (so random!).
  return _memicmp(s1, s2, ls);
}

// TODO: use a better sort impl
static HashSym* g_cur_hash_bucket_sort_syms = NULL;

// See caseInsensitiveComparePchPchCchCch() in microsoft-pdb gsi.cpp.
static int gsi_bucket_cmp(const void* a, const void* b) {
  const HRFile* hra = (const HRFile*)a;
  const HRFile* hrb = (const HRFile*)b;
  HashSym* left = &g_cur_hash_bucket_sort_syms[hra->off];
  HashSym* right = &g_cur_hash_bucket_sort_syms[hrb->off];
  assert(left->hash_bucket == right->hash_bucket);
  int cmp = gsi_record_cmp(left->name, right->name);
  if (cmp != 0) {
    return cmp < 0;
  }
  return left->offset < right->offset;
}

static void gsi_hash_builder_finish(GsiHashBuilder* hb) {
  // Figure out the exact bucket layout in the very arbitrary way that somebody
  // happened to decide on 30 years ago. The number of buckets in the
  // microsoft-pdb implementation is constant at IPHR_HASH afaict.

  // Figure out where each bucket starts.
  u32 bucket_starts[IPHR_HASH] = {0};
  {
    u32 num_mapped_to_bucket[IPHR_HASH] = {0};
    for (size_t i = 0; i < hb->sym_len; ++i) {
      ++num_mapped_to_bucket[hb->sym[i].hash_bucket];
    }

    u32 total = 0;
    for (size_t i = 0; i < IPHR_HASH; ++i) {
      bucket_starts[i] = total;
      total += num_mapped_to_bucket[i];
    }
  }

  // Put symbols into the table in bucket order, updating the bucket starts as
  // we go.
  u32 bucket_cursors[IPHR_HASH];
  memcpy(bucket_cursors, bucket_starts, sizeof(bucket_cursors));

  size_t num_syms = hb->sym_len;

  hb->hash_records = calloc(num_syms, sizeof(HRFile));
  hb->hash_records_len = num_syms;

  for (size_t i = 0; i < num_syms; ++i) {
    u32 hash_idx = bucket_cursors[hb->sym[i].hash_bucket]++;
    hb->hash_records[hash_idx].off = (u32)i;
    hb->hash_records[hash_idx].cref = 1;
  }

  g_cur_hash_bucket_sort_syms = hb->sym;
  // Sort each *bucket* (approximately) by the memcmp of the symbol's name. This
  // has to match microsoft-pdb, and it's bonkers. LLVM's implementation was
  // more helpful than microsoft-pdb's gsi.cpp for this one, and these hashes
  // aren't documented at all (in English) as of this writing as far as I know.
  for (size_t i = 0; i < IPHR_HASH; ++i) {
    size_t count = bucket_cursors[i] - bucket_starts[i];
    if (count > 0) {
      HRFile* begin = hb->hash_records + bucket_starts[i];
      qsort(begin, count, sizeof(HRFile), gsi_bucket_cmp);

      // Replace the indices with the stream offsets of each global, biased by 1
      // because 0 is treated specially.
      for (size_t j = 0; j < count; ++j) {
        begin[j].off = hb->sym[begin[j].off].offset + 1;
      }
    }
  }
  g_cur_hash_bucket_sort_syms = NULL;

  // Update the hash bitmap for each used bucket.
  for (u32 i = 0; i < sizeof(hb->hash_bitmap) / sizeof(hb->hash_bitmap[0]); ++i) {
    u32 word = 0;
    for (u32 j = 0; j < 32; ++j) {
      u32 bucket_idx = i * 32 + j;
      if (bucket_idx >= IPHR_HASH || bucket_starts[bucket_idx] == bucket_cursors[bucket_idx]) {
        continue;
      }
      word |= 1u << j;

      // Calculate what the offset of the first hash record int he chain would
      // be if it contained 32bit pointers: HROffsetCalc in microsoft-pdb gsi.h.
      u32 size_of_hr_offset_calc = 12;
      u32 chain_start_off = bucket_starts[bucket_idx] * size_of_hr_offset_calc;
      PUSH_BACK(hb->hash_buckets, chain_start_off);
    }
    hb->hash_bitmap[i] = word;
  }
}

static void gsi_hash_builder_write(DbpContext* ctx, GsiHashBuilder* hb, StreamData* stream) {
  SW_U32(0xffffffff);             // HdrSignature
  SW_U32(0xeffe0000 + 19990810);  // GSIHashSCImpv70
  SW_U32((u32)(hb->hash_records_len * sizeof(HRFile)));
  SW_U32((u32)(sizeof(hb->hash_bitmap) + hb->hash_buckets_len * sizeof(u32)));

  SW_BLOCK(hb->hash_records, hb->hash_records_len * sizeof(HRFile));
  SW_BLOCK(hb->hash_bitmap, sizeof(hb->hash_bitmap));
  SW_BLOCK(hb->hash_buckets, hb->hash_buckets_len * sizeof(u32));
}

static HashSym* g_cur_addr_map_sort_syms = NULL;
static int addr_map_cmp(const void* a, const void* b) {
  const u32* left_idx = (const u32*)a;
  const u32* right_idx = (const u32*)b;
  HashSym* left = &g_cur_addr_map_sort_syms[*left_idx];
  HashSym* right = &g_cur_addr_map_sort_syms[*right_idx];
  // Compare segment first, if we had one, but it's always 1.
  if (left->offset != right->offset)
    return left->offset < right->offset;
  return strcmp(left->name, right->name);
}

static void gsi_write_publics_stream(DbpContext* ctx, GsiHashBuilder* hb, StreamData* stream) {
  // microsoft-pdb PSGSIHDR first, then the hash table in the same format as
  // "globals" (gsi_hash_builder_write).
  u32 size_of_hash = (u32)(16 + (hb->hash_records_len * sizeof(HRFile)) + sizeof(hb->hash_bitmap) +
                           (hb->hash_buckets_len * sizeof(u32)));
  SW_U32(size_of_hash);                      // cbSymHash
  SW_U32((u32)(hb->sym_len * sizeof(u32)));  // cbAddrMap
  SW_U32(0);                                 // nThunks
  SW_U32(0);                                 // cbSizeOfThunk
  SW_U16(0);                                 // isectTunkTable
  SW_U16(0);                                 // padding
  SW_U32(0);                                 // offThunkTable
  SW_U32(0);                                 // nSects

  size_t before_hash_len = stream->data_length;

  gsi_hash_builder_write(ctx, hb, stream);

  size_t after_hash_len = stream->data_length;
  assert(after_hash_len - before_hash_len == size_of_hash &&
         "hash size calc doesn't match gsi_hash_builder_write");
  (void)before_hash_len;
  (void)after_hash_len;

  u32* addr_map = _alloca(sizeof(u32) * hb->sym_len);
  for (u32 i = 0; i < hb->sym_len; ++i)
    addr_map[i] = i;
  g_cur_addr_map_sort_syms = hb->sym;
  qsort(addr_map, hb->sym_len, sizeof(u32), addr_map_cmp);
  g_cur_addr_map_sort_syms = NULL;

  // Rewrite public symbol indices into symbol offsets.
  for (size_t i = 0; i < hb->sym_len; ++i) {
    addr_map[i] = hb->sym[addr_map[i]].offset;
  }

  SW_BLOCK(addr_map, hb->sym_len * sizeof(u32));
}

static void free_gsi_hash_builder(GsiHashBuilder* hb) {
  for (size_t i = 0; i < hb->sym_len; ++i) {
    free(hb->sym[i].name);
  }
  free(hb->sym);
  free(hb->hash_records);
  free(hb->hash_buckets);
}

static GsiData gsi_builder_finish(DbpContext* ctx, GsiBuilder* gsi) {
  gsi_hash_builder_finish(&gsi->publics);
  gsi_hash_builder_finish(&gsi->globals);

  gsi->global_hash_stream = add_stream(ctx);
  gsi_hash_builder_write(ctx, &gsi->globals, gsi->global_hash_stream);

  gsi->public_hash_stream = add_stream(ctx);
  gsi_write_publics_stream(ctx, &gsi->publics, gsi->public_hash_stream);

  GsiData result = {.global_symbol_stream = gsi->global_hash_stream->stream_index,
                    .public_symbol_stream = gsi->public_hash_stream->stream_index,
                    .sym_record_stream = gsi->sym_record_stream->stream_index};

  free_gsi_hash_builder(&gsi->publics);
  free_gsi_hash_builder(&gsi->globals);
  free(gsi);

  return result;
}

static GsiData build_gsi_data(DbpContext* ctx) {
  GsiBuilder* gsi = calloc(1, sizeof(GsiBuilder));
  gsi->sym_record_stream = add_stream(ctx);

  for (size_t i = 0; i < ctx->func_syms_len; ++i) {
    DbpFunctionSymbol* fs = ctx->func_syms[i];

    assert(fs->module_info_offset > 0 && "didn't write modi yet?");
    gsi_builder_add_procref(ctx, gsi, fs->module_info_offset, fs->name);

    gsi_builder_add_public(ctx, gsi, CVSPF_Function, fs->address, fs->name);
  }

  return gsi_builder_finish(ctx, gsi);
}

typedef struct ModuleData {
  StreamData* stream;
  u32 symbols_byte_size;
  u32 c13_byte_size;
} ModuleData;

typedef struct DebugLinesBlock {
  u32 checksum_buffer_offset;
  CV_LineNumberEntry* lines;
  size_t lines_len;
  size_t lines_cap;
} DebugLinesBlock;

typedef struct DebugLines {
  DebugLinesBlock* blocks;
  size_t blocks_len;
  size_t blocks_cap;

  u32 reloc_offset;
  u32 code_size;
} DebugLines;

static ModuleData write_module_stream(DbpContext* ctx) {
  StreamData* stream = add_stream(ctx);
  ModuleData module_data = {stream, 0, 0};

  u32 symbol_start = stream->data_length;

  SW_U32(4);  // Signature

  //
  // Symbols
  //
  CV_S_OBJNAME objname = {.record_type = 0x1101, .signature = 0};
  SW_CV_SYM_TRAILING_NAME(objname, synthetic_obj_name);

  CV_S_COMPILE3 compile3 = {
      .record_type = 0x113c,
      .flags = {.language = 0x00 /* CV_CFL_C */},
      .machine = 0xd0,  // x64
      .ver_fe_major = ctx->version_major,
      .ver_fe_minor = ctx->version_minor,
      .ver_fe_build = ctx->version_build,
      .ver_fe_qfe = ctx->version_qfe,
      .ver_be_major = ctx->version_major,
      .ver_be_minor = ctx->version_minor,
      .ver_be_build = ctx->version_build,
      .ver_be_qfe = ctx->version_qfe,
  };
  SW_CV_SYM_TRAILING_NAME(compile3, ctx->compiler_version_string);

  for (size_t i = 0; i < ctx->func_syms_len; ++i) {
    DbpFunctionSymbol* fs = ctx->func_syms[i];

    fs->module_info_offset = stream->data_length;

    CV_S_GPROC32 gproc32 = {
        .record_type = 0x1110,
        .parent = 0,
        .end = ~0U,
        .next = 0,
        .len = fs->length,
        .dbg_start = fs->address,   // not sure about these fields
        .dbg_end = fs->length - 1,  // not sure about these fields
        .type_index = 0x1001,       // hrm, first UDT, undefined but we're not writing types.
        .offset = fs->address,      /* address of proc */
        .seg = 1,
        .flags = {0},
    };
    SwFixup end_fixup = SW_CAPTURE_FIXUP(CV_S_GPROC32, end);
    SW_CV_SYM_TRAILING_NAME(gproc32, fs->name);

    CV_S_NODATA end = {.record_type = 0x0006};
    SW_WRITE_FIXUP_FOR_LOCATION_U32(end_fixup);
    SW_CV_SYM(end);
  }

  // TODO: could add all global data here too, but without types it's probably
  // pretty pointless.

  module_data.symbols_byte_size = stream->data_length - symbol_start;

  //
  // C13LineInfo
  //
  u32 c13_start = stream->data_length;

  // Need filename to offset-into-checksums block map. Record the name_index for
  // each string here, and find the index of name_index in this array. Then the
  // location where it's found in this array is the offset (times a constant
  // sizeof).
  u32* name_index_to_checksum_offset = _alloca(sizeof(u32) * ctx->names_nmt->num_names);

  // Checksums
  {
    // Write a block of checksums, except we actually write "None" checksums, so
    // it's just a table of indices pointed to by name_index above, that in turn
    // points to the offset into the /names NMT for the actual file name.
    size_t len = align_to((u32)sizeof(CV_FileChecksumEntryHeader), 4) * ctx->names_nmt->num_names;
    CV_DebugSubsectionHeader checksums_subsection_header = {.kind = CV_DSF_FileChecksums,
                                                            .length = align_to((u32)len, 4)};
    SW_BLOCK(&checksums_subsection_header, sizeof(checksums_subsection_header));

    // The layout of this section has to match how name_index_to_checksum_offset
    // is used above.
    NmtAlikeEnum it = nmtalike_enum_begin(ctx->names_nmt);
    size_t i = 0;
    while (nmtalike_enum_next(&it)) {
      u32 name_index;
      char* str;
      nmtalike_enum_get(&it, &name_index, &str);

      name_index_to_checksum_offset[i++] = name_index;

      CV_FileChecksumEntryHeader header = {
          .filename_offset = name_index, .checksum_size = 0, .checksum_kind = CV_FCSK_None};
      SW_BLOCK(&header, sizeof(header));
      SW_ALIGN(4);
    }
  }

  // Lines
  {
    size_t len = sizeof(CV_LineFragmentHeader);
    for (size_t i = 0; i < ctx->func_syms_len; ++i) {
      DbpFunctionSymbol* fs = ctx->func_syms[i];
      len += sizeof(CV_LineBlockFragmentHeader);
      len += fs->lines_len * sizeof(CV_LineNumberEntry);
    }

    CV_DebugSubsectionHeader lines_subsection_header = {.kind = CV_DSF_Lines, .length = (u32)len};
    SW_BLOCK(&lines_subsection_header, sizeof(lines_subsection_header));

    CV_LineFragmentHeader header = {.code_size = (u32)ctx->image_size,
                                    .flags = CF_LF_None,
                                    .reloc_segment = 1,
                                    .reloc_offset = 0};
    SW_BLOCK(&header, sizeof(header));

    for (size_t i = 0; i < ctx->func_syms_len; ++i) {
      DbpFunctionSymbol* fs = ctx->func_syms[i];

      CV_LineBlockFragmentHeader block_header;
      block_header.num_lines = (u32)fs->lines_len;
      block_header.block_size = sizeof(CV_LineBlockFragmentHeader);
      block_header.block_size += block_header.num_lines * sizeof(CV_LineNumberEntry);
      u32 name_index = nmtalike_name_index_for_string(ctx->names_nmt, fs->filename);
      u32 offset = ~0u;
      for (size_t j = 0; j < ctx->names_nmt->num_names; ++j) {
        if (name_index_to_checksum_offset[j] == name_index) {
          offset = (u32)(align_to((u32)sizeof(CV_FileChecksumEntryHeader), 4) * j);
          break;
        }
      }
      assert(offset != ~0u && "didn't find filename");
      block_header.checksum_block_offset = offset;
      SW_BLOCK(&block_header, sizeof(block_header));

      for (size_t j = 0; j < fs->lines_len; ++j) {
        CV_LineNumberEntry line_entry = {.offset = fs->lines[j].address,
                                         .line_num_start = fs->lines[j].line,
                                         .delta_line_end = 0,
                                         .is_statement = 1};
        SW_BLOCK(&line_entry, sizeof(line_entry));
      }
    }
  }

  module_data.c13_byte_size = stream->data_length - c13_start;

  //
  // GlobalRefs, don't know, don't write it.
  //
  SW_U32(0);  // GlobalRefsSize

  return module_data;
}

static int write_directory(DbpContext* ctx) {
  u32 directory_page = alloc_block(ctx);

  u32* block_map = get_block_ptr(ctx, ctx->superblock->block_map_addr);
  *block_map = directory_page;

  u32* start = get_block_ptr(ctx, directory_page);
  u32* dir = start;

  // Starts with number of streams.
  *dir++ = (u32)ctx->stream_data_len;

  // Then, the number of blocks in each stream.
  for (size_t i = 0; i < ctx->stream_data_len; ++i) {
    *dir++ = ctx->stream_data[i]->data_length;
    ENSURE((ctx->stream_data[i]->data_length + BLOCK_SIZE - 1) / BLOCK_SIZE,
           ctx->stream_data[i]->blocks_len);
  }

  // Then the list of blocks for each stream.
  for (size_t i = 0; i < ctx->stream_data_len; ++i) {
    for (size_t j = 0; j < ctx->stream_data[i]->blocks_len; ++j) {
      *dir++ = ctx->stream_data[i]->blocks[j];
    }
  }

  // And finally, update the super block with the number of bytes in the
  // directory.
  ctx->superblock->num_directory_bytes = (u32)((u32)(dir - start) * sizeof(u32));

  // This can't easily use StreamData because it's the directory of streams. It
  // would take a larger pdb that we expect to be writing here to overflow the
  // first block (especially since we don't write types), so just assert that we
  // didn't grow too large for now.
  if (ctx->superblock->num_directory_bytes > BLOCK_SIZE) {
    fprintf(stderr, "%s:%d: directory grew beyond BLOCK_SIZE\n", __FILE__, __LINE__);
    return 0;
  }

  return 1;
}

static int create_file_map(DbpContext* ctx) {
  ctx->file = CreateFile(ctx->output_pdb_name, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,
                         FILE_ATTRIBUTE_NORMAL, NULL);
  ENSURE_NE(ctx->file, INVALID_HANDLE_VALUE);

  ctx->file_size = BLOCK_SIZE * DEFAULT_NUM_BLOCKS;  // TODO: grow mapping as necessary
  ENSURE(SetFilePointer(ctx->file, (LONG)ctx->file_size, NULL, FILE_BEGIN), ctx->file_size);

  ENSURE(1, SetEndOfFile(ctx->file));

  HANDLE map_object = CreateFileMapping(ctx->file, NULL, PAGE_READWRITE, 0, 0, NULL);
  ENSURE_NE(map_object, NULL);

  ctx->data = MapViewOfFileEx(map_object, FILE_MAP_ALL_ACCESS, 0, 0, 0, NULL);
  ENSURE_NE(ctx->data, NULL);

  ENSURE(CloseHandle(map_object), 1);

  return 1;
}

typedef struct RsdsDataHeader {
  unsigned char magic[4];
  UUID unique_id;
  u32 age;
  // unsigned char name[]
} RsdsDataHeader;

static void file_fill_to_next_page(FILE* f, unsigned char with) {
  long long align_count = ftell(f);
  while (align_count % 0x200 != 0) {
    fwrite(&with, sizeof(with), 1, f);
    ++align_count;
  }
}

static int write_stub_dll(DbpContext* ctx, DbpExceptionTables* exception_tables) {
  FILE* f;
  if (fopen_s(&f, ctx->output_dll_name, "wb") != 0) {
    fprintf(stderr, "couldn't open %s\n", ctx->output_dll_name);
    return 0;
  }
  DWORD timedate = (DWORD)time(NULL);
  static unsigned char dos_stub_and_pe_magic[172] = {
      0x4d, 0x5a, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00,
      0x00, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0xa8, 0x00, 0x00, 0x00, 0x0e, 0x1f, 0xba, 0x0e, 0x00, 0xb4, 0x09, 0xcd, 0x21, 0xb8, 0x01,
      0x4c, 0xcd, 0x21, 0x54, 0x68, 0x69, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d,
      0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6e, 0x20,
      0x69, 0x6e, 0x20, 0x44, 0x4f, 0x53, 0x20, 0x6d, 0x6f, 0x64, 0x65, 0x2e, 0x0d, 0x0d, 0x0a,
      0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x61, 0x46, 0x33, 0xdf, 0x25, 0x27, 0x5d,
      0x8c, 0x25, 0x27, 0x5d, 0x8c, 0x25, 0x27, 0x5d, 0x8c, 0xe4, 0x5b, 0x59, 0x8d, 0x24, 0x27,
      0x5d, 0x8c, 0xe4, 0x5b, 0x5f, 0x8d, 0x24, 0x27, 0x5d, 0x8c, 0x52, 0x69, 0x63, 0x68, 0x25,
      0x27, 0x5d, 0x8c, 'P',  'E',  '\0', '\0',
  };
  // PE pointer is a 0x3c, points at 0xa8, IMAGE_FILE_HEADER starts there.
  fwrite(dos_stub_and_pe_magic, sizeof(dos_stub_and_pe_magic), 1, f);
  IMAGE_FILE_HEADER image_file_header = {
      .Machine = IMAGE_FILE_MACHINE_AMD64,
      .NumberOfSections = 2 + (exception_tables ? 2 : 0),
      .TimeDateStamp = timedate,
      .PointerToSymbolTable = 0,
      .NumberOfSymbols = 0,
      .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
      .Characteristics = IMAGE_FILE_RELOCS_STRIPPED | IMAGE_FILE_EXECUTABLE_IMAGE |
                         IMAGE_FILE_LARGE_ADDRESS_AWARE | IMAGE_FILE_DLL,
  };

  DWORD pdata_length =
      exception_tables ? (DWORD)(exception_tables->num_pdata_entries * sizeof(DbpRUNTIME_FUNCTION))
                       : 0;
  DWORD pdata_length_page_aligned = align_to(pdata_length, 0x1000);
  DWORD pdata_length_file_aligned = align_to(pdata_length, 0x200);

  DWORD xdata_length = exception_tables ? (DWORD)exception_tables->unwind_info_byte_length : 0;
  DWORD xdata_length_page_aligned = align_to(xdata_length, 0x1000);
  DWORD xdata_length_file_aligned = align_to(xdata_length, 0x200);

  DWORD code_start = 0x1000;
  DWORD rdata_virtual_start = (DWORD)(code_start + ctx->image_size);
  DWORD pdata_virtual_start = rdata_virtual_start + STUB_RDATA_SIZE;
  DWORD xdata_virtual_start = pdata_virtual_start + pdata_length_page_aligned;
  fwrite(&image_file_header, sizeof(image_file_header), 1, f);
  IMAGE_OPTIONAL_HEADER64 opt_header = {
      .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
      .MajorLinkerVersion = 14,  // Matches DBI stream.
      .MinorLinkerVersion = 11,
      .SizeOfCode = 0x200,
      .SizeOfInitializedData = 0x200,
      .SizeOfUninitializedData = 0,
      .AddressOfEntryPoint = 0,
      .BaseOfCode = 0x1000,
      .ImageBase = (ULONGLONG)ctx->base_addr,
      .SectionAlignment = 0x1000,
      .FileAlignment = 0x200,
      .MajorOperatingSystemVersion = 6,
      .MinorOperatingSystemVersion = 0,
      .MajorImageVersion = 6,
      .MinorImageVersion = 0,
      .MajorSubsystemVersion = 6,
      .MinorSubsystemVersion = 0,
      .Win32VersionValue = 0,
      // The documentation makes this seem like the size of the file, but I
      // think it's actually the virtual space occupied to the end of the
      // sections when loaded.
      .SizeOfImage = xdata_virtual_start + xdata_length_page_aligned,  // xdata is the last section.
      .SizeOfHeaders = 0x400,  // Address of where the section data starts.
      .CheckSum = 0,
      .Subsystem = IMAGE_SUBSYSTEM_WINDOWS_GUI,
      .DllCharacteristics =
          IMAGE_DLLCHARACTERISTICS_NX_COMPAT | IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA,
      .SizeOfStackReserve = 0x100000,
      .SizeOfStackCommit = 0x1000,
      .SizeOfHeapReserve = 0x100000,
      .SizeOfHeapCommit = 0x1000,
      .LoaderFlags = 0,
      .NumberOfRvaAndSizes = 0x10,
      .DataDirectory =
          {
              {0, 0},
              {0, 0},
              {0, 0},
              {exception_tables ? pdata_virtual_start : 0, (DWORD)pdata_length},
              {0, 0},
              {0, 0},
              {rdata_virtual_start, sizeof(IMAGE_DEBUG_DIRECTORY)},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0},
          },
  };
  fwrite(&opt_header, sizeof(opt_header), 1, f);

  //
  // .text header
  //
  IMAGE_SECTION_HEADER text = {
      .Name = ".text\0\0",
      .Misc = {.VirtualSize = (DWORD)ctx->image_size},
      .VirtualAddress = code_start,
      .SizeOfRawData = 0x200,     // This is the size of the block of .text in the dll.
      .PointerToRawData = 0x400,  // Aligned address in file.
      .PointerToRelocations = 0,
      .PointerToLinenumbers = 0,
      .NumberOfRelocations = 0,
      .NumberOfLinenumbers = 0,
      .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ,
  };
  fwrite(&text, sizeof(text), 1, f);

  //
  // .rdata header
  //
  RsdsDataHeader rsds_header = {
      .magic =
          {
              'R',
              'S',
              'D',
              'S',
          },
      .age = 1,
  };
  memcpy(&rsds_header.unique_id, &ctx->unique_id, sizeof(UUID));
  size_t name_len = strlen(ctx->output_pdb_name);
  DWORD rsds_len = (DWORD)(sizeof(rsds_header) + name_len + 1);

  IMAGE_SECTION_HEADER rdata = {
      .Name = ".rdata\0",
      .Misc = {.VirtualSize = (DWORD)(rsds_len + sizeof(IMAGE_DEBUG_DIRECTORY))},
      .VirtualAddress = rdata_virtual_start,
      .SizeOfRawData = 0x200,
      .PointerToRawData = 0x600,
      .PointerToRelocations = 0,
      .PointerToLinenumbers = 0,
      .NumberOfRelocations = 0,
      .NumberOfLinenumbers = 0,
      .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
  };
  fwrite(&rdata, sizeof(rdata), 1, f);

  if (exception_tables) {
    //
    // .pdata header
    //
    IMAGE_SECTION_HEADER pdata = {
        .Name = ".pdata\0",
        .Misc = {.VirtualSize = (DWORD)pdata_length},
        .VirtualAddress = pdata_virtual_start,
        .SizeOfRawData = pdata_length_file_aligned,
        .PointerToRawData = 0x800,  // Aligned address in file.
        .PointerToRelocations = 0,
        .PointerToLinenumbers = 0,
        .NumberOfRelocations = 0,
        .NumberOfLinenumbers = 0,
        .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
    };
    fwrite(&pdata, sizeof(pdata), 1, f);

    //
    // .xdata header
    //
    IMAGE_SECTION_HEADER xdata = {
        .Name = ".xdata\0",
        .Misc = {.VirtualSize = (DWORD)xdata_length},
        .VirtualAddress = xdata_virtual_start,
        .SizeOfRawData = xdata_length_file_aligned,
        .PointerToRawData = 0xa00,  // Aligned address in file.
        .PointerToRelocations = 0,
        .PointerToLinenumbers = 0,
        .NumberOfRelocations = 0,
        .NumberOfLinenumbers = 0,
        .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
    };
    fwrite(&xdata, sizeof(xdata), 1, f);
  } else {
    unsigned char zero = 0;
    fwrite(&zero, 1, 1, f);
  }

  file_fill_to_next_page(f, 0);
  assert(ftell(f) == 0x400);

  //
  // contents of .text
  //

  unsigned char int3 = 0xcc;
  fwrite(&int3, sizeof(int3), 1, f);
  file_fill_to_next_page(f, int3);
  assert(ftell(f) == 0x600);

  //
  // contents of .rdata
  //
  long long rdata_file_start = ftell(f);

  // Now the .rdata data which points to the pdb.
  IMAGE_DEBUG_DIRECTORY debug_dir = {
      .Characteristics = 0,
      .TimeDateStamp = timedate,
      .MajorVersion = 0,
      .MinorVersion = 0,
      .Type = IMAGE_DEBUG_TYPE_CODEVIEW,
      .SizeOfData = rsds_len,
      .AddressOfRawData = rdata_virtual_start + 0x1c,
      .PointerToRawData = (DWORD)(rdata_file_start + (long)sizeof(IMAGE_DEBUG_DIRECTORY)),
  };
  fwrite(&debug_dir, sizeof(debug_dir), 1, f);
  fwrite(&rsds_header, sizeof(rsds_header), 1, f);
  fwrite(ctx->output_pdb_name, 1, name_len + 1, f);

  file_fill_to_next_page(f, 0);
  assert(ftell(f) == 0x800);

  if (exception_tables) {
    //
    // contents of .pdata
    //
    for (size_t i = 0; i < exception_tables->num_pdata_entries; ++i) {
      exception_tables->pdata[i].begin_address += code_start;         // Fixup to .text RVA.
      exception_tables->pdata[i].end_address += code_start;           // Fixup to .text RVA.
      exception_tables->pdata[i].unwind_data += xdata_virtual_start;  // Fixup to .xdata RVA.
    }
    fwrite(exception_tables->pdata, sizeof(DbpRUNTIME_FUNCTION),
           exception_tables->num_pdata_entries, f);
    file_fill_to_next_page(f, 0);

    //
    // contents of .xdata
    //
    fwrite(exception_tables->unwind_info, 1, exception_tables->unwind_info_byte_length, f);
    file_fill_to_next_page(f, 0);
  }

  fclose(f);

  return 1;
}

static int force_symbol_load(DbpContext* ctx, DbpExceptionTables* exception_tables) {
  // Write stub dll with target address/size and fixed base address.
  ENSURE(write_stub_dll(ctx, exception_tables), 1);

  // Save current code block and then VirtualFree() it.
  void* tmp = malloc(ctx->image_size);
  memcpy(tmp, ctx->image_addr, ctx->image_size);
  VirtualFree(ctx->base_addr, 0, MEM_RELEASE);

  // There's a race here with other threads, but... I think that's the least of
  // our problems.
  ctx->dll_module = LoadLibraryEx(ctx->output_dll_name, NULL, DONT_RESOLVE_DLL_REFERENCES);

  // Make DLL writable and slam jitted code back into same location.
  DWORD old_protect;
  ENSURE(VirtualProtect(ctx->image_addr, ctx->image_size, PAGE_READWRITE, &old_protect), 1);

  memcpy(ctx->image_addr, tmp, ctx->image_size);
  free(tmp);

  ENSURE(VirtualProtect(ctx->image_addr, ctx->image_size, PAGE_EXECUTE_READ, &old_protect), 1);

  return 1;
}

int dbp_ready_to_execute(DbpContext* ctx, DbpExceptionTables* exception_tables) {
  if (!create_file_map(ctx))
    return 0;

  write_superblock(ctx);

  // Stream 0: "Old MSF Directory", empty.
  add_stream(ctx);

  // Stream 1: PDB Info Stream.
  StreamData* stream1 = add_stream(ctx);

  // Stream 2: TPI Stream.
  StreamData* stream2 = add_stream(ctx);
  ENSURE(1, write_empty_tpi_ipi_stream(ctx, stream2));

  // Stream 3: DBI Stream.
  StreamData* stream3 = add_stream(ctx);

  // Stream 4: IPI Stream.
  StreamData* stream4 = add_stream(ctx);
  ENSURE(write_empty_tpi_ipi_stream(ctx, stream4), 1);

  // "/names": named, so stream index doesn't matter.
  StreamData* names_stream = add_stream(ctx);
  ENSURE(write_names_stream(ctx, names_stream), 1);

  // Names must be written before module, because the line info refers to the
  // source files names (by offset into /names).
  ModuleData module_data = write_module_stream(ctx);

  // And the module stream must be written before the GSI stream because the
  // global procrefs contain the offset into the module data to locate the
  // actual function symbol.
  GsiData gsi_data = build_gsi_data(ctx);

  // Section Headers; empty. Referred to by DBI in 'optional' dbg headers, and
  // llvm-pdbutil wants it to exist, but handles an empty stream reasonably.
  StreamData* section_headers = add_stream(ctx);

  DbiWriteData dwd = {
      .gsi_data = gsi_data,
      .section_header_stream = section_headers->stream_index,
      .module_sym_stream = module_data.stream->stream_index,
      .module_symbols_byte_size = module_data.symbols_byte_size,
      .module_c13_byte_size = module_data.c13_byte_size,
      .num_source_files = 1,
  };
  ENSURE(write_dbi_stream(ctx, stream3, &dwd), 1);
  ENSURE(write_pdb_info_stream(ctx, stream1, names_stream->stream_index), 1);

  ENSURE(write_directory(ctx), 1);

  ENSURE(FlushViewOfFile(ctx->data, ctx->file_size), 1);
  ENSURE(UnmapViewOfFile(ctx->data), 1);
  CloseHandle(ctx->file);

  ENSURE(force_symbol_load(ctx, exception_tables), 1);

  return 1;
}

void dbp_free(DbpContext* ctx) {
  FreeLibrary(ctx->dll_module);

  for (size_t i = 0; i < ctx->func_syms_len; ++i) {
    free(ctx->func_syms[i]->name);
    free(ctx->func_syms[i]->filename);
    free(ctx->func_syms[i]->lines);
    free(ctx->func_syms[i]);
  }
  free(ctx->func_syms);
  free(ctx->output_pdb_name);
  free(ctx->output_dll_name);

  for (size_t i = 0; i < ctx->stream_data_len; ++i) {
    free(ctx->stream_data[i]->blocks);
    free(ctx->stream_data[i]);
  }
  free(ctx->stream_data);
  free(ctx->compiler_version_string);

  free(ctx->names_nmt->strings);
  free(ctx->names_nmt->hash);
  free(ctx->names_nmt);

  free(ctx);
}

#endif
//
// END OF ../../src/dyn_basic_pdb.h
//
#if X64WIN
#undef C
#undef L
#undef VOID
//
// START OF codegen.w.c
//
/*
** This file has been pre-processed with DynASM.
** https://luajit.org/dynasm.html
** DynASM version 1.5.0, DynASM x64 version 1.5.0
** DO NOT EDIT! The original file is in "../../src/codegen.in.c".
*/

#line 1 "../../src/codegen.in.c"

#define C(x) compiler_state.codegen__##x

#define DASM_CHECKS 1

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4127)
#pragma warning(disable : 4244)
#endif
#ifdef _MSC_VER
#pragma warning(pop)
#endif


//| .arch x64
#if DASM_VERSION != 10500
#error "Version mismatch between DynASM and included encoding engine"
#endif
#line 21 "../../src/codegen.in.c"
//| .section code, pdata
#define DASM_SECTION_CODE	0
#define DASM_SECTION_PDATA	1
#define DASM_MAXSECTION		2
#line 22 "../../src/codegen.in.c"
//| .actionlist dynasm_actions
static const unsigned char dynasm_actions[1711] = {
  249,255,80,255,64,88,240,42,255,72,131,252,236,8,252,242,15,17,4,36,255,252,
  242,64,15,16,4,240,140,36,72,131,196,8,255,252,243,15,16,0,255,252,242,15,
  16,0,255,219,40,255,15,182,0,255,15,190,0,255,15,183,0,255,15,191,0,255,72,
  99,0,255,72,139,0,255,68,138,128,233,68,136,129,233,255,252,243,15,17,1,255,
  252,242,15,17,1,255,219,57,255,136,1,255,102,137,1,255,72,137,1,255,72,139,
  133,233,255,72,141,133,233,255,72,141,5,245,255,249,72,184,237,237,255,72,
  129,192,239,255,15,87,201,15,46,193,255,102,15,87,201,102,15,46,193,255,217,
  252,238,223,232,221,216,255,131,252,248,0,255,72,131,252,248,0,255,15,190,
  192,255,15,182,192,255,15,191,192,255,15,183,192,255,252,243,15,42,192,255,
  72,99,192,255,252,242,15,42,192,255,137,68,36,252,252,219,68,36,252,252,255,
  137,192,252,243,72,15,42,192,255,137,192,255,137,192,252,242,72,15,42,192,
  255,137,192,72,137,68,36,252,248,223,108,36,252,248,255,72,133,192,15,136,
  244,247,102,15,252,239,192,252,242,72,15,42,192,252,233,244,248,248,1,72,
  137,193,131,224,1,102,15,252,239,192,72,209,252,233,72,9,193,252,242,72,15,
  42,193,252,242,15,88,192,248,2,255,72,137,68,36,252,248,223,108,36,252,248,
  72,133,192,15,137,244,247,184,0,0,128,95,137,68,36,252,252,216,68,36,252,
  252,248,1,255,252,243,15,44,192,15,190,192,255,252,243,15,44,192,15,182,192,
  255,252,243,15,44,192,15,191,192,255,252,243,15,44,192,15,183,192,255,252,
  243,15,44,192,255,252,243,72,15,44,192,255,252,243,15,90,192,255,252,243,
  15,17,68,36,252,252,217,68,36,252,252,255,252,242,15,44,192,15,190,192,255,
  252,242,15,44,192,15,182,192,255,252,242,15,44,192,15,191,192,255,252,242,
  15,44,192,15,183,192,255,252,242,15,44,192,255,252,242,72,15,44,192,255,252,
  242,15,90,192,255,252,242,15,17,68,36,252,248,221,68,36,252,248,255,217,124,
  36,252,246,15,183,68,36,252,246,128,204,12,102,137,68,36,252,244,217,108,
  36,252,244,255,219,92,36,232,217,108,36,252,246,15,191,68,36,232,255,219,
  92,36,232,217,108,36,252,246,15,183,68,36,232,37,252,255,0,0,0,255,219,92,
  36,232,217,108,36,252,246,15,183,68,36,232,255,219,92,36,232,217,108,36,252,
  246,139,68,36,232,255,223,124,36,232,217,108,36,252,246,72,139,68,36,232,
  255,217,92,36,252,248,252,243,15,16,68,36,252,248,255,221,92,36,252,248,252,
  242,15,16,68,36,252,248,255,15,149,208,15,182,192,255,72,129,252,236,239,
  255,68,138,144,233,68,136,148,253,36,233,255,73,141,131,233,255,65,83,73,
  137,227,255,72,131,252,236,8,255,72,131,252,236,16,219,60,36,255,252,243,
  15,17,133,233,255,252,242,15,17,133,233,255,136,133,233,72,193,232,8,255,
  252,243,64,15,17,133,253,240,140,233,255,252,242,64,15,17,133,253,240,140,
  233,255,64,136,133,253,240,131,233,72,193,232,240,35,8,255,72,137,193,255,
  252,243,15,16,1,255,252,242,15,16,1,255,72,199,192,0,0,0,0,255,72,193,224,
  8,102,139,129,233,255,252,243,64,15,16,65,240,140,8,255,252,242,64,15,16,
  65,240,140,8,255,72,199,192,240,35,0,0,0,0,255,72,193,224,240,35,8,64,138,
  129,253,240,131,233,255,72,139,141,233,255,138,144,233,136,145,233,255,72,
  131,193,15,131,225,252,240,255,76,139,141,233,73,41,225,72,137,224,72,41,
  204,72,137,226,248,1,73,131,252,249,0,15,132,244,248,68,138,0,68,136,2,72,
  252,255,194,72,252,255,192,73,252,255,201,252,233,244,1,248,2,255,72,139,
  133,233,72,41,200,72,137,133,233,255,184,237,102,72,15,110,192,255,72,184,
  237,237,102,72,15,110,192,255,72,184,237,237,72,137,68,36,252,240,72,184,
  237,237,72,137,68,36,252,248,219,108,36,252,240,255,72,199,192,237,255,72,
  199,192,1,0,0,0,72,193,224,31,102,72,15,110,200,15,87,193,255,72,199,192,
  1,0,0,0,72,193,224,63,102,72,15,110,200,102,15,87,193,255,217,224,255,72,
  252,247,216,255,72,193,224,235,255,72,193,232,235,255,72,193,252,248,235,
  255,72,199,192,237,137,133,233,255,73,137,192,255,72,137,193,72,129,225,239,
  72,193,225,235,255,72,139,4,36,255,73,199,193,237,76,33,200,72,9,200,255,
  76,137,192,255,87,255,72,199,193,237,72,141,189,233,176,0,252,243,170,255,
  95,255,15,132,245,255,252,233,245,249,255,15,148,208,72,15,182,192,255,72,
  252,247,208,255,15,132,245,72,199,192,1,0,0,0,252,233,245,249,72,199,192,
  0,0,0,0,249,255,15,133,245,255,15,133,245,72,199,192,0,0,0,0,252,233,245,
  249,72,199,192,1,0,0,0,249,255,72,131,192,8,255,102,72,15,126,192,240,132,
  240,36,255,72,129,252,236,239,73,137,194,65,252,255,210,72,129,196,239,255,
  65,91,255,72,137,133,233,72,141,133,233,255,73,137,194,72,199,192,237,65,
  252,255,210,72,129,196,239,255,252,240,15,176,17,255,102,252,240,15,177,17,
  255,252,240,72,15,177,17,255,15,148,209,15,132,244,247,255,65,136,0,255,102,
  65,137,0,255,73,137,0,255,248,1,15,182,193,255,134,1,255,102,135,1,255,72,
  135,1,255,252,243,15,88,193,255,252,242,15,88,193,255,252,243,15,92,193,255,
  252,242,15,92,193,255,252,243,15,89,193,255,252,242,15,89,193,255,252,243,
  15,94,193,255,252,242,15,94,193,255,15,46,200,255,102,15,46,200,255,15,148,
  208,15,155,210,32,208,255,15,149,208,15,154,210,8,208,255,15,151,208,255,
  15,147,208,255,36,1,72,15,182,192,255,222,193,255,222,225,255,222,201,255,
  222,252,241,255,223,252,241,221,216,255,15,148,208,255,15,149,208,255,72,
  1,200,255,72,41,200,255,72,15,175,193,255,72,199,194,0,0,0,0,72,252,247,252,
  241,255,186,0,0,0,0,252,247,252,241,255,72,153,255,72,252,247,252,249,255,
  72,137,208,255,72,33,200,255,72,49,200,255,72,57,200,255,15,146,208,255,15,
  156,208,255,15,150,208,255,15,158,208,255,72,137,201,255,72,211,224,255,72,
  211,232,255,72,211,252,248,255,15,133,245,249,255,72,129,252,248,239,255,
  72,137,193,72,129,252,233,239,72,129,252,249,239,255,137,193,129,252,233,
  239,129,252,249,239,255,15,134,245,255,252,233,245,255,252,255,224,255,64,
  136,133,253,240,131,233,255,102,64,137,133,253,240,139,233,255,72,137,133,
  253,240,131,233,255,254,0,85,72,137,229,255,249,73,186,237,237,255,65,252,
  255,210,72,41,196,255,254,1,250,3,249,235,255,235,235,255,235,236,255,235,
  235,235,235,255,72,137,165,233,255,199,133,233,237,199,133,233,237,72,137,
  173,233,72,131,133,233,16,72,137,173,233,72,129,133,233,239,255,72,137,189,
  233,72,137,181,233,72,137,149,233,72,137,141,233,76,137,133,233,76,137,141,
  233,252,242,15,17,133,233,252,242,15,17,141,233,252,242,15,17,149,233,252,
  242,15,17,157,233,252,242,15,17,165,233,252,242,15,17,173,233,252,242,15,
  17,181,233,252,242,15,17,189,233,255,72,137,141,233,72,137,149,233,76,137,
  133,233,76,137,141,233,255,72,141,101,0,255,72,137,252,236,255,93,195,255,
  250,3,249,254,0
};

#line 23 "../../src/codegen.in.c"
//| .globals dynasm_globals
enum {
  dynasm_globals_MAX
};
#line 24 "../../src/codegen.in.c"
//| .if WIN
//| .define X64WIN, 1
//| .endif

#define Dst &C(dynasm)

#define REG_DI 7
#define REG_SI 6
#define REG_DX 2
#define REG_CX 1
#define REG_R8 8
#define REG_R9 9

// Used with Rq(), Rd(), Rw(), Rb()
#if X64WIN
static int dasmargreg[] = {REG_CX, REG_DX, REG_R8, REG_R9};
#define REG_UTIL REG_CX
#define X64WIN_REG_MAX 4
#define PARAMETER_SAVE_SIZE (4 * 8)
#else
static int dasmargreg[] = {REG_DI, REG_SI, REG_DX, REG_CX, REG_R8, REG_R9};
#define REG_UTIL REG_DI
#define SYSV_GP_MAX 6
#define SYSV_FP_MAX 8
#endif
//| .if X64WIN
//| .define CARG1, rcx
//| .define CARG1d, ecx
//| .define CARG2, rdx
//| .define CARG3, r8
//| .define CARG4, r9
//| .define RUTIL, rcx
//| .define RUTILd, ecx
//| .define RUTILenc, 0x11
//| .else
//| .define CARG1, rdi
//| .define CARG1d, edi
//| .define CARG2, rsi
//| .define CARG3, rdx
//| .define CARG4, rcx
//| .define CARG5, r8
//| .define CARG6, r9
//| .define RUTIL, rdi
//| .define RUTILd, edi
//| .define RUTILenc, 0x17
//| .endif

static void gen_expr(Node* node);
static void gen_stmt(Node* node);

#if X64WIN
static void record_line_syminfo(int file_no, int line_no, int pclabel) {
  // If file and line haven't changed, then we're working through parts of a
  // single statement; just ignore.
  int cur_len = C(current_fn)->file_line_label_data.len;
  if (cur_len > 0 && C(current_fn)->file_line_label_data.data[cur_len - 1].a == file_no &&
      C(current_fn)->file_line_label_data.data[cur_len - 1].b == line_no) {
    return;
  }

  //|=>pclabel:
  dasm_put(Dst, 0, pclabel);
#line 85 "../../src/codegen.in.c"
  intintintarray_push(&C(current_fn)->file_line_label_data, (IntIntInt){file_no, line_no, pclabel},
                      AL_Compile);
  // printf("%s:%d:label %d\n", compiler_state.tokenize__all_tokenized_files.data[file_no]->name,
  // line_no, pclabel);
}
#endif

static int codegen_pclabel(void) {
  int ret = C(numlabels);
  dasm_growpc(&C(dynasm), ++C(numlabels));
  return ret;
}

static void push(void) {
  //| push rax
  dasm_put(Dst, 2);
#line 100 "../../src/codegen.in.c"
  C(depth)++;
}

static void pop(int dasmreg) {
  //| pop Rq(dasmreg)
  dasm_put(Dst, 4, (dasmreg));
#line 105 "../../src/codegen.in.c"
  C(depth)--;
}

static void pushf(void) {
  //| sub rsp, 8
  //| movsd qword [rsp], xmm0
  dasm_put(Dst, 9);
#line 111 "../../src/codegen.in.c"
  C(depth)++;
}

static void popf(int reg) {
  //| movsd xmm(reg), qword [rsp]
  //| add rsp, 8
  dasm_put(Dst, 21, (reg));
#line 117 "../../src/codegen.in.c"
  C(depth)--;
}

// Load a value from where %rax is pointing to.
static void load(Type* ty) {
  switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
    case TY_ARRAY:
    case TY_FUNC:
    case TY_VLA:
      // If it is an array, do not attempt to load a value to the
      // register because in general we can't load an entire array to a
      // register. As a result, the result of an evaluation of an array
      // becomes not the array itself but the address of the array.
      // This is where "array is automatically converted to a pointer to
      // the first element of the array in C" occurs.
      return;
    case TY_FLOAT:
      //| movss xmm0, dword [rax]
      dasm_put(Dst, 35);
#line 137 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd xmm0, qword [rax]
      dasm_put(Dst, 41);
#line 140 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fld tword [rax]
      dasm_put(Dst, 47);
#line 144 "../../src/codegen.in.c"
      return;
#endif
  }

  // When we load a char or a short value to a register, we always
  // extend them to the size of int, so we can assume the lower half of
  // a register always contains a valid value. The upper half of a
  // register for char, short and int may contain garbage. When we load
  // a long value to a register, it simply occupies the entire register.
  if (ty->size == 1) {
    if (ty->is_unsigned) {
      //| movzx eax, byte [rax]
      dasm_put(Dst, 50);
#line 156 "../../src/codegen.in.c"
    } else {
      //| movsx eax, byte [rax]
      dasm_put(Dst, 54);
#line 158 "../../src/codegen.in.c"
    }
  } else if (ty->size == 2) {
    if (ty->is_unsigned) {
      //| movzx eax, word [rax]
      dasm_put(Dst, 58);
#line 162 "../../src/codegen.in.c"
    } else {
      //| movsx eax, word [rax]
      dasm_put(Dst, 62);
#line 164 "../../src/codegen.in.c"
    }
  } else if (ty->size == 4) {
    //| movsxd rax, dword [rax]
    dasm_put(Dst, 66);
#line 167 "../../src/codegen.in.c"
  } else {
    //| mov rax, qword [rax]
    dasm_put(Dst, 70);
#line 169 "../../src/codegen.in.c"
  }
}

// Store %rax to an address that the stack top is pointing to.
static void store(Type* ty) {
  pop(REG_UTIL);

  switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      for (int i = 0; i < ty->size; i++) {
        //| mov r8b, [rax+i]
        //| mov [RUTIL+i], r8b
        dasm_put(Dst, 74, i, i);
#line 182 "../../src/codegen.in.c"
      }
      return;
    case TY_FLOAT:
      //| movss dword [RUTIL], xmm0
      dasm_put(Dst, 83);
#line 186 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd qword [RUTIL], xmm0
      dasm_put(Dst, 89);
#line 189 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fstp tword [RUTIL]
      dasm_put(Dst, 95);
#line 193 "../../src/codegen.in.c"
      return;
#endif
  }

  if (ty->size == 1) {
    //| mov [RUTIL], al
    dasm_put(Dst, 98);
#line 199 "../../src/codegen.in.c"
  } else if (ty->size == 2) {
    //| mov [RUTIL], ax
    dasm_put(Dst, 101);
#line 201 "../../src/codegen.in.c"
  } else if (ty->size == 4) {
    //| mov [RUTIL], eax
    dasm_put(Dst, 102);
#line 203 "../../src/codegen.in.c"
  } else {
    //| mov [RUTIL], rax
    dasm_put(Dst, 105);
#line 205 "../../src/codegen.in.c"
  }
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node* node) {
  switch (node->kind) {
    case ND_VAR:
      // Variable-length array, which is always local.
      if (node->var->ty->kind == TY_VLA) {
        //| mov rax, [rbp+node->var->offset]
        dasm_put(Dst, 109, node->var->offset);
#line 216 "../../src/codegen.in.c"
        return;
      }

      // Local variable
      if (node->var->is_local) {
        //| lea rax, [rbp+node->var->offset]
        dasm_put(Dst, 114, node->var->offset);
#line 222 "../../src/codegen.in.c"
        return;
      }

      // Thread-local variable
      if (node->var->is_tls) {
        // println("  mov rax, fs:0");
        // println("  add rax, [rel %s wrt ..gottpoff]", node->var->name);
        error_tok(node->tok, "TLS not implemented");
        return;
      }

      // Function
      if (node->ty->kind == TY_FUNC) {
        if (node->var->is_definition) {
          //| lea rax, [=>node->var->dasm_entry_label]
          dasm_put(Dst, 119, node->var->dasm_entry_label);
#line 237 "../../src/codegen.in.c"
        } else {
          int fixup_location = codegen_pclabel();
          strintarray_push(&C(fixups), (StringInt){node->var->name, fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
          //|=>fixup_location:
          //| mov64 rax, 0xc0dec0dec0dec0de
          dasm_put(Dst, 124, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 246 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
        }
        return;
      }

      // Global variable
      int fixup_location = codegen_pclabel();
      strintarray_push(&C(fixups), (StringInt){node->var->name, fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
      //|=>fixup_location:
      //| mov64 rax, 0xda7ada7ada7ada7a
      dasm_put(Dst, 124, fixup_location, (unsigned int)(0xda7ada7ada7ada7a), (unsigned int)((0xda7ada7ada7ada7a)>>32));
#line 262 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      return;
    case ND_DEREF:
      gen_expr(node->lhs);
      return;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_addr(node->rhs);
      return;
    case ND_MEMBER:
      gen_addr(node->lhs);
#if X64WIN
      if (node->lhs->kind == ND_VAR && node->lhs->var->is_param_passed_by_reference) {
        //| mov rax, [rax]
        dasm_put(Dst, 70);
#line 278 "../../src/codegen.in.c"
      }
#endif
      //| add rax, node->member->offset
      dasm_put(Dst, 130, node->member->offset);
#line 281 "../../src/codegen.in.c"
      return;
    case ND_FUNCALL:
      if (node->ret_buffer) {
        gen_expr(node);
        return;
      }
      break;
    case ND_ASSIGN:
    case ND_COND:
      if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
        gen_expr(node);
        return;
      }
      break;
    case ND_VLA_PTR:
      //| lea rax, [rbp+node->var->offset]
      dasm_put(Dst, 114, node->var->offset);
#line 297 "../../src/codegen.in.c"
      return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void cmp_zero(Type* ty) {
  switch (ty->kind) {
    case TY_FLOAT:
      //| xorps xmm1, xmm1
      //| ucomiss xmm0, xmm1
      dasm_put(Dst, 135);
#line 308 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| xorpd xmm1, xmm1
      //| ucomisd xmm0, xmm1
      dasm_put(Dst, 142);
#line 312 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fldz
      //| fucomip st0
      //| fstp st0
      dasm_put(Dst, 151);
#line 318 "../../src/codegen.in.c"
      return;
#endif
  }

  if (is_integer(ty) && ty->size <= 4) {
    //| cmp eax, 0
    dasm_put(Dst, 159);
#line 324 "../../src/codegen.in.c"
  } else {
    //| cmp rax, 0
    dasm_put(Dst, 164);
#line 326 "../../src/codegen.in.c"
  }
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

static int get_type_id(Type* ty) {
  switch (ty->kind) {
    case TY_CHAR:
      return ty->is_unsigned ? U8 : I8;
    case TY_SHORT:
      return ty->is_unsigned ? U16 : I16;
    case TY_INT:
      return ty->is_unsigned ? U32 : I32;
    case TY_LONG:
      return ty->is_unsigned ? U64 : I64;
    case TY_FLOAT:
      return F32;
    case TY_DOUBLE:
      return F64;
#if !X64WIN
    case TY_LDOUBLE:
      return F80;
#endif
  }
  return U64;
}

static void i32i8(void) {
  //| movsx eax, al
  dasm_put(Dst, 170);
#line 355 "../../src/codegen.in.c"
}
static void i32u8(void) {
  //| movzx eax, al
  dasm_put(Dst, 174);
#line 358 "../../src/codegen.in.c"
}
static void i32i16(void) {
  //| movsx eax, ax
  dasm_put(Dst, 178);
#line 361 "../../src/codegen.in.c"
}
static void i32u16(void) {
  //| movzx eax, ax
  dasm_put(Dst, 182);
#line 364 "../../src/codegen.in.c"
}
static void i32f32(void) {
  //| cvtsi2ss xmm0, eax
  dasm_put(Dst, 186);
#line 367 "../../src/codegen.in.c"
}
static void i32i64(void) {
  //| movsxd rax, eax
  dasm_put(Dst, 192);
#line 370 "../../src/codegen.in.c"
}
static void i32f64(void) {
  //| cvtsi2sd xmm0, eax
  dasm_put(Dst, 196);
#line 373 "../../src/codegen.in.c"
}
static void i32f80(void) {
  //| mov [rsp-4], eax
  //| fild dword [rsp-4]
  dasm_put(Dst, 202);
#line 377 "../../src/codegen.in.c"
}

static void u32f32(void) {
  //| mov eax, eax
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 213);
#line 382 "../../src/codegen.in.c"
}
static void u32i64(void) {
  //| mov eax, eax
  dasm_put(Dst, 222);
#line 385 "../../src/codegen.in.c"
}
static void u32f64(void) {
  //| mov eax, eax
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 225);
#line 389 "../../src/codegen.in.c"
}
static void u32f80(void) {
  //| mov eax, eax
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 234);
#line 394 "../../src/codegen.in.c"
}

static void i64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 215);
#line 398 "../../src/codegen.in.c"
}
static void i64f64(void) {
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 227);
#line 401 "../../src/codegen.in.c"
}
static void i64f80(void) {
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 236);
#line 405 "../../src/codegen.in.c"
}

static void u64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 215);
#line 409 "../../src/codegen.in.c"
}
static void u64f64(void) {
  //| test rax,rax
  //| js >1
  //| pxor xmm0,xmm0
  //| cvtsi2sd xmm0,rax
  //| jmp >2
  //|1:
  //| mov RUTIL,rax
  //| and eax,1
  //| pxor xmm0,xmm0
  //| shr RUTIL, 1
  //| or RUTIL,rax
  //| cvtsi2sd xmm0,RUTIL
  //| addsd xmm0,xmm0
  //|2:
  dasm_put(Dst, 248);
#line 425 "../../src/codegen.in.c"
}
static void u64f80(void) {
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  //| test rax, rax
  //| jns >1
  //| mov eax, 1602224128
  //| mov [rsp-4], eax
  //| fadd dword [rsp-4]
  //|1:
  dasm_put(Dst, 304);
#line 435 "../../src/codegen.in.c"
}

static void f32i8(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 340);
#line 440 "../../src/codegen.in.c"
}
static void f32u8(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 349);
#line 444 "../../src/codegen.in.c"
}
static void f32i16(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 358);
#line 448 "../../src/codegen.in.c"
}
static void f32u16(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 367);
#line 452 "../../src/codegen.in.c"
}
static void f32i32(void) {
  //| cvttss2si eax, xmm0
  dasm_put(Dst, 376);
#line 455 "../../src/codegen.in.c"
}
static void f32u32(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 382);
#line 458 "../../src/codegen.in.c"
}
static void f32i64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 382);
#line 461 "../../src/codegen.in.c"
}
static void f32u64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 382);
#line 464 "../../src/codegen.in.c"
}
static void f32f64(void) {
  //| cvtss2sd xmm0, xmm0
  dasm_put(Dst, 389);
#line 467 "../../src/codegen.in.c"
}
static void f32f80(void) {
  //| movss dword [rsp-4], xmm0
  //| fld dword [rsp-4]
  dasm_put(Dst, 395);
#line 471 "../../src/codegen.in.c"
}

static void f64i8(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 409);
#line 476 "../../src/codegen.in.c"
}
static void f64u8(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 418);
#line 480 "../../src/codegen.in.c"
}
static void f64i16(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 427);
#line 484 "../../src/codegen.in.c"
}
static void f64u16(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 436);
#line 488 "../../src/codegen.in.c"
}
static void f64i32(void) {
  //| cvttsd2si eax, xmm0
  dasm_put(Dst, 445);
#line 491 "../../src/codegen.in.c"
}
static void f64u32(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 451);
#line 494 "../../src/codegen.in.c"
}
static void f64i64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 451);
#line 497 "../../src/codegen.in.c"
}
static void f64u64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 451);
#line 500 "../../src/codegen.in.c"
}
static void f64f32(void) {
  //| cvtsd2ss xmm0, xmm0
  dasm_put(Dst, 458);
#line 503 "../../src/codegen.in.c"
}
static void f64f80(void) {
  //| movsd qword [rsp-8], xmm0
  //| fld qword [rsp-8]
  dasm_put(Dst, 464);
#line 507 "../../src/codegen.in.c"
}

static void from_f80_1(void) {
  //| fnstcw word [rsp-10]
  //| movzx eax, word [rsp-10]
  //| or ah, 12
  //| mov [rsp-12], ax
  //| fldcw word [rsp-12]
  dasm_put(Dst, 478);
#line 515 "../../src/codegen.in.c"
}

#define FROM_F80_2 " [rsp-24]\n fldcw [rsp-10]\n "

static void f80i8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 504);
#line 524 "../../src/codegen.in.c"
}
static void f80u8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  //| and eax, 0xff
  dasm_put(Dst, 519);
#line 531 "../../src/codegen.in.c"
}
static void f80i16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 504);
#line 537 "../../src/codegen.in.c"
}
static void f80u16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  dasm_put(Dst, 540);
#line 543 "../../src/codegen.in.c"
}
static void f80i32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 555);
#line 549 "../../src/codegen.in.c"
}
static void f80u32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 555);
#line 555 "../../src/codegen.in.c"
}
static void f80i64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 569);
#line 561 "../../src/codegen.in.c"
}
static void f80u64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 569);
#line 567 "../../src/codegen.in.c"
}
static void f80f32(void) {
  //| fstp dword [rsp-8]
  //| movss xmm0, dword [rsp-8]
  dasm_put(Dst, 584);
#line 571 "../../src/codegen.in.c"
}
static void f80f64(void) {
  //| fstp qword [rsp-8]
  //| movsd xmm0, qword [rsp-8]
  dasm_put(Dst, 598);
#line 575 "../../src/codegen.in.c"
}

typedef void (*DynasmCastFunc)(void);

// clang-format off

// The table for type casts
static DynasmCastFunc dynasm_cast_table[][11] = {
  // "to" is the rows, "from" the columns
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

// clang-format on

// This can't be "cast()" when amalgamated because parse has a cast() as well.
static void cg_cast(Type* from, Type* to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    //| setne al
    //| movzx eax, al
    dasm_put(Dst, 612);
#line 611 "../../src/codegen.in.c"
    return;
  }

  int t1 = get_type_id(from);
  int t2 = get_type_id(to);
  if (dynasm_cast_table[t1][t2]) {
    dynasm_cast_table[t1][t2]();
  }
}

#if !X64WIN

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
static bool has_flonum(Type* ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member* mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

static bool has_flonum1(Type* ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type* ty) {
  return has_flonum(ty, 8, 16, 0);
}

#endif

static int push_struct(Type* ty) {
  int sz = (int)align_to_s(ty->size, 8);
  //| sub rsp, sz
  dasm_put(Dst, 619, sz);
#line 666 "../../src/codegen.in.c"
  C(depth) += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    //| mov r10b, [rax+i]
    //| mov [rsp+i], r10b
    dasm_put(Dst, 625, i, i);
#line 671 "../../src/codegen.in.c"
  }

  return sz;
}

#if X64WIN

bool type_passed_in_register(Type* ty) {
  // https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention:
  //   "__m128 types, arrays, and strings are never passed by immediate value.
  //   Instead, a pointer is passed to memory allocated by the caller. Structs
  //   and unions of size 8, 16, 32, or 64 bits, and __m64 types, are passed as
  //   if they were integers of the same size."
  //
  // Note that e.g. a pragma pack 5 byte structure will be passed by reference,
  // so this is not just size <= 8 as it is for 16 on SysV.
  //
  // Arrays and strings as mentioned won't be TY_STRUCT/TY_UNION so they should
  // not use this function.
  return ty->size == 1 || ty->size == 2 || ty->size == 4 || ty->size == 8;
}

static void push_args2_win(Node* args, bool first_pass) {
  if (!args)
    return;
  push_args2_win(args->next, first_pass);

  // Push all the by-stack first, then on the second pass, push all the things
  // that will be popped back into registers by the actual call.
  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  if ((args->ty->kind != TY_STRUCT && args->ty->kind != TY_UNION) ||
      type_passed_in_register(args->ty)) {
    gen_expr(args);
  }

  switch (args->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (!type_passed_in_register(args->ty)) {
        assert(args->pass_by_reference);
        //| lea rax, [r11-args->pass_by_reference]
        dasm_put(Dst, 636, -args->pass_by_reference);
#line 714 "../../src/codegen.in.c"
      } else {
        //| mov rax, [rax]
        dasm_put(Dst, 70);
#line 716 "../../src/codegen.in.c"
      }
      push();
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      pushf();
      break;
    default:
      push();
      break;
  }
}

// --- Windows ---
// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// required by the Windows ABI.
//
// - Integer arguments in the leftmost four positions are passed in RCX, RDX,
//   R8, and R9.
//
// - Floating point arguments in the leftmost four position are passed in
//   XMM0-XM3.
//
// - The 5th and subsequent arguments are push on the stack in right-to-left
//   order.
//
// - Arguments larger than 8 bytes are always passed by reference.
//
// - When mixing integer and floating point arguments, the opposite type's
//   register is left unused, e.g.
//
//     void func(int a, double b, int c, float d, int e, float f);
//
//   would have a in RCX, b in XMM1, c in R8, d in XMM3, f then e pushed on stack.
//
// - Varargs follow the same conventions, but floating point values must also
//   have their value stored in the corresponding integer register for the first
//   four arguments.
//
// - For larger than 8 byte structs, they're passed by reference. So we first
//   need to make a local copy on the stack (since they're still passed by value
//   not reference as far as the language is concerned), but then pass a pointer
//   to the copy rather than to the actual data. This difference definitely
//   casues the most changes vs. SysV in the rest of the compiler.
//
// - Integer return values of 8 bytes or less are in RAX (including user-defined
//   types like small structures). Floating point are returned in XMM0. For
//   user-defined types that are larger than 8 bytes, the caller allocates a
//   buffer and passes the address of the buffer in RCX, taking up the first
//   integer register slot. The function returns the same address passed in RCX
//   in RAX.
//
// - RAX, RCX, RDX, R8, R9, R10, R11, and XMM0-XMM5 are volatile.
// - RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 are
//   non-volatile.
//
// --- Windows ---
static int push_args_win(Node* node, int* by_ref_copies_size) {
  int stack = 0, reg = 0;

  bool has_by_ref_args = false;
  for (Node* arg = node->args; arg; arg = arg->next) {
    if ((arg->ty->kind == TY_STRUCT || arg->ty->kind == TY_UNION) &&
        !type_passed_in_register(arg->ty)) {
      has_by_ref_args = true;
      break;
    }
  }

  if (has_by_ref_args) {
    // Use r11 as a base pointer for by-reference copies of structs.
    //| push r11
    //| mov r11, rsp
    dasm_put(Dst, 641);
#line 791 "../../src/codegen.in.c"
  }

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && !type_passed_in_register(node->ty))
    reg++;

  *by_ref_copies_size = 0;

  // Load as many arguments to the registers as possible.
  for (Node* arg = node->args; arg; arg = arg->next) {
    Type* ty = arg->ty;

    switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        // It's either small and so passed in a register, or isn't and then
        // we're instead storing the pointer to the larger struct.
        if (reg++ >= X64WIN_REG_MAX) {
          arg->pass_by_stack = true;
          ++stack;
        }
        if (!type_passed_in_register(ty)) {
          // Make a copy, and note the offset for passing by reference.
          gen_expr(arg);
          *by_ref_copies_size += push_struct(ty);
          arg->pass_by_reference = *by_ref_copies_size;
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (reg++ >= X64WIN_REG_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
        break;
      default:
        if (reg++ >= X64WIN_REG_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
    }
  }

  assert((*by_ref_copies_size == 0 && !has_by_ref_args) ||
         (*by_ref_copies_size && has_by_ref_args));

  if ((C(depth) + stack + (*by_ref_copies_size / 8)) % 2 == 1) {
    //| sub rsp, 8
    dasm_put(Dst, 647);
#line 840 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_win(node->args, true);
  push_args2_win(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && !type_passed_in_register(node->ty)) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 114, node->ret_buffer->offset);
#line 851 "../../src/codegen.in.c"
    push();
  }

  return stack;
}

#else

static void push_args2_sysv(Node* args, bool first_pass) {
  if (!args)
    return;
  push_args2_sysv(args->next, first_pass);

  // Push all the by-stack first, then on the second pass, push all the things
  // that will be popped back into registers by the actual call.
  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr(args);

  switch (args->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      push_struct(args->ty);
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      pushf();
      break;
    case TY_LDOUBLE:
      //| sub rsp, 16
      //| fstp tword [rsp]
      dasm_put(Dst, 653);
#line 883 "../../src/codegen.in.c"
      C(depth) += 2;
      break;
    default:
      push();
      break;
  }
}

// --- SysV ---
//
// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
// - Up to 6 arguments of integral type are passed using RDI, RSI,
//   RDX, RCX, R8 and R9.
//
// - Up to 8 arguments of floating-point type are passed using XMM0 to
//   XMM7.
//
// - If all registers of an appropriate type are already used, push an
//   argument to the stack in the right-to-left order.
//
// - Each argument passed on the stack takes 8 bytes, and the end of
//   the argument area must be aligned to a 16 byte boundary.
//
// - If a function is variadic, set the number of floating-point type
//   arguments to RAX.
//
// --- SysV ---
static int push_args_sysv(Node* node) {
  int stack = 0, gp = 0, fp = 0;

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16)
    gp++;

  // Load as many arguments to the registers as possible.
  for (Node* arg = node->args; arg; arg = arg->next) {
    Type* ty = arg->ty;

    switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size > 16) {
          arg->pass_by_stack = true;
          stack += align_to_s(ty->size, 8) / 8;
        } else {
          bool fp1 = has_flonum1(ty);
          bool fp2 = has_flonum2(ty);

          if (fp + fp1 + fp2 < SYSV_FP_MAX && gp + !fp1 + !fp2 < SYSV_GP_MAX) {
            fp = fp + fp1 + fp2;
            gp = gp + !fp1 + !fp2;
          } else {
            arg->pass_by_stack = true;
            stack += align_to_s(ty->size, 8) / 8;
          }
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp++ >= SYSV_FP_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
        break;
      case TY_LDOUBLE:
        arg->pass_by_stack = true;
        stack += 2;
        break;
      default:
        if (gp++ >= SYSV_GP_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
    }
  }

  if ((C(depth) + stack) % 2 == 1) {
    //| sub rsp, 8
    dasm_put(Dst, 647);
#line 966 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_sysv(node->args, true);
  push_args2_sysv(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 114, node->ret_buffer->offset);
#line 977 "../../src/codegen.in.c"
    push();
  }

  return stack;
}

static void copy_ret_buffer(Obj* var) {
  Type* ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4) {
      //| movss dword [rbp+var->offset], xmm0
      dasm_put(Dst, 662, var->offset);
#line 991 "../../src/codegen.in.c"
    } else {
      //| movsd qword [rbp+var->offset], xmm0
      dasm_put(Dst, 669, var->offset);
#line 993 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      //| mov [rbp+var->offset+i], al
      //| shr rax, 8
      dasm_put(Dst, 676, var->offset+i);
#line 999 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12) {
        //| movss dword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 684, (fp), var->offset+8);
#line 1008 "../../src/codegen.in.c"
      } else {
        //| movsd qword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 695, (fp), var->offset+8);
#line 1010 "../../src/codegen.in.c"
      }
    } else {
      for (int i = 8; i < MIN(16, ty->size); i++) {
        //| mov [rbp+var->offset+i], Rb(gp)
        //| shr Rq(gp), 8
        dasm_put(Dst, 706, (gp), var->offset+i, (gp));
#line 1015 "../../src/codegen.in.c"
      }
    }
  }
}

#endif

static void copy_struct_reg(void) {
#if X64WIN
  // TODO: I'm not sure if this is right/sufficient.
  //| mov rax, [rax]
  dasm_put(Dst, 70);
#line 1026 "../../src/codegen.in.c"
#else
  Type* ty = C(current_fn)->ty->return_ty;

  int gp = 0, fp = 0;

  //| mov RUTIL, rax
  dasm_put(Dst, 720);
#line 1032 "../../src/codegen.in.c"

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4) {
      //| movss xmm0, dword [RUTIL]
      dasm_put(Dst, 724);
#line 1037 "../../src/codegen.in.c"
    } else {
      //| movsd xmm0, qword [RUTIL]
      dasm_put(Dst, 730);
#line 1039 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    //| mov rax, 0
    dasm_put(Dst, 736);
#line 1043 "../../src/codegen.in.c"
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      //| shl rax, 8
      //| mov ax, [RUTIL+i]
      dasm_put(Dst, 744, i);
#line 1046 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4) {
        //| movss xmm(fp), dword [RUTIL+8]
        dasm_put(Dst, 753, (fp));
#line 1055 "../../src/codegen.in.c"
      } else {
        //| movsd xmm(fp), qword [RUTIL+8]
        dasm_put(Dst, 763, (fp));
#line 1057 "../../src/codegen.in.c"
      }
    } else {
      //| mov Rq(gp), 0
      dasm_put(Dst, 773, (gp));
#line 1060 "../../src/codegen.in.c"
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        //| shl Rq(gp), 8
        //| mov Rb(gp), [RUTIL+i]
        dasm_put(Dst, 783, (gp), (gp), i);
#line 1063 "../../src/codegen.in.c"
      }
    }
  }
#endif
}

static void copy_struct_mem(void) {
  Type* ty = C(current_fn)->ty->return_ty;
  Obj* var = C(current_fn)->params;

  //| mov RUTIL, [rbp+var->offset]
  dasm_put(Dst, 797, var->offset);
#line 1074 "../../src/codegen.in.c"

  for (int i = 0; i < ty->size; i++) {
    //| mov dl, [rax+i]
    //| mov [RUTIL+i], dl
    dasm_put(Dst, 802, i, i);
#line 1078 "../../src/codegen.in.c"
  }
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  //| add CARG1, 15
  //| and CARG1d, 0xfffffff0
  dasm_put(Dst, 809);
#line 1085 "../../src/codegen.in.c"

  // Shift the temporary area by CARG1.
  //| mov CARG4, [rbp+C(current_fn)->alloca_bottom->offset]
  //| sub CARG4, rsp
  //| mov rax, rsp
  //| sub rsp, CARG1
  //| mov rdx, rsp
  //|1:
  //| cmp CARG4, 0
  //| je >2
  //| mov r8b, [rax]
  //| mov [rdx], r8b
  //| inc rdx
  //| inc rax
  //| dec CARG4
  //| jmp <1
  //|2:
  dasm_put(Dst, 818, C(current_fn)->alloca_bottom->offset);
#line 1102 "../../src/codegen.in.c"

  // Move alloca_bottom pointer.
  //| mov rax, [rbp+C(current_fn)->alloca_bottom->offset]
  //| sub rax, CARG1
  //| mov [rbp+C(current_fn)->alloca_bottom->offset], rax
  dasm_put(Dst, 870, C(current_fn)->alloca_bottom->offset, C(current_fn)->alloca_bottom->offset);
#line 1107 "../../src/codegen.in.c"
}

// Generate code for a given node.
static void gen_expr(Node* node) {
  switch (node->kind) {
    case ND_NULL_EXPR:
      return;
    case ND_NUM: {
      switch (node->ty->kind) {
        case TY_FLOAT: {
          union {
            float f32;
            uint32_t u32;
          } u = {(float)node->fval};
          //| mov eax, u.u32
          //| movd xmm0, rax
          dasm_put(Dst, 882, u.u32);
#line 1123 "../../src/codegen.in.c"
          return;
        }
        case TY_DOUBLE: {
          union {
            double f64;
            uint64_t u64;
          } u = {(double)node->fval};
          //| mov64 rax, u.u64
          //| movd xmm0, rax
          dasm_put(Dst, 890, (unsigned int)(u.u64), (unsigned int)((u.u64)>>32));
#line 1132 "../../src/codegen.in.c"
          return;
        }
#if !X64WIN
        case TY_LDOUBLE: {
          union {
            long double f80;
            uint64_t u64[2];
          } u;
          memset(&u, 0, sizeof(u));
          u.f80 = node->fval;
          //| mov64 rax, u.u64[0]
          //| mov [rsp-16], rax
          //| mov64 rax, u.u64[1]
          //| mov [rsp-8], rax
          //| fld tword [rsp-16]
          dasm_put(Dst, 900, (unsigned int)(u.u64[0]), (unsigned int)((u.u64[0])>>32), (unsigned int)(u.u64[1]), (unsigned int)((u.u64[1])>>32));
#line 1147 "../../src/codegen.in.c"
          return;
        }
#endif
      }

      if (node->val < INT_MIN || node->val > INT_MAX) {
        //| mov64 rax, node->val
        dasm_put(Dst, 125, (unsigned int)(node->val), (unsigned int)((node->val)>>32));
#line 1154 "../../src/codegen.in.c"
      } else {
        //| mov rax, node->val
        dasm_put(Dst, 926, node->val);
#line 1156 "../../src/codegen.in.c"
      }
      return;
    }
    case ND_NEG:
      gen_expr(node->lhs);

      switch (node->ty->kind) {
        case TY_FLOAT:
          //| mov rax, 1
          //| shl rax, 31
          //| movd xmm1, rax
          //| xorps xmm0, xmm1
          dasm_put(Dst, 931);
#line 1168 "../../src/codegen.in.c"
          return;
        case TY_DOUBLE:
          //| mov rax, 1
          //| shl rax, 63
          //| movd xmm1, rax
          //| xorpd xmm0, xmm1
          dasm_put(Dst, 951);
#line 1174 "../../src/codegen.in.c"
          return;
#if !X64WIN
        case TY_LDOUBLE:
          //| fchs
          dasm_put(Dst, 972);
#line 1178 "../../src/codegen.in.c"
          return;
#endif
      }

      //| neg rax
      dasm_put(Dst, 975);
#line 1183 "../../src/codegen.in.c"
      return;
    case ND_VAR:
      gen_addr(node);
      load(node->ty);
      return;
    case ND_MEMBER: {
      gen_addr(node);
      load(node->ty);

      Member* mem = node->member;
      if (mem->is_bitfield) {
        //| shl rax, 64 - mem->bit_width - mem->bit_offset
        dasm_put(Dst, 980, 64 - mem->bit_width - mem->bit_offset);
#line 1195 "../../src/codegen.in.c"
        if (mem->ty->is_unsigned) {
          //| shr rax, 64 - mem->bit_width
          dasm_put(Dst, 985, 64 - mem->bit_width);
#line 1197 "../../src/codegen.in.c"
        } else {
          //| sar rax, 64 - mem->bit_width
          dasm_put(Dst, 990, 64 - mem->bit_width);
#line 1199 "../../src/codegen.in.c"
        }
      }
      return;
    }
    case ND_DEREF:
      gen_expr(node->lhs);
      load(node->ty);
      return;
    case ND_ADDR:
      gen_addr(node->lhs);
      return;
    case ND_ASSIGN:
      // Special case "int into a local". Normally this would compile to:
      //   lea rax,[rbp+node->lhs->offset]
      //   push rax
      //   mov rax, node->rhs->val
      //   pop rcx
      //   mov [rcx], eax
      if (node->lhs->kind == ND_VAR && node->lhs->var->is_local && node->rhs->kind == ND_NUM &&
          node->rhs->ty->kind != TY_FLOAT && node->rhs->ty->kind != TY_DOUBLE &&
          node->rhs->ty->kind != TY_LDOUBLE && node->rhs->val >= INT_MIN &&
          node->rhs->val <= INT_MAX) {
        //| mov rax, node->rhs->val
        //| mov dword [rbp+node->lhs->var->offset], eax
        dasm_put(Dst, 996, node->rhs->val, node->lhs->var->offset);
#line 1223 "../../src/codegen.in.c"
      } else {
        gen_addr(node->lhs);
        push();
        gen_expr(node->rhs);

        if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
          //| mov r8, rax
          dasm_put(Dst, 1004);
#line 1230 "../../src/codegen.in.c"

          // If the lhs is a bitfield, we need to read the current value
          // from memory and merge it with a new value.
          Member* mem = node->lhs->member;
          //| mov RUTIL, rax
          //| and RUTIL, (1L << mem->bit_width) - 1
          //| shl RUTIL, mem->bit_offset
          dasm_put(Dst, 1008, (1L << mem->bit_width) - 1, mem->bit_offset);
#line 1237 "../../src/codegen.in.c"

          //| mov rax, [rsp]
          dasm_put(Dst, 1020);
#line 1239 "../../src/codegen.in.c"
          load(mem->ty);

          long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
          //| mov r9, ~mask
          //| and rax, r9
          //| or rax, RUTIL
          dasm_put(Dst, 1025, ~mask);
#line 1245 "../../src/codegen.in.c"
          store(node->ty);
          //| mov rax, r8
          dasm_put(Dst, 1036);
#line 1247 "../../src/codegen.in.c"
          return;
        }

        store(node->ty);
      }
      return;
    case ND_STMT_EXPR:
      for (Node* n = node->body; n; n = n->next)
        gen_stmt(n);
      return;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      return;
    case ND_CAST:
      gen_expr(node->lhs);
      cg_cast(node->lhs->ty, node->ty);
      return;
    case ND_MEMZERO:
      // `rep stosb` is equivalent to `memset(rdi, al, rcx)`.
#if X64WIN
      //| push rdi
      dasm_put(Dst, 1040);
#line 1269 "../../src/codegen.in.c"
#endif
      //| mov rcx, node->var->ty->size
      //| lea rdi, [rbp+node->var->offset]
      //| mov al, 0
      //| rep
      //| stosb
      dasm_put(Dst, 1042, node->var->ty->size, node->var->offset);
#line 1275 "../../src/codegen.in.c"
#if X64WIN
      //| pop rdi
      dasm_put(Dst, 1056);
#line 1277 "../../src/codegen.in.c"
#endif
      return;
    case ND_COND: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1058, lelse);
#line 1285 "../../src/codegen.in.c"
      gen_expr(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1062, lend, lelse);
#line 1288 "../../src/codegen.in.c"
      gen_expr(node->els);
      //|=>lend:
      dasm_put(Dst, 0, lend);
#line 1290 "../../src/codegen.in.c"
      return;
    }
    case ND_NOT:
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| sete al
      //| movzx rax, al
      dasm_put(Dst, 1067);
#line 1297 "../../src/codegen.in.c"
      return;
    case ND_BITNOT:
      gen_expr(node->lhs);
      //| not rax
      dasm_put(Dst, 1075);
#line 1301 "../../src/codegen.in.c"
      return;
    case ND_LOGAND: {
      int lfalse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| je =>lfalse
      dasm_put(Dst, 1058, lfalse);
#line 1308 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| je =>lfalse
      //| mov rax, 1
      //| jmp =>lend
      //|=>lfalse:
      //| mov rax, 0
      //|=>lend:
      dasm_put(Dst, 1080, lfalse, lend, lfalse, lend);
#line 1316 "../../src/codegen.in.c"
      return;
    }
    case ND_LOGOR: {
      int ltrue = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| jne =>ltrue
      dasm_put(Dst, 1103, ltrue);
#line 1324 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| jne =>ltrue
      //| mov rax, 0
      //| jmp =>lend
      //|=>ltrue:
      //| mov rax, 1
      //|=>lend:
      dasm_put(Dst, 1107, ltrue, lend, ltrue, lend);
#line 1332 "../../src/codegen.in.c"
      return;
    }
    case ND_FUNCALL: {
      if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
        gen_expr(node->args);
        //| mov CARG1, rax
        dasm_put(Dst, 720);
#line 1338 "../../src/codegen.in.c"
        builtin_alloca();
        return;
      }

#if X64WIN
      if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "__va_start")) {
        // va_start(ap, x) turns into __va_start(&ap, x), so we only want the
        // expr here, not the address.
        gen_expr(node->args);
        push();
        // ToS is now &ap.

        gen_addr(node->args->next);
        // RAX is now &x, move it to the next qword.
        //| add rax, 8
        dasm_put(Dst, 1130);
#line 1353 "../../src/codegen.in.c"

        // Store one-past the second argument into &ap.
        pop(REG_UTIL);
        //| mov [RUTIL], rax
        dasm_put(Dst, 105);
#line 1357 "../../src/codegen.in.c"
        return;
      }
#endif

#if X64WIN

      int by_ref_copies_size = 0;
      int stack_args = push_args_win(node, &by_ref_copies_size);
      gen_expr(node->lhs);

      int reg = 0;

      // If the return type is a large struct/union, the caller passes
      // a pointer to a buffer as if it were the first argument.
      if (node->ret_buffer && !type_passed_in_register(node->ty)) {
        pop(dasmargreg[reg++]);
      }

      for (Node* arg = node->args; arg; arg = arg->next) {
        Type* ty = arg->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            if ((type_passed_in_register(ty) && reg < X64WIN_REG_MAX) ||
                (arg->pass_by_reference && reg < X64WIN_REG_MAX)) {
              pop(dasmargreg[reg++]);
            }
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            if (reg < X64WIN_REG_MAX) {
              popf(reg);
              // Varargs requires a copy of fp in gp.
              //| movd Rq(dasmargreg[reg]), xmm(reg)
              dasm_put(Dst, 1135, (reg), (dasmargreg[reg]));
#line 1392 "../../src/codegen.in.c"
              ++reg;
            }
            break;
          default:
            if (reg < X64WIN_REG_MAX) {
              pop(dasmargreg[reg++]);
            }
        }
      }

      //| sub rsp, PARAMETER_SAVE_SIZE
      //| mov r10, rax
      //| call r10
      //| add rsp, stack_args*8 + PARAMETER_SAVE_SIZE + by_ref_copies_size
      dasm_put(Dst, 1145, PARAMETER_SAVE_SIZE, stack_args*8 + PARAMETER_SAVE_SIZE + by_ref_copies_size);
#line 1406 "../../src/codegen.in.c"
      if (by_ref_copies_size > 0) {
        //| pop r11
        dasm_put(Dst, 1162);
#line 1408 "../../src/codegen.in.c"
      }

      C(depth) -= by_ref_copies_size / 8;
      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 174);
#line 1419 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 174);
#line 1423 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 170);
#line 1425 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 182);
#line 1430 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 178);
#line 1432 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned it's actually
      // returned in rax, so copy it back into the return buffer where we're
      // expecting it.
      if (node->ret_buffer && type_passed_in_register(node->ty)) {
        //| mov [rbp+node->ret_buffer->offset], rax
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 1165, node->ret_buffer->offset, node->ret_buffer->offset);
#line 1442 "../../src/codegen.in.c"
      }

#else  // SysV

      int stack_args = push_args_sysv(node);
      gen_expr(node->lhs);

      int gp = 0, fp = 0;

      // If the return type is a large struct/union, the caller passes
      // a pointer to a buffer as if it were the first argument.
      if (node->ret_buffer && node->ty->size > 16) {
        pop(dasmargreg[gp++]);
      }

      for (Node* arg = node->args; arg; arg = arg->next) {
        Type* ty = arg->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            if (ty->size > 16)
              continue;

            bool fp1 = has_flonum1(ty);
            bool fp2 = has_flonum2(ty);

            if (fp + fp1 + fp2 < SYSV_FP_MAX && gp + !fp1 + !fp2 < SYSV_GP_MAX) {
              if (fp1) {
                popf(fp++);
              } else {
                pop(dasmargreg[gp++]);
              }

              if (ty->size > 8) {
                if (fp2) {
                  popf(fp++);
                } else {
                  pop(dasmargreg[gp++]);
                }
              }
            }
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            if (fp < SYSV_FP_MAX)
              popf(fp++);
            break;
          case TY_LDOUBLE:
            break;
          default:
            if (gp < SYSV_GP_MAX) {
              pop(dasmargreg[gp++]);
            }
        }
      }

      //| mov r10, rax
      //| mov rax, fp
      //| call r10
      //| add rsp, stack_args*8
      dasm_put(Dst, 1174, fp, stack_args*8);
#line 1503 "../../src/codegen.in.c"

      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 174);
#line 1512 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 174);
#line 1516 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 170);
#line 1518 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 182);
#line 1523 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 178);
#line 1525 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned
      // using up to two registers.
      if (node->ret_buffer && node->ty->size <= 16) {
        copy_ret_buffer(node->ret_buffer);
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 114, node->ret_buffer->offset);
#line 1534 "../../src/codegen.in.c"
      }

#endif  // SysV

      return;
    }
    case ND_LABEL_VAL:
      //| lea rax, [=>node->pc_label]
      dasm_put(Dst, 119, node->pc_label);
#line 1542 "../../src/codegen.in.c"
      return;
    case ND_REFLECT_TYPE_PTR:
      //| mov64 rax, node->reflect_ty;
      dasm_put(Dst, 125, (unsigned int)(node->reflect_ty), (unsigned int)((node->reflect_ty)>>32));
#line 1545 "../../src/codegen.in.c"
      return;
    case ND_CAS:
    case ND_LOCKCE: {
      bool is_locked_ce = node->kind == ND_LOCKCE;

      gen_expr(node->cas_addr);
      push();
      gen_expr(node->cas_new);
      push();
      gen_expr(node->cas_old);
      if (!is_locked_ce) {
        //| mov r8, rax
        dasm_put(Dst, 1004);
#line 1557 "../../src/codegen.in.c"
        load(node->cas_old->ty->base);
      }
      pop(REG_DX);    // new
      pop(REG_UTIL);  // addr

      int sz = node->cas_addr->ty->base->size;
      // dynasm doesn't support cmpxchg, and I didn't grok the encoding yet.
      // Hack in the various bytes for the instructions we want since there's
      // limited forms. RUTILenc is either 0x17 for RDI or 0x11 for RCX
      // depending on whether we're encoding for Windows or SysV.
      switch (sz) {
        case 1:
          // lock cmpxchg BYTE PTR [rdi/rcx], dl
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb0
          //| .byte RUTILenc
          dasm_put(Dst, 1190);
#line 1574 "../../src/codegen.in.c"
          break;
        case 2:
          // lock cmpxchg WORD PTR [rdi/rcx],dx
          //| .byte 0x66
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1196);
#line 1582 "../../src/codegen.in.c"
          break;
        case 4:
          // lock cmpxchg DWORD PTR [rdi/rcx],edx
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1197);
#line 1589 "../../src/codegen.in.c"
          break;
        case 8:
          // lock cmpxchg QWORD PTR [rdi/rcx],rdx
          //| .byte 0xf0
          //| .byte 0x48
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1203);
#line 1597 "../../src/codegen.in.c"
          break;
        default:
          unreachable();
      }
      if (!is_locked_ce) {
        //| sete cl
        //| je >1
        dasm_put(Dst, 1210);
#line 1604 "../../src/codegen.in.c"
        switch (sz) {
          case 1:
            //| mov [r8], al
            dasm_put(Dst, 1218);
#line 1607 "../../src/codegen.in.c"
            break;
          case 2:
            //| mov [r8], ax
            dasm_put(Dst, 1222);
#line 1610 "../../src/codegen.in.c"
            break;
          case 4:
            //| mov [r8], eax
            dasm_put(Dst, 1223);
#line 1613 "../../src/codegen.in.c"
            break;
          case 8:
            //| mov [r8], rax
            dasm_put(Dst, 1227);
#line 1616 "../../src/codegen.in.c"
            break;
          default:
            unreachable();
        }
        //|1:
        //| movzx eax, cl
        dasm_put(Dst, 1231);
#line 1622 "../../src/codegen.in.c"
      }

      return;
    }
    case ND_EXCH: {
      gen_expr(node->lhs);
      push();
      gen_expr(node->rhs);
      pop(REG_UTIL);

      int sz = node->lhs->ty->base->size;
      switch (sz) {
        case 1:
          //| xchg [RUTIL], al
          dasm_put(Dst, 1237);
#line 1636 "../../src/codegen.in.c"
          break;
        case 2:
          //| xchg [RUTIL], ax
          dasm_put(Dst, 1240);
#line 1639 "../../src/codegen.in.c"
          break;
        case 4:
          //| xchg [RUTIL], eax
          dasm_put(Dst, 1241);
#line 1642 "../../src/codegen.in.c"
          break;
        case 8:
          //| xchg [RUTIL], rax
          dasm_put(Dst, 1244);
#line 1645 "../../src/codegen.in.c"
          break;
        default:
          unreachable();
      }
      return;
    }
  }

  switch (node->lhs->ty->kind) {
    case TY_FLOAT:
    case TY_DOUBLE: {
      gen_expr(node->rhs);
      pushf();
      gen_expr(node->lhs);
      popf(1);

      bool is_float = node->lhs->ty->kind == TY_FLOAT;

      switch (node->kind) {
        case ND_ADD:
          if (is_float) {
            //| addss xmm0, xmm1
            dasm_put(Dst, 1248);
#line 1667 "../../src/codegen.in.c"
          } else {
            //| addsd xmm0, xmm1
            dasm_put(Dst, 1254);
#line 1669 "../../src/codegen.in.c"
          }
          return;
        case ND_SUB:
          if (is_float) {
            //| subss xmm0, xmm1
            dasm_put(Dst, 1260);
#line 1674 "../../src/codegen.in.c"
          } else {
            //| subsd xmm0, xmm1
            dasm_put(Dst, 1266);
#line 1676 "../../src/codegen.in.c"
          }
          return;
        case ND_MUL:
          if (is_float) {
            //| mulss xmm0, xmm1
            dasm_put(Dst, 1272);
#line 1681 "../../src/codegen.in.c"
          } else {
            //| mulsd xmm0, xmm1
            dasm_put(Dst, 1278);
#line 1683 "../../src/codegen.in.c"
          }
          return;
        case ND_DIV:
          if (is_float) {
            //| divss xmm0, xmm1
            dasm_put(Dst, 1284);
#line 1688 "../../src/codegen.in.c"
          } else {
            //| divsd xmm0, xmm1
            dasm_put(Dst, 1290);
#line 1690 "../../src/codegen.in.c"
          }
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          if (is_float) {
            //| ucomiss xmm1, xmm0
            dasm_put(Dst, 1296);
#line 1698 "../../src/codegen.in.c"
          } else {
            //| ucomisd xmm1, xmm0
            dasm_put(Dst, 1300);
#line 1700 "../../src/codegen.in.c"
          }

          if (node->kind == ND_EQ) {
            //| sete al
            //| setnp dl
            //| and al, dl
            dasm_put(Dst, 1305);
#line 1706 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            //| setp dl
            //| or al, dl
            dasm_put(Dst, 1314);
#line 1710 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1323);
#line 1712 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1327);
#line 1714 "../../src/codegen.in.c"
          }

          //| and al, 1
          //| movzx rax, al
          dasm_put(Dst, 1331);
#line 1718 "../../src/codegen.in.c"
          return;
      }

      error_tok(node->tok, "invalid expression");
    }
#if !X64WIN
    case TY_LDOUBLE: {
      gen_expr(node->lhs);
      gen_expr(node->rhs);

      switch (node->kind) {
        case ND_ADD:
          //| faddp st1, st0
          dasm_put(Dst, 1338);
#line 1731 "../../src/codegen.in.c"
          return;
        case ND_SUB:
          //| fsubrp st1, st0
          dasm_put(Dst, 1341);
#line 1734 "../../src/codegen.in.c"
          return;
        case ND_MUL:
          //| fmulp st1, st0
          dasm_put(Dst, 1344);
#line 1737 "../../src/codegen.in.c"
          return;
        case ND_DIV:
          //| fdivrp st1, st0
          dasm_put(Dst, 1347);
#line 1740 "../../src/codegen.in.c"
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          //| fcomip st1
          //| fstp st0
          dasm_put(Dst, 1351);
#line 1747 "../../src/codegen.in.c"

          if (node->kind == ND_EQ) {
            //| sete al
            dasm_put(Dst, 1357);
#line 1750 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            dasm_put(Dst, 1361);
#line 1752 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1323);
#line 1754 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1327);
#line 1756 "../../src/codegen.in.c"
          }

          //| movzx rax, al
          dasm_put(Dst, 1070);
#line 1759 "../../src/codegen.in.c"
          return;
      }

      error_tok(node->tok, "invalid expression");
    }
#endif
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop(REG_UTIL);

  bool is_long = node->lhs->ty->kind == TY_LONG || node->lhs->ty->base;

  switch (node->kind) {
    case ND_ADD:
      if (is_long) {
        //| add rax, RUTIL
        dasm_put(Dst, 1365);
#line 1778 "../../src/codegen.in.c"
      } else {
        //| add eax, RUTILd
        dasm_put(Dst, 1366);
#line 1780 "../../src/codegen.in.c"
      }
      return;
    case ND_SUB:
      if (is_long) {
        //| sub rax, RUTIL
        dasm_put(Dst, 1369);
#line 1785 "../../src/codegen.in.c"
      } else {
        //| sub eax, RUTILd
        dasm_put(Dst, 1370);
#line 1787 "../../src/codegen.in.c"
      }
      return;
    case ND_MUL:
      if (is_long) {
        //| imul rax, RUTIL
        dasm_put(Dst, 1373);
#line 1792 "../../src/codegen.in.c"
      } else {
        //| imul eax, RUTILd
        dasm_put(Dst, 1374);
#line 1794 "../../src/codegen.in.c"
      }
      return;
    case ND_DIV:
    case ND_MOD:
      if (node->ty->is_unsigned) {
        if (is_long) {
          //| mov rdx, 0
          //| div RUTIL
          dasm_put(Dst, 1378);
#line 1802 "../../src/codegen.in.c"
        } else {
          //| mov edx, 0
          //| div RUTILd
          dasm_put(Dst, 1391);
#line 1805 "../../src/codegen.in.c"
        }
      } else {
        if (node->lhs->ty->size == 8) {
          //| cqo
          dasm_put(Dst, 1401);
#line 1809 "../../src/codegen.in.c"
        } else {
          //| cdq
          dasm_put(Dst, 1402);
#line 1811 "../../src/codegen.in.c"
        }
        if (is_long) {
          //| idiv RUTIL
          dasm_put(Dst, 1404);
#line 1814 "../../src/codegen.in.c"
        } else {
          //| idiv RUTILd
          dasm_put(Dst, 1405);
#line 1816 "../../src/codegen.in.c"
        }
      }

      if (node->kind == ND_MOD) {
        //| mov rax, rdx
        dasm_put(Dst, 1410);
#line 1821 "../../src/codegen.in.c"
      }
      return;
    case ND_BITAND:
      if (is_long) {
        //| and rax, RUTIL
        dasm_put(Dst, 1414);
#line 1826 "../../src/codegen.in.c"
      } else {
        //| and eax, RUTILd
        dasm_put(Dst, 1415);
#line 1828 "../../src/codegen.in.c"
      }
      return;
    case ND_BITOR:
      if (is_long) {
        //| or rax, RUTIL
        dasm_put(Dst, 1032);
#line 1833 "../../src/codegen.in.c"
      } else {
        //| or eax, RUTILd
        dasm_put(Dst, 1033);
#line 1835 "../../src/codegen.in.c"
      }
      return;
    case ND_BITXOR:
      if (is_long) {
        //| xor rax, RUTIL
        dasm_put(Dst, 1418);
#line 1840 "../../src/codegen.in.c"
      } else {
        //| xor eax, RUTILd
        dasm_put(Dst, 1419);
#line 1842 "../../src/codegen.in.c"
      }
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      if (is_long) {
        //| cmp rax, RUTIL
        dasm_put(Dst, 1422);
#line 1850 "../../src/codegen.in.c"
      } else {
        //| cmp eax, RUTILd
        dasm_put(Dst, 1423);
#line 1852 "../../src/codegen.in.c"
      }

      if (node->kind == ND_EQ) {
        //| sete al
        dasm_put(Dst, 1357);
#line 1856 "../../src/codegen.in.c"
      } else if (node->kind == ND_NE) {
        //| setne al
        dasm_put(Dst, 1361);
#line 1858 "../../src/codegen.in.c"
      } else if (node->kind == ND_LT) {
        if (node->lhs->ty->is_unsigned) {
          //| setb al
          dasm_put(Dst, 1426);
#line 1861 "../../src/codegen.in.c"
        } else {
          //| setl al
          dasm_put(Dst, 1430);
#line 1863 "../../src/codegen.in.c"
        }
      } else if (node->kind == ND_LE) {
        if (node->lhs->ty->is_unsigned) {
          //| setbe al
          dasm_put(Dst, 1434);
#line 1867 "../../src/codegen.in.c"
        } else {
          //| setle al
          dasm_put(Dst, 1438);
#line 1869 "../../src/codegen.in.c"
        }
      }

      //| movzx rax, al
      dasm_put(Dst, 1070);
#line 1873 "../../src/codegen.in.c"
      return;
    case ND_SHL:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1442);
#line 1876 "../../src/codegen.in.c"
      if (is_long) {
        //| shl rax, cl
        dasm_put(Dst, 1446);
#line 1878 "../../src/codegen.in.c"
      } else {
        //| shl eax, cl
        dasm_put(Dst, 1447);
#line 1880 "../../src/codegen.in.c"
      }
      return;
    case ND_SHR:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1442);
#line 1884 "../../src/codegen.in.c"
      if (node->lhs->ty->is_unsigned) {
        if (is_long) {
          //| shr rax, cl
          dasm_put(Dst, 1450);
#line 1887 "../../src/codegen.in.c"
        } else {
          //| shr eax, cl
          dasm_put(Dst, 1451);
#line 1889 "../../src/codegen.in.c"
        }
      } else {
        if (is_long) {
          //| sar rax, cl
          dasm_put(Dst, 1454);
#line 1893 "../../src/codegen.in.c"
        } else {
          //| sar eax, cl
          dasm_put(Dst, 1455);
#line 1895 "../../src/codegen.in.c"
        }
      }
      return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node* node) {
#if X64WIN
  if (user_context->generate_debug_symbols) {
    record_line_syminfo(node->tok->file->file_no, node->tok->line_no, codegen_pclabel());
  }
#endif

  switch (node->kind) {
    case ND_IF: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1058, lelse);
#line 1917 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1062, lend, lelse);
#line 1920 "../../src/codegen.in.c"
      if (node->els)
        gen_stmt(node->els);
      //|=>lend:
      dasm_put(Dst, 0, lend);
#line 1923 "../../src/codegen.in.c"
      return;
    }
    case ND_FOR: {
      if (node->init)
        gen_stmt(node->init);
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 0, lbegin);
#line 1930 "../../src/codegen.in.c"
      if (node->cond) {
        gen_expr(node->cond);
        cmp_zero(node->cond->ty);
        //| je =>node->brk_pc_label
        dasm_put(Dst, 1058, node->brk_pc_label);
#line 1934 "../../src/codegen.in.c"
      }
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 0, node->cont_pc_label);
#line 1937 "../../src/codegen.in.c"
      if (node->inc)
        gen_expr(node->inc);
      //| jmp =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1062, lbegin, node->brk_pc_label);
#line 1941 "../../src/codegen.in.c"
      return;
    }
    case ND_DO: {
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 0, lbegin);
#line 1946 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 0, node->cont_pc_label);
#line 1948 "../../src/codegen.in.c"
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| jne =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1459, lbegin, node->brk_pc_label);
#line 1952 "../../src/codegen.in.c"
      return;
    }
    case ND_SWITCH:
      gen_expr(node->cond);

      for (Node* n = node->case_next; n; n = n->case_next) {
        bool is_long = node->cond->ty->size == 8;

        if (n->begin == n->end) {
          if (is_long) {
            //| cmp rax, n->begin
            dasm_put(Dst, 1464, n->begin);
#line 1963 "../../src/codegen.in.c"
          } else {
            //| cmp eax, n->begin
            dasm_put(Dst, 1465, n->begin);
#line 1965 "../../src/codegen.in.c"
          }
          //| je =>n->pc_label
          dasm_put(Dst, 1058, n->pc_label);
#line 1967 "../../src/codegen.in.c"
          continue;
        }

        // [GNU] Case ranges
        if (is_long) {
          //| mov RUTIL, rax
          //| sub RUTIL, n->begin
          //| cmp RUTIL, n->end - n->begin
          dasm_put(Dst, 1470, n->begin, n->end - n->begin);
#line 1975 "../../src/codegen.in.c"
        } else {
          //| mov RUTILd, eax
          //| sub RUTILd, n->begin
          //| cmp RUTILd, n->end - n->begin
          dasm_put(Dst, 1484, n->begin, n->end - n->begin);
#line 1979 "../../src/codegen.in.c"
        }
        //| jbe =>n->pc_label
        dasm_put(Dst, 1495, n->pc_label);
#line 1981 "../../src/codegen.in.c"
      }

      if (node->default_case) {
        //| jmp =>node->default_case->pc_label
        dasm_put(Dst, 1499, node->default_case->pc_label);
#line 1985 "../../src/codegen.in.c"
      }

      //| jmp =>node->brk_pc_label
      dasm_put(Dst, 1499, node->brk_pc_label);
#line 1988 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->brk_pc_label:
      dasm_put(Dst, 0, node->brk_pc_label);
#line 1990 "../../src/codegen.in.c"
      return;
    case ND_CASE:
      //|=>node->pc_label:
      dasm_put(Dst, 0, node->pc_label);
#line 1993 "../../src/codegen.in.c"
      gen_stmt(node->lhs);
      return;
    case ND_BLOCK:
      for (Node* n = node->body; n; n = n->next)
        gen_stmt(n);
      return;
    case ND_GOTO:
      //| jmp =>node->pc_label
      dasm_put(Dst, 1499, node->pc_label);
#line 2001 "../../src/codegen.in.c"
      return;
    case ND_GOTO_EXPR:
      gen_expr(node->lhs);
      //| jmp rax
      dasm_put(Dst, 1503);
#line 2005 "../../src/codegen.in.c"
      return;
    case ND_LABEL:
      //|=>node->pc_label:
      dasm_put(Dst, 0, node->pc_label);
#line 2008 "../../src/codegen.in.c"
      gen_stmt(node->lhs);
      return;
    case ND_RETURN:
      if (node->lhs) {
        gen_expr(node->lhs);
        Type* ty = node->lhs->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            if (
#if X64WIN
                type_passed_in_register(ty)
#else
                ty->size <= 16
#endif
            ) {
              copy_struct_reg();
            } else {
              copy_struct_mem();
            }
            break;
        }
      }

      //| jmp =>C(current_fn)->dasm_return_label
      dasm_put(Dst, 1499, C(current_fn)->dasm_return_label);
#line 2034 "../../src/codegen.in.c"
      return;
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      return;
    case ND_ASM:
      error_tok(node->tok, "asm statement not supported");
  }

  error_tok(node->tok, "invalid statement");
}

#if X64WIN

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj* prog) {
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    // outaf("--- %s\n", fn->name);

    // The parameter home area starts at 16 above rbp:
    //   ...
    //   stack arg 2 (6th arg)
    //   stack arg 1 (5th arg)
    //   R9 home
    //   R8 home
    //   RDX home
    //   RCX home
    //   return address pushed by call instr
    //   old RBP (for the called function)  <<< RBP after push rbp; mov rbp, rsp
    //   ...
    //   ... stack space used by called function
    //   ...
    //
    // The top of the diagram is addr 0xffffffff.. and the bottom is 0.
    // PUSH decrements RSP and then stores.
    // So, "top" means the highest numbered address corresponding the to root
    // function and bottom moves to the frames for the leaf-ward functions.
    int top = 16;
    int bottom = 8;

    int reg = 0;

    // Assign offsets to pass-by-stack parameters and register homes.
    for (Obj* var = fn->params; var; var = var->next) {
      Type* ty = var->ty;

      switch (ty->kind) {
        case TY_STRUCT:
        case TY_UNION:
          if (!type_passed_in_register(ty)) {
            // If it's too big for a register, then the value we're getting is a
            // pointer to a copy, rather than the actual value, so flag it as
            // such and then either assign a register or stack slot for the
            // reference.
            // outaf("by ref %s\n", var->name);
            // var->passed_by_reference = true;
          }

          // If the pointer to a referenced value or the value itself can be
          // passed in a register then assign here.
          if (reg++ < X64WIN_REG_MAX) {
            var->offset = top;
            // outaf("  assigned reg offset 0x%x\n", var->offset);
            top += 8;
            continue;
          }

          // Otherwise fall through to the stack slot assignment below.
          break;
        case TY_FLOAT:
        case TY_DOUBLE:
          if (reg++ < X64WIN_REG_MAX) {
            var->offset = top;
            top += 8;
            continue;
          }
          break;
        default:
          if (reg++ < X64WIN_REG_MAX) {
            var->offset = top;
            top += 8;
            // outaf("int reg %s at home 0x%x\n", var->name, var->offset);
            continue;
          }
      }

      var->offset = top;
      // outaf("int stack %s at stack 0x%x\n", var->name, var->offset);
      top += MAX(8, var->ty->size);
    }

    // Assign offsets to local variables.
    for (Obj* var = fn->locals; var; var = var->next) {
      if (var->offset) {
        continue;
      }

      int align =
          (var->ty->kind == TY_ARRAY && var->ty->size >= 16) ? MAX(16, var->align) : var->align;

      bottom += var->ty->size;
      bottom = (int)align_to_s(bottom, align);
      var->offset = -bottom;
      // outaf("local %s at -0x%x\n", var->name, -var->offset);
    }

    fn->stack_size = (int)align_to_s(bottom, 16);
  }
}

#else  // SysV

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj* prog) {
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    // If a function has many parameters, some parameters are
    // inevitably passed by stack rather than by register.
    // The first passed-by-stack parameter resides at RBP+16.
    int top = 16;
    int bottom = 0;

    int gp = 0, fp = 0;

    // Assign offsets to pass-by-stack parameters.
    for (Obj* var = fn->params; var; var = var->next) {
      Type* ty = var->ty;

      switch (ty->kind) {
        case TY_STRUCT:
        case TY_UNION:
          if (ty->size <= 8) {
            bool fp1 = has_flonum(ty, 0, 8, 0);
            if (fp + fp1 < SYSV_FP_MAX && gp + !fp1 < SYSV_GP_MAX) {
              fp = fp + fp1;
              gp = gp + !fp1;
              continue;
            }
          } else if (ty->size <= 16) {
            bool fp1 = has_flonum(ty, 0, 8, 0);
            bool fp2 = has_flonum(ty, 8, 16, 8);
            if (fp + fp1 + fp2 < SYSV_FP_MAX && gp + !fp1 + !fp2 < SYSV_GP_MAX) {
              fp = fp + fp1 + fp2;
              gp = gp + !fp1 + !fp2;
              continue;
            }
          }
          break;
        case TY_FLOAT:
        case TY_DOUBLE:
          if (fp++ < SYSV_FP_MAX)
            continue;
          break;
        case TY_LDOUBLE:
          break;
        default:
          if (gp++ < SYSV_GP_MAX)
            continue;
      }

      top = align_to_s(top, 8);
      var->offset = top;
      top += var->ty->size;
    }

    // Assign offsets to pass-by-register parameters and local variables.
    for (Obj* var = fn->locals; var; var = var->next) {
      if (var->offset)
        continue;

      // AMD64 System V ABI has a special alignment rule for an array of
      // length at least 16 bytes. We need to align such array to at least
      // 16-byte boundaries. See p.14 of
      // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
      int align =
          (var->ty->kind == TY_ARRAY && var->ty->size >= 16) ? MAX(16, var->align) : var->align;

      bottom += var->ty->size;
      bottom = align_to_s(bottom, align);
      var->offset = -bottom;
    }

    fn->stack_size = align_to_s(bottom, 16);
  }
}

#endif  // SysV

static void linkfixup_push(FileLinkData* fld, char* target, char* fixup, int addend) {
  if (!fld->fixups) {
    fld->fixups = calloc(8, sizeof(LinkFixup));
    fld->fcap = 8;
  }

  if (fld->fcap == fld->flen) {
    fld->fixups = realloc(fld->fixups, sizeof(LinkFixup) * fld->fcap * 2);
    fld->fcap *= 2;
  }

  fld->fixups[fld->flen++] = (LinkFixup){fixup, strdup(target), addend};
}

static void emit_data(Obj* prog) {
  for (Obj* var = prog; var; var = var->next) {
    // outaf("var->name %s %d %d %d %d\n", var->name, var->is_function, var->is_definition,
    // var->is_static, var->is_tentative);
    if (var->is_function)
      continue;

    if (!var->is_definition) {
      continue;
    }

    int align =
        (var->ty->kind == TY_ARRAY && var->ty->size >= 16) ? MAX(16, var->align) : var->align;

    // - rodata, always free existing entry in either static/extern
    // global_data, and then recreate and reinitialize
    //
    // - if writeable data has an entry, it shouldn't be recreated. the
    // dyo version doesn't reprocess kTypeInitializerDataRelocation or
    // kTypeInitializerCodeRelocation; that's possibly a bug, but it'll
    // need some testing to get a case where it comes up.
    //
    // TODO: if it changes from static to extern, is it the same
    // variable? currently they're separate, so a switch causes a
    // reinit, a leak, and some confusion.
    //
    // can't easily make a large single data segment allocation for all
    // data because 1) the rodata change size link-over-link (put in
    // codeseg?); 2) wdata don't move or reinit, but new ones get added
    // as code evolves and we can't blow away or move the old ones.
    //
    // for now, just continue with individual regular aligned_allocate
    // for all data objects and maintain their addresses here.

    UserContext* uc = user_context;
    // bool was_freed = false;
    size_t idx = var->is_static ? C(file_index) : uc->num_files;
    void* prev = hashmap_get(&user_context->global_data[idx], var->name);
    if (prev) {
      if (var->is_rodata) {
        aligned_free(prev);
        // was_freed = true;
      } else {
        // data already created and initialized, don't reinit.
        continue;
      }
    }

    void* global_data = aligned_allocate(var->ty->size, align);
    memset(global_data, 0, var->ty->size);

    // TODO: Is this wrong (or above)? If writable |x| in one file
    // already existed and |x| in another is added, then it'll be
    // silently ignored. If it's rodata it'll be silently replaced here
    // by getting thrown away above and then recreated.
    // Need to figure out where/how to have a duplicate symbol check.
#if 0
      if (!was_freed) {
        void* prev = hashmap_get(&uc->global_data[idx], strings.data[name_index]);
        if (prev) {
          outaf("duplicated symbol: %s\n", strings.data[name_index]);
          goto fail;
        }
      }
#endif
    // TODO: intern
    hashmap_put(&uc->global_data[idx], strdup(var->name), global_data);

    char* fillp = global_data;
    FileLinkData* fld = &uc->files[C(file_index)];

    // .data or .tdata
    if (var->init_data) {
      Relocation* rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          assert(!(rel->string_label && rel->internal_code_label));  // Shouldn't be both.
          assert(rel->string_label ||
                 rel->internal_code_label);  // But should be at least one if we're here.

          if (rel->string_label) {
            linkfixup_push(fld, *rel->string_label, fillp, rel->addend);
          } else {
            int offset = dasm_getpclabel(&C(dynasm), *rel->internal_code_label);
            *((uintptr_t*)fillp) = (uintptr_t)(fld->codeseg_base_address + offset + rel->addend);
          }

          rel = rel->next;
          pos += 8;
          fillp += 8;
        } else {
          *fillp++ = var->init_data[pos++];
        }
      }

      continue;
    }

    // If no init_data, then already allocated and cleared (.bss).
  }
}

static void store_fp(int r, int offset, int sz) {
  switch (sz) {
    case 4:
      //| movss dword [rbp+offset], xmm(r)
      dasm_put(Dst, 684, (r), offset);
#line 2347 "../../src/codegen.in.c"
      return;
    case 8:
      //| movsd qword [rbp+offset], xmm(r)
      dasm_put(Dst, 695, (r), offset);
#line 2350 "../../src/codegen.in.c"
      return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
    case 1:
      //| mov [rbp+offset], Rb(dasmargreg[r])
      dasm_put(Dst, 1507, (dasmargreg[r]), offset);
#line 2359 "../../src/codegen.in.c"
      return;
    case 2:
      //| mov [rbp+offset], Rw(dasmargreg[r])
      dasm_put(Dst, 1515, (dasmargreg[r]), offset);
#line 2362 "../../src/codegen.in.c"
      return;
      return;
    case 4:
      //| mov [rbp+offset], Rd(dasmargreg[r])
      dasm_put(Dst, 1516, (dasmargreg[r]), offset);
#line 2366 "../../src/codegen.in.c"
      return;
    case 8:
      //| mov [rbp+offset], Rq(dasmargreg[r])
      dasm_put(Dst, 1524, (dasmargreg[r]), offset);
#line 2369 "../../src/codegen.in.c"
      return;
    default:
      for (int i = 0; i < sz; i++) {
        //| mov [rbp+offset+i], Rb(dasmargreg[r])
        //| shr Rq(dasmargreg[r]), 8
        dasm_put(Dst, 706, (dasmargreg[r]), offset+i, (dasmargreg[r]));
#line 2374 "../../src/codegen.in.c"
      }
      return;
  }
}

#if X64WIN
extern int __chkstk(void);
#endif  // X64WIN

static void emit_text(Obj* prog) {
  // Preallocate the dasm labels so they can be used in functions out of order.
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    fn->dasm_return_label = codegen_pclabel();
    fn->dasm_entry_label = codegen_pclabel();
    fn->dasm_end_of_function_label = codegen_pclabel();
    fn->dasm_unwind_info_label = codegen_pclabel();
  }

  //| .code
  dasm_put(Dst, 1532);
#line 2396 "../../src/codegen.in.c"

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    //|=>fn->dasm_entry_label:
    dasm_put(Dst, 0, fn->dasm_entry_label);
#line 2402 "../../src/codegen.in.c"

    C(current_fn) = fn;

#if X64WIN
    record_line_syminfo(fn->ty->name->file->file_no, fn->ty->name->line_no, codegen_pclabel());
#endif

    // outaf("---- %s\n", fn->name);

    // Prologue
    //| push rbp
    //| mov rbp, rsp
    dasm_put(Dst, 1534);
#line 2414 "../../src/codegen.in.c"

#if X64WIN
    // Stack probe on Windows if necessary. The MSDN reference for __chkstk says
    // it's only necessary beyond 8k for x64, but cl does it at 4k.
    if (fn->stack_size >= 4096) {
      //| mov rax, fn->stack_size
      dasm_put(Dst, 926, fn->stack_size);
#line 2420 "../../src/codegen.in.c"
      int fixup_location = codegen_pclabel();
      strintarray_push(&C(fixups), (StringInt){"__chkstk", fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
      //|=>fixup_location:
      //| mov64 r10, 0xc0dec0dec0dec0de
      dasm_put(Dst, 1539, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 2428 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      //| call r10
      //| sub rsp, rax
      dasm_put(Dst, 1545);
#line 2433 "../../src/codegen.in.c"

      // TODO: pdata emission
    } else
#endif

    {
      //| sub rsp, fn->stack_size
      dasm_put(Dst, 619, fn->stack_size);
#line 2440 "../../src/codegen.in.c"

      // TODO: add a label here to assert that the prolog size is as expected

#if X64WIN
      // RtlAddFunctionTable() requires these to be at an offset with the same
      // base as the function offsets, so we need to emit these into the main
      // codeseg allocation, rather than just allocating them separately, since
      // we can't easily guarantee a <4G offset to them otherwise.

      // Unfortunately, we can't build another section with this as dynasm
      // doesn't seem to allow resolving these offsets, so this is done later
      //| .dword =>fn->dasm_entry_label
      //| .dword =>fn->dasm_end_of_function_label
      //| .dword =>fn->dasm_unwind_info_label

      // TODO: probably need to relocate and add info about r11 used as a base
      // for the value stack copies, and also rdi pushed for memsets.

      // https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170
      enum {
        UWOP_PUSH_NONVOL = 0,
        UWOP_ALLOC_LARGE = 1,
        UWOP_ALLOC_SMALL = 2,
        UWOP_SET_FPREG = 3,
      };

      // These are the UNWIND_INFO structure that is referenced by the third
      // element of RUNTIME_FUNCTION.
      //| .pdata
      dasm_put(Dst, 1553);
#line 2469 "../../src/codegen.in.c"
      // This takes care of cases where CountOfCodes is odd.
      //| .align 4
      //|=>fn->dasm_unwind_info_label:
      //| .byte 1  /* Version:3 (1) and Flags:5 (0) */
      dasm_put(Dst, 1555, fn->dasm_unwind_info_label, 1  /* Version:3 (1) and Flags:5 (0) */);
#line 2473 "../../src/codegen.in.c"
      bool small_stack = fn->stack_size / 8 - 1 <= 15;
      if (small_stack) {
        // We just happen to "know" this is the form used for small stack sizes.
        // xxxxxxxxxxxx0000 55                   push        rbp
        // xxxxxxxxxxxx0001 48 89 E5             mov         rbp,rsp
        // xxxxxxxxxxxx0004 48 83 EC 10          sub         rsp,10h
        // xxxxxxxxxxxx0009 ...
        //| .byte 8  /* SizeOfProlog */
        //| .byte 3  /* CountOfCodes */
        dasm_put(Dst, 1560, 8  /* SizeOfProlog */, 3  /* CountOfCodes */);
#line 2482 "../../src/codegen.in.c"
      } else {
        // And this one for larger reservations.
        // xxxxxxxxxxxx0000 55                   push        rbp
        // xxxxxxxxxxxx0001 48 89 E5             mov         rbp,rsp
        // xxxxxxxxxxxx0004 48 81 EC B0 01 00 00 sub         rsp,1B0h
        // xxxxxxxxxxxx000b ...
        //| .byte 11  /* SizeOfProlog */
        //| .byte 4  /* CountOfCodes */
        dasm_put(Dst, 1560, 11  /* SizeOfProlog */, 4  /* CountOfCodes */);
#line 2490 "../../src/codegen.in.c"
      }
      //| .byte 5  /* FrameRegister:4 (RBP) | FrameOffset:4: 0 offset */
      dasm_put(Dst, 983, 5  /* FrameRegister:4 (RBP) | FrameOffset:4: 0 offset */);
#line 2492 "../../src/codegen.in.c"

      if (small_stack) {
        //| .byte 8  /* CodeOffset */
        //| .byte UWOP_ALLOC_SMALL | (((unsigned char)((fn->stack_size / 8) - 1)) << 4)
        dasm_put(Dst, 1560, 8  /* CodeOffset */, UWOP_ALLOC_SMALL | (((unsigned char)((fn->stack_size / 8) - 1)) << 4));
#line 2496 "../../src/codegen.in.c"
      } else {
        //| .byte 11  /* CodeOffset */
        dasm_put(Dst, 983, 11  /* CodeOffset */);
#line 2498 "../../src/codegen.in.c"
        assert(fn->stack_size / 8 <= 65535 && "todo; not UWOP_ALLOC_LARGE 0-style");
        //| .byte UWOP_ALLOC_LARGE
        //| .word fn->stack_size / 8
        dasm_put(Dst, 1563, UWOP_ALLOC_LARGE, fn->stack_size / 8);
#line 2501 "../../src/codegen.in.c"
      }
      //| .byte 4  /* CodeOffset */
      //| .byte UWOP_SET_FPREG
      //| .byte 1  /* CodeOffset */
      //| .byte UWOP_PUSH_NONVOL | (5 /* RBP */ << 4)
      dasm_put(Dst, 1566, 4  /* CodeOffset */, UWOP_SET_FPREG, 1  /* CodeOffset */, UWOP_PUSH_NONVOL | (5 /* RBP */ << 4));
#line 2506 "../../src/codegen.in.c"

      //| .code
      dasm_put(Dst, 1532);
#line 2508 "../../src/codegen.in.c"
#endif
    }

    //| mov [rbp+fn->alloca_bottom->offset], rsp
    dasm_put(Dst, 1571, fn->alloca_bottom->offset);
#line 2512 "../../src/codegen.in.c"

#if !X64WIN
    // Save arg registers if function is variadic
    if (fn->va_area) {
      int gp = 0, fp = 0;
      for (Obj* var = fn->params; var; var = var->next) {
        if (is_flonum(var->ty))
          fp++;
        else
          gp++;
      }

      int off = fn->va_area->offset;

      // va_elem
      //| mov dword [rbp+off], gp*8            // gp_offset
      //| mov dword [rbp+off+4], fp * 8 + 48   // fp_offset
      //| mov [rbp+off+8], rbp                 // overflow_arg_area
      //| add qword [rbp+off+8], 16
      //| mov [rbp+off+16], rbp                // reg_save_area
      //| add qword [rbp+off+16], off+24
      dasm_put(Dst, 1576, off, gp*8, off+4, fp * 8 + 48, off+8, off+8, off+16, off+16, off+24);
#line 2533 "../../src/codegen.in.c"

      // __reg_save_area__
      //| mov [rbp + off + 24], rdi
      //| mov [rbp + off + 32], rsi
      //| mov [rbp + off + 40], rdx
      //| mov [rbp + off + 48], rcx
      //| mov [rbp + off + 56], r8
      //| mov [rbp + off + 64], r9
      //| movsd qword [rbp + off + 72], xmm0
      //| movsd qword [rbp + off + 80], xmm1
      //| movsd qword [rbp + off + 88], xmm2
      //| movsd qword [rbp + off + 96], xmm3
      //| movsd qword [rbp + off + 104], xmm4
      //| movsd qword [rbp + off + 112], xmm5
      //| movsd qword [rbp + off + 120], xmm6
      //| movsd qword [rbp + off + 128], xmm7
      dasm_put(Dst, 1603, off + 24, off + 32, off + 40, off + 48, off + 56, off + 64, off + 72, off + 80, off + 88, off + 96, off + 104, off + 112, off + 120, off + 128);
#line 2549 "../../src/codegen.in.c"
    }
#endif

#if X64WIN
    // If variadic, we have to store all registers; floats will have been
    // duplicated into the integer registers.
    if (fn->ty->is_variadic) {
      //| mov [rbp + 16], CARG1
      //| mov [rbp + 24], CARG2
      //| mov [rbp + 32], CARG3
      //| mov [rbp + 40], CARG4
      dasm_put(Dst, 1676, 16, 24, 32, 40);
#line 2560 "../../src/codegen.in.c"
    } else {
      // Save passed-by-register arguments to the stack
      int reg = 0;
      for (Obj* var = fn->params; var; var = var->next) {
        if (var->offset >= 16 + PARAMETER_SAVE_SIZE)
          continue;

        Type* ty = var->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            // It's either small and so passed in a register, or isn't and then
            // we're instead storing the pointer to the larger struct.
            if (type_passed_in_register(ty)) {
              store_gp(reg++, var->offset, ty->size);
            } else {
              store_gp(reg++, var->offset, 8);
            }
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            store_fp(reg++, var->offset, ty->size);
            break;
          default:
            store_gp(reg++, var->offset, ty->size);
            break;
        }
      }
    }
#else
    // Save passed-by-register arguments to the stack
    int gp = 0, fp = 0;
    for (Obj* var = fn->params; var; var = var->next) {
      if (var->offset > 0)
        continue;

      Type* ty = var->ty;

      switch (ty->kind) {
        case TY_STRUCT:
        case TY_UNION:
          assert(ty->size <= 16);
          if (has_flonum(ty, 0, 8, 0))
            store_fp(fp++, var->offset, MIN(8, ty->size));
          else
            store_gp(gp++, var->offset, MIN(8, ty->size));

          if (ty->size > 8) {
            if (has_flonum(ty, 8, 16, 0))
              store_fp(fp++, var->offset + 8, ty->size - 8);
            else
              store_gp(gp++, var->offset + 8, ty->size - 8);
          }
          break;
        case TY_FLOAT:
        case TY_DOUBLE:
          store_fp(fp++, var->offset, ty->size);
          break;
        default:
          store_gp(gp++, var->offset, ty->size);
      }
    }
#endif

    // Emit code
    gen_stmt(fn->body);
    assert(C(depth) == 0);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(fn->name, "main") == 0) {
      //| mov rax, 0
      dasm_put(Dst, 736);
#line 2635 "../../src/codegen.in.c"
    }

    // Epilogue
    //|=>fn->dasm_return_label:
    dasm_put(Dst, 0, fn->dasm_return_label);
#line 2639 "../../src/codegen.in.c"
#if X64WIN
    // https://learn.microsoft.com/en-us/cpp/build/prolog-and-epilog?view=msvc-170#epilog-code
    // says this the required form to recognize an epilog.
    //| lea rsp, [rbp]
    dasm_put(Dst, 1693);
#line 2643 "../../src/codegen.in.c"
#else
    //| mov rsp, rbp
    dasm_put(Dst, 1698);
#line 2645 "../../src/codegen.in.c"
#endif
    //| pop rbp
    //| ret
    dasm_put(Dst, 1703);
#line 2648 "../../src/codegen.in.c"

    //|=>fn->dasm_end_of_function_label:
    dasm_put(Dst, 0, fn->dasm_end_of_function_label);
#line 2650 "../../src/codegen.in.c"
  }
}

static void fill_out_text_exports(Obj* prog, char* codeseg_base_address) {
  // per-file from any previous need to be cleared out for this round.
  hashmap_clear_manual_key_owned_value_unowned(&user_context->exports[C(file_index)]);

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    int offset = dasm_getpclabel(&C(dynasm), fn->dasm_entry_label);
    size_t idx = fn->is_static ? C(file_index) : user_context->num_files;
    hashmap_put(&user_context->exports[idx], strdup(fn->name), codeseg_base_address + offset);
  }
}

static void free_link_fixups(FileLinkData* fld) {
  for (int i = 0; i < fld->flen; ++i) {
    free(fld->fixups[i].name);
  }
  free(fld->fixups);
  fld->fixups = NULL;
  fld->flen = 0;
  fld->fcap = 0;
}

static void fill_out_fixups(FileLinkData* fld) {
  for (int i = 0; i < C(fixups).len; ++i) {
    int offset = dasm_getpclabel(&C(dynasm), C(fixups).data[i].i);
    // +2 is a hack taking advantage of the fact that import fixups are always
    // of the form `mov64 rax, <ADDR>` which is encoded as:
    //   48 B8 <8 byte address>
    // so skip over the mov64 prefix and point directly at the address to be
    // slapped into place.
    offset += 2;

    char* fixup = fld->codeseg_base_address + offset;
    linkfixup_push(fld, C(fixups).data[i].str, fixup, /*addend=*/0);
  }
}

#if X64WIN

typedef struct RuntimeFunction {
  unsigned long BeginAddress;
  unsigned long EndAddress;
  unsigned long UnwindData;
} RuntimeFunction;

static void emit_symbols_and_exception_function_table(Obj* prog,
                                                      char* base_addr,
                                                      int pdata_start_offset,
                                                      int pdata_end_offset) {
  int func_count = 0;
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    ++func_count;
  }

  size_t alloc_size = (sizeof(RuntimeFunction) * func_count);

  unregister_and_free_function_table_data(user_context);
  char* function_table_data = malloc(alloc_size);
  user_context->function_table_data = function_table_data;
  char* pfuncs = function_table_data;

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    RuntimeFunction* rf = (RuntimeFunction*)pfuncs;
    int func_start_offset = dasm_getpclabel(&C(dynasm), fn->dasm_entry_label);
    rf->BeginAddress = func_start_offset;
    rf->EndAddress = dasm_getpclabel(&C(dynasm), fn->dasm_end_of_function_label);
    rf->UnwindData = dasm_getpclabel(&C(dynasm), fn->dasm_unwind_info_label);
    pfuncs += sizeof(RuntimeFunction);

    if (user_context->generate_debug_symbols) {
      DbpFunctionSymbol* dbp_func_sym =
          dbp_add_function_symbol(user_context->dbp_ctx, fn->name, fn->ty->name->filename,
                                  dasm_getpclabel(&C(dynasm), fn->dasm_entry_label),
                                  dasm_getpclabel(&C(dynasm), fn->dasm_end_of_function_label));
      for (int i = 0; i < fn->file_line_label_data.len; ++i) {
        // TODO: ignoring file index, might not be needed unless something got
        // inlined (which doesn't happen). Maybe there's a macro case that could
        // cause it already though?
        int offset = dasm_getpclabel(&C(dynasm), fn->file_line_label_data.data[i].c);
        dbp_add_line_mapping(user_context->dbp_ctx, dbp_func_sym, offset,
                             fn->file_line_label_data.data[i].b);
      }
    }
  }

  if (user_context->generate_debug_symbols) {
    char* unwind_base = base_addr + pdata_start_offset;
    size_t unwind_len = pdata_end_offset - pdata_start_offset;
    DbpExceptionTables exception_tables = {
        .pdata = (DbpRUNTIME_FUNCTION*)user_context->function_table_data,
        .num_pdata_entries = func_count,
        .unwind_info = (unsigned char*)unwind_base,
        .unwind_info_byte_length = unwind_len,
    };
    dbp_ready_to_execute(user_context->dbp_ctx, &exception_tables);
  }

  register_function_table_data(user_context, func_count, base_addr);
}

#else  // !X64WIN

static void emit_symbols_and_exception_function_table(Obj* prog,
                                                      char* base_addr,
                                                      int pdata_start_offset,
                                                      int pdata_end_offset) {
  (void)prog;
  (void)base_addr;
  (void)pdata_start_offset;
  (void)pdata_end_offset;
}

#endif

static void codegen_init(void) {
  dasm_init(&C(dynasm), DASM_MAXSECTION);
  dasm_growpc(&C(dynasm), 1 << 16);  // Arbitrary number to avoid lots of reallocs of that array.

  C(numlabels) = 1;
}

static void codegen(Obj* prog, size_t file_index) {
  C(file_index) = file_index;

  void* globals[dynasm_globals_MAX + 1];
  dasm_setupglobal(&C(dynasm), globals, dynasm_globals_MAX + 1);

  dasm_setup(&C(dynasm), dynasm_actions);

  //| .pdata
  dasm_put(Dst, 1553);
#line 2791 "../../src/codegen.in.c"
  int start_of_pdata = codegen_pclabel();
  //|.align 4
  //|=>start_of_pdata:
  //| .code
  dasm_put(Dst, 1706, start_of_pdata);
#line 2795 "../../src/codegen.in.c"

  assign_lvar_offsets(prog);
  emit_text(prog);

  //| .pdata
  dasm_put(Dst, 1553);
#line 2800 "../../src/codegen.in.c"
  int end_of_pdata = codegen_pclabel();
  //|=>end_of_pdata:
  //| .code
  dasm_put(Dst, 1708, end_of_pdata);
#line 2803 "../../src/codegen.in.c"

  size_t code_size;
  dasm_link(&C(dynasm), &code_size);

  FileLinkData* fld = &user_context->files[C(file_index)];
  if (fld->codeseg_base_address) {
    free_executable_memory(fld->codeseg_base_address, fld->codeseg_size);
  }
  // VirtualAlloc and mmap don't accept 0.
  if (code_size == 0)
    code_size = 1;
  unsigned int page_sized = (unsigned int)align_to_u(code_size, get_page_size());

  fld->codeseg_size = page_sized;
#if X64WIN
  if (user_context->generate_debug_symbols) {
    user_context->dbp_ctx = dbp_create(fld->codeseg_size, get_temp_pdb_filename(AL_Compile));
    fld->codeseg_base_address = dbp_get_image_base(user_context->dbp_ctx);
  } else {
    fld->codeseg_base_address = allocate_writable_memory(page_sized);
  }
#else
  fld->codeseg_base_address = allocate_writable_memory(page_sized);
#endif
  // outaf("code_size: %zu, page_sized: %zu\n", code_size, page_sized);

  fill_out_text_exports(prog, fld->codeseg_base_address);

  free_link_fixups(fld);
  emit_data(prog);  // This needs to point into code for fixups, so has to go late-ish.
  fill_out_fixups(fld);

  dasm_encode(&C(dynasm), fld->codeseg_base_address);

#if 0
  FILE* f = fopen("code.raw", "wb");
  fwrite(fld->codeseg_base_address, code_size, 1, f);
  fclose(f);
  system("ndisasm -b64 code.raw");
#endif

  int check_result = dasm_checkstep(&C(dynasm), 0);
  if (check_result != DASM_S_OK) {
    outaf("check_result: 0x%08x\n", check_result);
    ABORT("dasm_checkstep failed");
  }

  emit_symbols_and_exception_function_table(prog, fld->codeseg_base_address,
                                            dasm_getpclabel(&C(dynasm), start_of_pdata),
                                            dasm_getpclabel(&C(dynasm), end_of_pdata));

  codegen_free();
}

// This can be called after a longjmp in update.
static void codegen_free(void) {
  if (C(dynasm)) {
    dasm_free(&C(dynasm));
  }
}
//
// END OF codegen.w.c
//
#else // ^^^ X64WIN / !X64WIN vvv
#undef C
#undef L
#undef VOID
//
// START OF codegen.l.c
//
/*
** This file has been pre-processed with DynASM.
** https://luajit.org/dynasm.html
** DynASM version 1.5.0, DynASM x64 version 1.5.0
** DO NOT EDIT! The original file is in "../../src/codegen.in.c".
*/

#line 1 "../../src/codegen.in.c"

#define C(x) compiler_state.codegen__##x

#define DASM_CHECKS 1

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4127)
#pragma warning(disable : 4244)
#endif
#ifdef _MSC_VER
#pragma warning(pop)
#endif


//| .arch x64
#if DASM_VERSION != 10500
#error "Version mismatch between DynASM and included encoding engine"
#endif
#line 21 "../../src/codegen.in.c"
//| .section code, pdata
#define DASM_SECTION_CODE	0
#define DASM_SECTION_PDATA	1
#define DASM_MAXSECTION		2
#line 22 "../../src/codegen.in.c"
//| .actionlist dynasm_actions
static const unsigned char dynasm_actions[1720] = {
  249,255,80,255,64,88,240,42,255,72,131,252,236,8,252,242,15,17,4,36,255,252,
  242,64,15,16,4,240,140,36,72,131,196,8,255,252,243,15,16,0,255,252,242,15,
  16,0,255,219,40,255,15,182,0,255,15,190,0,255,15,183,0,255,15,191,0,255,72,
  99,0,255,72,139,0,255,68,138,128,233,68,136,135,233,255,252,243,15,17,7,255,
  252,242,15,17,7,255,219,63,255,136,7,255,102,137,7,255,72,137,7,255,72,139,
  133,233,255,72,141,133,233,255,72,141,5,245,255,249,72,184,237,237,255,72,
  129,192,239,255,15,87,201,15,46,193,255,102,15,87,201,102,15,46,193,255,217,
  252,238,223,232,221,216,255,131,252,248,0,255,72,131,252,248,0,255,15,190,
  192,255,15,182,192,255,15,191,192,255,15,183,192,255,252,243,15,42,192,255,
  72,99,192,255,252,242,15,42,192,255,137,68,36,252,252,219,68,36,252,252,255,
  137,192,252,243,72,15,42,192,255,137,192,255,137,192,252,242,72,15,42,192,
  255,137,192,72,137,68,36,252,248,223,108,36,252,248,255,72,133,192,15,136,
  244,247,102,15,252,239,192,252,242,72,15,42,192,252,233,244,248,248,1,72,
  137,199,131,224,1,102,15,252,239,192,72,209,252,239,72,9,199,252,242,72,15,
  42,199,252,242,15,88,192,248,2,255,72,137,68,36,252,248,223,108,36,252,248,
  72,133,192,15,137,244,247,184,0,0,128,95,137,68,36,252,252,216,68,36,252,
  252,248,1,255,252,243,15,44,192,15,190,192,255,252,243,15,44,192,15,182,192,
  255,252,243,15,44,192,15,191,192,255,252,243,15,44,192,15,183,192,255,252,
  243,15,44,192,255,252,243,72,15,44,192,255,252,243,15,90,192,255,252,243,
  15,17,68,36,252,252,217,68,36,252,252,255,252,242,15,44,192,15,190,192,255,
  252,242,15,44,192,15,182,192,255,252,242,15,44,192,15,191,192,255,252,242,
  15,44,192,15,183,192,255,252,242,15,44,192,255,252,242,72,15,44,192,255,252,
  242,15,90,192,255,252,242,15,17,68,36,252,248,221,68,36,252,248,255,217,124,
  36,252,246,15,183,68,36,252,246,128,204,12,102,137,68,36,252,244,217,108,
  36,252,244,255,219,92,36,232,217,108,36,252,246,15,191,68,36,232,255,219,
  92,36,232,217,108,36,252,246,15,183,68,36,232,37,252,255,0,0,0,255,219,92,
  36,232,217,108,36,252,246,15,183,68,36,232,255,219,92,36,232,217,108,36,252,
  246,139,68,36,232,255,223,124,36,232,217,108,36,252,246,72,139,68,36,232,
  255,217,92,36,252,248,252,243,15,16,68,36,252,248,255,221,92,36,252,248,252,
  242,15,16,68,36,252,248,255,15,149,208,15,182,192,255,72,129,252,236,239,
  255,68,138,144,233,68,136,148,253,36,233,255,73,141,131,233,255,65,83,73,
  137,227,255,72,131,252,236,8,255,72,131,252,236,16,219,60,36,255,252,243,
  15,17,133,233,255,252,242,15,17,133,233,255,136,133,233,72,193,232,8,255,
  252,243,64,15,17,133,253,240,140,233,255,252,242,64,15,17,133,253,240,140,
  233,255,64,136,133,253,240,131,233,72,193,232,240,35,8,255,72,137,199,255,
  252,243,15,16,7,255,252,242,15,16,7,255,72,199,192,0,0,0,0,255,72,193,224,
  8,102,139,135,233,255,252,243,64,15,16,71,240,140,8,255,252,242,64,15,16,
  71,240,140,8,255,72,199,192,240,35,0,0,0,0,255,72,193,224,240,35,8,64,138,
  135,253,240,131,233,255,72,139,189,233,255,138,144,233,136,151,233,255,72,
  131,199,15,131,231,252,240,255,72,139,141,233,72,41,225,72,137,224,72,41,
  252,252,72,137,226,248,1,72,131,252,249,0,15,132,244,248,68,138,0,68,136,
  2,72,252,255,194,72,252,255,192,72,252,255,201,252,233,244,1,248,2,255,72,
  139,133,233,72,41,252,248,72,137,133,233,255,184,237,102,72,15,110,192,255,
  72,184,237,237,102,72,15,110,192,255,72,184,237,237,72,137,68,36,252,240,
  72,184,237,237,72,137,68,36,252,248,219,108,36,252,240,255,72,199,192,237,
  255,72,199,192,1,0,0,0,72,193,224,31,102,72,15,110,200,15,87,193,255,72,199,
  192,1,0,0,0,72,193,224,63,102,72,15,110,200,102,15,87,193,255,217,224,255,
  72,252,247,216,255,72,193,224,235,255,72,193,232,235,255,72,193,252,248,235,
  255,72,199,192,237,137,133,233,255,73,137,192,255,72,137,199,72,129,231,239,
  72,193,231,235,255,72,139,4,36,255,73,199,193,237,76,33,200,72,9,252,248,
  255,76,137,192,255,87,255,72,199,193,237,72,141,189,233,176,0,252,243,170,
  255,95,255,15,132,245,255,252,233,245,249,255,15,148,208,72,15,182,192,255,
  72,252,247,208,255,15,132,245,72,199,192,1,0,0,0,252,233,245,249,72,199,192,
  0,0,0,0,249,255,15,133,245,255,15,133,245,72,199,192,0,0,0,0,252,233,245,
  249,72,199,192,1,0,0,0,249,255,72,131,192,8,255,102,72,15,126,192,240,132,
  240,36,255,72,129,252,236,239,73,137,194,65,252,255,210,72,129,196,239,255,
  65,91,255,72,137,133,233,72,141,133,233,255,73,137,194,72,199,192,237,65,
  252,255,210,72,129,196,239,255,252,240,15,176,23,255,102,252,240,15,177,23,
  255,252,240,72,15,177,23,255,15,148,209,15,132,244,247,255,65,136,0,255,102,
  65,137,0,255,73,137,0,255,248,1,15,182,193,255,134,7,255,102,135,7,255,72,
  135,7,255,252,243,15,88,193,255,252,242,15,88,193,255,252,243,15,92,193,255,
  252,242,15,92,193,255,252,243,15,89,193,255,252,242,15,89,193,255,252,243,
  15,94,193,255,252,242,15,94,193,255,15,46,200,255,102,15,46,200,255,15,148,
  208,15,155,210,32,208,255,15,149,208,15,154,210,8,208,255,15,151,208,255,
  15,147,208,255,36,1,72,15,182,192,255,222,193,255,222,225,255,222,201,255,
  222,252,241,255,223,252,241,221,216,255,15,148,208,255,15,149,208,255,72,
  1,252,248,255,72,41,252,248,255,72,15,175,199,255,72,199,194,0,0,0,0,72,252,
  247,252,247,255,186,0,0,0,0,252,247,252,247,255,72,153,255,72,252,247,252,
  255,255,72,137,208,255,72,33,252,248,255,72,49,252,248,255,72,57,252,248,
  255,15,146,208,255,15,156,208,255,15,150,208,255,15,158,208,255,72,137,252,
  249,255,72,211,224,255,72,211,232,255,72,211,252,248,255,15,133,245,249,255,
  72,129,252,248,239,255,72,137,199,72,129,252,239,239,72,129,252,255,239,255,
  137,199,129,252,239,239,129,252,255,239,255,15,134,245,255,252,233,245,255,
  252,255,224,255,64,136,133,253,240,131,233,255,102,64,137,133,253,240,139,
  233,255,72,137,133,253,240,131,233,255,254,0,85,72,137,229,255,249,73,186,
  237,237,255,65,252,255,210,72,41,196,255,254,1,250,3,249,235,255,235,235,
  255,235,236,255,235,235,235,235,255,72,137,165,233,255,199,133,233,237,199,
  133,233,237,72,137,173,233,72,131,133,233,16,72,137,173,233,72,129,133,233,
  239,255,72,137,189,233,72,137,181,233,72,137,149,233,72,137,141,233,76,137,
  133,233,76,137,141,233,252,242,15,17,133,233,252,242,15,17,141,233,252,242,
  15,17,149,233,252,242,15,17,157,233,252,242,15,17,165,233,252,242,15,17,173,
  233,252,242,15,17,181,233,252,242,15,17,189,233,255,72,137,189,233,72,137,
  181,233,72,137,149,233,72,137,141,233,255,72,141,101,0,255,72,137,252,236,
  255,93,195,255,250,3,249,254,0
};

#line 23 "../../src/codegen.in.c"
//| .globals dynasm_globals
enum {
  dynasm_globals_MAX
};
#line 24 "../../src/codegen.in.c"
//| .if WIN
//| .define X64WIN, 1
//| .endif

#define Dst &C(dynasm)

#define REG_DI 7
#define REG_SI 6
#define REG_DX 2
#define REG_CX 1
#define REG_R8 8
#define REG_R9 9

// Used with Rq(), Rd(), Rw(), Rb()
#if X64WIN
static int dasmargreg[] = {REG_CX, REG_DX, REG_R8, REG_R9};
#define REG_UTIL REG_CX
#define X64WIN_REG_MAX 4
#define PARAMETER_SAVE_SIZE (4 * 8)
#else
static int dasmargreg[] = {REG_DI, REG_SI, REG_DX, REG_CX, REG_R8, REG_R9};
#define REG_UTIL REG_DI
#define SYSV_GP_MAX 6
#define SYSV_FP_MAX 8
#endif
//| .if X64WIN
//| .define CARG1, rcx
//| .define CARG1d, ecx
//| .define CARG2, rdx
//| .define CARG3, r8
//| .define CARG4, r9
//| .define RUTIL, rcx
//| .define RUTILd, ecx
//| .define RUTILenc, 0x11
//| .else
//| .define CARG1, rdi
//| .define CARG1d, edi
//| .define CARG2, rsi
//| .define CARG3, rdx
//| .define CARG4, rcx
//| .define CARG5, r8
//| .define CARG6, r9
//| .define RUTIL, rdi
//| .define RUTILd, edi
//| .define RUTILenc, 0x17
//| .endif

static void gen_expr(Node* node);
static void gen_stmt(Node* node);

#if X64WIN
static void record_line_syminfo(int file_no, int line_no, int pclabel) {
  // If file and line haven't changed, then we're working through parts of a
  // single statement; just ignore.
  int cur_len = C(current_fn)->file_line_label_data.len;
  if (cur_len > 0 && C(current_fn)->file_line_label_data.data[cur_len - 1].a == file_no &&
      C(current_fn)->file_line_label_data.data[cur_len - 1].b == line_no) {
    return;
  }

  //|=>pclabel:
  dasm_put(Dst, 0, pclabel);
#line 85 "../../src/codegen.in.c"
  intintintarray_push(&C(current_fn)->file_line_label_data, (IntIntInt){file_no, line_no, pclabel},
                      AL_Compile);
  // printf("%s:%d:label %d\n", compiler_state.tokenize__all_tokenized_files.data[file_no]->name,
  // line_no, pclabel);
}
#endif

static int codegen_pclabel(void) {
  int ret = C(numlabels);
  dasm_growpc(&C(dynasm), ++C(numlabels));
  return ret;
}

static void push(void) {
  //| push rax
  dasm_put(Dst, 2);
#line 100 "../../src/codegen.in.c"
  C(depth)++;
}

static void pop(int dasmreg) {
  //| pop Rq(dasmreg)
  dasm_put(Dst, 4, (dasmreg));
#line 105 "../../src/codegen.in.c"
  C(depth)--;
}

static void pushf(void) {
  //| sub rsp, 8
  //| movsd qword [rsp], xmm0
  dasm_put(Dst, 9);
#line 111 "../../src/codegen.in.c"
  C(depth)++;
}

static void popf(int reg) {
  //| movsd xmm(reg), qword [rsp]
  //| add rsp, 8
  dasm_put(Dst, 21, (reg));
#line 117 "../../src/codegen.in.c"
  C(depth)--;
}

// Load a value from where %rax is pointing to.
static void load(Type* ty) {
  switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
    case TY_ARRAY:
    case TY_FUNC:
    case TY_VLA:
      // If it is an array, do not attempt to load a value to the
      // register because in general we can't load an entire array to a
      // register. As a result, the result of an evaluation of an array
      // becomes not the array itself but the address of the array.
      // This is where "array is automatically converted to a pointer to
      // the first element of the array in C" occurs.
      return;
    case TY_FLOAT:
      //| movss xmm0, dword [rax]
      dasm_put(Dst, 35);
#line 137 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd xmm0, qword [rax]
      dasm_put(Dst, 41);
#line 140 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fld tword [rax]
      dasm_put(Dst, 47);
#line 144 "../../src/codegen.in.c"
      return;
#endif
  }

  // When we load a char or a short value to a register, we always
  // extend them to the size of int, so we can assume the lower half of
  // a register always contains a valid value. The upper half of a
  // register for char, short and int may contain garbage. When we load
  // a long value to a register, it simply occupies the entire register.
  if (ty->size == 1) {
    if (ty->is_unsigned) {
      //| movzx eax, byte [rax]
      dasm_put(Dst, 50);
#line 156 "../../src/codegen.in.c"
    } else {
      //| movsx eax, byte [rax]
      dasm_put(Dst, 54);
#line 158 "../../src/codegen.in.c"
    }
  } else if (ty->size == 2) {
    if (ty->is_unsigned) {
      //| movzx eax, word [rax]
      dasm_put(Dst, 58);
#line 162 "../../src/codegen.in.c"
    } else {
      //| movsx eax, word [rax]
      dasm_put(Dst, 62);
#line 164 "../../src/codegen.in.c"
    }
  } else if (ty->size == 4) {
    //| movsxd rax, dword [rax]
    dasm_put(Dst, 66);
#line 167 "../../src/codegen.in.c"
  } else {
    //| mov rax, qword [rax]
    dasm_put(Dst, 70);
#line 169 "../../src/codegen.in.c"
  }
}

// Store %rax to an address that the stack top is pointing to.
static void store(Type* ty) {
  pop(REG_UTIL);

  switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      for (int i = 0; i < ty->size; i++) {
        //| mov r8b, [rax+i]
        //| mov [RUTIL+i], r8b
        dasm_put(Dst, 74, i, i);
#line 182 "../../src/codegen.in.c"
      }
      return;
    case TY_FLOAT:
      //| movss dword [RUTIL], xmm0
      dasm_put(Dst, 83);
#line 186 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd qword [RUTIL], xmm0
      dasm_put(Dst, 89);
#line 189 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fstp tword [RUTIL]
      dasm_put(Dst, 95);
#line 193 "../../src/codegen.in.c"
      return;
#endif
  }

  if (ty->size == 1) {
    //| mov [RUTIL], al
    dasm_put(Dst, 98);
#line 199 "../../src/codegen.in.c"
  } else if (ty->size == 2) {
    //| mov [RUTIL], ax
    dasm_put(Dst, 101);
#line 201 "../../src/codegen.in.c"
  } else if (ty->size == 4) {
    //| mov [RUTIL], eax
    dasm_put(Dst, 102);
#line 203 "../../src/codegen.in.c"
  } else {
    //| mov [RUTIL], rax
    dasm_put(Dst, 105);
#line 205 "../../src/codegen.in.c"
  }
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node* node) {
  switch (node->kind) {
    case ND_VAR:
      // Variable-length array, which is always local.
      if (node->var->ty->kind == TY_VLA) {
        //| mov rax, [rbp+node->var->offset]
        dasm_put(Dst, 109, node->var->offset);
#line 216 "../../src/codegen.in.c"
        return;
      }

      // Local variable
      if (node->var->is_local) {
        //| lea rax, [rbp+node->var->offset]
        dasm_put(Dst, 114, node->var->offset);
#line 222 "../../src/codegen.in.c"
        return;
      }

      // Thread-local variable
      if (node->var->is_tls) {
        // println("  mov rax, fs:0");
        // println("  add rax, [rel %s wrt ..gottpoff]", node->var->name);
        error_tok(node->tok, "TLS not implemented");
        return;
      }

      // Function
      if (node->ty->kind == TY_FUNC) {
        if (node->var->is_definition) {
          //| lea rax, [=>node->var->dasm_entry_label]
          dasm_put(Dst, 119, node->var->dasm_entry_label);
#line 237 "../../src/codegen.in.c"
        } else {
          int fixup_location = codegen_pclabel();
          strintarray_push(&C(fixups), (StringInt){node->var->name, fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
          //|=>fixup_location:
          //| mov64 rax, 0xc0dec0dec0dec0de
          dasm_put(Dst, 124, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 246 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
        }
        return;
      }

      // Global variable
      int fixup_location = codegen_pclabel();
      strintarray_push(&C(fixups), (StringInt){node->var->name, fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
      //|=>fixup_location:
      //| mov64 rax, 0xda7ada7ada7ada7a
      dasm_put(Dst, 124, fixup_location, (unsigned int)(0xda7ada7ada7ada7a), (unsigned int)((0xda7ada7ada7ada7a)>>32));
#line 262 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      return;
    case ND_DEREF:
      gen_expr(node->lhs);
      return;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_addr(node->rhs);
      return;
    case ND_MEMBER:
      gen_addr(node->lhs);
#if X64WIN
      if (node->lhs->kind == ND_VAR && node->lhs->var->is_param_passed_by_reference) {
        //| mov rax, [rax]
        dasm_put(Dst, 70);
#line 278 "../../src/codegen.in.c"
      }
#endif
      //| add rax, node->member->offset
      dasm_put(Dst, 130, node->member->offset);
#line 281 "../../src/codegen.in.c"
      return;
    case ND_FUNCALL:
      if (node->ret_buffer) {
        gen_expr(node);
        return;
      }
      break;
    case ND_ASSIGN:
    case ND_COND:
      if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
        gen_expr(node);
        return;
      }
      break;
    case ND_VLA_PTR:
      //| lea rax, [rbp+node->var->offset]
      dasm_put(Dst, 114, node->var->offset);
#line 297 "../../src/codegen.in.c"
      return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void cmp_zero(Type* ty) {
  switch (ty->kind) {
    case TY_FLOAT:
      //| xorps xmm1, xmm1
      //| ucomiss xmm0, xmm1
      dasm_put(Dst, 135);
#line 308 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| xorpd xmm1, xmm1
      //| ucomisd xmm0, xmm1
      dasm_put(Dst, 142);
#line 312 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fldz
      //| fucomip st0
      //| fstp st0
      dasm_put(Dst, 151);
#line 318 "../../src/codegen.in.c"
      return;
#endif
  }

  if (is_integer(ty) && ty->size <= 4) {
    //| cmp eax, 0
    dasm_put(Dst, 159);
#line 324 "../../src/codegen.in.c"
  } else {
    //| cmp rax, 0
    dasm_put(Dst, 164);
#line 326 "../../src/codegen.in.c"
  }
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

static int get_type_id(Type* ty) {
  switch (ty->kind) {
    case TY_CHAR:
      return ty->is_unsigned ? U8 : I8;
    case TY_SHORT:
      return ty->is_unsigned ? U16 : I16;
    case TY_INT:
      return ty->is_unsigned ? U32 : I32;
    case TY_LONG:
      return ty->is_unsigned ? U64 : I64;
    case TY_FLOAT:
      return F32;
    case TY_DOUBLE:
      return F64;
#if !X64WIN
    case TY_LDOUBLE:
      return F80;
#endif
  }
  return U64;
}

static void i32i8(void) {
  //| movsx eax, al
  dasm_put(Dst, 170);
#line 355 "../../src/codegen.in.c"
}
static void i32u8(void) {
  //| movzx eax, al
  dasm_put(Dst, 174);
#line 358 "../../src/codegen.in.c"
}
static void i32i16(void) {
  //| movsx eax, ax
  dasm_put(Dst, 178);
#line 361 "../../src/codegen.in.c"
}
static void i32u16(void) {
  //| movzx eax, ax
  dasm_put(Dst, 182);
#line 364 "../../src/codegen.in.c"
}
static void i32f32(void) {
  //| cvtsi2ss xmm0, eax
  dasm_put(Dst, 186);
#line 367 "../../src/codegen.in.c"
}
static void i32i64(void) {
  //| movsxd rax, eax
  dasm_put(Dst, 192);
#line 370 "../../src/codegen.in.c"
}
static void i32f64(void) {
  //| cvtsi2sd xmm0, eax
  dasm_put(Dst, 196);
#line 373 "../../src/codegen.in.c"
}
static void i32f80(void) {
  //| mov [rsp-4], eax
  //| fild dword [rsp-4]
  dasm_put(Dst, 202);
#line 377 "../../src/codegen.in.c"
}

static void u32f32(void) {
  //| mov eax, eax
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 213);
#line 382 "../../src/codegen.in.c"
}
static void u32i64(void) {
  //| mov eax, eax
  dasm_put(Dst, 222);
#line 385 "../../src/codegen.in.c"
}
static void u32f64(void) {
  //| mov eax, eax
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 225);
#line 389 "../../src/codegen.in.c"
}
static void u32f80(void) {
  //| mov eax, eax
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 234);
#line 394 "../../src/codegen.in.c"
}

static void i64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 215);
#line 398 "../../src/codegen.in.c"
}
static void i64f64(void) {
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 227);
#line 401 "../../src/codegen.in.c"
}
static void i64f80(void) {
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 236);
#line 405 "../../src/codegen.in.c"
}

static void u64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 215);
#line 409 "../../src/codegen.in.c"
}
static void u64f64(void) {
  //| test rax,rax
  //| js >1
  //| pxor xmm0,xmm0
  //| cvtsi2sd xmm0,rax
  //| jmp >2
  //|1:
  //| mov RUTIL,rax
  //| and eax,1
  //| pxor xmm0,xmm0
  //| shr RUTIL, 1
  //| or RUTIL,rax
  //| cvtsi2sd xmm0,RUTIL
  //| addsd xmm0,xmm0
  //|2:
  dasm_put(Dst, 248);
#line 425 "../../src/codegen.in.c"
}
static void u64f80(void) {
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  //| test rax, rax
  //| jns >1
  //| mov eax, 1602224128
  //| mov [rsp-4], eax
  //| fadd dword [rsp-4]
  //|1:
  dasm_put(Dst, 304);
#line 435 "../../src/codegen.in.c"
}

static void f32i8(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 340);
#line 440 "../../src/codegen.in.c"
}
static void f32u8(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 349);
#line 444 "../../src/codegen.in.c"
}
static void f32i16(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 358);
#line 448 "../../src/codegen.in.c"
}
static void f32u16(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 367);
#line 452 "../../src/codegen.in.c"
}
static void f32i32(void) {
  //| cvttss2si eax, xmm0
  dasm_put(Dst, 376);
#line 455 "../../src/codegen.in.c"
}
static void f32u32(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 382);
#line 458 "../../src/codegen.in.c"
}
static void f32i64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 382);
#line 461 "../../src/codegen.in.c"
}
static void f32u64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 382);
#line 464 "../../src/codegen.in.c"
}
static void f32f64(void) {
  //| cvtss2sd xmm0, xmm0
  dasm_put(Dst, 389);
#line 467 "../../src/codegen.in.c"
}
static void f32f80(void) {
  //| movss dword [rsp-4], xmm0
  //| fld dword [rsp-4]
  dasm_put(Dst, 395);
#line 471 "../../src/codegen.in.c"
}

static void f64i8(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 409);
#line 476 "../../src/codegen.in.c"
}
static void f64u8(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 418);
#line 480 "../../src/codegen.in.c"
}
static void f64i16(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 427);
#line 484 "../../src/codegen.in.c"
}
static void f64u16(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 436);
#line 488 "../../src/codegen.in.c"
}
static void f64i32(void) {
  //| cvttsd2si eax, xmm0
  dasm_put(Dst, 445);
#line 491 "../../src/codegen.in.c"
}
static void f64u32(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 451);
#line 494 "../../src/codegen.in.c"
}
static void f64i64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 451);
#line 497 "../../src/codegen.in.c"
}
static void f64u64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 451);
#line 500 "../../src/codegen.in.c"
}
static void f64f32(void) {
  //| cvtsd2ss xmm0, xmm0
  dasm_put(Dst, 458);
#line 503 "../../src/codegen.in.c"
}
static void f64f80(void) {
  //| movsd qword [rsp-8], xmm0
  //| fld qword [rsp-8]
  dasm_put(Dst, 464);
#line 507 "../../src/codegen.in.c"
}

static void from_f80_1(void) {
  //| fnstcw word [rsp-10]
  //| movzx eax, word [rsp-10]
  //| or ah, 12
  //| mov [rsp-12], ax
  //| fldcw word [rsp-12]
  dasm_put(Dst, 478);
#line 515 "../../src/codegen.in.c"
}

#define FROM_F80_2 " [rsp-24]\n fldcw [rsp-10]\n "

static void f80i8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 504);
#line 524 "../../src/codegen.in.c"
}
static void f80u8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  //| and eax, 0xff
  dasm_put(Dst, 519);
#line 531 "../../src/codegen.in.c"
}
static void f80i16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 504);
#line 537 "../../src/codegen.in.c"
}
static void f80u16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  dasm_put(Dst, 540);
#line 543 "../../src/codegen.in.c"
}
static void f80i32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 555);
#line 549 "../../src/codegen.in.c"
}
static void f80u32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 555);
#line 555 "../../src/codegen.in.c"
}
static void f80i64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 569);
#line 561 "../../src/codegen.in.c"
}
static void f80u64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 569);
#line 567 "../../src/codegen.in.c"
}
static void f80f32(void) {
  //| fstp dword [rsp-8]
  //| movss xmm0, dword [rsp-8]
  dasm_put(Dst, 584);
#line 571 "../../src/codegen.in.c"
}
static void f80f64(void) {
  //| fstp qword [rsp-8]
  //| movsd xmm0, qword [rsp-8]
  dasm_put(Dst, 598);
#line 575 "../../src/codegen.in.c"
}

typedef void (*DynasmCastFunc)(void);

// clang-format off

// The table for type casts
static DynasmCastFunc dynasm_cast_table[][11] = {
  // "to" is the rows, "from" the columns
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

// clang-format on

// This can't be "cast()" when amalgamated because parse has a cast() as well.
static void cg_cast(Type* from, Type* to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    //| setne al
    //| movzx eax, al
    dasm_put(Dst, 612);
#line 611 "../../src/codegen.in.c"
    return;
  }

  int t1 = get_type_id(from);
  int t2 = get_type_id(to);
  if (dynasm_cast_table[t1][t2]) {
    dynasm_cast_table[t1][t2]();
  }
}

#if !X64WIN

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
static bool has_flonum(Type* ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member* mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

static bool has_flonum1(Type* ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type* ty) {
  return has_flonum(ty, 8, 16, 0);
}

#endif

static int push_struct(Type* ty) {
  int sz = (int)align_to_s(ty->size, 8);
  //| sub rsp, sz
  dasm_put(Dst, 619, sz);
#line 666 "../../src/codegen.in.c"
  C(depth) += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    //| mov r10b, [rax+i]
    //| mov [rsp+i], r10b
    dasm_put(Dst, 625, i, i);
#line 671 "../../src/codegen.in.c"
  }

  return sz;
}

#if X64WIN

bool type_passed_in_register(Type* ty) {
  // https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention:
  //   "__m128 types, arrays, and strings are never passed by immediate value.
  //   Instead, a pointer is passed to memory allocated by the caller. Structs
  //   and unions of size 8, 16, 32, or 64 bits, and __m64 types, are passed as
  //   if they were integers of the same size."
  //
  // Note that e.g. a pragma pack 5 byte structure will be passed by reference,
  // so this is not just size <= 8 as it is for 16 on SysV.
  //
  // Arrays and strings as mentioned won't be TY_STRUCT/TY_UNION so they should
  // not use this function.
  return ty->size == 1 || ty->size == 2 || ty->size == 4 || ty->size == 8;
}

static void push_args2_win(Node* args, bool first_pass) {
  if (!args)
    return;
  push_args2_win(args->next, first_pass);

  // Push all the by-stack first, then on the second pass, push all the things
  // that will be popped back into registers by the actual call.
  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  if ((args->ty->kind != TY_STRUCT && args->ty->kind != TY_UNION) ||
      type_passed_in_register(args->ty)) {
    gen_expr(args);
  }

  switch (args->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (!type_passed_in_register(args->ty)) {
        assert(args->pass_by_reference);
        //| lea rax, [r11-args->pass_by_reference]
        dasm_put(Dst, 636, -args->pass_by_reference);
#line 714 "../../src/codegen.in.c"
      } else {
        //| mov rax, [rax]
        dasm_put(Dst, 70);
#line 716 "../../src/codegen.in.c"
      }
      push();
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      pushf();
      break;
    default:
      push();
      break;
  }
}

// --- Windows ---
// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// required by the Windows ABI.
//
// - Integer arguments in the leftmost four positions are passed in RCX, RDX,
//   R8, and R9.
//
// - Floating point arguments in the leftmost four position are passed in
//   XMM0-XM3.
//
// - The 5th and subsequent arguments are push on the stack in right-to-left
//   order.
//
// - Arguments larger than 8 bytes are always passed by reference.
//
// - When mixing integer and floating point arguments, the opposite type's
//   register is left unused, e.g.
//
//     void func(int a, double b, int c, float d, int e, float f);
//
//   would have a in RCX, b in XMM1, c in R8, d in XMM3, f then e pushed on stack.
//
// - Varargs follow the same conventions, but floating point values must also
//   have their value stored in the corresponding integer register for the first
//   four arguments.
//
// - For larger than 8 byte structs, they're passed by reference. So we first
//   need to make a local copy on the stack (since they're still passed by value
//   not reference as far as the language is concerned), but then pass a pointer
//   to the copy rather than to the actual data. This difference definitely
//   casues the most changes vs. SysV in the rest of the compiler.
//
// - Integer return values of 8 bytes or less are in RAX (including user-defined
//   types like small structures). Floating point are returned in XMM0. For
//   user-defined types that are larger than 8 bytes, the caller allocates a
//   buffer and passes the address of the buffer in RCX, taking up the first
//   integer register slot. The function returns the same address passed in RCX
//   in RAX.
//
// - RAX, RCX, RDX, R8, R9, R10, R11, and XMM0-XMM5 are volatile.
// - RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 are
//   non-volatile.
//
// --- Windows ---
static int push_args_win(Node* node, int* by_ref_copies_size) {
  int stack = 0, reg = 0;

  bool has_by_ref_args = false;
  for (Node* arg = node->args; arg; arg = arg->next) {
    if ((arg->ty->kind == TY_STRUCT || arg->ty->kind == TY_UNION) &&
        !type_passed_in_register(arg->ty)) {
      has_by_ref_args = true;
      break;
    }
  }

  if (has_by_ref_args) {
    // Use r11 as a base pointer for by-reference copies of structs.
    //| push r11
    //| mov r11, rsp
    dasm_put(Dst, 641);
#line 791 "../../src/codegen.in.c"
  }

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && !type_passed_in_register(node->ty))
    reg++;

  *by_ref_copies_size = 0;

  // Load as many arguments to the registers as possible.
  for (Node* arg = node->args; arg; arg = arg->next) {
    Type* ty = arg->ty;

    switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        // It's either small and so passed in a register, or isn't and then
        // we're instead storing the pointer to the larger struct.
        if (reg++ >= X64WIN_REG_MAX) {
          arg->pass_by_stack = true;
          ++stack;
        }
        if (!type_passed_in_register(ty)) {
          // Make a copy, and note the offset for passing by reference.
          gen_expr(arg);
          *by_ref_copies_size += push_struct(ty);
          arg->pass_by_reference = *by_ref_copies_size;
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (reg++ >= X64WIN_REG_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
        break;
      default:
        if (reg++ >= X64WIN_REG_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
    }
  }

  assert((*by_ref_copies_size == 0 && !has_by_ref_args) ||
         (*by_ref_copies_size && has_by_ref_args));

  if ((C(depth) + stack + (*by_ref_copies_size / 8)) % 2 == 1) {
    //| sub rsp, 8
    dasm_put(Dst, 647);
#line 840 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_win(node->args, true);
  push_args2_win(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && !type_passed_in_register(node->ty)) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 114, node->ret_buffer->offset);
#line 851 "../../src/codegen.in.c"
    push();
  }

  return stack;
}

#else

static void push_args2_sysv(Node* args, bool first_pass) {
  if (!args)
    return;
  push_args2_sysv(args->next, first_pass);

  // Push all the by-stack first, then on the second pass, push all the things
  // that will be popped back into registers by the actual call.
  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr(args);

  switch (args->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      push_struct(args->ty);
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      pushf();
      break;
    case TY_LDOUBLE:
      //| sub rsp, 16
      //| fstp tword [rsp]
      dasm_put(Dst, 653);
#line 883 "../../src/codegen.in.c"
      C(depth) += 2;
      break;
    default:
      push();
      break;
  }
}

// --- SysV ---
//
// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
// - Up to 6 arguments of integral type are passed using RDI, RSI,
//   RDX, RCX, R8 and R9.
//
// - Up to 8 arguments of floating-point type are passed using XMM0 to
//   XMM7.
//
// - If all registers of an appropriate type are already used, push an
//   argument to the stack in the right-to-left order.
//
// - Each argument passed on the stack takes 8 bytes, and the end of
//   the argument area must be aligned to a 16 byte boundary.
//
// - If a function is variadic, set the number of floating-point type
//   arguments to RAX.
//
// --- SysV ---
static int push_args_sysv(Node* node) {
  int stack = 0, gp = 0, fp = 0;

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16)
    gp++;

  // Load as many arguments to the registers as possible.
  for (Node* arg = node->args; arg; arg = arg->next) {
    Type* ty = arg->ty;

    switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size > 16) {
          arg->pass_by_stack = true;
          stack += align_to_s(ty->size, 8) / 8;
        } else {
          bool fp1 = has_flonum1(ty);
          bool fp2 = has_flonum2(ty);

          if (fp + fp1 + fp2 < SYSV_FP_MAX && gp + !fp1 + !fp2 < SYSV_GP_MAX) {
            fp = fp + fp1 + fp2;
            gp = gp + !fp1 + !fp2;
          } else {
            arg->pass_by_stack = true;
            stack += align_to_s(ty->size, 8) / 8;
          }
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp++ >= SYSV_FP_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
        break;
      case TY_LDOUBLE:
        arg->pass_by_stack = true;
        stack += 2;
        break;
      default:
        if (gp++ >= SYSV_GP_MAX) {
          arg->pass_by_stack = true;
          stack++;
        }
    }
  }

  if ((C(depth) + stack) % 2 == 1) {
    //| sub rsp, 8
    dasm_put(Dst, 647);
#line 966 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_sysv(node->args, true);
  push_args2_sysv(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 114, node->ret_buffer->offset);
#line 977 "../../src/codegen.in.c"
    push();
  }

  return stack;
}

static void copy_ret_buffer(Obj* var) {
  Type* ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4) {
      //| movss dword [rbp+var->offset], xmm0
      dasm_put(Dst, 662, var->offset);
#line 991 "../../src/codegen.in.c"
    } else {
      //| movsd qword [rbp+var->offset], xmm0
      dasm_put(Dst, 669, var->offset);
#line 993 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      //| mov [rbp+var->offset+i], al
      //| shr rax, 8
      dasm_put(Dst, 676, var->offset+i);
#line 999 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12) {
        //| movss dword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 684, (fp), var->offset+8);
#line 1008 "../../src/codegen.in.c"
      } else {
        //| movsd qword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 695, (fp), var->offset+8);
#line 1010 "../../src/codegen.in.c"
      }
    } else {
      for (int i = 8; i < MIN(16, ty->size); i++) {
        //| mov [rbp+var->offset+i], Rb(gp)
        //| shr Rq(gp), 8
        dasm_put(Dst, 706, (gp), var->offset+i, (gp));
#line 1015 "../../src/codegen.in.c"
      }
    }
  }
}

#endif

static void copy_struct_reg(void) {
#if X64WIN
  // TODO: I'm not sure if this is right/sufficient.
  //| mov rax, [rax]
  dasm_put(Dst, 70);
#line 1026 "../../src/codegen.in.c"
#else
  Type* ty = C(current_fn)->ty->return_ty;

  int gp = 0, fp = 0;

  //| mov RUTIL, rax
  dasm_put(Dst, 720);
#line 1032 "../../src/codegen.in.c"

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4) {
      //| movss xmm0, dword [RUTIL]
      dasm_put(Dst, 724);
#line 1037 "../../src/codegen.in.c"
    } else {
      //| movsd xmm0, qword [RUTIL]
      dasm_put(Dst, 730);
#line 1039 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    //| mov rax, 0
    dasm_put(Dst, 736);
#line 1043 "../../src/codegen.in.c"
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      //| shl rax, 8
      //| mov ax, [RUTIL+i]
      dasm_put(Dst, 744, i);
#line 1046 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4) {
        //| movss xmm(fp), dword [RUTIL+8]
        dasm_put(Dst, 753, (fp));
#line 1055 "../../src/codegen.in.c"
      } else {
        //| movsd xmm(fp), qword [RUTIL+8]
        dasm_put(Dst, 763, (fp));
#line 1057 "../../src/codegen.in.c"
      }
    } else {
      //| mov Rq(gp), 0
      dasm_put(Dst, 773, (gp));
#line 1060 "../../src/codegen.in.c"
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        //| shl Rq(gp), 8
        //| mov Rb(gp), [RUTIL+i]
        dasm_put(Dst, 783, (gp), (gp), i);
#line 1063 "../../src/codegen.in.c"
      }
    }
  }
#endif
}

static void copy_struct_mem(void) {
  Type* ty = C(current_fn)->ty->return_ty;
  Obj* var = C(current_fn)->params;

  //| mov RUTIL, [rbp+var->offset]
  dasm_put(Dst, 797, var->offset);
#line 1074 "../../src/codegen.in.c"

  for (int i = 0; i < ty->size; i++) {
    //| mov dl, [rax+i]
    //| mov [RUTIL+i], dl
    dasm_put(Dst, 802, i, i);
#line 1078 "../../src/codegen.in.c"
  }
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  //| add CARG1, 15
  //| and CARG1d, 0xfffffff0
  dasm_put(Dst, 809);
#line 1085 "../../src/codegen.in.c"

  // Shift the temporary area by CARG1.
  //| mov CARG4, [rbp+C(current_fn)->alloca_bottom->offset]
  //| sub CARG4, rsp
  //| mov rax, rsp
  //| sub rsp, CARG1
  //| mov rdx, rsp
  //|1:
  //| cmp CARG4, 0
  //| je >2
  //| mov r8b, [rax]
  //| mov [rdx], r8b
  //| inc rdx
  //| inc rax
  //| dec CARG4
  //| jmp <1
  //|2:
  dasm_put(Dst, 818, C(current_fn)->alloca_bottom->offset);
#line 1102 "../../src/codegen.in.c"

  // Move alloca_bottom pointer.
  //| mov rax, [rbp+C(current_fn)->alloca_bottom->offset]
  //| sub rax, CARG1
  //| mov [rbp+C(current_fn)->alloca_bottom->offset], rax
  dasm_put(Dst, 871, C(current_fn)->alloca_bottom->offset, C(current_fn)->alloca_bottom->offset);
#line 1107 "../../src/codegen.in.c"
}

// Generate code for a given node.
static void gen_expr(Node* node) {
  switch (node->kind) {
    case ND_NULL_EXPR:
      return;
    case ND_NUM: {
      switch (node->ty->kind) {
        case TY_FLOAT: {
          union {
            float f32;
            uint32_t u32;
          } u = {(float)node->fval};
          //| mov eax, u.u32
          //| movd xmm0, rax
          dasm_put(Dst, 884, u.u32);
#line 1123 "../../src/codegen.in.c"
          return;
        }
        case TY_DOUBLE: {
          union {
            double f64;
            uint64_t u64;
          } u = {(double)node->fval};
          //| mov64 rax, u.u64
          //| movd xmm0, rax
          dasm_put(Dst, 892, (unsigned int)(u.u64), (unsigned int)((u.u64)>>32));
#line 1132 "../../src/codegen.in.c"
          return;
        }
#if !X64WIN
        case TY_LDOUBLE: {
          union {
            long double f80;
            uint64_t u64[2];
          } u;
          memset(&u, 0, sizeof(u));
          u.f80 = node->fval;
          //| mov64 rax, u.u64[0]
          //| mov [rsp-16], rax
          //| mov64 rax, u.u64[1]
          //| mov [rsp-8], rax
          //| fld tword [rsp-16]
          dasm_put(Dst, 902, (unsigned int)(u.u64[0]), (unsigned int)((u.u64[0])>>32), (unsigned int)(u.u64[1]), (unsigned int)((u.u64[1])>>32));
#line 1147 "../../src/codegen.in.c"
          return;
        }
#endif
      }

      if (node->val < INT_MIN || node->val > INT_MAX) {
        //| mov64 rax, node->val
        dasm_put(Dst, 125, (unsigned int)(node->val), (unsigned int)((node->val)>>32));
#line 1154 "../../src/codegen.in.c"
      } else {
        //| mov rax, node->val
        dasm_put(Dst, 928, node->val);
#line 1156 "../../src/codegen.in.c"
      }
      return;
    }
    case ND_NEG:
      gen_expr(node->lhs);

      switch (node->ty->kind) {
        case TY_FLOAT:
          //| mov rax, 1
          //| shl rax, 31
          //| movd xmm1, rax
          //| xorps xmm0, xmm1
          dasm_put(Dst, 933);
#line 1168 "../../src/codegen.in.c"
          return;
        case TY_DOUBLE:
          //| mov rax, 1
          //| shl rax, 63
          //| movd xmm1, rax
          //| xorpd xmm0, xmm1
          dasm_put(Dst, 953);
#line 1174 "../../src/codegen.in.c"
          return;
#if !X64WIN
        case TY_LDOUBLE:
          //| fchs
          dasm_put(Dst, 974);
#line 1178 "../../src/codegen.in.c"
          return;
#endif
      }

      //| neg rax
      dasm_put(Dst, 977);
#line 1183 "../../src/codegen.in.c"
      return;
    case ND_VAR:
      gen_addr(node);
      load(node->ty);
      return;
    case ND_MEMBER: {
      gen_addr(node);
      load(node->ty);

      Member* mem = node->member;
      if (mem->is_bitfield) {
        //| shl rax, 64 - mem->bit_width - mem->bit_offset
        dasm_put(Dst, 982, 64 - mem->bit_width - mem->bit_offset);
#line 1195 "../../src/codegen.in.c"
        if (mem->ty->is_unsigned) {
          //| shr rax, 64 - mem->bit_width
          dasm_put(Dst, 987, 64 - mem->bit_width);
#line 1197 "../../src/codegen.in.c"
        } else {
          //| sar rax, 64 - mem->bit_width
          dasm_put(Dst, 992, 64 - mem->bit_width);
#line 1199 "../../src/codegen.in.c"
        }
      }
      return;
    }
    case ND_DEREF:
      gen_expr(node->lhs);
      load(node->ty);
      return;
    case ND_ADDR:
      gen_addr(node->lhs);
      return;
    case ND_ASSIGN:
      // Special case "int into a local". Normally this would compile to:
      //   lea rax,[rbp+node->lhs->offset]
      //   push rax
      //   mov rax, node->rhs->val
      //   pop rcx
      //   mov [rcx], eax
      if (node->lhs->kind == ND_VAR && node->lhs->var->is_local && node->rhs->kind == ND_NUM &&
          node->rhs->ty->kind != TY_FLOAT && node->rhs->ty->kind != TY_DOUBLE &&
          node->rhs->ty->kind != TY_LDOUBLE && node->rhs->val >= INT_MIN &&
          node->rhs->val <= INT_MAX) {
        //| mov rax, node->rhs->val
        //| mov dword [rbp+node->lhs->var->offset], eax
        dasm_put(Dst, 998, node->rhs->val, node->lhs->var->offset);
#line 1223 "../../src/codegen.in.c"
      } else {
        gen_addr(node->lhs);
        push();
        gen_expr(node->rhs);

        if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
          //| mov r8, rax
          dasm_put(Dst, 1006);
#line 1230 "../../src/codegen.in.c"

          // If the lhs is a bitfield, we need to read the current value
          // from memory and merge it with a new value.
          Member* mem = node->lhs->member;
          //| mov RUTIL, rax
          //| and RUTIL, (1L << mem->bit_width) - 1
          //| shl RUTIL, mem->bit_offset
          dasm_put(Dst, 1010, (1L << mem->bit_width) - 1, mem->bit_offset);
#line 1237 "../../src/codegen.in.c"

          //| mov rax, [rsp]
          dasm_put(Dst, 1022);
#line 1239 "../../src/codegen.in.c"
          load(mem->ty);

          long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
          //| mov r9, ~mask
          //| and rax, r9
          //| or rax, RUTIL
          dasm_put(Dst, 1027, ~mask);
#line 1245 "../../src/codegen.in.c"
          store(node->ty);
          //| mov rax, r8
          dasm_put(Dst, 1039);
#line 1247 "../../src/codegen.in.c"
          return;
        }

        store(node->ty);
      }
      return;
    case ND_STMT_EXPR:
      for (Node* n = node->body; n; n = n->next)
        gen_stmt(n);
      return;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      return;
    case ND_CAST:
      gen_expr(node->lhs);
      cg_cast(node->lhs->ty, node->ty);
      return;
    case ND_MEMZERO:
      // `rep stosb` is equivalent to `memset(rdi, al, rcx)`.
#if X64WIN
      //| push rdi
      dasm_put(Dst, 1043);
#line 1269 "../../src/codegen.in.c"
#endif
      //| mov rcx, node->var->ty->size
      //| lea rdi, [rbp+node->var->offset]
      //| mov al, 0
      //| rep
      //| stosb
      dasm_put(Dst, 1045, node->var->ty->size, node->var->offset);
#line 1275 "../../src/codegen.in.c"
#if X64WIN
      //| pop rdi
      dasm_put(Dst, 1059);
#line 1277 "../../src/codegen.in.c"
#endif
      return;
    case ND_COND: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1061, lelse);
#line 1285 "../../src/codegen.in.c"
      gen_expr(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1065, lend, lelse);
#line 1288 "../../src/codegen.in.c"
      gen_expr(node->els);
      //|=>lend:
      dasm_put(Dst, 0, lend);
#line 1290 "../../src/codegen.in.c"
      return;
    }
    case ND_NOT:
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| sete al
      //| movzx rax, al
      dasm_put(Dst, 1070);
#line 1297 "../../src/codegen.in.c"
      return;
    case ND_BITNOT:
      gen_expr(node->lhs);
      //| not rax
      dasm_put(Dst, 1078);
#line 1301 "../../src/codegen.in.c"
      return;
    case ND_LOGAND: {
      int lfalse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| je =>lfalse
      dasm_put(Dst, 1061, lfalse);
#line 1308 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| je =>lfalse
      //| mov rax, 1
      //| jmp =>lend
      //|=>lfalse:
      //| mov rax, 0
      //|=>lend:
      dasm_put(Dst, 1083, lfalse, lend, lfalse, lend);
#line 1316 "../../src/codegen.in.c"
      return;
    }
    case ND_LOGOR: {
      int ltrue = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| jne =>ltrue
      dasm_put(Dst, 1106, ltrue);
#line 1324 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| jne =>ltrue
      //| mov rax, 0
      //| jmp =>lend
      //|=>ltrue:
      //| mov rax, 1
      //|=>lend:
      dasm_put(Dst, 1110, ltrue, lend, ltrue, lend);
#line 1332 "../../src/codegen.in.c"
      return;
    }
    case ND_FUNCALL: {
      if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
        gen_expr(node->args);
        //| mov CARG1, rax
        dasm_put(Dst, 720);
#line 1338 "../../src/codegen.in.c"
        builtin_alloca();
        return;
      }

#if X64WIN
      if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "__va_start")) {
        // va_start(ap, x) turns into __va_start(&ap, x), so we only want the
        // expr here, not the address.
        gen_expr(node->args);
        push();
        // ToS is now &ap.

        gen_addr(node->args->next);
        // RAX is now &x, move it to the next qword.
        //| add rax, 8
        dasm_put(Dst, 1133);
#line 1353 "../../src/codegen.in.c"

        // Store one-past the second argument into &ap.
        pop(REG_UTIL);
        //| mov [RUTIL], rax
        dasm_put(Dst, 105);
#line 1357 "../../src/codegen.in.c"
        return;
      }
#endif

#if X64WIN

      int by_ref_copies_size = 0;
      int stack_args = push_args_win(node, &by_ref_copies_size);
      gen_expr(node->lhs);

      int reg = 0;

      // If the return type is a large struct/union, the caller passes
      // a pointer to a buffer as if it were the first argument.
      if (node->ret_buffer && !type_passed_in_register(node->ty)) {
        pop(dasmargreg[reg++]);
      }

      for (Node* arg = node->args; arg; arg = arg->next) {
        Type* ty = arg->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            if ((type_passed_in_register(ty) && reg < X64WIN_REG_MAX) ||
                (arg->pass_by_reference && reg < X64WIN_REG_MAX)) {
              pop(dasmargreg[reg++]);
            }
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            if (reg < X64WIN_REG_MAX) {
              popf(reg);
              // Varargs requires a copy of fp in gp.
              //| movd Rq(dasmargreg[reg]), xmm(reg)
              dasm_put(Dst, 1138, (reg), (dasmargreg[reg]));
#line 1392 "../../src/codegen.in.c"
              ++reg;
            }
            break;
          default:
            if (reg < X64WIN_REG_MAX) {
              pop(dasmargreg[reg++]);
            }
        }
      }

      //| sub rsp, PARAMETER_SAVE_SIZE
      //| mov r10, rax
      //| call r10
      //| add rsp, stack_args*8 + PARAMETER_SAVE_SIZE + by_ref_copies_size
      dasm_put(Dst, 1148, PARAMETER_SAVE_SIZE, stack_args*8 + PARAMETER_SAVE_SIZE + by_ref_copies_size);
#line 1406 "../../src/codegen.in.c"
      if (by_ref_copies_size > 0) {
        //| pop r11
        dasm_put(Dst, 1165);
#line 1408 "../../src/codegen.in.c"
      }

      C(depth) -= by_ref_copies_size / 8;
      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 174);
#line 1419 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 174);
#line 1423 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 170);
#line 1425 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 182);
#line 1430 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 178);
#line 1432 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned it's actually
      // returned in rax, so copy it back into the return buffer where we're
      // expecting it.
      if (node->ret_buffer && type_passed_in_register(node->ty)) {
        //| mov [rbp+node->ret_buffer->offset], rax
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 1168, node->ret_buffer->offset, node->ret_buffer->offset);
#line 1442 "../../src/codegen.in.c"
      }

#else  // SysV

      int stack_args = push_args_sysv(node);
      gen_expr(node->lhs);

      int gp = 0, fp = 0;

      // If the return type is a large struct/union, the caller passes
      // a pointer to a buffer as if it were the first argument.
      if (node->ret_buffer && node->ty->size > 16) {
        pop(dasmargreg[gp++]);
      }

      for (Node* arg = node->args; arg; arg = arg->next) {
        Type* ty = arg->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            if (ty->size > 16)
              continue;

            bool fp1 = has_flonum1(ty);
            bool fp2 = has_flonum2(ty);

            if (fp + fp1 + fp2 < SYSV_FP_MAX && gp + !fp1 + !fp2 < SYSV_GP_MAX) {
              if (fp1) {
                popf(fp++);
              } else {
                pop(dasmargreg[gp++]);
              }

              if (ty->size > 8) {
                if (fp2) {
                  popf(fp++);
                } else {
                  pop(dasmargreg[gp++]);
                }
              }
            }
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            if (fp < SYSV_FP_MAX)
              popf(fp++);
            break;
          case TY_LDOUBLE:
            break;
          default:
            if (gp < SYSV_GP_MAX) {
              pop(dasmargreg[gp++]);
            }
        }
      }

      //| mov r10, rax
      //| mov rax, fp
      //| call r10
      //| add rsp, stack_args*8
      dasm_put(Dst, 1177, fp, stack_args*8);
#line 1503 "../../src/codegen.in.c"

      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 174);
#line 1512 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 174);
#line 1516 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 170);
#line 1518 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 182);
#line 1523 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 178);
#line 1525 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned
      // using up to two registers.
      if (node->ret_buffer && node->ty->size <= 16) {
        copy_ret_buffer(node->ret_buffer);
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 114, node->ret_buffer->offset);
#line 1534 "../../src/codegen.in.c"
      }

#endif  // SysV

      return;
    }
    case ND_LABEL_VAL:
      //| lea rax, [=>node->pc_label]
      dasm_put(Dst, 119, node->pc_label);
#line 1542 "../../src/codegen.in.c"
      return;
    case ND_REFLECT_TYPE_PTR:
      //| mov64 rax, node->reflect_ty;
      dasm_put(Dst, 125, (unsigned int)(node->reflect_ty), (unsigned int)((node->reflect_ty)>>32));
#line 1545 "../../src/codegen.in.c"
      return;
    case ND_CAS:
    case ND_LOCKCE: {
      bool is_locked_ce = node->kind == ND_LOCKCE;

      gen_expr(node->cas_addr);
      push();
      gen_expr(node->cas_new);
      push();
      gen_expr(node->cas_old);
      if (!is_locked_ce) {
        //| mov r8, rax
        dasm_put(Dst, 1006);
#line 1557 "../../src/codegen.in.c"
        load(node->cas_old->ty->base);
      }
      pop(REG_DX);    // new
      pop(REG_UTIL);  // addr

      int sz = node->cas_addr->ty->base->size;
      // dynasm doesn't support cmpxchg, and I didn't grok the encoding yet.
      // Hack in the various bytes for the instructions we want since there's
      // limited forms. RUTILenc is either 0x17 for RDI or 0x11 for RCX
      // depending on whether we're encoding for Windows or SysV.
      switch (sz) {
        case 1:
          // lock cmpxchg BYTE PTR [rdi/rcx], dl
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb0
          //| .byte RUTILenc
          dasm_put(Dst, 1193);
#line 1574 "../../src/codegen.in.c"
          break;
        case 2:
          // lock cmpxchg WORD PTR [rdi/rcx],dx
          //| .byte 0x66
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1199);
#line 1582 "../../src/codegen.in.c"
          break;
        case 4:
          // lock cmpxchg DWORD PTR [rdi/rcx],edx
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1200);
#line 1589 "../../src/codegen.in.c"
          break;
        case 8:
          // lock cmpxchg QWORD PTR [rdi/rcx],rdx
          //| .byte 0xf0
          //| .byte 0x48
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1206);
#line 1597 "../../src/codegen.in.c"
          break;
        default:
          unreachable();
      }
      if (!is_locked_ce) {
        //| sete cl
        //| je >1
        dasm_put(Dst, 1213);
#line 1604 "../../src/codegen.in.c"
        switch (sz) {
          case 1:
            //| mov [r8], al
            dasm_put(Dst, 1221);
#line 1607 "../../src/codegen.in.c"
            break;
          case 2:
            //| mov [r8], ax
            dasm_put(Dst, 1225);
#line 1610 "../../src/codegen.in.c"
            break;
          case 4:
            //| mov [r8], eax
            dasm_put(Dst, 1226);
#line 1613 "../../src/codegen.in.c"
            break;
          case 8:
            //| mov [r8], rax
            dasm_put(Dst, 1230);
#line 1616 "../../src/codegen.in.c"
            break;
          default:
            unreachable();
        }
        //|1:
        //| movzx eax, cl
        dasm_put(Dst, 1234);
#line 1622 "../../src/codegen.in.c"
      }

      return;
    }
    case ND_EXCH: {
      gen_expr(node->lhs);
      push();
      gen_expr(node->rhs);
      pop(REG_UTIL);

      int sz = node->lhs->ty->base->size;
      switch (sz) {
        case 1:
          //| xchg [RUTIL], al
          dasm_put(Dst, 1240);
#line 1636 "../../src/codegen.in.c"
          break;
        case 2:
          //| xchg [RUTIL], ax
          dasm_put(Dst, 1243);
#line 1639 "../../src/codegen.in.c"
          break;
        case 4:
          //| xchg [RUTIL], eax
          dasm_put(Dst, 1244);
#line 1642 "../../src/codegen.in.c"
          break;
        case 8:
          //| xchg [RUTIL], rax
          dasm_put(Dst, 1247);
#line 1645 "../../src/codegen.in.c"
          break;
        default:
          unreachable();
      }
      return;
    }
  }

  switch (node->lhs->ty->kind) {
    case TY_FLOAT:
    case TY_DOUBLE: {
      gen_expr(node->rhs);
      pushf();
      gen_expr(node->lhs);
      popf(1);

      bool is_float = node->lhs->ty->kind == TY_FLOAT;

      switch (node->kind) {
        case ND_ADD:
          if (is_float) {
            //| addss xmm0, xmm1
            dasm_put(Dst, 1251);
#line 1667 "../../src/codegen.in.c"
          } else {
            //| addsd xmm0, xmm1
            dasm_put(Dst, 1257);
#line 1669 "../../src/codegen.in.c"
          }
          return;
        case ND_SUB:
          if (is_float) {
            //| subss xmm0, xmm1
            dasm_put(Dst, 1263);
#line 1674 "../../src/codegen.in.c"
          } else {
            //| subsd xmm0, xmm1
            dasm_put(Dst, 1269);
#line 1676 "../../src/codegen.in.c"
          }
          return;
        case ND_MUL:
          if (is_float) {
            //| mulss xmm0, xmm1
            dasm_put(Dst, 1275);
#line 1681 "../../src/codegen.in.c"
          } else {
            //| mulsd xmm0, xmm1
            dasm_put(Dst, 1281);
#line 1683 "../../src/codegen.in.c"
          }
          return;
        case ND_DIV:
          if (is_float) {
            //| divss xmm0, xmm1
            dasm_put(Dst, 1287);
#line 1688 "../../src/codegen.in.c"
          } else {
            //| divsd xmm0, xmm1
            dasm_put(Dst, 1293);
#line 1690 "../../src/codegen.in.c"
          }
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          if (is_float) {
            //| ucomiss xmm1, xmm0
            dasm_put(Dst, 1299);
#line 1698 "../../src/codegen.in.c"
          } else {
            //| ucomisd xmm1, xmm0
            dasm_put(Dst, 1303);
#line 1700 "../../src/codegen.in.c"
          }

          if (node->kind == ND_EQ) {
            //| sete al
            //| setnp dl
            //| and al, dl
            dasm_put(Dst, 1308);
#line 1706 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            //| setp dl
            //| or al, dl
            dasm_put(Dst, 1317);
#line 1710 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1326);
#line 1712 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1330);
#line 1714 "../../src/codegen.in.c"
          }

          //| and al, 1
          //| movzx rax, al
          dasm_put(Dst, 1334);
#line 1718 "../../src/codegen.in.c"
          return;
      }

      error_tok(node->tok, "invalid expression");
    }
#if !X64WIN
    case TY_LDOUBLE: {
      gen_expr(node->lhs);
      gen_expr(node->rhs);

      switch (node->kind) {
        case ND_ADD:
          //| faddp st1, st0
          dasm_put(Dst, 1341);
#line 1731 "../../src/codegen.in.c"
          return;
        case ND_SUB:
          //| fsubrp st1, st0
          dasm_put(Dst, 1344);
#line 1734 "../../src/codegen.in.c"
          return;
        case ND_MUL:
          //| fmulp st1, st0
          dasm_put(Dst, 1347);
#line 1737 "../../src/codegen.in.c"
          return;
        case ND_DIV:
          //| fdivrp st1, st0
          dasm_put(Dst, 1350);
#line 1740 "../../src/codegen.in.c"
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          //| fcomip st1
          //| fstp st0
          dasm_put(Dst, 1354);
#line 1747 "../../src/codegen.in.c"

          if (node->kind == ND_EQ) {
            //| sete al
            dasm_put(Dst, 1360);
#line 1750 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            dasm_put(Dst, 1364);
#line 1752 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1326);
#line 1754 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1330);
#line 1756 "../../src/codegen.in.c"
          }

          //| movzx rax, al
          dasm_put(Dst, 1073);
#line 1759 "../../src/codegen.in.c"
          return;
      }

      error_tok(node->tok, "invalid expression");
    }
#endif
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop(REG_UTIL);

  bool is_long = node->lhs->ty->kind == TY_LONG || node->lhs->ty->base;

  switch (node->kind) {
    case ND_ADD:
      if (is_long) {
        //| add rax, RUTIL
        dasm_put(Dst, 1368);
#line 1778 "../../src/codegen.in.c"
      } else {
        //| add eax, RUTILd
        dasm_put(Dst, 1369);
#line 1780 "../../src/codegen.in.c"
      }
      return;
    case ND_SUB:
      if (is_long) {
        //| sub rax, RUTIL
        dasm_put(Dst, 1373);
#line 1785 "../../src/codegen.in.c"
      } else {
        //| sub eax, RUTILd
        dasm_put(Dst, 1374);
#line 1787 "../../src/codegen.in.c"
      }
      return;
    case ND_MUL:
      if (is_long) {
        //| imul rax, RUTIL
        dasm_put(Dst, 1378);
#line 1792 "../../src/codegen.in.c"
      } else {
        //| imul eax, RUTILd
        dasm_put(Dst, 1379);
#line 1794 "../../src/codegen.in.c"
      }
      return;
    case ND_DIV:
    case ND_MOD:
      if (node->ty->is_unsigned) {
        if (is_long) {
          //| mov rdx, 0
          //| div RUTIL
          dasm_put(Dst, 1383);
#line 1802 "../../src/codegen.in.c"
        } else {
          //| mov edx, 0
          //| div RUTILd
          dasm_put(Dst, 1396);
#line 1805 "../../src/codegen.in.c"
        }
      } else {
        if (node->lhs->ty->size == 8) {
          //| cqo
          dasm_put(Dst, 1406);
#line 1809 "../../src/codegen.in.c"
        } else {
          //| cdq
          dasm_put(Dst, 1407);
#line 1811 "../../src/codegen.in.c"
        }
        if (is_long) {
          //| idiv RUTIL
          dasm_put(Dst, 1409);
#line 1814 "../../src/codegen.in.c"
        } else {
          //| idiv RUTILd
          dasm_put(Dst, 1410);
#line 1816 "../../src/codegen.in.c"
        }
      }

      if (node->kind == ND_MOD) {
        //| mov rax, rdx
        dasm_put(Dst, 1415);
#line 1821 "../../src/codegen.in.c"
      }
      return;
    case ND_BITAND:
      if (is_long) {
        //| and rax, RUTIL
        dasm_put(Dst, 1419);
#line 1826 "../../src/codegen.in.c"
      } else {
        //| and eax, RUTILd
        dasm_put(Dst, 1420);
#line 1828 "../../src/codegen.in.c"
      }
      return;
    case ND_BITOR:
      if (is_long) {
        //| or rax, RUTIL
        dasm_put(Dst, 1034);
#line 1833 "../../src/codegen.in.c"
      } else {
        //| or eax, RUTILd
        dasm_put(Dst, 1035);
#line 1835 "../../src/codegen.in.c"
      }
      return;
    case ND_BITXOR:
      if (is_long) {
        //| xor rax, RUTIL
        dasm_put(Dst, 1424);
#line 1840 "../../src/codegen.in.c"
      } else {
        //| xor eax, RUTILd
        dasm_put(Dst, 1425);
#line 1842 "../../src/codegen.in.c"
      }
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      if (is_long) {
        //| cmp rax, RUTIL
        dasm_put(Dst, 1429);
#line 1850 "../../src/codegen.in.c"
      } else {
        //| cmp eax, RUTILd
        dasm_put(Dst, 1430);
#line 1852 "../../src/codegen.in.c"
      }

      if (node->kind == ND_EQ) {
        //| sete al
        dasm_put(Dst, 1360);
#line 1856 "../../src/codegen.in.c"
      } else if (node->kind == ND_NE) {
        //| setne al
        dasm_put(Dst, 1364);
#line 1858 "../../src/codegen.in.c"
      } else if (node->kind == ND_LT) {
        if (node->lhs->ty->is_unsigned) {
          //| setb al
          dasm_put(Dst, 1434);
#line 1861 "../../src/codegen.in.c"
        } else {
          //| setl al
          dasm_put(Dst, 1438);
#line 1863 "../../src/codegen.in.c"
        }
      } else if (node->kind == ND_LE) {
        if (node->lhs->ty->is_unsigned) {
          //| setbe al
          dasm_put(Dst, 1442);
#line 1867 "../../src/codegen.in.c"
        } else {
          //| setle al
          dasm_put(Dst, 1446);
#line 1869 "../../src/codegen.in.c"
        }
      }

      //| movzx rax, al
      dasm_put(Dst, 1073);
#line 1873 "../../src/codegen.in.c"
      return;
    case ND_SHL:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1450);
#line 1876 "../../src/codegen.in.c"
      if (is_long) {
        //| shl rax, cl
        dasm_put(Dst, 1455);
#line 1878 "../../src/codegen.in.c"
      } else {
        //| shl eax, cl
        dasm_put(Dst, 1456);
#line 1880 "../../src/codegen.in.c"
      }
      return;
    case ND_SHR:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1450);
#line 1884 "../../src/codegen.in.c"
      if (node->lhs->ty->is_unsigned) {
        if (is_long) {
          //| shr rax, cl
          dasm_put(Dst, 1459);
#line 1887 "../../src/codegen.in.c"
        } else {
          //| shr eax, cl
          dasm_put(Dst, 1460);
#line 1889 "../../src/codegen.in.c"
        }
      } else {
        if (is_long) {
          //| sar rax, cl
          dasm_put(Dst, 1463);
#line 1893 "../../src/codegen.in.c"
        } else {
          //| sar eax, cl
          dasm_put(Dst, 1464);
#line 1895 "../../src/codegen.in.c"
        }
      }
      return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node* node) {
#if X64WIN
  if (user_context->generate_debug_symbols) {
    record_line_syminfo(node->tok->file->file_no, node->tok->line_no, codegen_pclabel());
  }
#endif

  switch (node->kind) {
    case ND_IF: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1061, lelse);
#line 1917 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1065, lend, lelse);
#line 1920 "../../src/codegen.in.c"
      if (node->els)
        gen_stmt(node->els);
      //|=>lend:
      dasm_put(Dst, 0, lend);
#line 1923 "../../src/codegen.in.c"
      return;
    }
    case ND_FOR: {
      if (node->init)
        gen_stmt(node->init);
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 0, lbegin);
#line 1930 "../../src/codegen.in.c"
      if (node->cond) {
        gen_expr(node->cond);
        cmp_zero(node->cond->ty);
        //| je =>node->brk_pc_label
        dasm_put(Dst, 1061, node->brk_pc_label);
#line 1934 "../../src/codegen.in.c"
      }
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 0, node->cont_pc_label);
#line 1937 "../../src/codegen.in.c"
      if (node->inc)
        gen_expr(node->inc);
      //| jmp =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1065, lbegin, node->brk_pc_label);
#line 1941 "../../src/codegen.in.c"
      return;
    }
    case ND_DO: {
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 0, lbegin);
#line 1946 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 0, node->cont_pc_label);
#line 1948 "../../src/codegen.in.c"
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| jne =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1468, lbegin, node->brk_pc_label);
#line 1952 "../../src/codegen.in.c"
      return;
    }
    case ND_SWITCH:
      gen_expr(node->cond);

      for (Node* n = node->case_next; n; n = n->case_next) {
        bool is_long = node->cond->ty->size == 8;

        if (n->begin == n->end) {
          if (is_long) {
            //| cmp rax, n->begin
            dasm_put(Dst, 1473, n->begin);
#line 1963 "../../src/codegen.in.c"
          } else {
            //| cmp eax, n->begin
            dasm_put(Dst, 1474, n->begin);
#line 1965 "../../src/codegen.in.c"
          }
          //| je =>n->pc_label
          dasm_put(Dst, 1061, n->pc_label);
#line 1967 "../../src/codegen.in.c"
          continue;
        }

        // [GNU] Case ranges
        if (is_long) {
          //| mov RUTIL, rax
          //| sub RUTIL, n->begin
          //| cmp RUTIL, n->end - n->begin
          dasm_put(Dst, 1479, n->begin, n->end - n->begin);
#line 1975 "../../src/codegen.in.c"
        } else {
          //| mov RUTILd, eax
          //| sub RUTILd, n->begin
          //| cmp RUTILd, n->end - n->begin
          dasm_put(Dst, 1493, n->begin, n->end - n->begin);
#line 1979 "../../src/codegen.in.c"
        }
        //| jbe =>n->pc_label
        dasm_put(Dst, 1504, n->pc_label);
#line 1981 "../../src/codegen.in.c"
      }

      if (node->default_case) {
        //| jmp =>node->default_case->pc_label
        dasm_put(Dst, 1508, node->default_case->pc_label);
#line 1985 "../../src/codegen.in.c"
      }

      //| jmp =>node->brk_pc_label
      dasm_put(Dst, 1508, node->brk_pc_label);
#line 1988 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->brk_pc_label:
      dasm_put(Dst, 0, node->brk_pc_label);
#line 1990 "../../src/codegen.in.c"
      return;
    case ND_CASE:
      //|=>node->pc_label:
      dasm_put(Dst, 0, node->pc_label);
#line 1993 "../../src/codegen.in.c"
      gen_stmt(node->lhs);
      return;
    case ND_BLOCK:
      for (Node* n = node->body; n; n = n->next)
        gen_stmt(n);
      return;
    case ND_GOTO:
      //| jmp =>node->pc_label
      dasm_put(Dst, 1508, node->pc_label);
#line 2001 "../../src/codegen.in.c"
      return;
    case ND_GOTO_EXPR:
      gen_expr(node->lhs);
      //| jmp rax
      dasm_put(Dst, 1512);
#line 2005 "../../src/codegen.in.c"
      return;
    case ND_LABEL:
      //|=>node->pc_label:
      dasm_put(Dst, 0, node->pc_label);
#line 2008 "../../src/codegen.in.c"
      gen_stmt(node->lhs);
      return;
    case ND_RETURN:
      if (node->lhs) {
        gen_expr(node->lhs);
        Type* ty = node->lhs->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            if (
#if X64WIN
                type_passed_in_register(ty)
#else
                ty->size <= 16
#endif
            ) {
              copy_struct_reg();
            } else {
              copy_struct_mem();
            }
            break;
        }
      }

      //| jmp =>C(current_fn)->dasm_return_label
      dasm_put(Dst, 1508, C(current_fn)->dasm_return_label);
#line 2034 "../../src/codegen.in.c"
      return;
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      return;
    case ND_ASM:
      error_tok(node->tok, "asm statement not supported");
  }

  error_tok(node->tok, "invalid statement");
}

#if X64WIN

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj* prog) {
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    // outaf("--- %s\n", fn->name);

    // The parameter home area starts at 16 above rbp:
    //   ...
    //   stack arg 2 (6th arg)
    //   stack arg 1 (5th arg)
    //   R9 home
    //   R8 home
    //   RDX home
    //   RCX home
    //   return address pushed by call instr
    //   old RBP (for the called function)  <<< RBP after push rbp; mov rbp, rsp
    //   ...
    //   ... stack space used by called function
    //   ...
    //
    // The top of the diagram is addr 0xffffffff.. and the bottom is 0.
    // PUSH decrements RSP and then stores.
    // So, "top" means the highest numbered address corresponding the to root
    // function and bottom moves to the frames for the leaf-ward functions.
    int top = 16;
    int bottom = 8;

    int reg = 0;

    // Assign offsets to pass-by-stack parameters and register homes.
    for (Obj* var = fn->params; var; var = var->next) {
      Type* ty = var->ty;

      switch (ty->kind) {
        case TY_STRUCT:
        case TY_UNION:
          if (!type_passed_in_register(ty)) {
            // If it's too big for a register, then the value we're getting is a
            // pointer to a copy, rather than the actual value, so flag it as
            // such and then either assign a register or stack slot for the
            // reference.
            // outaf("by ref %s\n", var->name);
            // var->passed_by_reference = true;
          }

          // If the pointer to a referenced value or the value itself can be
          // passed in a register then assign here.
          if (reg++ < X64WIN_REG_MAX) {
            var->offset = top;
            // outaf("  assigned reg offset 0x%x\n", var->offset);
            top += 8;
            continue;
          }

          // Otherwise fall through to the stack slot assignment below.
          break;
        case TY_FLOAT:
        case TY_DOUBLE:
          if (reg++ < X64WIN_REG_MAX) {
            var->offset = top;
            top += 8;
            continue;
          }
          break;
        default:
          if (reg++ < X64WIN_REG_MAX) {
            var->offset = top;
            top += 8;
            // outaf("int reg %s at home 0x%x\n", var->name, var->offset);
            continue;
          }
      }

      var->offset = top;
      // outaf("int stack %s at stack 0x%x\n", var->name, var->offset);
      top += MAX(8, var->ty->size);
    }

    // Assign offsets to local variables.
    for (Obj* var = fn->locals; var; var = var->next) {
      if (var->offset) {
        continue;
      }

      int align =
          (var->ty->kind == TY_ARRAY && var->ty->size >= 16) ? MAX(16, var->align) : var->align;

      bottom += var->ty->size;
      bottom = (int)align_to_s(bottom, align);
      var->offset = -bottom;
      // outaf("local %s at -0x%x\n", var->name, -var->offset);
    }

    fn->stack_size = (int)align_to_s(bottom, 16);
  }
}

#else  // SysV

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj* prog) {
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    // If a function has many parameters, some parameters are
    // inevitably passed by stack rather than by register.
    // The first passed-by-stack parameter resides at RBP+16.
    int top = 16;
    int bottom = 0;

    int gp = 0, fp = 0;

    // Assign offsets to pass-by-stack parameters.
    for (Obj* var = fn->params; var; var = var->next) {
      Type* ty = var->ty;

      switch (ty->kind) {
        case TY_STRUCT:
        case TY_UNION:
          if (ty->size <= 8) {
            bool fp1 = has_flonum(ty, 0, 8, 0);
            if (fp + fp1 < SYSV_FP_MAX && gp + !fp1 < SYSV_GP_MAX) {
              fp = fp + fp1;
              gp = gp + !fp1;
              continue;
            }
          } else if (ty->size <= 16) {
            bool fp1 = has_flonum(ty, 0, 8, 0);
            bool fp2 = has_flonum(ty, 8, 16, 8);
            if (fp + fp1 + fp2 < SYSV_FP_MAX && gp + !fp1 + !fp2 < SYSV_GP_MAX) {
              fp = fp + fp1 + fp2;
              gp = gp + !fp1 + !fp2;
              continue;
            }
          }
          break;
        case TY_FLOAT:
        case TY_DOUBLE:
          if (fp++ < SYSV_FP_MAX)
            continue;
          break;
        case TY_LDOUBLE:
          break;
        default:
          if (gp++ < SYSV_GP_MAX)
            continue;
      }

      top = align_to_s(top, 8);
      var->offset = top;
      top += var->ty->size;
    }

    // Assign offsets to pass-by-register parameters and local variables.
    for (Obj* var = fn->locals; var; var = var->next) {
      if (var->offset)
        continue;

      // AMD64 System V ABI has a special alignment rule for an array of
      // length at least 16 bytes. We need to align such array to at least
      // 16-byte boundaries. See p.14 of
      // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
      int align =
          (var->ty->kind == TY_ARRAY && var->ty->size >= 16) ? MAX(16, var->align) : var->align;

      bottom += var->ty->size;
      bottom = align_to_s(bottom, align);
      var->offset = -bottom;
    }

    fn->stack_size = align_to_s(bottom, 16);
  }
}

#endif  // SysV

static void linkfixup_push(FileLinkData* fld, char* target, char* fixup, int addend) {
  if (!fld->fixups) {
    fld->fixups = calloc(8, sizeof(LinkFixup));
    fld->fcap = 8;
  }

  if (fld->fcap == fld->flen) {
    fld->fixups = realloc(fld->fixups, sizeof(LinkFixup) * fld->fcap * 2);
    fld->fcap *= 2;
  }

  fld->fixups[fld->flen++] = (LinkFixup){fixup, strdup(target), addend};
}

static void emit_data(Obj* prog) {
  for (Obj* var = prog; var; var = var->next) {
    // outaf("var->name %s %d %d %d %d\n", var->name, var->is_function, var->is_definition,
    // var->is_static, var->is_tentative);
    if (var->is_function)
      continue;

    if (!var->is_definition) {
      continue;
    }

    int align =
        (var->ty->kind == TY_ARRAY && var->ty->size >= 16) ? MAX(16, var->align) : var->align;

    // - rodata, always free existing entry in either static/extern
    // global_data, and then recreate and reinitialize
    //
    // - if writeable data has an entry, it shouldn't be recreated. the
    // dyo version doesn't reprocess kTypeInitializerDataRelocation or
    // kTypeInitializerCodeRelocation; that's possibly a bug, but it'll
    // need some testing to get a case where it comes up.
    //
    // TODO: if it changes from static to extern, is it the same
    // variable? currently they're separate, so a switch causes a
    // reinit, a leak, and some confusion.
    //
    // can't easily make a large single data segment allocation for all
    // data because 1) the rodata change size link-over-link (put in
    // codeseg?); 2) wdata don't move or reinit, but new ones get added
    // as code evolves and we can't blow away or move the old ones.
    //
    // for now, just continue with individual regular aligned_allocate
    // for all data objects and maintain their addresses here.

    UserContext* uc = user_context;
    // bool was_freed = false;
    size_t idx = var->is_static ? C(file_index) : uc->num_files;
    void* prev = hashmap_get(&user_context->global_data[idx], var->name);
    if (prev) {
      if (var->is_rodata) {
        aligned_free(prev);
        // was_freed = true;
      } else {
        // data already created and initialized, don't reinit.
        continue;
      }
    }

    void* global_data = aligned_allocate(var->ty->size, align);
    memset(global_data, 0, var->ty->size);

    // TODO: Is this wrong (or above)? If writable |x| in one file
    // already existed and |x| in another is added, then it'll be
    // silently ignored. If it's rodata it'll be silently replaced here
    // by getting thrown away above and then recreated.
    // Need to figure out where/how to have a duplicate symbol check.
#if 0
      if (!was_freed) {
        void* prev = hashmap_get(&uc->global_data[idx], strings.data[name_index]);
        if (prev) {
          outaf("duplicated symbol: %s\n", strings.data[name_index]);
          goto fail;
        }
      }
#endif
    // TODO: intern
    hashmap_put(&uc->global_data[idx], strdup(var->name), global_data);

    char* fillp = global_data;
    FileLinkData* fld = &uc->files[C(file_index)];

    // .data or .tdata
    if (var->init_data) {
      Relocation* rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          assert(!(rel->string_label && rel->internal_code_label));  // Shouldn't be both.
          assert(rel->string_label ||
                 rel->internal_code_label);  // But should be at least one if we're here.

          if (rel->string_label) {
            linkfixup_push(fld, *rel->string_label, fillp, rel->addend);
          } else {
            int offset = dasm_getpclabel(&C(dynasm), *rel->internal_code_label);
            *((uintptr_t*)fillp) = (uintptr_t)(fld->codeseg_base_address + offset + rel->addend);
          }

          rel = rel->next;
          pos += 8;
          fillp += 8;
        } else {
          *fillp++ = var->init_data[pos++];
        }
      }

      continue;
    }

    // If no init_data, then already allocated and cleared (.bss).
  }
}

static void store_fp(int r, int offset, int sz) {
  switch (sz) {
    case 4:
      //| movss dword [rbp+offset], xmm(r)
      dasm_put(Dst, 684, (r), offset);
#line 2347 "../../src/codegen.in.c"
      return;
    case 8:
      //| movsd qword [rbp+offset], xmm(r)
      dasm_put(Dst, 695, (r), offset);
#line 2350 "../../src/codegen.in.c"
      return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
    case 1:
      //| mov [rbp+offset], Rb(dasmargreg[r])
      dasm_put(Dst, 1516, (dasmargreg[r]), offset);
#line 2359 "../../src/codegen.in.c"
      return;
    case 2:
      //| mov [rbp+offset], Rw(dasmargreg[r])
      dasm_put(Dst, 1524, (dasmargreg[r]), offset);
#line 2362 "../../src/codegen.in.c"
      return;
      return;
    case 4:
      //| mov [rbp+offset], Rd(dasmargreg[r])
      dasm_put(Dst, 1525, (dasmargreg[r]), offset);
#line 2366 "../../src/codegen.in.c"
      return;
    case 8:
      //| mov [rbp+offset], Rq(dasmargreg[r])
      dasm_put(Dst, 1533, (dasmargreg[r]), offset);
#line 2369 "../../src/codegen.in.c"
      return;
    default:
      for (int i = 0; i < sz; i++) {
        //| mov [rbp+offset+i], Rb(dasmargreg[r])
        //| shr Rq(dasmargreg[r]), 8
        dasm_put(Dst, 706, (dasmargreg[r]), offset+i, (dasmargreg[r]));
#line 2374 "../../src/codegen.in.c"
      }
      return;
  }
}

#if X64WIN
extern int __chkstk(void);
#endif  // X64WIN

static void emit_text(Obj* prog) {
  // Preallocate the dasm labels so they can be used in functions out of order.
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    fn->dasm_return_label = codegen_pclabel();
    fn->dasm_entry_label = codegen_pclabel();
    fn->dasm_end_of_function_label = codegen_pclabel();
    fn->dasm_unwind_info_label = codegen_pclabel();
  }

  //| .code
  dasm_put(Dst, 1541);
#line 2396 "../../src/codegen.in.c"

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    //|=>fn->dasm_entry_label:
    dasm_put(Dst, 0, fn->dasm_entry_label);
#line 2402 "../../src/codegen.in.c"

    C(current_fn) = fn;

#if X64WIN
    record_line_syminfo(fn->ty->name->file->file_no, fn->ty->name->line_no, codegen_pclabel());
#endif

    // outaf("---- %s\n", fn->name);

    // Prologue
    //| push rbp
    //| mov rbp, rsp
    dasm_put(Dst, 1543);
#line 2414 "../../src/codegen.in.c"

#if X64WIN
    // Stack probe on Windows if necessary. The MSDN reference for __chkstk says
    // it's only necessary beyond 8k for x64, but cl does it at 4k.
    if (fn->stack_size >= 4096) {
      //| mov rax, fn->stack_size
      dasm_put(Dst, 928, fn->stack_size);
#line 2420 "../../src/codegen.in.c"
      int fixup_location = codegen_pclabel();
      strintarray_push(&C(fixups), (StringInt){"__chkstk", fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
      //|=>fixup_location:
      //| mov64 r10, 0xc0dec0dec0dec0de
      dasm_put(Dst, 1548, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 2428 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      //| call r10
      //| sub rsp, rax
      dasm_put(Dst, 1554);
#line 2433 "../../src/codegen.in.c"

      // TODO: pdata emission
    } else
#endif

    {
      //| sub rsp, fn->stack_size
      dasm_put(Dst, 619, fn->stack_size);
#line 2440 "../../src/codegen.in.c"

      // TODO: add a label here to assert that the prolog size is as expected

#if X64WIN
      // RtlAddFunctionTable() requires these to be at an offset with the same
      // base as the function offsets, so we need to emit these into the main
      // codeseg allocation, rather than just allocating them separately, since
      // we can't easily guarantee a <4G offset to them otherwise.

      // Unfortunately, we can't build another section with this as dynasm
      // doesn't seem to allow resolving these offsets, so this is done later
      //| .dword =>fn->dasm_entry_label
      //| .dword =>fn->dasm_end_of_function_label
      //| .dword =>fn->dasm_unwind_info_label

      // TODO: probably need to relocate and add info about r11 used as a base
      // for the value stack copies, and also rdi pushed for memsets.

      // https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170
      enum {
        UWOP_PUSH_NONVOL = 0,
        UWOP_ALLOC_LARGE = 1,
        UWOP_ALLOC_SMALL = 2,
        UWOP_SET_FPREG = 3,
      };

      // These are the UNWIND_INFO structure that is referenced by the third
      // element of RUNTIME_FUNCTION.
      //| .pdata
      dasm_put(Dst, 1562);
#line 2469 "../../src/codegen.in.c"
      // This takes care of cases where CountOfCodes is odd.
      //| .align 4
      //|=>fn->dasm_unwind_info_label:
      //| .byte 1  /* Version:3 (1) and Flags:5 (0) */
      dasm_put(Dst, 1564, fn->dasm_unwind_info_label, 1  /* Version:3 (1) and Flags:5 (0) */);
#line 2473 "../../src/codegen.in.c"
      bool small_stack = fn->stack_size / 8 - 1 <= 15;
      if (small_stack) {
        // We just happen to "know" this is the form used for small stack sizes.
        // xxxxxxxxxxxx0000 55                   push        rbp
        // xxxxxxxxxxxx0001 48 89 E5             mov         rbp,rsp
        // xxxxxxxxxxxx0004 48 83 EC 10          sub         rsp,10h
        // xxxxxxxxxxxx0009 ...
        //| .byte 8  /* SizeOfProlog */
        //| .byte 3  /* CountOfCodes */
        dasm_put(Dst, 1569, 8  /* SizeOfProlog */, 3  /* CountOfCodes */);
#line 2482 "../../src/codegen.in.c"
      } else {
        // And this one for larger reservations.
        // xxxxxxxxxxxx0000 55                   push        rbp
        // xxxxxxxxxxxx0001 48 89 E5             mov         rbp,rsp
        // xxxxxxxxxxxx0004 48 81 EC B0 01 00 00 sub         rsp,1B0h
        // xxxxxxxxxxxx000b ...
        //| .byte 11  /* SizeOfProlog */
        //| .byte 4  /* CountOfCodes */
        dasm_put(Dst, 1569, 11  /* SizeOfProlog */, 4  /* CountOfCodes */);
#line 2490 "../../src/codegen.in.c"
      }
      //| .byte 5  /* FrameRegister:4 (RBP) | FrameOffset:4: 0 offset */
      dasm_put(Dst, 985, 5  /* FrameRegister:4 (RBP) | FrameOffset:4: 0 offset */);
#line 2492 "../../src/codegen.in.c"

      if (small_stack) {
        //| .byte 8  /* CodeOffset */
        //| .byte UWOP_ALLOC_SMALL | (((unsigned char)((fn->stack_size / 8) - 1)) << 4)
        dasm_put(Dst, 1569, 8  /* CodeOffset */, UWOP_ALLOC_SMALL | (((unsigned char)((fn->stack_size / 8) - 1)) << 4));
#line 2496 "../../src/codegen.in.c"
      } else {
        //| .byte 11  /* CodeOffset */
        dasm_put(Dst, 985, 11  /* CodeOffset */);
#line 2498 "../../src/codegen.in.c"
        assert(fn->stack_size / 8 <= 65535 && "todo; not UWOP_ALLOC_LARGE 0-style");
        //| .byte UWOP_ALLOC_LARGE
        //| .word fn->stack_size / 8
        dasm_put(Dst, 1572, UWOP_ALLOC_LARGE, fn->stack_size / 8);
#line 2501 "../../src/codegen.in.c"
      }
      //| .byte 4  /* CodeOffset */
      //| .byte UWOP_SET_FPREG
      //| .byte 1  /* CodeOffset */
      //| .byte UWOP_PUSH_NONVOL | (5 /* RBP */ << 4)
      dasm_put(Dst, 1575, 4  /* CodeOffset */, UWOP_SET_FPREG, 1  /* CodeOffset */, UWOP_PUSH_NONVOL | (5 /* RBP */ << 4));
#line 2506 "../../src/codegen.in.c"

      //| .code
      dasm_put(Dst, 1541);
#line 2508 "../../src/codegen.in.c"
#endif
    }

    //| mov [rbp+fn->alloca_bottom->offset], rsp
    dasm_put(Dst, 1580, fn->alloca_bottom->offset);
#line 2512 "../../src/codegen.in.c"

#if !X64WIN
    // Save arg registers if function is variadic
    if (fn->va_area) {
      int gp = 0, fp = 0;
      for (Obj* var = fn->params; var; var = var->next) {
        if (is_flonum(var->ty))
          fp++;
        else
          gp++;
      }

      int off = fn->va_area->offset;

      // va_elem
      //| mov dword [rbp+off], gp*8            // gp_offset
      //| mov dword [rbp+off+4], fp * 8 + 48   // fp_offset
      //| mov [rbp+off+8], rbp                 // overflow_arg_area
      //| add qword [rbp+off+8], 16
      //| mov [rbp+off+16], rbp                // reg_save_area
      //| add qword [rbp+off+16], off+24
      dasm_put(Dst, 1585, off, gp*8, off+4, fp * 8 + 48, off+8, off+8, off+16, off+16, off+24);
#line 2533 "../../src/codegen.in.c"

      // __reg_save_area__
      //| mov [rbp + off + 24], rdi
      //| mov [rbp + off + 32], rsi
      //| mov [rbp + off + 40], rdx
      //| mov [rbp + off + 48], rcx
      //| mov [rbp + off + 56], r8
      //| mov [rbp + off + 64], r9
      //| movsd qword [rbp + off + 72], xmm0
      //| movsd qword [rbp + off + 80], xmm1
      //| movsd qword [rbp + off + 88], xmm2
      //| movsd qword [rbp + off + 96], xmm3
      //| movsd qword [rbp + off + 104], xmm4
      //| movsd qword [rbp + off + 112], xmm5
      //| movsd qword [rbp + off + 120], xmm6
      //| movsd qword [rbp + off + 128], xmm7
      dasm_put(Dst, 1612, off + 24, off + 32, off + 40, off + 48, off + 56, off + 64, off + 72, off + 80, off + 88, off + 96, off + 104, off + 112, off + 120, off + 128);
#line 2549 "../../src/codegen.in.c"
    }
#endif

#if X64WIN
    // If variadic, we have to store all registers; floats will have been
    // duplicated into the integer registers.
    if (fn->ty->is_variadic) {
      //| mov [rbp + 16], CARG1
      //| mov [rbp + 24], CARG2
      //| mov [rbp + 32], CARG3
      //| mov [rbp + 40], CARG4
      dasm_put(Dst, 1685, 16, 24, 32, 40);
#line 2560 "../../src/codegen.in.c"
    } else {
      // Save passed-by-register arguments to the stack
      int reg = 0;
      for (Obj* var = fn->params; var; var = var->next) {
        if (var->offset >= 16 + PARAMETER_SAVE_SIZE)
          continue;

        Type* ty = var->ty;

        switch (ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            // It's either small and so passed in a register, or isn't and then
            // we're instead storing the pointer to the larger struct.
            if (type_passed_in_register(ty)) {
              store_gp(reg++, var->offset, ty->size);
            } else {
              store_gp(reg++, var->offset, 8);
            }
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            store_fp(reg++, var->offset, ty->size);
            break;
          default:
            store_gp(reg++, var->offset, ty->size);
            break;
        }
      }
    }
#else
    // Save passed-by-register arguments to the stack
    int gp = 0, fp = 0;
    for (Obj* var = fn->params; var; var = var->next) {
      if (var->offset > 0)
        continue;

      Type* ty = var->ty;

      switch (ty->kind) {
        case TY_STRUCT:
        case TY_UNION:
          assert(ty->size <= 16);
          if (has_flonum(ty, 0, 8, 0))
            store_fp(fp++, var->offset, MIN(8, ty->size));
          else
            store_gp(gp++, var->offset, MIN(8, ty->size));

          if (ty->size > 8) {
            if (has_flonum(ty, 8, 16, 0))
              store_fp(fp++, var->offset + 8, ty->size - 8);
            else
              store_gp(gp++, var->offset + 8, ty->size - 8);
          }
          break;
        case TY_FLOAT:
        case TY_DOUBLE:
          store_fp(fp++, var->offset, ty->size);
          break;
        default:
          store_gp(gp++, var->offset, ty->size);
      }
    }
#endif

    // Emit code
    gen_stmt(fn->body);
    assert(C(depth) == 0);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(fn->name, "main") == 0) {
      //| mov rax, 0
      dasm_put(Dst, 736);
#line 2635 "../../src/codegen.in.c"
    }

    // Epilogue
    //|=>fn->dasm_return_label:
    dasm_put(Dst, 0, fn->dasm_return_label);
#line 2639 "../../src/codegen.in.c"
#if X64WIN
    // https://learn.microsoft.com/en-us/cpp/build/prolog-and-epilog?view=msvc-170#epilog-code
    // says this the required form to recognize an epilog.
    //| lea rsp, [rbp]
    dasm_put(Dst, 1702);
#line 2643 "../../src/codegen.in.c"
#else
    //| mov rsp, rbp
    dasm_put(Dst, 1707);
#line 2645 "../../src/codegen.in.c"
#endif
    //| pop rbp
    //| ret
    dasm_put(Dst, 1712);
#line 2648 "../../src/codegen.in.c"

    //|=>fn->dasm_end_of_function_label:
    dasm_put(Dst, 0, fn->dasm_end_of_function_label);
#line 2650 "../../src/codegen.in.c"
  }
}

static void fill_out_text_exports(Obj* prog, char* codeseg_base_address) {
  // per-file from any previous need to be cleared out for this round.
  hashmap_clear_manual_key_owned_value_unowned(&user_context->exports[C(file_index)]);

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    int offset = dasm_getpclabel(&C(dynasm), fn->dasm_entry_label);
    size_t idx = fn->is_static ? C(file_index) : user_context->num_files;
    hashmap_put(&user_context->exports[idx], strdup(fn->name), codeseg_base_address + offset);
  }
}

static void free_link_fixups(FileLinkData* fld) {
  for (int i = 0; i < fld->flen; ++i) {
    free(fld->fixups[i].name);
  }
  free(fld->fixups);
  fld->fixups = NULL;
  fld->flen = 0;
  fld->fcap = 0;
}

static void fill_out_fixups(FileLinkData* fld) {
  for (int i = 0; i < C(fixups).len; ++i) {
    int offset = dasm_getpclabel(&C(dynasm), C(fixups).data[i].i);
    // +2 is a hack taking advantage of the fact that import fixups are always
    // of the form `mov64 rax, <ADDR>` which is encoded as:
    //   48 B8 <8 byte address>
    // so skip over the mov64 prefix and point directly at the address to be
    // slapped into place.
    offset += 2;

    char* fixup = fld->codeseg_base_address + offset;
    linkfixup_push(fld, C(fixups).data[i].str, fixup, /*addend=*/0);
  }
}

#if X64WIN

typedef struct RuntimeFunction {
  unsigned long BeginAddress;
  unsigned long EndAddress;
  unsigned long UnwindData;
} RuntimeFunction;

static void emit_symbols_and_exception_function_table(Obj* prog,
                                                      char* base_addr,
                                                      int pdata_start_offset,
                                                      int pdata_end_offset) {
  int func_count = 0;
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    ++func_count;
  }

  size_t alloc_size = (sizeof(RuntimeFunction) * func_count);

  unregister_and_free_function_table_data(user_context);
  char* function_table_data = malloc(alloc_size);
  user_context->function_table_data = function_table_data;
  char* pfuncs = function_table_data;

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    RuntimeFunction* rf = (RuntimeFunction*)pfuncs;
    int func_start_offset = dasm_getpclabel(&C(dynasm), fn->dasm_entry_label);
    rf->BeginAddress = func_start_offset;
    rf->EndAddress = dasm_getpclabel(&C(dynasm), fn->dasm_end_of_function_label);
    rf->UnwindData = dasm_getpclabel(&C(dynasm), fn->dasm_unwind_info_label);
    pfuncs += sizeof(RuntimeFunction);

    if (user_context->generate_debug_symbols) {
      DbpFunctionSymbol* dbp_func_sym =
          dbp_add_function_symbol(user_context->dbp_ctx, fn->name, fn->ty->name->filename,
                                  dasm_getpclabel(&C(dynasm), fn->dasm_entry_label),
                                  dasm_getpclabel(&C(dynasm), fn->dasm_end_of_function_label));
      for (int i = 0; i < fn->file_line_label_data.len; ++i) {
        // TODO: ignoring file index, might not be needed unless something got
        // inlined (which doesn't happen). Maybe there's a macro case that could
        // cause it already though?
        int offset = dasm_getpclabel(&C(dynasm), fn->file_line_label_data.data[i].c);
        dbp_add_line_mapping(user_context->dbp_ctx, dbp_func_sym, offset,
                             fn->file_line_label_data.data[i].b);
      }
    }
  }

  if (user_context->generate_debug_symbols) {
    char* unwind_base = base_addr + pdata_start_offset;
    size_t unwind_len = pdata_end_offset - pdata_start_offset;
    DbpExceptionTables exception_tables = {
        .pdata = (DbpRUNTIME_FUNCTION*)user_context->function_table_data,
        .num_pdata_entries = func_count,
        .unwind_info = (unsigned char*)unwind_base,
        .unwind_info_byte_length = unwind_len,
    };
    dbp_ready_to_execute(user_context->dbp_ctx, &exception_tables);
  }

  register_function_table_data(user_context, func_count, base_addr);
}

#else  // !X64WIN

static void emit_symbols_and_exception_function_table(Obj* prog,
                                                      char* base_addr,
                                                      int pdata_start_offset,
                                                      int pdata_end_offset) {
  (void)prog;
  (void)base_addr;
  (void)pdata_start_offset;
  (void)pdata_end_offset;
}

#endif

static void codegen_init(void) {
  dasm_init(&C(dynasm), DASM_MAXSECTION);
  dasm_growpc(&C(dynasm), 1 << 16);  // Arbitrary number to avoid lots of reallocs of that array.

  C(numlabels) = 1;
}

static void codegen(Obj* prog, size_t file_index) {
  C(file_index) = file_index;

  void* globals[dynasm_globals_MAX + 1];
  dasm_setupglobal(&C(dynasm), globals, dynasm_globals_MAX + 1);

  dasm_setup(&C(dynasm), dynasm_actions);

  //| .pdata
  dasm_put(Dst, 1562);
#line 2791 "../../src/codegen.in.c"
  int start_of_pdata = codegen_pclabel();
  //|.align 4
  //|=>start_of_pdata:
  //| .code
  dasm_put(Dst, 1715, start_of_pdata);
#line 2795 "../../src/codegen.in.c"

  assign_lvar_offsets(prog);
  emit_text(prog);

  //| .pdata
  dasm_put(Dst, 1562);
#line 2800 "../../src/codegen.in.c"
  int end_of_pdata = codegen_pclabel();
  //|=>end_of_pdata:
  //| .code
  dasm_put(Dst, 1717, end_of_pdata);
#line 2803 "../../src/codegen.in.c"

  size_t code_size;
  dasm_link(&C(dynasm), &code_size);

  FileLinkData* fld = &user_context->files[C(file_index)];
  if (fld->codeseg_base_address) {
    free_executable_memory(fld->codeseg_base_address, fld->codeseg_size);
  }
  // VirtualAlloc and mmap don't accept 0.
  if (code_size == 0)
    code_size = 1;
  unsigned int page_sized = (unsigned int)align_to_u(code_size, get_page_size());

  fld->codeseg_size = page_sized;
#if X64WIN
  if (user_context->generate_debug_symbols) {
    user_context->dbp_ctx = dbp_create(fld->codeseg_size, get_temp_pdb_filename(AL_Compile));
    fld->codeseg_base_address = dbp_get_image_base(user_context->dbp_ctx);
  } else {
    fld->codeseg_base_address = allocate_writable_memory(page_sized);
  }
#else
  fld->codeseg_base_address = allocate_writable_memory(page_sized);
#endif
  // outaf("code_size: %zu, page_sized: %zu\n", code_size, page_sized);

  fill_out_text_exports(prog, fld->codeseg_base_address);

  free_link_fixups(fld);
  emit_data(prog);  // This needs to point into code for fixups, so has to go late-ish.
  fill_out_fixups(fld);

  dasm_encode(&C(dynasm), fld->codeseg_base_address);

#if 0
  FILE* f = fopen("code.raw", "wb");
  fwrite(fld->codeseg_base_address, code_size, 1, f);
  fclose(f);
  system("ndisasm -b64 code.raw");
#endif

  int check_result = dasm_checkstep(&C(dynasm), 0);
  if (check_result != DASM_S_OK) {
    outaf("check_result: 0x%08x\n", check_result);
    ABORT("dasm_checkstep failed");
  }

  emit_symbols_and_exception_function_table(prog, fld->codeseg_base_address,
                                            dasm_getpclabel(&C(dynasm), start_of_pdata),
                                            dasm_getpclabel(&C(dynasm), end_of_pdata));

  codegen_free();
}

// This can be called after a longjmp in update.
static void codegen_free(void) {
  if (C(dynasm)) {
    dasm_free(&C(dynasm));
  }
}
//
// END OF codegen.l.c
//
#endif // !X64WIN
