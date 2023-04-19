//
// Amalgamated (single file) build of https://github.com/sgraham/dyibicc.
// Revision: 47ae33d77e512f42fb6c811e98033a13abf4d81a
//
// This file should not be edited or modified, patches should be to the
// non-amalgamated files in src/. The user-facing API is in libdyibicc.h
// which should be located in the same directory as this .c file.
//

#undef C
#undef L
#undef VOID
//
// START OF src/dyibicc.h
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
static bool make_memory_executable(void* m, size_t size);
static void free_executable_memory(void* p, size_t size);

//
// util.c
//

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

typedef struct ByteArray {
  char* data;
  int capacity;
  int len;
} ByteArray;

typedef struct IntInt {
  int a;
  int b;
} IntInt;

typedef struct IntIntArray {
  IntInt* data;
  int capacity;
  int len;
} IntIntArray;

static char* bumpstrndup(const char* s, size_t n, AllocLifetime lifetime);
static char* bumpstrdup(const char* s, AllocLifetime lifetime);
static char* dirname(char* s);
static uint64_t align_to_u(uint64_t n, uint64_t align);
static int64_t align_to_s(int64_t n, int64_t align);
static unsigned int get_page_size(void);
static void strarray_push(StringArray* arr, char* s, AllocLifetime lifetime);
static void strintarray_push(StringIntArray* arr, StringInt item, AllocLifetime lifetime);
static char* format(AllocLifetime lifetime, char* fmt, ...)
    __attribute__((format(printf, 2, 3)));
static char* read_file(char* path, AllocLifetime lifetime);
static NORETURN void error(char* fmt, ...) __attribute__((format(printf, 1, 2)));
static NORETURN void error_at(char* loc, char* fmt, ...) __attribute__((format(printf, 2, 3)));
static NORETURN void error_tok(Token* tok, char* fmt, ...)
    __attribute__((format(printf, 2, 3)));
static NORETURN void error_internal(char* file, int line, char* msg);
static int outaf(const char* fmt, ...) __attribute__((format(printf, 1, 2)));
static void warn_tok(Token* tok, char* fmt, ...) __attribute__((format(printf, 2, 3)));

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

typedef struct {
  char* name;
  char* contents;

  // For #line directive
  char* display_name;
  int line_delta;
} File;

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

  uintptr_t rty;
};

static Node* new_cast(Node* expr, Type* ty);
static int64_t const_expr(Token** rest, Token* tok);
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
static Type* array_of(Type* base, int size);
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

typedef struct UserContext {
  DyibiccFunctionLookupFn get_function_address;
  DyibiccOutputFn output_function;
  bool use_ansi_codes;

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
} UserContext;

typedef struct dasm_State dasm_State;

typedef struct CompilerState {
  // tokenize.c
  File* tokenize__current_file;  // Input file
  bool tokenize__at_bol;         // True if the current position is at the beginning of a line
  bool tokenize__has_space;      // True if the current position follows a space character
  HashMap tokenize__keyword_map;

  // preprocess.c
  HashMap preprocess__macros;
  CondIncl* preprocess__cond_incl;
  HashMap preprocess__pragma_once;
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

  // codegen.in.c
  int codegen__depth;
  size_t codegen__file_index;
  dasm_State* codegen__dynasm;
  Obj* codegen__current_fn;
  int codegen__numlabels;
  StringIntArray codegen__fixups;
  IntIntArray codegen__pending_code_pclabels;

  // main.c
  char* main__base_file;
} CompilerState;

typedef struct LinkerState {
  // link.c
  HashMap link__runtime_function_map;
} LinkerState;

//
// END OF src/dyibicc.h
//
#undef C
#undef L
#undef VOID
//
// START OF include/all/reflect.h
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
// END OF include/all/reflect.h
//
#undef C
#undef L
#undef VOID
//
// START OF src/khash.h
//
/* The MIT License

   Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

/*
  An example:

KHASH_MAP_INIT_INT(32, char)
int main() {
        int ret, is_missing;
        khiter_t k;
        khash_t(32) *h = kh_init(32);
        k = kh_put(32, h, 5, &ret);
        kh_value(h, k) = 10;
        k = kh_get(32, h, 10);
        is_missing = (k == kh_end(h));
        k = kh_get(32, h, 5);
        kh_del(32, h, k);
        for (k = kh_begin(h); k != kh_end(h); ++k)
                if (kh_exist(h, k)) kh_value(h, k) = 1;
        kh_destroy(32, h);
        return 0;
}
*/

/*
  2013-05-02 (0.2.8):

        * Use quadratic probing. When the capacity is power of 2, stepping function
          i*(i+1)/2 guarantees to traverse each bucket. It is better than double
          hashing on cache performance and is more robust than linear probing.

          In theory, double hashing should be more robust than quadratic probing.
          However, my implementation is probably not for large hash tables, because
          the second hash function is closely tied to the first hash function,
          which reduce the effectiveness of double hashing.

        Reference: http://research.cs.vt.edu/AVresearch/hashing/quadratic.php

  2011-12-29 (0.2.7):

    * Minor code clean up; no actual effect.

  2011-09-16 (0.2.6):

        * The capacity is a power of 2. This seems to dramatically improve the
          speed for simple keys. Thank Zilong Tan for the suggestion. Reference:

           - http://code.google.com/p/ulib/
           - http://nothings.org/computer/judy/

        * Allow to optionally use linear probing which usually has better
          performance for random input. Double hashing is still the default as it
          is more robust to certain non-random input.

        * Added Wang's integer hash function (not used by default). This hash
          function is more robust to certain non-random input.

  2011-02-14 (0.2.5):

    * Allow to declare global functions.

  2009-09-26 (0.2.4):

    * Improve portability

  2008-09-19 (0.2.3):

        * Corrected the example
        * Improved interfaces

  2008-09-11 (0.2.2):

        * Improved speed a little in kh_put()

  2008-09-10 (0.2.1):

        * Added kh_clear()
        * Fixed a compiling error

  2008-09-02 (0.2.0):

        * Changed to token concatenation which increases flexibility.

  2008-08-31 (0.1.2):

        * Fixed a bug in kh_get(), which has not been tested previously.

  2008-08-31 (0.1.1):

        * Added destructor
*/

#ifndef __AC_KHASH_H
#define __AC_KHASH_H

/*!
  @header

  Generic hash table library.
 */

#define AC_VERSION_KHASH_H "0.2.8"

#include <limits.h>
#include <stdlib.h>
#include <string.h>

/* compiler specific configuration */

#if UINT_MAX == 0xffffffffu
typedef unsigned int khint32_t;
#elif ULONG_MAX == 0xffffffffu
typedef unsigned long khint32_t;
#endif

#if ULONG_MAX == ULLONG_MAX
typedef unsigned long khint64_t;
#else
typedef unsigned long long khint64_t;
#endif

#ifndef kh_inline
#ifdef _MSC_VER
#define kh_inline __inline
#else
#define kh_inline inline
#endif
#endif /* kh_inline */

#ifndef klib_unused
#if (defined __clang__ && __clang_major__ >= 3) || (defined __GNUC__ && __GNUC__ >= 3)
#define klib_unused __attribute__((__unused__))
#else
#define klib_unused
#endif
#endif /* klib_unused */

typedef khint32_t khint_t;
typedef khint_t khiter_t;

#define __ac_isempty(flag, i) ((flag[i >> 4] >> ((i & 0xfU) << 1)) & 2)
#define __ac_isdel(flag, i) ((flag[i >> 4] >> ((i & 0xfU) << 1)) & 1)
#define __ac_iseither(flag, i) ((flag[i >> 4] >> ((i & 0xfU) << 1)) & 3)
#define __ac_set_isdel_false(flag, i) (flag[i >> 4] &= ~(1ul << ((i & 0xfU) << 1)))
#define __ac_set_isempty_false(flag, i) (flag[i >> 4] &= ~(2ul << ((i & 0xfU) << 1)))
#define __ac_set_isboth_false(flag, i) (flag[i >> 4] &= ~(3ul << ((i & 0xfU) << 1)))
#define __ac_set_isdel_true(flag, i) (flag[i >> 4] |= 1ul << ((i & 0xfU) << 1))

#define __ac_fsize(m) ((m) < 16 ? 1 : (m) >> 4)

#ifndef kroundup32
#define kroundup32(x)                                                                           \
  (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4, (x) |= (x) >> 8, (x) |= (x) >> 16, \
   ++(x))
#endif

#ifndef kcalloc
#define kcalloc(N, Z) calloc(N, Z)
#endif
#ifndef kmalloc
#define kmalloc(Z) malloc(Z)
#endif
#ifndef krealloc
#define krealloc(P, Z) realloc(P, Z)
#endif
#ifndef kfree
#define kfree(P) free(P)
#endif

static const double __ac_HASH_UPPER = 0.77;

#define __KHASH_TYPE(name, khkey_t, khval_t)          \
  typedef struct kh_##name##_s {                      \
    khint_t n_buckets, size, n_occupied, upper_bound; \
    khint32_t* flags;                                 \
    khkey_t* keys;                                    \
    khval_t* vals;                                    \
  } kh_##name##_t;

#define __KHASH_PROTOTYPES(name, khkey_t, khval_t)                       \
  extern kh_##name##_t* kh_init_##name(void);                            \
  extern void kh_destroy_##name(kh_##name##_t* h);                       \
  extern void kh_clear_##name(kh_##name##_t* h);                         \
  extern khint_t kh_get_##name(const kh_##name##_t* h, khkey_t key);     \
  extern int kh_resize_##name(kh_##name##_t* h, khint_t new_n_buckets);  \
  extern khint_t kh_put_##name(kh_##name##_t* h, khkey_t key, int* ret); \
  extern void kh_del_##name(kh_##name##_t* h, khint_t x);

#define __KHASH_IMPL(name, SCOPE, khkey_t, khval_t, kh_is_map, __hash_func, __hash_equal)          \
  SCOPE kh_##name##_t* kh_init_##name(void) {                                                      \
    return (kh_##name##_t*)kcalloc(1, sizeof(kh_##name##_t));                                      \
  }                                                                                                \
  SCOPE void kh_destroy_##name(kh_##name##_t* h) {                                                 \
    if (h) {                                                                                       \
      kfree((void*)h->keys);                                                                       \
      kfree(h->flags);                                                                             \
      kfree((void*)h->vals);                                                                       \
      kfree(h);                                                                                    \
    }                                                                                              \
  }                                                                                                \
  SCOPE void kh_clear_##name(kh_##name##_t* h) {                                                   \
    if (h && h->flags) {                                                                           \
      memset(h->flags, 0xaa, __ac_fsize(h->n_buckets) * sizeof(khint32_t));                        \
      h->size = h->n_occupied = 0;                                                                 \
    }                                                                                              \
  }                                                                                                \
  SCOPE khint_t kh_get_##name(const kh_##name##_t* h, khkey_t key) {                               \
    if (h->n_buckets) {                                                                            \
      khint_t k, i, last, mask, step = 0;                                                          \
      mask = h->n_buckets - 1;                                                                     \
      k = __hash_func(key);                                                                        \
      i = k & mask;                                                                                \
      last = i;                                                                                    \
      while (!__ac_isempty(h->flags, i) &&                                                         \
             (__ac_isdel(h->flags, i) || !__hash_equal(h->keys[i], key))) {                        \
        i = (i + (++step)) & mask;                                                                 \
        if (i == last)                                                                             \
          return h->n_buckets;                                                                     \
      }                                                                                            \
      return __ac_iseither(h->flags, i) ? h->n_buckets : i;                                        \
    } else                                                                                         \
      return 0;                                                                                    \
  }                                                                                                \
  SCOPE int kh_resize_##name(                                                                      \
      kh_##name##_t* h,                                                                            \
      khint_t new_n_buckets) { /* This function uses 0.25*n_buckets bytes of working space instead \
                                  of [sizeof(key_t+val_t)+.25]*n_buckets. */                       \
    khint32_t* new_flags = 0;                                                                      \
    khint_t j = 1;                                                                                 \
    {                                                                                              \
      kroundup32(new_n_buckets);                                                                   \
      if (new_n_buckets < 4)                                                                       \
        new_n_buckets = 4;                                                                         \
      if (h->size >= (khint_t)(new_n_buckets * __ac_HASH_UPPER + 0.5))                             \
        j = 0; /* requested size is too small */                                                   \
      else {   /* hash table size to be changed (shrink or expand); rehash */                      \
        new_flags = (khint32_t*)kmalloc(__ac_fsize(new_n_buckets) * sizeof(khint32_t));            \
        if (!new_flags)                                                                            \
          return -1;                                                                               \
        memset(new_flags, 0xaa, __ac_fsize(new_n_buckets) * sizeof(khint32_t));                    \
        if (h->n_buckets < new_n_buckets) { /* expand */                                           \
          khkey_t* new_keys = (khkey_t*)krealloc((void*)h->keys, new_n_buckets * sizeof(khkey_t)); \
          if (!new_keys) {                                                                         \
            kfree(new_flags);                                                                      \
            return -1;                                                                             \
          }                                                                                        \
          h->keys = new_keys;                                                                      \
          if (kh_is_map) {                                                                         \
            khval_t* new_vals =                                                                    \
                (khval_t*)krealloc((void*)h->vals, new_n_buckets * sizeof(khval_t));               \
            if (!new_vals) {                                                                       \
              kfree(new_flags);                                                                    \
              return -1;                                                                           \
            }                                                                                      \
            h->vals = new_vals;                                                                    \
          }                                                                                        \
        } /* otherwise shrink */                                                                   \
      }                                                                                            \
    }                                                                                              \
    if (j) { /* rehashing is needed */                                                             \
      for (j = 0; j != h->n_buckets; ++j) {                                                        \
        if (__ac_iseither(h->flags, j) == 0) {                                                     \
          khkey_t key = h->keys[j];                                                                \
          khval_t val;                                                                             \
          khint_t new_mask;                                                                        \
          new_mask = new_n_buckets - 1;                                                            \
          if (kh_is_map)                                                                           \
            val = h->vals[j];                                                                      \
          __ac_set_isdel_true(h->flags, j);                                                        \
          while (1) { /* kick-out process; sort of like in Cuckoo hashing */                       \
            khint_t k, i, step = 0;                                                                \
            k = __hash_func(key);                                                                  \
            i = k & new_mask;                                                                      \
            while (!__ac_isempty(new_flags, i))                                                    \
              i = (i + (++step)) & new_mask;                                                       \
            __ac_set_isempty_false(new_flags, i);                                                  \
            if (i < h->n_buckets &&                                                                \
                __ac_iseither(h->flags, i) == 0) { /* kick out the existing element */             \
              {                                                                                    \
                khkey_t tmp = h->keys[i];                                                          \
                h->keys[i] = key;                                                                  \
                key = tmp;                                                                         \
              }                                                                                    \
              if (kh_is_map) {                                                                     \
                khval_t tmp = h->vals[i];                                                          \
                h->vals[i] = val;                                                                  \
                val = tmp;                                                                         \
              }                                                                                    \
              __ac_set_isdel_true(h->flags, i); /* mark it as deleted in the old hash table */     \
            } else {                            /* write the element and jump out of the loop */   \
              h->keys[i] = key;                                                                    \
              if (kh_is_map)                                                                       \
                h->vals[i] = val;                                                                  \
              break;                                                                               \
            }                                                                                      \
          }                                                                                        \
        }                                                                                          \
      }                                                                                            \
      if (h->n_buckets > new_n_buckets) { /* shrink the hash table */                              \
        h->keys = (khkey_t*)krealloc((void*)h->keys, new_n_buckets * sizeof(khkey_t));             \
        if (kh_is_map)                                                                             \
          h->vals = (khval_t*)krealloc((void*)h->vals, new_n_buckets * sizeof(khval_t));           \
      }                                                                                            \
      kfree(h->flags); /* free the working space */                                                \
      h->flags = new_flags;                                                                        \
      h->n_buckets = new_n_buckets;                                                                \
      h->n_occupied = h->size;                                                                     \
      h->upper_bound = (khint_t)(h->n_buckets * __ac_HASH_UPPER + 0.5);                            \
    }                                                                                              \
    return 0;                                                                                      \
  }                                                                                                \
  SCOPE khint_t kh_put_##name(kh_##name##_t* h, khkey_t key, int* ret) {                           \
    khint_t x;                                                                                     \
    if (h->n_occupied >= h->upper_bound) { /* update the hash table */                             \
      if (h->n_buckets > (h->size << 1)) {                                                         \
        if (kh_resize_##name(h, h->n_buckets - 1) < 0) { /* clear "deleted" elements */            \
          *ret = -1;                                                                               \
          return h->n_buckets;                                                                     \
        }                                                                                          \
      } else if (kh_resize_##name(h, h->n_buckets + 1) < 0) { /* expand the hash table */          \
        *ret = -1;                                                                                 \
        return h->n_buckets;                                                                       \
      }                                                                                            \
    } /* TODO: to implement automatically shrinking; resize() already support shrinking */         \
    {                                                                                              \
      khint_t k, i, site, last, mask = h->n_buckets - 1, step = 0;                                 \
      x = site = h->n_buckets;                                                                     \
      k = __hash_func(key);                                                                        \
      i = k & mask;                                                                                \
      if (__ac_isempty(h->flags, i))                                                               \
        x = i; /* for speed up */                                                                  \
      else {                                                                                       \
        last = i;                                                                                  \
        while (!__ac_isempty(h->flags, i) &&                                                       \
               (__ac_isdel(h->flags, i) || !__hash_equal(h->keys[i], key))) {                      \
          if (__ac_isdel(h->flags, i))                                                             \
            site = i;                                                                              \
          i = (i + (++step)) & mask;                                                               \
          if (i == last) {                                                                         \
            x = site;                                                                              \
            break;                                                                                 \
          }                                                                                        \
        }                                                                                          \
        if (x == h->n_buckets) {                                                                   \
          if (__ac_isempty(h->flags, i) && site != h->n_buckets)                                   \
            x = site;                                                                              \
          else                                                                                     \
            x = i;                                                                                 \
        }                                                                                          \
      }                                                                                            \
    }                                                                                              \
    if (__ac_isempty(h->flags, x)) { /* not present at all */                                      \
      h->keys[x] = key;                                                                            \
      __ac_set_isboth_false(h->flags, x);                                                          \
      ++h->size;                                                                                   \
      ++h->n_occupied;                                                                             \
      *ret = 1;                                                                                    \
    } else if (__ac_isdel(h->flags, x)) { /* deleted */                                            \
      h->keys[x] = key;                                                                            \
      __ac_set_isboth_false(h->flags, x);                                                          \
      ++h->size;                                                                                   \
      *ret = 2;                                                                                    \
    } else                                                                                         \
      *ret = 0; /* Don't touch h->keys[x] if present and not deleted */                            \
    return x;                                                                                      \
  }                                                                                                \
  SCOPE void kh_del_##name(kh_##name##_t* h, khint_t x) {                                          \
    if (x != h->n_buckets && !__ac_iseither(h->flags, x)) {                                        \
      __ac_set_isdel_true(h->flags, x);                                                            \
      --h->size;                                                                                   \
    }                                                                                              \
  }

#define KHASH_DECLARE(name, khkey_t, khval_t) \
  __KHASH_TYPE(name, khkey_t, khval_t)        \
  __KHASH_PROTOTYPES(name, khkey_t, khval_t)

#define KHASH_INIT2(name, SCOPE, khkey_t, khval_t, kh_is_map, __hash_func, __hash_equal) \
  __KHASH_TYPE(name, khkey_t, khval_t)                                                   \
  __KHASH_IMPL(name, SCOPE, khkey_t, khval_t, kh_is_map, __hash_func, __hash_equal)

#define KHASH_INIT(name, khkey_t, khval_t, kh_is_map, __hash_func, __hash_equal)            \
  KHASH_INIT2(name, static kh_inline klib_unused, khkey_t, khval_t, kh_is_map, __hash_func, \
              __hash_equal)

/* --- BEGIN OF HASH FUNCTIONS --- */

/*! @function
  @abstract     Integer hash function
  @param  key   The integer [khint32_t]
  @return       The hash value [khint_t]
 */
#define kh_int_hash_func(key) (khint32_t)(key)
/*! @function
  @abstract     Integer comparison function
 */
#define kh_int_hash_equal(a, b) ((a) == (b))
/*! @function
  @abstract     64-bit integer hash function
  @param  key   The integer [khint64_t]
  @return       The hash value [khint_t]
 */
#define kh_int64_hash_func(key) (khint32_t)((key) >> 33 ^ (key) ^ (key) << 11)
/*! @function
  @abstract     64-bit integer comparison function
 */
#define kh_int64_hash_equal(a, b) ((a) == (b))
/*! @function
  @abstract     const char* hash function
  @param  s     Pointer to a null terminated string
  @return       The hash value
 */
static kh_inline khint_t __ac_X31_hash_string(const char* s) {
  khint_t h = (khint_t)*s;
  if (h)
    for (++s; *s; ++s)
      h = (h << 5) - h + (khint_t)*s;
  return h;
}
/*! @function
  @abstract     Another interface to const char* hash function
  @param  key   Pointer to a null terminated string [const char*]
  @return       The hash value [khint_t]
 */
#define kh_str_hash_func(key) __ac_X31_hash_string(key)
/*! @function
  @abstract     Const char* comparison function
 */
#define kh_str_hash_equal(a, b) (strcmp(a, b) == 0)

static kh_inline khint_t __ac_Wang_hash(khint_t key) {
  key += ~(key << 15);
  key ^= (key >> 10);
  key += (key << 3);
  key ^= (key >> 6);
  key += ~(key << 11);
  key ^= (key >> 16);
  return key;
}
#define kh_int_hash_func2(key) __ac_Wang_hash((khint_t)key)

/* --- END OF HASH FUNCTIONS --- */

/* Other convenient macros... */

/*!
  @abstract Type of the hash table.
  @param  name  Name of the hash table [symbol]
 */
#define khash_t(name) kh_##name##_t

/*! @function
  @abstract     Initiate a hash table.
  @param  name  Name of the hash table [symbol]
  @return       Pointer to the hash table [khash_t(name)*]
 */
#define kh_init(name) kh_init_##name()

/*! @function
  @abstract     Destroy a hash table.
  @param  name  Name of the hash table [symbol]
  @param  h     Pointer to the hash table [khash_t(name)*]
 */
#define kh_destroy(name, h) kh_destroy_##name(h)

/*! @function
  @abstract     Reset a hash table without deallocating memory.
  @param  name  Name of the hash table [symbol]
  @param  h     Pointer to the hash table [khash_t(name)*]
 */
#define kh_clear(name, h) kh_clear_##name(h)

/*! @function
  @abstract     Resize a hash table.
  @param  name  Name of the hash table [symbol]
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  s     New size [khint_t]
 */
#define kh_resize(name, h, s) kh_resize_##name(h, s)

/*! @function
  @abstract     Insert a key to the hash table.
  @param  name  Name of the hash table [symbol]
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  k     Key [type of keys]
  @param  r     Extra return code: -1 if the operation failed;
                0 if the key is present in the hash table;
                1 if the bucket is empty (never used); 2 if the element in
                                the bucket has been deleted [int*]
  @return       Iterator to the inserted element [khint_t]
 */
#define kh_put(name, h, k, r) kh_put_##name(h, k, r)

/*! @function
  @abstract     Retrieve a key from the hash table.
  @param  name  Name of the hash table [symbol]
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  k     Key [type of keys]
  @return       Iterator to the found element, or kh_end(h) if the element is absent [khint_t]
 */
#define kh_get(name, h, k) kh_get_##name(h, k)

/*! @function
  @abstract     Remove a key from the hash table.
  @param  name  Name of the hash table [symbol]
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  k     Iterator to the element to be deleted [khint_t]
 */
#define kh_del(name, h, k) kh_del_##name(h, k)

/*! @function
  @abstract     Test whether a bucket contains data.
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  x     Iterator to the bucket [khint_t]
  @return       1 if containing data; 0 otherwise [int]
 */
#define kh_exist(h, x) (!__ac_iseither((h)->flags, (x)))

/*! @function
  @abstract     Get key given an iterator
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  x     Iterator to the bucket [khint_t]
  @return       Key [type of keys]
 */
#define kh_key(h, x) ((h)->keys[x])

/*! @function
  @abstract     Get value given an iterator
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  x     Iterator to the bucket [khint_t]
  @return       Value [type of values]
  @discussion   For hash sets, calling this results in segfault.
 */
#define kh_val(h, x) ((h)->vals[x])

/*! @function
  @abstract     Alias of kh_val()
 */
#define kh_value(h, x) ((h)->vals[x])

/*! @function
  @abstract     Get the start iterator
  @param  h     Pointer to the hash table [khash_t(name)*]
  @return       The start iterator [khint_t]
 */
#define kh_begin(h) (khint_t)(0)

/*! @function
  @abstract     Get the end iterator
  @param  h     Pointer to the hash table [khash_t(name)*]
  @return       The end iterator [khint_t]
 */
#define kh_end(h) ((h)->n_buckets)

/*! @function
  @abstract     Get the number of elements in the hash table
  @param  h     Pointer to the hash table [khash_t(name)*]
  @return       Number of elements in the hash table [khint_t]
 */
#define kh_size(h) ((h)->size)

/*! @function
  @abstract     Get the number of buckets in the hash table
  @param  h     Pointer to the hash table [khash_t(name)*]
  @return       Number of buckets in the hash table [khint_t]
 */
#define kh_n_buckets(h) ((h)->n_buckets)

/*! @function
  @abstract     Iterate over the entries in the hash table
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  kvar  Variable to which key will be assigned
  @param  vvar  Variable to which value will be assigned
  @param  code  Block of code to execute
 */
#define kh_foreach(h, kvar, vvar, code)                \
  {                                                    \
    khint_t __i;                                       \
    for (__i = kh_begin(h); __i != kh_end(h); ++__i) { \
      if (!kh_exist(h, __i))                           \
        continue;                                      \
      (kvar) = kh_key(h, __i);                         \
      (vvar) = kh_val(h, __i);                         \
      code;                                            \
    }                                                  \
  }

/*! @function
  @abstract     Iterate over the values in the hash table
  @param  h     Pointer to the hash table [khash_t(name)*]
  @param  vvar  Variable to which value will be assigned
  @param  code  Block of code to execute
 */
#define kh_foreach_value(h, vvar, code)                \
  {                                                    \
    khint_t __i;                                       \
    for (__i = kh_begin(h); __i != kh_end(h); ++__i) { \
      if (!kh_exist(h, __i))                           \
        continue;                                      \
      (vvar) = kh_val(h, __i);                         \
      code;                                            \
    }                                                  \
  }

/* More convenient interfaces */

/*! @function
  @abstract     Instantiate a hash set containing integer keys
  @param  name  Name of the hash table [symbol]
 */
#define KHASH_SET_INIT_INT(name) \
  KHASH_INIT(name, khint32_t, char, 0, kh_int_hash_func, kh_int_hash_equal)

/*! @function
  @abstract     Instantiate a hash map containing integer keys
  @param  name  Name of the hash table [symbol]
  @param  khval_t  Type of values [type]
 */
#define KHASH_MAP_INIT_INT(name, khval_t) \
  KHASH_INIT(name, khint32_t, khval_t, 1, kh_int_hash_func, kh_int_hash_equal)

/*! @function
  @abstract     Instantiate a hash set containing 64-bit integer keys
  @param  name  Name of the hash table [symbol]
 */
#define KHASH_SET_INIT_INT64(name) \
  KHASH_INIT(name, khint64_t, char, 0, kh_int64_hash_func, kh_int64_hash_equal)

/*! @function
  @abstract     Instantiate a hash map containing 64-bit integer keys
  @param  name  Name of the hash table [symbol]
  @param  khval_t  Type of values [type]
 */
#define KHASH_MAP_INIT_INT64(name, khval_t) \
  KHASH_INIT(name, khint64_t, khval_t, 1, kh_int64_hash_func, kh_int64_hash_equal)

typedef const char* kh_cstr_t;
/*! @function
  @abstract     Instantiate a hash map containing const char* keys
  @param  name  Name of the hash table [symbol]
 */
#define KHASH_SET_INIT_STR(name) \
  KHASH_INIT(name, kh_cstr_t, char, 0, kh_str_hash_func, kh_str_hash_equal)

/*! @function
  @abstract     Instantiate a hash map containing const char* keys
  @param  name  Name of the hash table [symbol]
  @param  khval_t  Type of values [type]
 */
#define KHASH_MAP_INIT_STR(name, khval_t) \
  KHASH_INIT(name, kh_cstr_t, khval_t, 1, kh_str_hash_func, kh_str_hash_equal)

#endif /* __AC_KHASH_H */
//
// END OF src/khash.h
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
// START OF src/dynasm/dasm_proto.h
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
// END OF src/dynasm/dasm_proto.h
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
// START OF src/dynasm/dasm_x86.h
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
// END OF src/dynasm/dasm_x86.h
//
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#undef C
#undef L
#undef VOID
//
// START OF src/type.c
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

static Type* array_of(Type* base, int len) {
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
      if (node->lhs->ty->kind != TY_STRUCT)
        node->rhs = new_cast(node->rhs, node->lhs->ty);
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
// END OF src/type.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/alloc.c
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
// END OF src/alloc.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/util.c
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

// Returns the contents of a given file. Doesn't support '-' for reading from
// stdin.
static char* read_file(char* path, AllocLifetime lifetime) {
  FILE* fp = fopen(path, "rb");
  if (!fp) {
    return NULL;
  }

  fseek(fp, 0, SEEK_END);
  long long size = ftell(fp);
  rewind(fp);
  char* buf = bumpcalloc(1, size + 1, lifetime);  // TODO: doesn't really need a calloc
  long long n = fread(buf, 1, size, fp);
  fclose(fp);
  buf[n] = 0;
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
//
// END OF src/util.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/hashmap.c
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

    if (match(ent, key, keylen))
      return ent;

    if (ent->key == TOMBSTONE) {
      ent->key = key;
      ent->keylen = keylen;
      return ent;
    }

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
  if (ent)
    ent->key = TOMBSTONE;
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
// END OF src/hashmap.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/link.c
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


KHASH_SET_INIT_INT64(voidp)

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
    X(_chmod);
    X(_chmod);
    X(_ctime64);
    X(_ctime64_s);
    X(_difftime64);
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
    X(_invalid_parameter_noinfo);
    X(_isatty);
    X(_isctype_l);
    X(_localtime64);
    X(_localtime64_s);
    X(_mkdir);
    X(_mkdir);
    X(_mkgmtime64);
    X(_mktime64);
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
    X(_wassert);
    X(_wcsicmp);
    X(_wctime64);
    X(_wctime64_s);
    X(_wunlink);
    X(AreFileApisANSI);
    X(atoi);
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
    X(exit);
    X(fclose);
    X(fflush);
    X(fgetc);
    X(fgets);
    X(FindClose);
    X(FindFirstFileW);
    X(FlushFileBuffers);
    X(FlushViewOfFile);
    X(fopen);
    X(FormatMessageA);
    X(FormatMessageW);
    X(fprintf);
    X(fputc);
    X(fputs);
    X(fread);
    X(free);
    X(FreeLibrary);
    X(fseek);
    X(ftell);
    X(fwrite);
    X(GetConsoleScreenBufferInfo);
    X(GetCurrentProcess);
    X(GetCurrentProcessId);
    X(GetDiskFreeSpaceA);
    X(GetDiskFreeSpaceW);
    X(getenv);
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
    X(isalnum);
    X(isalpha);
    X(isdigit);
    X(isprint);
    X(isspace);
    X(LoadLibraryA);
    X(LoadLibraryW);
    X(LocalFree);
    X(LockFile);
    X(LockFileEx);
    X(lstrcmpiW);
    X(lstrcmpW);
    X(lstrlenW);
    X(malloc);
    X(MapViewOfFile);
    X(MapViewOfFileNuma2);
    X(memcmp);
    X(memcpy);
    X(memmove);
    X(memset);
    X(MessageBoxA);
    X(MultiByteToWideChar);
    X(OutputDebugStringA);
    X(OutputDebugStringW);
    X(printf);
    X(putc);
    X(QueryPerformanceCounter);
    X(ReadFile);
    X(realloc);
    X(rewind);
    X(SetConsoleCtrlHandler);
    X(SetConsoleTextAttribute);
    X(SetCurrentDirectoryW);
    X(SetEndOfFile);
    X(SetFilePointer);
    X(SetFileTime);
    X(SetProcessDPIAware);
    X(setvbuf);
    X(Sleep);
    X(sprintf);
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
    X(strstr);
    X(strtol);
    X(system);
    X(SystemTimeToFileTime);
    X(SystemTimeToFileTime);
    X(tolower);
    X(uaw_lstrcmpiW);
    X(uaw_lstrcmpW);
    X(uaw_lstrlenW);
    X(uaw_wcschr);
    X(uaw_wcscpy);
    X(uaw_wcsicmp);
    X(uaw_wcslen);
    X(uaw_wcsrchr);
    X(UnlockFile);
    X(UnlockFileEx);
    X(UnmapViewOfFile);
    X(vfprintf);
    X(vsprintf);
    X(WaitForSingleObject);
    X(WaitForSingleObjectEx);
    X(wcschr);
    X(wcscpy);
    X(wcscpy_s);
    X(wcslen);
    X(wcsnlen);
    X(wcsrchr);
    X(wcstok);
    X(WideCharToMultiByte);
    X(WriteFile);
    X(EnterCriticalSection);
    X(GetCurrentThreadId);
    X(InitializeCriticalSection);
    X(LeaveCriticalSection);
    X(TryEnterCriticalSection);
    X(_beginthreadex);
    X(_byteswap_ulong);
    X(_byteswap_ushort);
    X(_chgsign);
    X(_copysign);
    X(_endthreadex);
    X(_hypot);
    X(_hypotf);
    X(_msize);
    X(acos);
    X(asin);
    X(atan);
    X(atan2);
    X(ceil);
    X(cos);
    X(cosh);
    X(exp);
    X(fabs);
    X(floor);
    X(fmod);
    X(frexp);
    X(ldexp);
    X(log);
    X(log10);
    X(modf);
    X(pow);
    X(sin);
    X(sinh);
    X(sqrt);
    X(strrchr);
    X(tan);
    X(tanh);

    Y("__stosb", Xstosb);
    Y("_ReadWriteBarrier", XReadWriteBarrier);
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
  // This is a hack to avoid disabling -Wunused-function, since these are in
  // khash.h and aren't instantiated.
  (void)__ac_X31_hash_string;
  (void)__ac_Wang_hash;

  UserContext* uc = user_context;

  if (uc->num_files == 0)
    return false;

  // Process fixups.
  for (size_t i = 0; i < uc->num_files; ++i) {
    FileLinkData* fld = &uc->files[i];
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
  }

  for (size_t i = 0; i < uc->num_files; ++i) {
    FileLinkData* fld = &uc->files[i];
    if (!make_memory_executable(fld->codeseg_base_address, fld->codeseg_size)) {
      outaf("failed to make %p size %zu executable\n", fld->codeseg_base_address,
            fld->codeseg_size);
      return false;
    }
  }

  return true;
}
//
// END OF src/link.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/main.c
//
// Notes and todos
// ---------------
//
// Windows x64 .pdata generation:
//
//   Need to RtlAddFunctionTable() so that even minimal stackwalking in
//   Disassembly view is correct in VS. Required for SEH too. cl /Fa emits
//   without using any helper macros for samples.
//
// Break up into small symbol-sized 'sections':
//
//   In order to update code without losing global state, need to be able to
//   replace and relink. Right now, link_dyos() does all the allocation of
//   global data at the same time as mapping the code in to executable pages.
//
//   The simplest fix would be to keep the mappings of globals around and not
//   reallocate them on updates (one map for global symbols, plus one per
//   translation unit). The code updating could still be tossing all code, and
//   relinking everything, but using the old hashmaps for data addresses.
//
//   Alternatively, it might be a better direction to break everything up into
//   symbol-sized chunks (i.e. either a variable or a function indexed by symbol
//   name). Initial and update become more similar, in that if any symbol is
//   updated, the old one (if any) gets thrown away, the new one gets mapped in
//   (whether code or data), and then everything that refers to it is patched.
//
//   The main gotchas that come to mind on the second approach are:
//
//     - The parser (and DynASM to assign labels) need to be initialized before
//     processing the whole file; C is just never going to be able to compile a
//     single function in isolation. So emit_data() and emit_text() need to make
//     sure that each symbol blob can be ripped out of the generated block, and
//     any offsets have to be saved relative to the start of that symbol for
//     emitting fixups. Probably codegen_pclabel() a start/end for each for
//     rippage/re-offseting.
//
//     - Need figure out how to name things. If everything becomes a flat bag of
//     symbols, we need to make sure that statics from a.c are local to a.c, so
//     they'll need to be file prefixed.
//
//     - Probably wll need to switch to a new format (some kv store or
//     something), as symbol-per-dyo would be a spamming of files to deal with.
//
// Testing for relinking:
//
//   Basic relinking is implemented, but there's no test driver that sequences a
//   bunch of code changes to make sure that the updates can be applied
//   successfully.
//
// khash <-> swisstable:
//
//   Look into hashtable libs, khash is used in link.c now and it seems ok, but
//   the interface isn't that pleasant. Possibly wrap and extern C a few common
//   instantiations of absl's with a more pleasant interface (and that could
//   replace hashmap.c too). Need to consider how they would/can integrate with
//   bumpalloc.
//
// Debugger:
//
//   Picking either ELF/DWARF or PE/COFF (and dropping .dyo) would probably be
//   the more practical way to get a better debugging experience, but then,
//   clang-win would also be a lot better. Tomorrow Corp demo for inspiration of
//   what needs to be implemented/included. Possibly still go with debug adapter
//   json thing (with extension messages?) so that an existing well-written UI
//   can be used.
//
// Improved codegen:
//
//   Bit of a black hole of effort and probably doesn't matter for a dev-focused
//   tool. But it would be easier to trace through asm if the data flow was less
//   hidden. Possibly basic use of otherwise-unused gp registers, possibly some
//   peephole, or higher level amalgamated instructions for codegen to use that
//   avoid the common cases of load/push, push/something/pop.
//
// Various "C+" language extensions:
//
//   Some possibilities:
//     - an import instead of #include that can be used when not touching system
//     stuff
//     - string type with syntax integration
//     - basic polymophic containers (dict, list, slice, sizedarray)
//     - range-based for loop (to go with containers)
//     - range notation
//
// rep stosb for local clear:
//
//   Especially on Windows where rdi is non-volatile, it seems like quite a lot
//   of instructions. At the very least we could only do one memset for all
//   locals to clear out a range.
//
// Don't emit __func__, __FUNCTION__ unless used:
//
//   Doesn't affect anything other than dyo size, but it bothers me seeing them
//   in there.
//
// Improve dumpdyo:
//
//   - Cross-reference the name to which fixups will be bound in disasm
//   - include dump as string for initializer bytes
//
// Implement TLS:
//
//   If needed.
//
// Implement inline ASM:
//
//   If needed.
//
// .dyo cache:
//
//   Based on compiler binary, "environment", and the contents of the .c file,
//   make a hash-based cache of dyos so that recompile can only build the
//   required files and relink while passing the whole module/program still.
//   Since there's no -D or other flags, "enviroment" could either be a hash of
//   all the files in the include search path, or alternatively hash after
//   preprocessing, or probably track all files include and include all of the
//   includes in the hash. Not overly important if total compile/link times
//   remain fast.
//
// In-memory dyo:
//
//   Alternatively to caching, maybe just save to a memory structure. Might be a
//   little faster for direct use, could still have a dump-dyo-from-mem for
//   debugging purposes. Goes more with an always-live compiler host hooked to
//   target.
//
// Consider merging some of the record types in dyo:
//
//     kTypeImport is (offset-to-fix, string-to-reference)
//     kTypeCodeReferenceToGlobal is (offset-to-fix, string-to-reference)
//     kTypeInitializerDataRelocation is (string-to-reference, addend)
//
//   The only difference between the first two is that one does GetProcAddress()
//   or similar, and the other looks in the export tables for other dyos. But we
//   might want data imported from host too.
//
//   The third is different in that the address to fix up is implicit because
//   it's in a sequence of data segment initializers, but just having all
//   imports be:
//
//      (offset-to-fix, string-to-reference, addend)
//
//   might be nicer.
//
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
      logout("\n");
    if (tok->has_space && !tok->at_bol)
      logout(" ");
    logout("%.*s", tok->len, tok->loc);
    line++;
  }
  logout("\n");
}
#endif

static int default_output_fn(const char* fmt, va_list ap) {
  int ret = vfprintf(stdout, fmt, ap);
  return ret;
}

DyibiccContext* dyibicc_set_environment(DyibiccEnviromentData* env_data) {
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
  strarray_push(&sys_inc_paths, format(AL_Temp, "%s/win", env_data->dyibicc_include_dir), AL_Temp);
  strarray_push(&sys_inc_paths, format(AL_Temp, "%s/all", env_data->dyibicc_include_dir), AL_Temp);

  strarray_push(&sys_inc_paths,
                "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.22621.0\\ucrt", AL_Temp);
  strarray_push(&sys_inc_paths,
                "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.22621.0\\um", AL_Temp);
  strarray_push(&sys_inc_paths,
                "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.22621.0\\shared",
                AL_Temp);
  strarray_push(&sys_inc_paths,
                "C:\\Program Files\\Microsoft Visual "
                "Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.34.31933\\include",
                AL_Temp);
#else
  strarray_push(&sys_inc_paths, format(AL_Temp, "%s/linux", env_data->dyibicc_include_dir),
                AL_Temp);
  strarray_push(&sys_inc_paths, format(AL_Temp, "%s/all", env_data->dyibicc_include_dir), AL_Temp);

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

  data->get_function_address = env_data->get_function_address;
  data->output_function = env_data->output_function;
  if (!data->output_function) {
    data->output_function = default_output_fn;
  }
  data->use_ansi_codes = env_data->use_ansi_codes;

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
// END OF src/main.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/parse.c
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
static Node* funcall(Token** rest, Token* tok, Node* node);
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
  node->rty = (uintptr_t)rty;
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

static Initializer* new_initializer(Type* ty, bool is_flexible) {
  Initializer* init = bumpcalloc(1, sizeof(Initializer), AL_Compile);
  init->ty = ty;

  if (ty->kind == TY_ARRAY) {
    if (is_flexible && ty->size < 0) {
      init->is_flexible = true;
      return init;
    }

    init->children = bumpcalloc(ty->array_len, sizeof(Initializer*), AL_Compile);
    for (int i = 0; i < ty->array_len; i++)
      init->children[i] = new_initializer(ty->base, false);
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
        init->children[mem->idx] = new_initializer(mem->ty, false);
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

    if (ty2->kind == TY_ARRAY) {
      // "array of T" is converted to "pointer to T" only in the parameter
      // context. For example, *argv[] is converted to **argv by this.
      ty2 = pointer_to(ty2->base);
      ty2->name = name;
    } else if (ty2->kind == TY_FUNC) {
      // Likewise, a function is converted to a pointer to a function
      // only in the parameter context.
      ty2 = pointer_to(ty2);
      ty2->name = name;
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
    return array_of(ty, -1);
  }

  Node* expr = conditional(&tok, tok);
  tok = skip(tok, "]");
  ty = type_suffix(rest, tok, ty);

  if (ty->kind == TY_VLA || !is_const_expr(expr))
    return vla_of(ty, expr);
  return array_of(ty, (int)eval(expr));
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
    *init = *new_initializer(array_of(init->ty->base, tok->ty->array_len), false);

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
  if (*begin >= ty->array_len)
    error_tok(tok, "array designator index exceeds array bounds");

  if (equal(tok, "...")) {
    *end = (int)const_expr(&tok, tok->next);
    if (*end >= ty->array_len)
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
    for (int i = begin; i <= end; i++)
      designation(&tok2, tok, init->children[i]);
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
  Initializer* dummy = new_initializer(ty->base, true);

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
    *init = *new_initializer(array_of(init->ty->base, len), false);
  }

  bool first = true;

  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(array_of(init->ty->base, len), false);
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
    *init = *new_initializer(array_of(init->ty->base, len), false);
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
  Initializer* init = new_initializer(ty, true);
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
    case ND_DIV:
      if (node->ty->is_unsigned)
        return (uint64_t)eval(node->lhs) / eval(node->rhs);
      return eval(node->lhs) / eval(node->rhs);
    case ND_NEG:
      return -eval(node->lhs);
    case ND_MOD:
      if (node->ty->is_unsigned)
        return (uint64_t)eval(node->lhs) % eval(node->rhs);
      return eval(node->lhs) % eval(node->rhs);
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
      if (node->var->is_local)
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
  if (lhs->ty->base->kind == TY_VLA) {
    rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  // ptr + num
  rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
  return new_binary(ND_ADD, lhs, rhs, tok);
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
  if (lhs->ty->base->kind == TY_VLA) {
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
  if (equal(tok, "++"))
    return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));

  // Read --i as i-=1
  if (equal(tok, "--"))
    return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));

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
    cur->ty = array_of(cur->ty->base, 0);
    ty->is_flexible = true;
  }

  *rest = tok->next;
  ty->members = head.next;
}

// attribute = ("__attribute__" "(" "(" "packed" ")" ")")*
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
    error_tok(node->tok, "not a struct nor a union");

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
      node = funcall(&tok, tok->next, node);
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

    if (equal(tok, "->")) {
      // x->y is short for (*x).y
      node = new_unary(ND_DEREF, node, tok);
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "++")) {
      node = new_inc_dec(node, tok, 1);
      tok = tok->next;
      continue;
    }

    if (equal(tok, "--")) {
      node = new_inc_dec(node, tok, -1);
      tok = tok->next;
      continue;
    }

    *rest = tok;
    return node;
  }
}

// funcall = (assign ("," assign)*)? ")"
static Node* funcall(Token** rest, Token* tok, Node* fn) {
  add_type(fn);

  if (fn->ty->kind != TY_FUNC && (fn->ty->kind != TY_PTR || fn->ty->base->kind != TY_FUNC))
    error_tok(tok, "not a function");

  Type* ty = (fn->ty->kind == TY_FUNC) ? fn->ty : fn->ty->base;
  Type* param_ty = ty->params;

  Node head = {0};
  Node* cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");

    Node* arg = assign(&tok, tok);
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
    fn->va_area = new_lvar("__va_area__", array_of(ty_char, 136));
#endif
  fn->alloca_bottom = new_lvar("__alloca_size__", pointer_to(ty_char));

  tok = skip(tok, "{");

  // [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
  // automatically defined as a local variable containing the
  // current function name.
  push_scope("__func__")->var =
      new_string_literal(fn->name, array_of(ty_char, (int)strlen(fn->name) + 1));

  // [GNU] __FUNCTION__ is yet another name of __func__.
  push_scope("__FUNCTION__")->var =
      new_string_literal(fn->name, array_of(ty_char, (int)strlen(fn->name) + 1));

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
// END OF src/parse.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/preprocess.c
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

typedef Token* macro_handler_fn(Token*);

typedef struct Macro Macro;
struct Macro {
  char* name;
  bool is_objlike;  // Object-like or function-like
  MacroParam* params;
  char* va_args_name;
  Token* body;
  macro_handler_fn* handler;
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

  for (; !tok->at_bol; tok = tok->next)
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
  long val = (long)const_expr(&rest2, expr);
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

static void read_macro_definition(Token** rest, Token* tok) {
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
  } else {
    // Object-like macro
    add_macro(name, true, copy_line(rest, tok));
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

static bool file_exists(char* path) {
  struct stat st;
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
    *rest = m->handler(tok);
    (*rest)->next = tok->next;
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

  Token* tok2 = tokenize_file(path);
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
      tok = skip_line(tok->next->next);
      if (!defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "ifndef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, !defined);
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

static void define_function_macro(char* buf) {
  Token* tok = tokenize(new_file("<built-in>", buf));
  Token* rest = tok;
  read_macro_definition(&rest, tok);
}

static Macro* add_builtin(char* name, macro_handler_fn* fn) {
  Macro* m = add_macro(name, true, NULL);
  m->handler = fn;
  return m;
}

static Token* file_macro(Token* tmpl) {
  while (tmpl->origin)
    tmpl = tmpl->origin;
  return new_str_token(tmpl->file->display_name, tmpl);
}

static Token* line_macro(Token* tmpl) {
  while (tmpl->origin)
    tmpl = tmpl->origin;
  int i = tmpl->line_no + tmpl->file->line_delta;
  return new_num_token(i, tmpl);
}

// __COUNTER__ is expanded to serial values starting from 0.
static Token* counter_macro(Token* tmpl) {
  return new_num_token(C(counter_macro_i)++, tmpl);
}

// __TIMESTAMP__ is expanded to a string describing the last
// modification time of the current file. E.g.
// "Fri Jul 24 01:32:50 2020"
static Token* timestamp_macro(Token* tmpl) {
  return new_str_token("Mon May 02 01:23:45 1977", tmpl);
}

static Token* base_file_macro(Token* tmpl) {
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
  define_function_macro("__pragma(_)\n");
  define_function_macro("__declspec(_)\n");
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
  (void)define_function_macro;
#endif

  add_builtin("__FILE__", file_macro);
  add_builtin("__LINE__", line_macro);
  add_builtin("__COUNTER__", counter_macro);
  add_builtin("__TIMESTAMP__", timestamp_macro);
  add_builtin("__BASE_FILE__", base_file_macro);

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
    tok1->ty = array_of(tok1->ty->base, len);
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
//
// END OF src/preprocess.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/tokenize.c
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
      "<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=",
      "++",  "--",  "%=",  "&=", "|=", "^=", "&&", "||", "<<", ">>", "##",
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
  tok->ty = array_of(ty_char, len + 1);
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
  tok->ty = array_of(ty_ushort, len + 1);
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
  tok->ty = array_of(ty, len + 1);
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
      while (*p != '\n')
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
  return tokenize(file);
}

Token* tokenize_file(char* path) {
  char* p = read_file(path, AL_Compile);
  if (!p)
    return NULL;
  return tokenize_filecontents(path, p);
}
//
// END OF src/tokenize.c
//
#undef C
#undef L
#undef VOID
//
// START OF src/unicode.c
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
#if 0

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

#else
  // From http://bjoern.hoehrmann.de/utf-8/decoder/dfa/#variations

  // This is maybe only a tiny amout faster (under /Ox /GL), likely because most
  // code is ASCII so we hit the < 128 early out in the plain code.

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

  // clang-format off
  static const uint8_t utf8d[] = {
    // The first part of the table maps bytes to character classes that
    // to reduce the size of the transition table and create bitmasks.
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

    // The second part is a transition table that maps a combination
    // of a state of the automaton and a character class to a state.
    0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
    12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
    12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
    12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
    12,36,12,12,12,12,12,12,12,12,12,12,
  };
  // clang-format on

  uint32_t state = 0;
  uint32_t codep = 0;
  char* start = p;

  while (*p) {
    uint8_t byte = *p++;
    uint32_t type = utf8d[byte];
    codep = (state != UTF8_ACCEPT) ? (byte & 0x3fu) | (codep << 6) : (0xff >> type) & (byte);
    state = utf8d[256 + state + type];
    if (!state)
      break;
  }
  if (!*p && state != UTF8_ACCEPT) {
    error_at(start, "invalid UTF-8 sequence");
  }
  *new_pos = p;
  return codep;
#endif
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
// END OF src/unicode.c
//
#if X64WIN
#undef C
#undef L
#undef VOID
//
// START OF out/wr/codegen.w.c
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
#line 19 "../../src/codegen.in.c"
//| .section main
#define DASM_SECTION_MAIN	0
#define DASM_MAXSECTION		1
#line 20 "../../src/codegen.in.c"
//| .actionlist dynasm_actions
static const unsigned char dynasm_actions[1671] = {
  80,255,64,88,240,42,255,72,131,252,236,8,252,242,15,17,4,36,255,252,242,64,
  15,16,4,240,140,36,72,131,196,8,255,252,243,15,16,0,255,252,242,15,16,0,255,
  219,40,255,15,182,0,255,15,190,0,255,15,183,0,255,15,191,0,255,72,99,0,255,
  72,139,0,255,68,138,128,233,68,136,129,233,255,252,243,15,17,1,255,252,242,
  15,17,1,255,219,57,255,136,1,255,102,137,1,255,72,137,1,255,72,139,133,233,
  255,72,141,133,233,255,72,141,5,245,255,249,72,184,237,237,255,72,129,192,
  239,255,15,87,201,15,46,193,255,102,15,87,201,102,15,46,193,255,217,252,238,
  223,232,221,216,255,131,252,248,0,255,72,131,252,248,0,255,15,190,192,255,
  15,182,192,255,15,191,192,255,15,183,192,255,252,243,15,42,192,255,72,99,
  192,255,252,242,15,42,192,255,137,68,36,252,252,219,68,36,252,252,255,137,
  192,252,243,72,15,42,192,255,137,192,255,137,192,252,242,72,15,42,192,255,
  137,192,72,137,68,36,252,248,223,108,36,252,248,255,72,133,192,15,136,244,
  247,102,15,252,239,192,252,242,72,15,42,192,252,233,244,248,248,1,72,137,
  193,131,224,1,102,15,252,239,192,72,209,252,233,72,9,193,252,242,72,15,42,
  193,252,242,15,88,192,248,2,255,72,137,68,36,252,248,223,108,36,252,248,72,
  133,192,15,137,244,247,184,0,0,128,95,137,68,36,252,252,216,68,36,252,252,
  248,1,255,252,243,15,44,192,15,190,192,255,252,243,15,44,192,15,182,192,255,
  252,243,15,44,192,15,191,192,255,252,243,15,44,192,15,183,192,255,252,243,
  15,44,192,255,252,243,72,15,44,192,255,252,243,15,90,192,255,252,243,15,17,
  68,36,252,252,217,68,36,252,252,255,252,242,15,44,192,15,190,192,255,252,
  242,15,44,192,15,182,192,255,252,242,15,44,192,15,191,192,255,252,242,15,
  44,192,15,183,192,255,252,242,15,44,192,255,252,242,72,15,44,192,255,252,
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
  255,73,137,192,255,72,137,193,72,129,225,239,72,193,225,235,255,72,139,4,
  36,255,73,199,193,237,76,33,200,72,9,200,255,76,137,192,255,87,255,72,199,
  193,237,72,141,189,233,176,0,252,243,170,255,95,255,15,132,245,255,252,233,
  245,249,255,15,148,208,72,15,182,192,255,72,252,247,208,255,15,132,245,72,
  199,192,1,0,0,0,252,233,245,249,72,199,192,0,0,0,0,249,255,15,133,245,255,
  15,133,245,72,199,192,0,0,0,0,252,233,245,249,72,199,192,1,0,0,0,249,255,
  72,131,192,8,255,102,72,15,126,192,240,132,240,36,255,72,129,252,236,239,
  73,137,194,65,252,255,210,72,129,196,239,255,65,91,255,72,137,133,233,72,
  141,133,233,255,73,137,194,72,199,192,237,65,252,255,210,72,129,196,239,255,
  252,240,15,176,17,255,102,252,240,15,177,17,255,252,240,72,15,177,17,255,
  15,148,209,15,132,244,247,255,65,136,0,255,102,65,137,0,255,73,137,0,255,
  248,1,15,182,193,255,134,1,255,102,135,1,255,72,135,1,255,252,243,15,88,193,
  255,252,242,15,88,193,255,252,243,15,92,193,255,252,242,15,92,193,255,252,
  243,15,89,193,255,252,242,15,89,193,255,252,243,15,94,193,255,252,242,15,
  94,193,255,15,46,200,255,102,15,46,200,255,15,148,208,15,155,210,32,208,255,
  15,149,208,15,154,210,8,208,255,15,151,208,255,15,147,208,255,36,1,72,15,
  182,192,255,222,193,255,222,225,255,222,201,255,222,252,241,255,223,252,241,
  221,216,255,15,148,208,255,15,149,208,255,72,1,200,255,72,41,200,255,72,15,
  175,193,255,72,199,194,0,0,0,0,72,252,247,252,241,255,186,0,0,0,0,252,247,
  252,241,255,72,153,255,72,252,247,252,249,255,72,137,208,255,72,33,200,255,
  72,49,200,255,72,57,200,255,15,146,208,255,15,156,208,255,15,150,208,255,
  15,158,208,255,72,137,201,255,72,211,224,255,72,211,232,255,72,211,252,248,
  255,15,133,245,249,255,72,129,252,248,239,255,72,137,193,72,129,252,233,239,
  72,129,252,249,239,255,137,193,129,252,233,239,129,252,249,239,255,15,134,
  245,255,252,233,245,255,252,255,224,255,64,136,133,253,240,131,233,255,102,
  64,137,133,253,240,139,233,255,72,137,133,253,240,131,233,255,85,72,137,229,
  255,249,73,186,237,237,255,65,252,255,210,72,41,196,255,72,137,165,233,255,
  199,133,233,237,199,133,233,237,72,137,173,233,72,131,133,233,16,72,137,173,
  233,72,129,133,233,239,255,72,137,189,233,72,137,181,233,72,137,149,233,72,
  137,141,233,76,137,133,233,76,137,141,233,252,242,15,17,133,233,252,242,15,
  17,141,233,252,242,15,17,149,233,252,242,15,17,157,233,252,242,15,17,165,
  233,252,242,15,17,173,233,252,242,15,17,181,233,252,242,15,17,189,233,255,
  72,137,141,233,72,137,149,233,76,137,133,233,76,137,141,233,255,249,72,137,
  252,236,93,195,255
};

#line 21 "../../src/codegen.in.c"
//| .globals dynasm_globals
enum {
  dynasm_globals_MAX
};
#line 22 "../../src/codegen.in.c"
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

static int codegen_pclabel(void) {
  int ret = C(numlabels);
  dasm_growpc(&C(dynasm), ++C(numlabels));
  return ret;
}

static void push(void) {
  //| push rax
  dasm_put(Dst, 0);
#line 80 "../../src/codegen.in.c"
  C(depth)++;
}

static void pop(int dasmreg) {
  //| pop Rq(dasmreg)
  dasm_put(Dst, 2, (dasmreg));
#line 85 "../../src/codegen.in.c"
  C(depth)--;
}

static void pushf(void) {
  //| sub rsp, 8
  //| movsd qword [rsp], xmm0
  dasm_put(Dst, 7);
#line 91 "../../src/codegen.in.c"
  C(depth)++;
}

static void popf(int reg) {
  //| movsd xmm(reg), qword [rsp]
  //| add rsp, 8
  dasm_put(Dst, 19, (reg));
#line 97 "../../src/codegen.in.c"
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
      dasm_put(Dst, 33);
#line 117 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd xmm0, qword [rax]
      dasm_put(Dst, 39);
#line 120 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fld tword [rax]
      dasm_put(Dst, 45);
#line 124 "../../src/codegen.in.c"
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
      dasm_put(Dst, 48);
#line 136 "../../src/codegen.in.c"
    } else {
      //| movsx eax, byte [rax]
      dasm_put(Dst, 52);
#line 138 "../../src/codegen.in.c"
    }
  } else if (ty->size == 2) {
    if (ty->is_unsigned) {
      //| movzx eax, word [rax]
      dasm_put(Dst, 56);
#line 142 "../../src/codegen.in.c"
    } else {
      //| movsx eax, word [rax]
      dasm_put(Dst, 60);
#line 144 "../../src/codegen.in.c"
    }
  } else if (ty->size == 4) {
    //| movsxd rax, dword [rax]
    dasm_put(Dst, 64);
#line 147 "../../src/codegen.in.c"
  } else {
    //| mov rax, qword [rax]
    dasm_put(Dst, 68);
#line 149 "../../src/codegen.in.c"
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
        dasm_put(Dst, 72, i, i);
#line 162 "../../src/codegen.in.c"
      }
      return;
    case TY_FLOAT:
      //| movss dword [RUTIL], xmm0
      dasm_put(Dst, 81);
#line 166 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd qword [RUTIL], xmm0
      dasm_put(Dst, 87);
#line 169 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fstp tword [RUTIL]
      dasm_put(Dst, 93);
#line 173 "../../src/codegen.in.c"
      return;
#endif
  }

  if (ty->size == 1) {
    //| mov [RUTIL], al
    dasm_put(Dst, 96);
#line 179 "../../src/codegen.in.c"
  } else if (ty->size == 2) {
    //| mov [RUTIL], ax
    dasm_put(Dst, 99);
#line 181 "../../src/codegen.in.c"
  } else if (ty->size == 4) {
    //| mov [RUTIL], eax
    dasm_put(Dst, 100);
#line 183 "../../src/codegen.in.c"
  } else {
    //| mov [RUTIL], rax
    dasm_put(Dst, 103);
#line 185 "../../src/codegen.in.c"
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
        dasm_put(Dst, 107, node->var->offset);
#line 196 "../../src/codegen.in.c"
        return;
      }

      // Local variable
      if (node->var->is_local) {
        //| lea rax, [rbp+node->var->offset]
        dasm_put(Dst, 112, node->var->offset);
#line 202 "../../src/codegen.in.c"
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
          dasm_put(Dst, 117, node->var->dasm_entry_label);
#line 217 "../../src/codegen.in.c"
        } else {
          int fixup_location = codegen_pclabel();
          strintarray_push(&C(fixups), (StringInt){node->var->name, fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
          //|=>fixup_location:
          //| mov64 rax, 0xc0dec0dec0dec0de
          dasm_put(Dst, 122, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 226 "../../src/codegen.in.c"
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
      dasm_put(Dst, 122, fixup_location, (unsigned int)(0xda7ada7ada7ada7a), (unsigned int)((0xda7ada7ada7ada7a)>>32));
#line 242 "../../src/codegen.in.c"
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
        dasm_put(Dst, 68);
#line 258 "../../src/codegen.in.c"
      }
#endif
      //| add rax, node->member->offset
      dasm_put(Dst, 128, node->member->offset);
#line 261 "../../src/codegen.in.c"
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
      dasm_put(Dst, 112, node->var->offset);
#line 277 "../../src/codegen.in.c"
      return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void cmp_zero(Type* ty) {
  switch (ty->kind) {
    case TY_FLOAT:
      //| xorps xmm1, xmm1
      //| ucomiss xmm0, xmm1
      dasm_put(Dst, 133);
#line 288 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| xorpd xmm1, xmm1
      //| ucomisd xmm0, xmm1
      dasm_put(Dst, 140);
#line 292 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fldz
      //| fucomip st0
      //| fstp st0
      dasm_put(Dst, 149);
#line 298 "../../src/codegen.in.c"
      return;
#endif
  }

  if (is_integer(ty) && ty->size <= 4) {
    //| cmp eax, 0
    dasm_put(Dst, 157);
#line 304 "../../src/codegen.in.c"
  } else {
    //| cmp rax, 0
    dasm_put(Dst, 162);
#line 306 "../../src/codegen.in.c"
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
  dasm_put(Dst, 168);
#line 335 "../../src/codegen.in.c"
}
static void i32u8(void) {
  //| movzx eax, al
  dasm_put(Dst, 172);
#line 338 "../../src/codegen.in.c"
}
static void i32i16(void) {
  //| movsx eax, ax
  dasm_put(Dst, 176);
#line 341 "../../src/codegen.in.c"
}
static void i32u16(void) {
  //| movzx eax, ax
  dasm_put(Dst, 180);
#line 344 "../../src/codegen.in.c"
}
static void i32f32(void) {
  //| cvtsi2ss xmm0, eax
  dasm_put(Dst, 184);
#line 347 "../../src/codegen.in.c"
}
static void i32i64(void) {
  //| movsxd rax, eax
  dasm_put(Dst, 190);
#line 350 "../../src/codegen.in.c"
}
static void i32f64(void) {
  //| cvtsi2sd xmm0, eax
  dasm_put(Dst, 194);
#line 353 "../../src/codegen.in.c"
}
static void i32f80(void) {
  //| mov [rsp-4], eax
  //| fild dword [rsp-4]
  dasm_put(Dst, 200);
#line 357 "../../src/codegen.in.c"
}

static void u32f32(void) {
  //| mov eax, eax
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 211);
#line 362 "../../src/codegen.in.c"
}
static void u32i64(void) {
  //| mov eax, eax
  dasm_put(Dst, 220);
#line 365 "../../src/codegen.in.c"
}
static void u32f64(void) {
  //| mov eax, eax
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 223);
#line 369 "../../src/codegen.in.c"
}
static void u32f80(void) {
  //| mov eax, eax
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 232);
#line 374 "../../src/codegen.in.c"
}

static void i64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 213);
#line 378 "../../src/codegen.in.c"
}
static void i64f64(void) {
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 225);
#line 381 "../../src/codegen.in.c"
}
static void i64f80(void) {
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 234);
#line 385 "../../src/codegen.in.c"
}

static void u64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 213);
#line 389 "../../src/codegen.in.c"
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
  dasm_put(Dst, 246);
#line 405 "../../src/codegen.in.c"
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
  dasm_put(Dst, 302);
#line 415 "../../src/codegen.in.c"
}

static void f32i8(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 338);
#line 420 "../../src/codegen.in.c"
}
static void f32u8(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 347);
#line 424 "../../src/codegen.in.c"
}
static void f32i16(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 356);
#line 428 "../../src/codegen.in.c"
}
static void f32u16(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 365);
#line 432 "../../src/codegen.in.c"
}
static void f32i32(void) {
  //| cvttss2si eax, xmm0
  dasm_put(Dst, 374);
#line 435 "../../src/codegen.in.c"
}
static void f32u32(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 380);
#line 438 "../../src/codegen.in.c"
}
static void f32i64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 380);
#line 441 "../../src/codegen.in.c"
}
static void f32u64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 380);
#line 444 "../../src/codegen.in.c"
}
static void f32f64(void) {
  //| cvtss2sd xmm0, xmm0
  dasm_put(Dst, 387);
#line 447 "../../src/codegen.in.c"
}
static void f32f80(void) {
  //| movss dword [rsp-4], xmm0
  //| fld dword [rsp-4]
  dasm_put(Dst, 393);
#line 451 "../../src/codegen.in.c"
}

static void f64i8(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 407);
#line 456 "../../src/codegen.in.c"
}
static void f64u8(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 416);
#line 460 "../../src/codegen.in.c"
}
static void f64i16(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 425);
#line 464 "../../src/codegen.in.c"
}
static void f64u16(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 434);
#line 468 "../../src/codegen.in.c"
}
static void f64i32(void) {
  //| cvttsd2si eax, xmm0
  dasm_put(Dst, 443);
#line 471 "../../src/codegen.in.c"
}
static void f64u32(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 449);
#line 474 "../../src/codegen.in.c"
}
static void f64i64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 449);
#line 477 "../../src/codegen.in.c"
}
static void f64u64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 449);
#line 480 "../../src/codegen.in.c"
}
static void f64f32(void) {
  //| cvtsd2ss xmm0, xmm0
  dasm_put(Dst, 456);
#line 483 "../../src/codegen.in.c"
}
static void f64f80(void) {
  //| movsd qword [rsp-8], xmm0
  //| fld qword [rsp-8]
  dasm_put(Dst, 462);
#line 487 "../../src/codegen.in.c"
}

static void from_f80_1(void) {
  //| fnstcw word [rsp-10]
  //| movzx eax, word [rsp-10]
  //| or ah, 12
  //| mov [rsp-12], ax
  //| fldcw word [rsp-12]
  dasm_put(Dst, 476);
#line 495 "../../src/codegen.in.c"
}

#define FROM_F80_2 " [rsp-24]\n fldcw [rsp-10]\n "

static void f80i8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 502);
#line 504 "../../src/codegen.in.c"
}
static void f80u8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  //| and eax, 0xff
  dasm_put(Dst, 517);
#line 511 "../../src/codegen.in.c"
}
static void f80i16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 502);
#line 517 "../../src/codegen.in.c"
}
static void f80u16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  dasm_put(Dst, 538);
#line 523 "../../src/codegen.in.c"
}
static void f80i32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 553);
#line 529 "../../src/codegen.in.c"
}
static void f80u32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 553);
#line 535 "../../src/codegen.in.c"
}
static void f80i64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 567);
#line 541 "../../src/codegen.in.c"
}
static void f80u64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 567);
#line 547 "../../src/codegen.in.c"
}
static void f80f32(void) {
  //| fstp dword [rsp-8]
  //| movss xmm0, dword [rsp-8]
  dasm_put(Dst, 582);
#line 551 "../../src/codegen.in.c"
}
static void f80f64(void) {
  //| fstp qword [rsp-8]
  //| movsd xmm0, qword [rsp-8]
  dasm_put(Dst, 596);
#line 555 "../../src/codegen.in.c"
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
    dasm_put(Dst, 610);
#line 591 "../../src/codegen.in.c"
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
  dasm_put(Dst, 617, sz);
#line 646 "../../src/codegen.in.c"
  C(depth) += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    //| mov r10b, [rax+i]
    //| mov [rsp+i], r10b
    dasm_put(Dst, 623, i, i);
#line 651 "../../src/codegen.in.c"
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
        dasm_put(Dst, 634, -args->pass_by_reference);
#line 694 "../../src/codegen.in.c"
      } else {
        //| mov rax, [rax]
        dasm_put(Dst, 68);
#line 696 "../../src/codegen.in.c"
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
    dasm_put(Dst, 639);
#line 771 "../../src/codegen.in.c"
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

  if ((C(depth) + stack) % 2 == 1) {
    //| sub rsp, 8
    dasm_put(Dst, 645);
#line 820 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_win(node->args, true);
  push_args2_win(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && !type_passed_in_register(node->ty)) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 112, node->ret_buffer->offset);
#line 831 "../../src/codegen.in.c"
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
      dasm_put(Dst, 651);
#line 863 "../../src/codegen.in.c"
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
    dasm_put(Dst, 645);
#line 946 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_sysv(node->args, true);
  push_args2_sysv(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 112, node->ret_buffer->offset);
#line 957 "../../src/codegen.in.c"
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
      dasm_put(Dst, 660, var->offset);
#line 971 "../../src/codegen.in.c"
    } else {
      //| movsd qword [rbp+var->offset], xmm0
      dasm_put(Dst, 667, var->offset);
#line 973 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      //| mov [rbp+var->offset+i], al
      //| shr rax, 8
      dasm_put(Dst, 674, var->offset+i);
#line 979 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12) {
        //| movss dword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 682, (fp), var->offset+8);
#line 988 "../../src/codegen.in.c"
      } else {
        //| movsd qword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 693, (fp), var->offset+8);
#line 990 "../../src/codegen.in.c"
      }
    } else {
      for (int i = 8; i < MIN(16, ty->size); i++) {
        //| mov [rbp+var->offset+i], Rb(gp)
        //| shr Rq(gp), 8
        dasm_put(Dst, 704, (gp), var->offset+i, (gp));
#line 995 "../../src/codegen.in.c"
      }
    }
  }
}

#endif

static void copy_struct_reg(void) {
#if X64WIN
  // TODO: I'm not sure if this is right/sufficient.
  //| mov rax, [rax]
  dasm_put(Dst, 68);
#line 1006 "../../src/codegen.in.c"
#else
  Type* ty = C(current_fn)->ty->return_ty;

  int gp = 0, fp = 0;

  //| mov RUTIL, rax
  dasm_put(Dst, 718);
#line 1012 "../../src/codegen.in.c"

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4) {
      //| movss xmm0, dword [RUTIL]
      dasm_put(Dst, 722);
#line 1017 "../../src/codegen.in.c"
    } else {
      //| movsd xmm0, qword [RUTIL]
      dasm_put(Dst, 728);
#line 1019 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    //| mov rax, 0
    dasm_put(Dst, 734);
#line 1023 "../../src/codegen.in.c"
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      //| shl rax, 8
      //| mov ax, [RUTIL+i]
      dasm_put(Dst, 742, i);
#line 1026 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4) {
        //| movss xmm(fp), dword [RUTIL+8]
        dasm_put(Dst, 751, (fp));
#line 1035 "../../src/codegen.in.c"
      } else {
        //| movsd xmm(fp), qword [RUTIL+8]
        dasm_put(Dst, 761, (fp));
#line 1037 "../../src/codegen.in.c"
      }
    } else {
      //| mov Rq(gp), 0
      dasm_put(Dst, 771, (gp));
#line 1040 "../../src/codegen.in.c"
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        //| shl Rq(gp), 8
        //| mov Rb(gp), [RUTIL+i]
        dasm_put(Dst, 781, (gp), (gp), i);
#line 1043 "../../src/codegen.in.c"
      }
    }
  }
#endif
}

static void copy_struct_mem(void) {
  Type* ty = C(current_fn)->ty->return_ty;
  Obj* var = C(current_fn)->params;

  //| mov RUTIL, [rbp+var->offset]
  dasm_put(Dst, 795, var->offset);
#line 1054 "../../src/codegen.in.c"

  for (int i = 0; i < ty->size; i++) {
    //| mov dl, [rax+i]
    //| mov [RUTIL+i], dl
    dasm_put(Dst, 800, i, i);
#line 1058 "../../src/codegen.in.c"
  }
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  //| add CARG1, 15
  //| and CARG1d, 0xfffffff0
  dasm_put(Dst, 807);
#line 1065 "../../src/codegen.in.c"

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
  dasm_put(Dst, 816, C(current_fn)->alloca_bottom->offset);
#line 1082 "../../src/codegen.in.c"

  // Move alloca_bottom pointer.
  //| mov rax, [rbp+C(current_fn)->alloca_bottom->offset]
  //| sub rax, CARG1
  //| mov [rbp+C(current_fn)->alloca_bottom->offset], rax
  dasm_put(Dst, 868, C(current_fn)->alloca_bottom->offset, C(current_fn)->alloca_bottom->offset);
#line 1087 "../../src/codegen.in.c"
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
          dasm_put(Dst, 880, u.u32);
#line 1103 "../../src/codegen.in.c"
          return;
        }
        case TY_DOUBLE: {
          union {
            double f64;
            uint64_t u64;
          } u = {(double)node->fval};
          //| mov64 rax, u.u64
          //| movd xmm0, rax
          dasm_put(Dst, 888, (unsigned int)(u.u64), (unsigned int)((u.u64)>>32));
#line 1112 "../../src/codegen.in.c"
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
          dasm_put(Dst, 898, (unsigned int)(u.u64[0]), (unsigned int)((u.u64[0])>>32), (unsigned int)(u.u64[1]), (unsigned int)((u.u64[1])>>32));
#line 1127 "../../src/codegen.in.c"
          return;
        }
#endif
      }

      if (node->val < INT_MIN || node->val > INT_MAX) {
        //| mov64 rax, node->val
        dasm_put(Dst, 123, (unsigned int)(node->val), (unsigned int)((node->val)>>32));
#line 1134 "../../src/codegen.in.c"
      } else {
        //| mov rax, node->val
        dasm_put(Dst, 924, node->val);
#line 1136 "../../src/codegen.in.c"
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
          dasm_put(Dst, 929);
#line 1148 "../../src/codegen.in.c"
          return;
        case TY_DOUBLE:
          //| mov rax, 1
          //| shl rax, 63
          //| movd xmm1, rax
          //| xorpd xmm0, xmm1
          dasm_put(Dst, 949);
#line 1154 "../../src/codegen.in.c"
          return;
#if !X64WIN
        case TY_LDOUBLE:
          //| fchs
          dasm_put(Dst, 970);
#line 1158 "../../src/codegen.in.c"
          return;
#endif
      }

      //| neg rax
      dasm_put(Dst, 973);
#line 1163 "../../src/codegen.in.c"
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
        dasm_put(Dst, 978, 64 - mem->bit_width - mem->bit_offset);
#line 1175 "../../src/codegen.in.c"
        if (mem->ty->is_unsigned) {
          //| shr rax, 64 - mem->bit_width
          dasm_put(Dst, 983, 64 - mem->bit_width);
#line 1177 "../../src/codegen.in.c"
        } else {
          //| sar rax, 64 - mem->bit_width
          dasm_put(Dst, 988, 64 - mem->bit_width);
#line 1179 "../../src/codegen.in.c"
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
      gen_addr(node->lhs);
      push();
      gen_expr(node->rhs);

      if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
        //| mov r8, rax
        dasm_put(Dst, 994);
#line 1197 "../../src/codegen.in.c"

        // If the lhs is a bitfield, we need to read the current value
        // from memory and merge it with a new value.
        Member* mem = node->lhs->member;
        //| mov RUTIL, rax
        //| and RUTIL, (1L << mem->bit_width) - 1
        //| shl RUTIL, mem->bit_offset
        dasm_put(Dst, 998, (1L << mem->bit_width) - 1, mem->bit_offset);
#line 1204 "../../src/codegen.in.c"

        //| mov rax, [rsp]
        dasm_put(Dst, 1010);
#line 1206 "../../src/codegen.in.c"
        load(mem->ty);

        long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
        //| mov r9, ~mask
        //| and rax, r9
        //| or rax, RUTIL
        dasm_put(Dst, 1015, ~mask);
#line 1212 "../../src/codegen.in.c"
        store(node->ty);
        //| mov rax, r8
        dasm_put(Dst, 1026);
#line 1214 "../../src/codegen.in.c"
        return;
      }

      store(node->ty);
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
      dasm_put(Dst, 1030);
#line 1235 "../../src/codegen.in.c"
#endif
      //| mov rcx, node->var->ty->size
      //| lea rdi, [rbp+node->var->offset]
      //| mov al, 0
      //| rep
      //| stosb
      dasm_put(Dst, 1032, node->var->ty->size, node->var->offset);
#line 1241 "../../src/codegen.in.c"
#if X64WIN
      //| pop rdi
      dasm_put(Dst, 1046);
#line 1243 "../../src/codegen.in.c"
#endif
      return;
    case ND_COND: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1048, lelse);
#line 1251 "../../src/codegen.in.c"
      gen_expr(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1052, lend, lelse);
#line 1254 "../../src/codegen.in.c"
      gen_expr(node->els);
      //|=>lend:
      dasm_put(Dst, 1055, lend);
#line 1256 "../../src/codegen.in.c"
      return;
    }
    case ND_NOT:
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| sete al
      //| movzx rax, al
      dasm_put(Dst, 1057);
#line 1263 "../../src/codegen.in.c"
      return;
    case ND_BITNOT:
      gen_expr(node->lhs);
      //| not rax
      dasm_put(Dst, 1065);
#line 1267 "../../src/codegen.in.c"
      return;
    case ND_LOGAND: {
      int lfalse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| je =>lfalse
      dasm_put(Dst, 1048, lfalse);
#line 1274 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| je =>lfalse
      //| mov rax, 1
      //| jmp =>lend
      //|=>lfalse:
      //| mov rax, 0
      //|=>lend:
      dasm_put(Dst, 1070, lfalse, lend, lfalse, lend);
#line 1282 "../../src/codegen.in.c"
      return;
    }
    case ND_LOGOR: {
      int ltrue = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| jne =>ltrue
      dasm_put(Dst, 1093, ltrue);
#line 1290 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| jne =>ltrue
      //| mov rax, 0
      //| jmp =>lend
      //|=>ltrue:
      //| mov rax, 1
      //|=>lend:
      dasm_put(Dst, 1097, ltrue, lend, ltrue, lend);
#line 1298 "../../src/codegen.in.c"
      return;
    }
    case ND_FUNCALL: {
      if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
        gen_expr(node->args);
        //| mov CARG1, rax
        dasm_put(Dst, 718);
#line 1304 "../../src/codegen.in.c"
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
        dasm_put(Dst, 1120);
#line 1319 "../../src/codegen.in.c"

        // Store one-past the second argument into &ap.
        pop(REG_UTIL);
        //| mov [RUTIL], rax
        dasm_put(Dst, 103);
#line 1323 "../../src/codegen.in.c"
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
              dasm_put(Dst, 1125, (reg), (dasmargreg[reg]));
#line 1358 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1135, PARAMETER_SAVE_SIZE, stack_args*8 + PARAMETER_SAVE_SIZE + by_ref_copies_size);
#line 1372 "../../src/codegen.in.c"
      if (by_ref_copies_size > 0) {
        //| pop r11
        dasm_put(Dst, 1152);
#line 1374 "../../src/codegen.in.c"
      }

      C(depth) -= by_ref_copies_size / 8;
      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 172);
#line 1385 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 172);
#line 1389 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 168);
#line 1391 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 180);
#line 1396 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 176);
#line 1398 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned it's actually
      // returned in rax, so copy it back into the return buffer where we're
      // expecting it.
      if (node->ret_buffer && type_passed_in_register(node->ty)) {
        //| mov [rbp+node->ret_buffer->offset], rax
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 1155, node->ret_buffer->offset, node->ret_buffer->offset);
#line 1408 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1164, fp, stack_args*8);
#line 1469 "../../src/codegen.in.c"

      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 172);
#line 1478 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 172);
#line 1482 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 168);
#line 1484 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 180);
#line 1489 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 176);
#line 1491 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned
      // using up to two registers.
      if (node->ret_buffer && node->ty->size <= 16) {
        copy_ret_buffer(node->ret_buffer);
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 112, node->ret_buffer->offset);
#line 1500 "../../src/codegen.in.c"
      }

#endif  // SysV

      return;
    }
    case ND_LABEL_VAL:
      //| lea rax, [=>node->pc_label]
      dasm_put(Dst, 117, node->pc_label);
#line 1508 "../../src/codegen.in.c"
      return;
    case ND_REFLECT_TYPE_PTR:
      //| mov64 rax, node->rty;
      dasm_put(Dst, 123, (unsigned int)(node->rty), (unsigned int)((node->rty)>>32));
#line 1511 "../../src/codegen.in.c"
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
        dasm_put(Dst, 994);
#line 1523 "../../src/codegen.in.c"
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
          dasm_put(Dst, 1180);
#line 1540 "../../src/codegen.in.c"
          break;
        case 2:
          // lock cmpxchg WORD PTR [rdi/rcx],dx
          //| .byte 0x66
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1186);
#line 1548 "../../src/codegen.in.c"
          break;
        case 4:
          // lock cmpxchg DWORD PTR [rdi/rcx],edx
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1187);
#line 1555 "../../src/codegen.in.c"
          break;
        case 8:
          // lock cmpxchg QWORD PTR [rdi/rcx],rdx
          //| .byte 0xf0
          //| .byte 0x48
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1193);
#line 1563 "../../src/codegen.in.c"
          break;
        default:
          unreachable();
      }
      if (!is_locked_ce) {
        //| sete cl
        //| je >1
        dasm_put(Dst, 1200);
#line 1570 "../../src/codegen.in.c"
        switch (sz) {
          case 1:
            //| mov [r8], al
            dasm_put(Dst, 1208);
#line 1573 "../../src/codegen.in.c"
            break;
          case 2:
            //| mov [r8], ax
            dasm_put(Dst, 1212);
#line 1576 "../../src/codegen.in.c"
            break;
          case 4:
            //| mov [r8], eax
            dasm_put(Dst, 1213);
#line 1579 "../../src/codegen.in.c"
            break;
          case 8:
            //| mov [r8], rax
            dasm_put(Dst, 1217);
#line 1582 "../../src/codegen.in.c"
            break;
          default:
            unreachable();
        }
        //|1:
        //| movzx eax, cl
        dasm_put(Dst, 1221);
#line 1588 "../../src/codegen.in.c"
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
          dasm_put(Dst, 1227);
#line 1602 "../../src/codegen.in.c"
          break;
        case 2:
          //| xchg [RUTIL], ax
          dasm_put(Dst, 1230);
#line 1605 "../../src/codegen.in.c"
          break;
        case 4:
          //| xchg [RUTIL], eax
          dasm_put(Dst, 1231);
#line 1608 "../../src/codegen.in.c"
          break;
        case 8:
          //| xchg [RUTIL], rax
          dasm_put(Dst, 1234);
#line 1611 "../../src/codegen.in.c"
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
            dasm_put(Dst, 1238);
#line 1633 "../../src/codegen.in.c"
          } else {
            //| addsd xmm0, xmm1
            dasm_put(Dst, 1244);
#line 1635 "../../src/codegen.in.c"
          }
          return;
        case ND_SUB:
          if (is_float) {
            //| subss xmm0, xmm1
            dasm_put(Dst, 1250);
#line 1640 "../../src/codegen.in.c"
          } else {
            //| subsd xmm0, xmm1
            dasm_put(Dst, 1256);
#line 1642 "../../src/codegen.in.c"
          }
          return;
        case ND_MUL:
          if (is_float) {
            //| mulss xmm0, xmm1
            dasm_put(Dst, 1262);
#line 1647 "../../src/codegen.in.c"
          } else {
            //| mulsd xmm0, xmm1
            dasm_put(Dst, 1268);
#line 1649 "../../src/codegen.in.c"
          }
          return;
        case ND_DIV:
          if (is_float) {
            //| divss xmm0, xmm1
            dasm_put(Dst, 1274);
#line 1654 "../../src/codegen.in.c"
          } else {
            //| divsd xmm0, xmm1
            dasm_put(Dst, 1280);
#line 1656 "../../src/codegen.in.c"
          }
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          if (is_float) {
            //| ucomiss xmm1, xmm0
            dasm_put(Dst, 1286);
#line 1664 "../../src/codegen.in.c"
          } else {
            //| ucomisd xmm1, xmm0
            dasm_put(Dst, 1290);
#line 1666 "../../src/codegen.in.c"
          }

          if (node->kind == ND_EQ) {
            //| sete al
            //| setnp dl
            //| and al, dl
            dasm_put(Dst, 1295);
#line 1672 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            //| setp dl
            //| or al, dl
            dasm_put(Dst, 1304);
#line 1676 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1313);
#line 1678 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1317);
#line 1680 "../../src/codegen.in.c"
          }

          //| and al, 1
          //| movzx rax, al
          dasm_put(Dst, 1321);
#line 1684 "../../src/codegen.in.c"
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
          dasm_put(Dst, 1328);
#line 1697 "../../src/codegen.in.c"
          return;
        case ND_SUB:
          //| fsubrp st1, st0
          dasm_put(Dst, 1331);
#line 1700 "../../src/codegen.in.c"
          return;
        case ND_MUL:
          //| fmulp st1, st0
          dasm_put(Dst, 1334);
#line 1703 "../../src/codegen.in.c"
          return;
        case ND_DIV:
          //| fdivrp st1, st0
          dasm_put(Dst, 1337);
#line 1706 "../../src/codegen.in.c"
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          //| fcomip st1
          //| fstp st0
          dasm_put(Dst, 1341);
#line 1713 "../../src/codegen.in.c"

          if (node->kind == ND_EQ) {
            //| sete al
            dasm_put(Dst, 1347);
#line 1716 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            dasm_put(Dst, 1351);
#line 1718 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1313);
#line 1720 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1317);
#line 1722 "../../src/codegen.in.c"
          }

          //| movzx rax, al
          dasm_put(Dst, 1060);
#line 1725 "../../src/codegen.in.c"
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
        dasm_put(Dst, 1355);
#line 1744 "../../src/codegen.in.c"
      } else {
        //| add eax, RUTILd
        dasm_put(Dst, 1356);
#line 1746 "../../src/codegen.in.c"
      }
      return;
    case ND_SUB:
      if (is_long) {
        //| sub rax, RUTIL
        dasm_put(Dst, 1359);
#line 1751 "../../src/codegen.in.c"
      } else {
        //| sub eax, RUTILd
        dasm_put(Dst, 1360);
#line 1753 "../../src/codegen.in.c"
      }
      return;
    case ND_MUL:
      if (is_long) {
        //| imul rax, RUTIL
        dasm_put(Dst, 1363);
#line 1758 "../../src/codegen.in.c"
      } else {
        //| imul eax, RUTILd
        dasm_put(Dst, 1364);
#line 1760 "../../src/codegen.in.c"
      }
      return;
    case ND_DIV:
    case ND_MOD:
      if (node->ty->is_unsigned) {
        if (is_long) {
          //| mov rdx, 0
          //| div RUTIL
          dasm_put(Dst, 1368);
#line 1768 "../../src/codegen.in.c"
        } else {
          //| mov edx, 0
          //| div RUTILd
          dasm_put(Dst, 1381);
#line 1771 "../../src/codegen.in.c"
        }
      } else {
        if (node->lhs->ty->size == 8) {
          //| cqo
          dasm_put(Dst, 1391);
#line 1775 "../../src/codegen.in.c"
        } else {
          //| cdq
          dasm_put(Dst, 1392);
#line 1777 "../../src/codegen.in.c"
        }
        if (is_long) {
          //| idiv RUTIL
          dasm_put(Dst, 1394);
#line 1780 "../../src/codegen.in.c"
        } else {
          //| idiv RUTILd
          dasm_put(Dst, 1395);
#line 1782 "../../src/codegen.in.c"
        }
      }

      if (node->kind == ND_MOD) {
        //| mov rax, rdx
        dasm_put(Dst, 1400);
#line 1787 "../../src/codegen.in.c"
      }
      return;
    case ND_BITAND:
      if (is_long) {
        //| and rax, RUTIL
        dasm_put(Dst, 1404);
#line 1792 "../../src/codegen.in.c"
      } else {
        //| and eax, RUTILd
        dasm_put(Dst, 1405);
#line 1794 "../../src/codegen.in.c"
      }
      return;
    case ND_BITOR:
      if (is_long) {
        //| or rax, RUTIL
        dasm_put(Dst, 1022);
#line 1799 "../../src/codegen.in.c"
      } else {
        //| or eax, RUTILd
        dasm_put(Dst, 1023);
#line 1801 "../../src/codegen.in.c"
      }
      return;
    case ND_BITXOR:
      if (is_long) {
        //| xor rax, RUTIL
        dasm_put(Dst, 1408);
#line 1806 "../../src/codegen.in.c"
      } else {
        //| xor eax, RUTILd
        dasm_put(Dst, 1409);
#line 1808 "../../src/codegen.in.c"
      }
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      if (is_long) {
        //| cmp rax, RUTIL
        dasm_put(Dst, 1412);
#line 1816 "../../src/codegen.in.c"
      } else {
        //| cmp eax, RUTILd
        dasm_put(Dst, 1413);
#line 1818 "../../src/codegen.in.c"
      }

      if (node->kind == ND_EQ) {
        //| sete al
        dasm_put(Dst, 1347);
#line 1822 "../../src/codegen.in.c"
      } else if (node->kind == ND_NE) {
        //| setne al
        dasm_put(Dst, 1351);
#line 1824 "../../src/codegen.in.c"
      } else if (node->kind == ND_LT) {
        if (node->lhs->ty->is_unsigned) {
          //| setb al
          dasm_put(Dst, 1416);
#line 1827 "../../src/codegen.in.c"
        } else {
          //| setl al
          dasm_put(Dst, 1420);
#line 1829 "../../src/codegen.in.c"
        }
      } else if (node->kind == ND_LE) {
        if (node->lhs->ty->is_unsigned) {
          //| setbe al
          dasm_put(Dst, 1424);
#line 1833 "../../src/codegen.in.c"
        } else {
          //| setle al
          dasm_put(Dst, 1428);
#line 1835 "../../src/codegen.in.c"
        }
      }

      //| movzx rax, al
      dasm_put(Dst, 1060);
#line 1839 "../../src/codegen.in.c"
      return;
    case ND_SHL:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1432);
#line 1842 "../../src/codegen.in.c"
      if (is_long) {
        //| shl rax, cl
        dasm_put(Dst, 1436);
#line 1844 "../../src/codegen.in.c"
      } else {
        //| shl eax, cl
        dasm_put(Dst, 1437);
#line 1846 "../../src/codegen.in.c"
      }
      return;
    case ND_SHR:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1432);
#line 1850 "../../src/codegen.in.c"
      if (node->lhs->ty->is_unsigned) {
        if (is_long) {
          //| shr rax, cl
          dasm_put(Dst, 1440);
#line 1853 "../../src/codegen.in.c"
        } else {
          //| shr eax, cl
          dasm_put(Dst, 1441);
#line 1855 "../../src/codegen.in.c"
        }
      } else {
        if (is_long) {
          //| sar rax, cl
          dasm_put(Dst, 1444);
#line 1859 "../../src/codegen.in.c"
        } else {
          //| sar eax, cl
          dasm_put(Dst, 1445);
#line 1861 "../../src/codegen.in.c"
        }
      }
      return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node* node) {
  switch (node->kind) {
    case ND_IF: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1048, lelse);
#line 1877 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1052, lend, lelse);
#line 1880 "../../src/codegen.in.c"
      if (node->els)
        gen_stmt(node->els);
      //|=>lend:
      dasm_put(Dst, 1055, lend);
#line 1883 "../../src/codegen.in.c"
      return;
    }
    case ND_FOR: {
      if (node->init)
        gen_stmt(node->init);
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 1055, lbegin);
#line 1890 "../../src/codegen.in.c"
      if (node->cond) {
        gen_expr(node->cond);
        cmp_zero(node->cond->ty);
        //| je =>node->brk_pc_label
        dasm_put(Dst, 1048, node->brk_pc_label);
#line 1894 "../../src/codegen.in.c"
      }
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 1055, node->cont_pc_label);
#line 1897 "../../src/codegen.in.c"
      if (node->inc)
        gen_expr(node->inc);
      //| jmp =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1052, lbegin, node->brk_pc_label);
#line 1901 "../../src/codegen.in.c"
      return;
    }
    case ND_DO: {
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 1055, lbegin);
#line 1906 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 1055, node->cont_pc_label);
#line 1908 "../../src/codegen.in.c"
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| jne =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1449, lbegin, node->brk_pc_label);
#line 1912 "../../src/codegen.in.c"
      return;
    }
    case ND_SWITCH:
      gen_expr(node->cond);

      for (Node* n = node->case_next; n; n = n->case_next) {
        bool is_long = node->cond->ty->size == 8;

        if (n->begin == n->end) {
          if (is_long) {
            //| cmp rax, n->begin
            dasm_put(Dst, 1454, n->begin);
#line 1923 "../../src/codegen.in.c"
          } else {
            //| cmp eax, n->begin
            dasm_put(Dst, 1455, n->begin);
#line 1925 "../../src/codegen.in.c"
          }
          //| je =>n->pc_label
          dasm_put(Dst, 1048, n->pc_label);
#line 1927 "../../src/codegen.in.c"
          continue;
        }

        // [GNU] Case ranges
        if (is_long) {
          //| mov RUTIL, rax
          //| sub RUTIL, n->begin
          //| cmp RUTIL, n->end - n->begin
          dasm_put(Dst, 1460, n->begin, n->end - n->begin);
#line 1935 "../../src/codegen.in.c"
        } else {
          //| mov RUTILd, eax
          //| sub RUTILd, n->begin
          //| cmp RUTILd, n->end - n->begin
          dasm_put(Dst, 1474, n->begin, n->end - n->begin);
#line 1939 "../../src/codegen.in.c"
        }
        //| jbe =>n->pc_label
        dasm_put(Dst, 1485, n->pc_label);
#line 1941 "../../src/codegen.in.c"
      }

      if (node->default_case) {
        //| jmp =>node->default_case->pc_label
        dasm_put(Dst, 1489, node->default_case->pc_label);
#line 1945 "../../src/codegen.in.c"
      }

      //| jmp =>node->brk_pc_label
      dasm_put(Dst, 1489, node->brk_pc_label);
#line 1948 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1055, node->brk_pc_label);
#line 1950 "../../src/codegen.in.c"
      return;
    case ND_CASE:
      //|=>node->pc_label:
      dasm_put(Dst, 1055, node->pc_label);
#line 1953 "../../src/codegen.in.c"
      gen_stmt(node->lhs);
      return;
    case ND_BLOCK:
      for (Node* n = node->body; n; n = n->next)
        gen_stmt(n);
      return;
    case ND_GOTO:
      //| jmp =>node->pc_label
      dasm_put(Dst, 1489, node->pc_label);
#line 1961 "../../src/codegen.in.c"
      return;
    case ND_GOTO_EXPR:
      gen_expr(node->lhs);
      //| jmp rax
      dasm_put(Dst, 1493);
#line 1965 "../../src/codegen.in.c"
      return;
    case ND_LABEL:
      //|=>node->pc_label:
      dasm_put(Dst, 1055, node->pc_label);
#line 1968 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1489, C(current_fn)->dasm_return_label);
#line 1994 "../../src/codegen.in.c"
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
    int bottom = 0;

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
    //
    // LinkFixup won't work as is
    // - offset is codeseg relative. it can be made a pointer for the
    // codeseg imports instead because it's recreated for each compile
    // anyway
    // - need to remap initializer_code_relocation to name, but...
    // actually we have the real address at this point now, so if the
    // data was allocated it can just be written directly i think rather
    // than deferred to a relocation

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
      dasm_put(Dst, 682, (r), offset);
#line 2316 "../../src/codegen.in.c"
      return;
    case 8:
      //| movsd qword [rbp+offset], xmm(r)
      dasm_put(Dst, 693, (r), offset);
#line 2319 "../../src/codegen.in.c"
      return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
    case 1:
      //| mov [rbp+offset], Rb(dasmargreg[r])
      dasm_put(Dst, 1497, (dasmargreg[r]), offset);
#line 2328 "../../src/codegen.in.c"
      return;
    case 2:
      //| mov [rbp+offset], Rw(dasmargreg[r])
      dasm_put(Dst, 1505, (dasmargreg[r]), offset);
#line 2331 "../../src/codegen.in.c"
      return;
      return;
    case 4:
      //| mov [rbp+offset], Rd(dasmargreg[r])
      dasm_put(Dst, 1506, (dasmargreg[r]), offset);
#line 2335 "../../src/codegen.in.c"
      return;
    case 8:
      //| mov [rbp+offset], Rq(dasmargreg[r])
      dasm_put(Dst, 1514, (dasmargreg[r]), offset);
#line 2338 "../../src/codegen.in.c"
      return;
    default:
      for (int i = 0; i < sz; i++) {
        //| mov [rbp+offset+i], Rb(dasmargreg[r])
        //| shr Rq(dasmargreg[r]), 8
        dasm_put(Dst, 704, (dasmargreg[r]), offset+i, (dasmargreg[r]));
#line 2343 "../../src/codegen.in.c"
      }
      return;
  }
}

#if X64WIN
extern int __chkstk(void);
#endif

static void emit_text(Obj* prog) {
  // Preallocate the dasm labels so they can be used in functions out of order.
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    fn->dasm_return_label = codegen_pclabel();
    fn->dasm_entry_label = codegen_pclabel();
  }

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    //|=>fn->dasm_entry_label:
    dasm_put(Dst, 1055, fn->dasm_entry_label);
#line 2367 "../../src/codegen.in.c"

    C(current_fn) = fn;

    // outaf("---- %s\n", fn->name);

    // Prologue
    //| push rbp
    //| mov rbp, rsp
    dasm_put(Dst, 1522);
#line 2375 "../../src/codegen.in.c"

#if X64WIN
    // Stack probe on Windows if necessary. The MSDN reference for __chkstk says
    // it's only necessary beyond 8k for x64, but cl does it at 4k.
    if (fn->stack_size >= 4096) {
      //| mov rax, fn->stack_size
      dasm_put(Dst, 924, fn->stack_size);
#line 2381 "../../src/codegen.in.c"
      int fixup_location = codegen_pclabel();
      strintarray_push(&C(fixups), (StringInt){"__chkstk", fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
      //|=>fixup_location:
      //| mov64 r10, 0xc0dec0dec0dec0de
      dasm_put(Dst, 1527, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 2389 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      //| call r10
      //| sub rsp, rax
      dasm_put(Dst, 1533);
#line 2394 "../../src/codegen.in.c"
    } else
#endif

    {
      //| sub rsp, fn->stack_size
      dasm_put(Dst, 617, fn->stack_size);
#line 2399 "../../src/codegen.in.c"
    }
    //| mov [rbp+fn->alloca_bottom->offset], rsp
    dasm_put(Dst, 1541, fn->alloca_bottom->offset);
#line 2401 "../../src/codegen.in.c"

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
      dasm_put(Dst, 1546, off, gp*8, off+4, fp * 8 + 48, off+8, off+8, off+16, off+16, off+24);
#line 2422 "../../src/codegen.in.c"

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
      dasm_put(Dst, 1573, off + 24, off + 32, off + 40, off + 48, off + 56, off + 64, off + 72, off + 80, off + 88, off + 96, off + 104, off + 112, off + 120, off + 128);
#line 2438 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1646, 16, 24, 32, 40);
#line 2449 "../../src/codegen.in.c"
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
            store_gp(reg++, var->offset, MIN(8, ty->size));
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
      dasm_put(Dst, 734);
#line 2520 "../../src/codegen.in.c"
    }

    // Epilogue
    //|=>fn->dasm_return_label:
    //| mov rsp, rbp
    //| pop rbp
    //| ret
    dasm_put(Dst, 1663, fn->dasm_return_label);
#line 2527 "../../src/codegen.in.c"
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

  assign_lvar_offsets(prog);
  emit_text(prog);

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
  fld->codeseg_base_address = allocate_writable_memory(page_sized);
  // outaf("code_size: %zu, page_sized: %zu\n", code_size, page_sized);

  fill_out_text_exports(prog, fld->codeseg_base_address);

  free_link_fixups(fld);
  emit_data(prog);  // This needs to point into code for fixups, so has to go late-ish.
  fill_out_fixups(fld);

  dasm_encode(&C(dynasm), fld->codeseg_base_address);

  int check_result = dasm_checkstep(&C(dynasm), DASM_SECTION_MAIN);
  if (check_result != DASM_S_OK) {
    outaf("check_result: 0x%08x\n", check_result);
    ABORT("dasm_checkstep failed");
  }

  codegen_free();
}

// This can be called after a longjmp in update.
static void codegen_free(void) {
  if (C(dynasm)) {
    dasm_free(&C(dynasm));
  }
}
//
// END OF out/wr/codegen.w.c
//
#else // ^^^ X64WIN / !X64WIN vvv
#undef C
#undef L
#undef VOID
//
// START OF out/lr/codegen.l.c
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
#line 19 "../../src/codegen.in.c"
//| .section main
#define DASM_SECTION_MAIN	0
#define DASM_MAXSECTION		1
#line 20 "../../src/codegen.in.c"
//| .actionlist dynasm_actions
static const unsigned char dynasm_actions[1680] = {
  80,255,64,88,240,42,255,72,131,252,236,8,252,242,15,17,4,36,255,252,242,64,
  15,16,4,240,140,36,72,131,196,8,255,252,243,15,16,0,255,252,242,15,16,0,255,
  219,40,255,15,182,0,255,15,190,0,255,15,183,0,255,15,191,0,255,72,99,0,255,
  72,139,0,255,68,138,128,233,68,136,135,233,255,252,243,15,17,7,255,252,242,
  15,17,7,255,219,63,255,136,7,255,102,137,7,255,72,137,7,255,72,139,133,233,
  255,72,141,133,233,255,72,141,5,245,255,249,72,184,237,237,255,72,129,192,
  239,255,15,87,201,15,46,193,255,102,15,87,201,102,15,46,193,255,217,252,238,
  223,232,221,216,255,131,252,248,0,255,72,131,252,248,0,255,15,190,192,255,
  15,182,192,255,15,191,192,255,15,183,192,255,252,243,15,42,192,255,72,99,
  192,255,252,242,15,42,192,255,137,68,36,252,252,219,68,36,252,252,255,137,
  192,252,243,72,15,42,192,255,137,192,255,137,192,252,242,72,15,42,192,255,
  137,192,72,137,68,36,252,248,223,108,36,252,248,255,72,133,192,15,136,244,
  247,102,15,252,239,192,252,242,72,15,42,192,252,233,244,248,248,1,72,137,
  199,131,224,1,102,15,252,239,192,72,209,252,239,72,9,199,252,242,72,15,42,
  199,252,242,15,88,192,248,2,255,72,137,68,36,252,248,223,108,36,252,248,72,
  133,192,15,137,244,247,184,0,0,128,95,137,68,36,252,252,216,68,36,252,252,
  248,1,255,252,243,15,44,192,15,190,192,255,252,243,15,44,192,15,182,192,255,
  252,243,15,44,192,15,191,192,255,252,243,15,44,192,15,183,192,255,252,243,
  15,44,192,255,252,243,72,15,44,192,255,252,243,15,90,192,255,252,243,15,17,
  68,36,252,252,217,68,36,252,252,255,252,242,15,44,192,15,190,192,255,252,
  242,15,44,192,15,182,192,255,252,242,15,44,192,15,191,192,255,252,242,15,
  44,192,15,183,192,255,252,242,15,44,192,255,252,242,72,15,44,192,255,252,
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
  255,73,137,192,255,72,137,199,72,129,231,239,72,193,231,235,255,72,139,4,
  36,255,73,199,193,237,76,33,200,72,9,252,248,255,76,137,192,255,87,255,72,
  199,193,237,72,141,189,233,176,0,252,243,170,255,95,255,15,132,245,255,252,
  233,245,249,255,15,148,208,72,15,182,192,255,72,252,247,208,255,15,132,245,
  72,199,192,1,0,0,0,252,233,245,249,72,199,192,0,0,0,0,249,255,15,133,245,
  255,15,133,245,72,199,192,0,0,0,0,252,233,245,249,72,199,192,1,0,0,0,249,
  255,72,131,192,8,255,102,72,15,126,192,240,132,240,36,255,72,129,252,236,
  239,73,137,194,65,252,255,210,72,129,196,239,255,65,91,255,72,137,133,233,
  72,141,133,233,255,73,137,194,72,199,192,237,65,252,255,210,72,129,196,239,
  255,252,240,15,176,23,255,102,252,240,15,177,23,255,252,240,72,15,177,23,
  255,15,148,209,15,132,244,247,255,65,136,0,255,102,65,137,0,255,73,137,0,
  255,248,1,15,182,193,255,134,7,255,102,135,7,255,72,135,7,255,252,243,15,
  88,193,255,252,242,15,88,193,255,252,243,15,92,193,255,252,242,15,92,193,
  255,252,243,15,89,193,255,252,242,15,89,193,255,252,243,15,94,193,255,252,
  242,15,94,193,255,15,46,200,255,102,15,46,200,255,15,148,208,15,155,210,32,
  208,255,15,149,208,15,154,210,8,208,255,15,151,208,255,15,147,208,255,36,
  1,72,15,182,192,255,222,193,255,222,225,255,222,201,255,222,252,241,255,223,
  252,241,221,216,255,15,148,208,255,15,149,208,255,72,1,252,248,255,72,41,
  252,248,255,72,15,175,199,255,72,199,194,0,0,0,0,72,252,247,252,247,255,186,
  0,0,0,0,252,247,252,247,255,72,153,255,72,252,247,252,255,255,72,137,208,
  255,72,33,252,248,255,72,49,252,248,255,72,57,252,248,255,15,146,208,255,
  15,156,208,255,15,150,208,255,15,158,208,255,72,137,252,249,255,72,211,224,
  255,72,211,232,255,72,211,252,248,255,15,133,245,249,255,72,129,252,248,239,
  255,72,137,199,72,129,252,239,239,72,129,252,255,239,255,137,199,129,252,
  239,239,129,252,255,239,255,15,134,245,255,252,233,245,255,252,255,224,255,
  64,136,133,253,240,131,233,255,102,64,137,133,253,240,139,233,255,72,137,
  133,253,240,131,233,255,85,72,137,229,255,249,73,186,237,237,255,65,252,255,
  210,72,41,196,255,72,137,165,233,255,199,133,233,237,199,133,233,237,72,137,
  173,233,72,131,133,233,16,72,137,173,233,72,129,133,233,239,255,72,137,189,
  233,72,137,181,233,72,137,149,233,72,137,141,233,76,137,133,233,76,137,141,
  233,252,242,15,17,133,233,252,242,15,17,141,233,252,242,15,17,149,233,252,
  242,15,17,157,233,252,242,15,17,165,233,252,242,15,17,173,233,252,242,15,
  17,181,233,252,242,15,17,189,233,255,72,137,189,233,72,137,181,233,72,137,
  149,233,72,137,141,233,255,249,72,137,252,236,93,195,255
};

#line 21 "../../src/codegen.in.c"
//| .globals dynasm_globals
enum {
  dynasm_globals_MAX
};
#line 22 "../../src/codegen.in.c"
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

static int codegen_pclabel(void) {
  int ret = C(numlabels);
  dasm_growpc(&C(dynasm), ++C(numlabels));
  return ret;
}

static void push(void) {
  //| push rax
  dasm_put(Dst, 0);
#line 80 "../../src/codegen.in.c"
  C(depth)++;
}

static void pop(int dasmreg) {
  //| pop Rq(dasmreg)
  dasm_put(Dst, 2, (dasmreg));
#line 85 "../../src/codegen.in.c"
  C(depth)--;
}

static void pushf(void) {
  //| sub rsp, 8
  //| movsd qword [rsp], xmm0
  dasm_put(Dst, 7);
#line 91 "../../src/codegen.in.c"
  C(depth)++;
}

static void popf(int reg) {
  //| movsd xmm(reg), qword [rsp]
  //| add rsp, 8
  dasm_put(Dst, 19, (reg));
#line 97 "../../src/codegen.in.c"
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
      dasm_put(Dst, 33);
#line 117 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd xmm0, qword [rax]
      dasm_put(Dst, 39);
#line 120 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fld tword [rax]
      dasm_put(Dst, 45);
#line 124 "../../src/codegen.in.c"
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
      dasm_put(Dst, 48);
#line 136 "../../src/codegen.in.c"
    } else {
      //| movsx eax, byte [rax]
      dasm_put(Dst, 52);
#line 138 "../../src/codegen.in.c"
    }
  } else if (ty->size == 2) {
    if (ty->is_unsigned) {
      //| movzx eax, word [rax]
      dasm_put(Dst, 56);
#line 142 "../../src/codegen.in.c"
    } else {
      //| movsx eax, word [rax]
      dasm_put(Dst, 60);
#line 144 "../../src/codegen.in.c"
    }
  } else if (ty->size == 4) {
    //| movsxd rax, dword [rax]
    dasm_put(Dst, 64);
#line 147 "../../src/codegen.in.c"
  } else {
    //| mov rax, qword [rax]
    dasm_put(Dst, 68);
#line 149 "../../src/codegen.in.c"
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
        dasm_put(Dst, 72, i, i);
#line 162 "../../src/codegen.in.c"
      }
      return;
    case TY_FLOAT:
      //| movss dword [RUTIL], xmm0
      dasm_put(Dst, 81);
#line 166 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| movsd qword [RUTIL], xmm0
      dasm_put(Dst, 87);
#line 169 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fstp tword [RUTIL]
      dasm_put(Dst, 93);
#line 173 "../../src/codegen.in.c"
      return;
#endif
  }

  if (ty->size == 1) {
    //| mov [RUTIL], al
    dasm_put(Dst, 96);
#line 179 "../../src/codegen.in.c"
  } else if (ty->size == 2) {
    //| mov [RUTIL], ax
    dasm_put(Dst, 99);
#line 181 "../../src/codegen.in.c"
  } else if (ty->size == 4) {
    //| mov [RUTIL], eax
    dasm_put(Dst, 100);
#line 183 "../../src/codegen.in.c"
  } else {
    //| mov [RUTIL], rax
    dasm_put(Dst, 103);
#line 185 "../../src/codegen.in.c"
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
        dasm_put(Dst, 107, node->var->offset);
#line 196 "../../src/codegen.in.c"
        return;
      }

      // Local variable
      if (node->var->is_local) {
        //| lea rax, [rbp+node->var->offset]
        dasm_put(Dst, 112, node->var->offset);
#line 202 "../../src/codegen.in.c"
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
          dasm_put(Dst, 117, node->var->dasm_entry_label);
#line 217 "../../src/codegen.in.c"
        } else {
          int fixup_location = codegen_pclabel();
          strintarray_push(&C(fixups), (StringInt){node->var->name, fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
          //|=>fixup_location:
          //| mov64 rax, 0xc0dec0dec0dec0de
          dasm_put(Dst, 122, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 226 "../../src/codegen.in.c"
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
      dasm_put(Dst, 122, fixup_location, (unsigned int)(0xda7ada7ada7ada7a), (unsigned int)((0xda7ada7ada7ada7a)>>32));
#line 242 "../../src/codegen.in.c"
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
        dasm_put(Dst, 68);
#line 258 "../../src/codegen.in.c"
      }
#endif
      //| add rax, node->member->offset
      dasm_put(Dst, 128, node->member->offset);
#line 261 "../../src/codegen.in.c"
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
      dasm_put(Dst, 112, node->var->offset);
#line 277 "../../src/codegen.in.c"
      return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void cmp_zero(Type* ty) {
  switch (ty->kind) {
    case TY_FLOAT:
      //| xorps xmm1, xmm1
      //| ucomiss xmm0, xmm1
      dasm_put(Dst, 133);
#line 288 "../../src/codegen.in.c"
      return;
    case TY_DOUBLE:
      //| xorpd xmm1, xmm1
      //| ucomisd xmm0, xmm1
      dasm_put(Dst, 140);
#line 292 "../../src/codegen.in.c"
      return;
#if !X64WIN
    case TY_LDOUBLE:
      //| fldz
      //| fucomip st0
      //| fstp st0
      dasm_put(Dst, 149);
#line 298 "../../src/codegen.in.c"
      return;
#endif
  }

  if (is_integer(ty) && ty->size <= 4) {
    //| cmp eax, 0
    dasm_put(Dst, 157);
#line 304 "../../src/codegen.in.c"
  } else {
    //| cmp rax, 0
    dasm_put(Dst, 162);
#line 306 "../../src/codegen.in.c"
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
  dasm_put(Dst, 168);
#line 335 "../../src/codegen.in.c"
}
static void i32u8(void) {
  //| movzx eax, al
  dasm_put(Dst, 172);
#line 338 "../../src/codegen.in.c"
}
static void i32i16(void) {
  //| movsx eax, ax
  dasm_put(Dst, 176);
#line 341 "../../src/codegen.in.c"
}
static void i32u16(void) {
  //| movzx eax, ax
  dasm_put(Dst, 180);
#line 344 "../../src/codegen.in.c"
}
static void i32f32(void) {
  //| cvtsi2ss xmm0, eax
  dasm_put(Dst, 184);
#line 347 "../../src/codegen.in.c"
}
static void i32i64(void) {
  //| movsxd rax, eax
  dasm_put(Dst, 190);
#line 350 "../../src/codegen.in.c"
}
static void i32f64(void) {
  //| cvtsi2sd xmm0, eax
  dasm_put(Dst, 194);
#line 353 "../../src/codegen.in.c"
}
static void i32f80(void) {
  //| mov [rsp-4], eax
  //| fild dword [rsp-4]
  dasm_put(Dst, 200);
#line 357 "../../src/codegen.in.c"
}

static void u32f32(void) {
  //| mov eax, eax
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 211);
#line 362 "../../src/codegen.in.c"
}
static void u32i64(void) {
  //| mov eax, eax
  dasm_put(Dst, 220);
#line 365 "../../src/codegen.in.c"
}
static void u32f64(void) {
  //| mov eax, eax
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 223);
#line 369 "../../src/codegen.in.c"
}
static void u32f80(void) {
  //| mov eax, eax
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 232);
#line 374 "../../src/codegen.in.c"
}

static void i64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 213);
#line 378 "../../src/codegen.in.c"
}
static void i64f64(void) {
  //| cvtsi2sd xmm0, rax
  dasm_put(Dst, 225);
#line 381 "../../src/codegen.in.c"
}
static void i64f80(void) {
  //| mov [rsp-8], rax
  //| fild qword [rsp-8]
  dasm_put(Dst, 234);
#line 385 "../../src/codegen.in.c"
}

static void u64f32(void) {
  //| cvtsi2ss xmm0, rax
  dasm_put(Dst, 213);
#line 389 "../../src/codegen.in.c"
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
  dasm_put(Dst, 246);
#line 405 "../../src/codegen.in.c"
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
  dasm_put(Dst, 302);
#line 415 "../../src/codegen.in.c"
}

static void f32i8(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 338);
#line 420 "../../src/codegen.in.c"
}
static void f32u8(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 347);
#line 424 "../../src/codegen.in.c"
}
static void f32i16(void) {
  //| cvttss2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 356);
#line 428 "../../src/codegen.in.c"
}
static void f32u16(void) {
  //| cvttss2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 365);
#line 432 "../../src/codegen.in.c"
}
static void f32i32(void) {
  //| cvttss2si eax, xmm0
  dasm_put(Dst, 374);
#line 435 "../../src/codegen.in.c"
}
static void f32u32(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 380);
#line 438 "../../src/codegen.in.c"
}
static void f32i64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 380);
#line 441 "../../src/codegen.in.c"
}
static void f32u64(void) {
  //| cvttss2si rax, xmm0
  dasm_put(Dst, 380);
#line 444 "../../src/codegen.in.c"
}
static void f32f64(void) {
  //| cvtss2sd xmm0, xmm0
  dasm_put(Dst, 387);
#line 447 "../../src/codegen.in.c"
}
static void f32f80(void) {
  //| movss dword [rsp-4], xmm0
  //| fld dword [rsp-4]
  dasm_put(Dst, 393);
#line 451 "../../src/codegen.in.c"
}

static void f64i8(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, al
  dasm_put(Dst, 407);
#line 456 "../../src/codegen.in.c"
}
static void f64u8(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, al
  dasm_put(Dst, 416);
#line 460 "../../src/codegen.in.c"
}
static void f64i16(void) {
  //| cvttsd2si eax, xmm0
  //| movsx eax, ax
  dasm_put(Dst, 425);
#line 464 "../../src/codegen.in.c"
}
static void f64u16(void) {
  //| cvttsd2si eax, xmm0
  //| movzx eax, ax
  dasm_put(Dst, 434);
#line 468 "../../src/codegen.in.c"
}
static void f64i32(void) {
  //| cvttsd2si eax, xmm0
  dasm_put(Dst, 443);
#line 471 "../../src/codegen.in.c"
}
static void f64u32(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 449);
#line 474 "../../src/codegen.in.c"
}
static void f64i64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 449);
#line 477 "../../src/codegen.in.c"
}
static void f64u64(void) {
  //| cvttsd2si rax, xmm0
  dasm_put(Dst, 449);
#line 480 "../../src/codegen.in.c"
}
static void f64f32(void) {
  //| cvtsd2ss xmm0, xmm0
  dasm_put(Dst, 456);
#line 483 "../../src/codegen.in.c"
}
static void f64f80(void) {
  //| movsd qword [rsp-8], xmm0
  //| fld qword [rsp-8]
  dasm_put(Dst, 462);
#line 487 "../../src/codegen.in.c"
}

static void from_f80_1(void) {
  //| fnstcw word [rsp-10]
  //| movzx eax, word [rsp-10]
  //| or ah, 12
  //| mov [rsp-12], ax
  //| fldcw word [rsp-12]
  dasm_put(Dst, 476);
#line 495 "../../src/codegen.in.c"
}

#define FROM_F80_2 " [rsp-24]\n fldcw [rsp-10]\n "

static void f80i8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 502);
#line 504 "../../src/codegen.in.c"
}
static void f80u8(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  //| and eax, 0xff
  dasm_put(Dst, 517);
#line 511 "../../src/codegen.in.c"
}
static void f80i16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movsx eax, word [rsp-24]
  dasm_put(Dst, 502);
#line 517 "../../src/codegen.in.c"
}
static void f80u16(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| movzx eax, word [rsp-24]
  dasm_put(Dst, 538);
#line 523 "../../src/codegen.in.c"
}
static void f80i32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 553);
#line 529 "../../src/codegen.in.c"
}
static void f80u32(void) {
  from_f80_1();
  //| fistp dword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov eax, [rsp-24]
  dasm_put(Dst, 553);
#line 535 "../../src/codegen.in.c"
}
static void f80i64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 567);
#line 541 "../../src/codegen.in.c"
}
static void f80u64(void) {
  from_f80_1();
  //| fistp qword [rsp-24]
  //| fldcw word [rsp-10]
  //| mov rax, [rsp-24]
  dasm_put(Dst, 567);
#line 547 "../../src/codegen.in.c"
}
static void f80f32(void) {
  //| fstp dword [rsp-8]
  //| movss xmm0, dword [rsp-8]
  dasm_put(Dst, 582);
#line 551 "../../src/codegen.in.c"
}
static void f80f64(void) {
  //| fstp qword [rsp-8]
  //| movsd xmm0, qword [rsp-8]
  dasm_put(Dst, 596);
#line 555 "../../src/codegen.in.c"
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
    dasm_put(Dst, 610);
#line 591 "../../src/codegen.in.c"
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
  dasm_put(Dst, 617, sz);
#line 646 "../../src/codegen.in.c"
  C(depth) += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    //| mov r10b, [rax+i]
    //| mov [rsp+i], r10b
    dasm_put(Dst, 623, i, i);
#line 651 "../../src/codegen.in.c"
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
        dasm_put(Dst, 634, -args->pass_by_reference);
#line 694 "../../src/codegen.in.c"
      } else {
        //| mov rax, [rax]
        dasm_put(Dst, 68);
#line 696 "../../src/codegen.in.c"
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
    dasm_put(Dst, 639);
#line 771 "../../src/codegen.in.c"
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

  if ((C(depth) + stack) % 2 == 1) {
    //| sub rsp, 8
    dasm_put(Dst, 645);
#line 820 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_win(node->args, true);
  push_args2_win(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && !type_passed_in_register(node->ty)) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 112, node->ret_buffer->offset);
#line 831 "../../src/codegen.in.c"
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
      dasm_put(Dst, 651);
#line 863 "../../src/codegen.in.c"
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
    dasm_put(Dst, 645);
#line 946 "../../src/codegen.in.c"
    C(depth)++;
    stack++;
  }

  push_args2_sysv(node->args, true);
  push_args2_sysv(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    //| lea rax, [rbp+node->ret_buffer->offset]
    dasm_put(Dst, 112, node->ret_buffer->offset);
#line 957 "../../src/codegen.in.c"
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
      dasm_put(Dst, 660, var->offset);
#line 971 "../../src/codegen.in.c"
    } else {
      //| movsd qword [rbp+var->offset], xmm0
      dasm_put(Dst, 667, var->offset);
#line 973 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      //| mov [rbp+var->offset+i], al
      //| shr rax, 8
      dasm_put(Dst, 674, var->offset+i);
#line 979 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12) {
        //| movss dword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 682, (fp), var->offset+8);
#line 988 "../../src/codegen.in.c"
      } else {
        //| movsd qword [rbp+var->offset+8], xmm(fp)
        dasm_put(Dst, 693, (fp), var->offset+8);
#line 990 "../../src/codegen.in.c"
      }
    } else {
      for (int i = 8; i < MIN(16, ty->size); i++) {
        //| mov [rbp+var->offset+i], Rb(gp)
        //| shr Rq(gp), 8
        dasm_put(Dst, 704, (gp), var->offset+i, (gp));
#line 995 "../../src/codegen.in.c"
      }
    }
  }
}

#endif

static void copy_struct_reg(void) {
#if X64WIN
  // TODO: I'm not sure if this is right/sufficient.
  //| mov rax, [rax]
  dasm_put(Dst, 68);
#line 1006 "../../src/codegen.in.c"
#else
  Type* ty = C(current_fn)->ty->return_ty;

  int gp = 0, fp = 0;

  //| mov RUTIL, rax
  dasm_put(Dst, 718);
#line 1012 "../../src/codegen.in.c"

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4) {
      //| movss xmm0, dword [RUTIL]
      dasm_put(Dst, 722);
#line 1017 "../../src/codegen.in.c"
    } else {
      //| movsd xmm0, qword [RUTIL]
      dasm_put(Dst, 728);
#line 1019 "../../src/codegen.in.c"
    }
    fp++;
  } else {
    //| mov rax, 0
    dasm_put(Dst, 734);
#line 1023 "../../src/codegen.in.c"
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      //| shl rax, 8
      //| mov ax, [RUTIL+i]
      dasm_put(Dst, 742, i);
#line 1026 "../../src/codegen.in.c"
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4) {
        //| movss xmm(fp), dword [RUTIL+8]
        dasm_put(Dst, 751, (fp));
#line 1035 "../../src/codegen.in.c"
      } else {
        //| movsd xmm(fp), qword [RUTIL+8]
        dasm_put(Dst, 761, (fp));
#line 1037 "../../src/codegen.in.c"
      }
    } else {
      //| mov Rq(gp), 0
      dasm_put(Dst, 771, (gp));
#line 1040 "../../src/codegen.in.c"
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        //| shl Rq(gp), 8
        //| mov Rb(gp), [RUTIL+i]
        dasm_put(Dst, 781, (gp), (gp), i);
#line 1043 "../../src/codegen.in.c"
      }
    }
  }
#endif
}

static void copy_struct_mem(void) {
  Type* ty = C(current_fn)->ty->return_ty;
  Obj* var = C(current_fn)->params;

  //| mov RUTIL, [rbp+var->offset]
  dasm_put(Dst, 795, var->offset);
#line 1054 "../../src/codegen.in.c"

  for (int i = 0; i < ty->size; i++) {
    //| mov dl, [rax+i]
    //| mov [RUTIL+i], dl
    dasm_put(Dst, 800, i, i);
#line 1058 "../../src/codegen.in.c"
  }
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  //| add CARG1, 15
  //| and CARG1d, 0xfffffff0
  dasm_put(Dst, 807);
#line 1065 "../../src/codegen.in.c"

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
  dasm_put(Dst, 816, C(current_fn)->alloca_bottom->offset);
#line 1082 "../../src/codegen.in.c"

  // Move alloca_bottom pointer.
  //| mov rax, [rbp+C(current_fn)->alloca_bottom->offset]
  //| sub rax, CARG1
  //| mov [rbp+C(current_fn)->alloca_bottom->offset], rax
  dasm_put(Dst, 869, C(current_fn)->alloca_bottom->offset, C(current_fn)->alloca_bottom->offset);
#line 1087 "../../src/codegen.in.c"
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
#line 1103 "../../src/codegen.in.c"
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
#line 1112 "../../src/codegen.in.c"
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
#line 1127 "../../src/codegen.in.c"
          return;
        }
#endif
      }

      if (node->val < INT_MIN || node->val > INT_MAX) {
        //| mov64 rax, node->val
        dasm_put(Dst, 123, (unsigned int)(node->val), (unsigned int)((node->val)>>32));
#line 1134 "../../src/codegen.in.c"
      } else {
        //| mov rax, node->val
        dasm_put(Dst, 926, node->val);
#line 1136 "../../src/codegen.in.c"
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
#line 1148 "../../src/codegen.in.c"
          return;
        case TY_DOUBLE:
          //| mov rax, 1
          //| shl rax, 63
          //| movd xmm1, rax
          //| xorpd xmm0, xmm1
          dasm_put(Dst, 951);
#line 1154 "../../src/codegen.in.c"
          return;
#if !X64WIN
        case TY_LDOUBLE:
          //| fchs
          dasm_put(Dst, 972);
#line 1158 "../../src/codegen.in.c"
          return;
#endif
      }

      //| neg rax
      dasm_put(Dst, 975);
#line 1163 "../../src/codegen.in.c"
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
#line 1175 "../../src/codegen.in.c"
        if (mem->ty->is_unsigned) {
          //| shr rax, 64 - mem->bit_width
          dasm_put(Dst, 985, 64 - mem->bit_width);
#line 1177 "../../src/codegen.in.c"
        } else {
          //| sar rax, 64 - mem->bit_width
          dasm_put(Dst, 990, 64 - mem->bit_width);
#line 1179 "../../src/codegen.in.c"
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
      gen_addr(node->lhs);
      push();
      gen_expr(node->rhs);

      if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
        //| mov r8, rax
        dasm_put(Dst, 996);
#line 1197 "../../src/codegen.in.c"

        // If the lhs is a bitfield, we need to read the current value
        // from memory and merge it with a new value.
        Member* mem = node->lhs->member;
        //| mov RUTIL, rax
        //| and RUTIL, (1L << mem->bit_width) - 1
        //| shl RUTIL, mem->bit_offset
        dasm_put(Dst, 1000, (1L << mem->bit_width) - 1, mem->bit_offset);
#line 1204 "../../src/codegen.in.c"

        //| mov rax, [rsp]
        dasm_put(Dst, 1012);
#line 1206 "../../src/codegen.in.c"
        load(mem->ty);

        long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
        //| mov r9, ~mask
        //| and rax, r9
        //| or rax, RUTIL
        dasm_put(Dst, 1017, ~mask);
#line 1212 "../../src/codegen.in.c"
        store(node->ty);
        //| mov rax, r8
        dasm_put(Dst, 1029);
#line 1214 "../../src/codegen.in.c"
        return;
      }

      store(node->ty);
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
      dasm_put(Dst, 1033);
#line 1235 "../../src/codegen.in.c"
#endif
      //| mov rcx, node->var->ty->size
      //| lea rdi, [rbp+node->var->offset]
      //| mov al, 0
      //| rep
      //| stosb
      dasm_put(Dst, 1035, node->var->ty->size, node->var->offset);
#line 1241 "../../src/codegen.in.c"
#if X64WIN
      //| pop rdi
      dasm_put(Dst, 1049);
#line 1243 "../../src/codegen.in.c"
#endif
      return;
    case ND_COND: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1051, lelse);
#line 1251 "../../src/codegen.in.c"
      gen_expr(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1055, lend, lelse);
#line 1254 "../../src/codegen.in.c"
      gen_expr(node->els);
      //|=>lend:
      dasm_put(Dst, 1058, lend);
#line 1256 "../../src/codegen.in.c"
      return;
    }
    case ND_NOT:
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| sete al
      //| movzx rax, al
      dasm_put(Dst, 1060);
#line 1263 "../../src/codegen.in.c"
      return;
    case ND_BITNOT:
      gen_expr(node->lhs);
      //| not rax
      dasm_put(Dst, 1068);
#line 1267 "../../src/codegen.in.c"
      return;
    case ND_LOGAND: {
      int lfalse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| je =>lfalse
      dasm_put(Dst, 1051, lfalse);
#line 1274 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| je =>lfalse
      //| mov rax, 1
      //| jmp =>lend
      //|=>lfalse:
      //| mov rax, 0
      //|=>lend:
      dasm_put(Dst, 1073, lfalse, lend, lfalse, lend);
#line 1282 "../../src/codegen.in.c"
      return;
    }
    case ND_LOGOR: {
      int ltrue = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      //| jne =>ltrue
      dasm_put(Dst, 1096, ltrue);
#line 1290 "../../src/codegen.in.c"
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      //| jne =>ltrue
      //| mov rax, 0
      //| jmp =>lend
      //|=>ltrue:
      //| mov rax, 1
      //|=>lend:
      dasm_put(Dst, 1100, ltrue, lend, ltrue, lend);
#line 1298 "../../src/codegen.in.c"
      return;
    }
    case ND_FUNCALL: {
      if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
        gen_expr(node->args);
        //| mov CARG1, rax
        dasm_put(Dst, 718);
#line 1304 "../../src/codegen.in.c"
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
        dasm_put(Dst, 1123);
#line 1319 "../../src/codegen.in.c"

        // Store one-past the second argument into &ap.
        pop(REG_UTIL);
        //| mov [RUTIL], rax
        dasm_put(Dst, 103);
#line 1323 "../../src/codegen.in.c"
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
              dasm_put(Dst, 1128, (reg), (dasmargreg[reg]));
#line 1358 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1138, PARAMETER_SAVE_SIZE, stack_args*8 + PARAMETER_SAVE_SIZE + by_ref_copies_size);
#line 1372 "../../src/codegen.in.c"
      if (by_ref_copies_size > 0) {
        //| pop r11
        dasm_put(Dst, 1155);
#line 1374 "../../src/codegen.in.c"
      }

      C(depth) -= by_ref_copies_size / 8;
      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 172);
#line 1385 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 172);
#line 1389 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 168);
#line 1391 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 180);
#line 1396 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 176);
#line 1398 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned it's actually
      // returned in rax, so copy it back into the return buffer where we're
      // expecting it.
      if (node->ret_buffer && type_passed_in_register(node->ty)) {
        //| mov [rbp+node->ret_buffer->offset], rax
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 1158, node->ret_buffer->offset, node->ret_buffer->offset);
#line 1408 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1167, fp, stack_args*8);
#line 1469 "../../src/codegen.in.c"

      C(depth) -= stack_args;

      // It looks like the most significant 48 or 56 bits in RAX may
      // contain garbage if a function return type is short or bool/char,
      // respectively. We clear the upper bits here.
      switch (node->ty->kind) {
        case TY_BOOL:
          //| movzx eax, al
          dasm_put(Dst, 172);
#line 1478 "../../src/codegen.in.c"
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            //| movzx eax, al
            dasm_put(Dst, 172);
#line 1482 "../../src/codegen.in.c"
          } else {
            //| movsx eax, al
            dasm_put(Dst, 168);
#line 1484 "../../src/codegen.in.c"
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            //| movzx eax, ax
            dasm_put(Dst, 180);
#line 1489 "../../src/codegen.in.c"
          } else {
            //| movsx eax, ax
            dasm_put(Dst, 176);
#line 1491 "../../src/codegen.in.c"
          }
          return;
      }

      // If the return type is a small struct, a value is returned
      // using up to two registers.
      if (node->ret_buffer && node->ty->size <= 16) {
        copy_ret_buffer(node->ret_buffer);
        //| lea rax, [rbp+node->ret_buffer->offset]
        dasm_put(Dst, 112, node->ret_buffer->offset);
#line 1500 "../../src/codegen.in.c"
      }

#endif  // SysV

      return;
    }
    case ND_LABEL_VAL:
      //| lea rax, [=>node->pc_label]
      dasm_put(Dst, 117, node->pc_label);
#line 1508 "../../src/codegen.in.c"
      return;
    case ND_REFLECT_TYPE_PTR:
      //| mov64 rax, node->rty;
      dasm_put(Dst, 123, (unsigned int)(node->rty), (unsigned int)((node->rty)>>32));
#line 1511 "../../src/codegen.in.c"
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
        dasm_put(Dst, 996);
#line 1523 "../../src/codegen.in.c"
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
          dasm_put(Dst, 1183);
#line 1540 "../../src/codegen.in.c"
          break;
        case 2:
          // lock cmpxchg WORD PTR [rdi/rcx],dx
          //| .byte 0x66
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1189);
#line 1548 "../../src/codegen.in.c"
          break;
        case 4:
          // lock cmpxchg DWORD PTR [rdi/rcx],edx
          //| .byte 0xf0
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1190);
#line 1555 "../../src/codegen.in.c"
          break;
        case 8:
          // lock cmpxchg QWORD PTR [rdi/rcx],rdx
          //| .byte 0xf0
          //| .byte 0x48
          //| .byte 0x0f
          //| .byte 0xb1
          //| .byte RUTILenc
          dasm_put(Dst, 1196);
#line 1563 "../../src/codegen.in.c"
          break;
        default:
          unreachable();
      }
      if (!is_locked_ce) {
        //| sete cl
        //| je >1
        dasm_put(Dst, 1203);
#line 1570 "../../src/codegen.in.c"
        switch (sz) {
          case 1:
            //| mov [r8], al
            dasm_put(Dst, 1211);
#line 1573 "../../src/codegen.in.c"
            break;
          case 2:
            //| mov [r8], ax
            dasm_put(Dst, 1215);
#line 1576 "../../src/codegen.in.c"
            break;
          case 4:
            //| mov [r8], eax
            dasm_put(Dst, 1216);
#line 1579 "../../src/codegen.in.c"
            break;
          case 8:
            //| mov [r8], rax
            dasm_put(Dst, 1220);
#line 1582 "../../src/codegen.in.c"
            break;
          default:
            unreachable();
        }
        //|1:
        //| movzx eax, cl
        dasm_put(Dst, 1224);
#line 1588 "../../src/codegen.in.c"
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
          dasm_put(Dst, 1230);
#line 1602 "../../src/codegen.in.c"
          break;
        case 2:
          //| xchg [RUTIL], ax
          dasm_put(Dst, 1233);
#line 1605 "../../src/codegen.in.c"
          break;
        case 4:
          //| xchg [RUTIL], eax
          dasm_put(Dst, 1234);
#line 1608 "../../src/codegen.in.c"
          break;
        case 8:
          //| xchg [RUTIL], rax
          dasm_put(Dst, 1237);
#line 1611 "../../src/codegen.in.c"
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
            dasm_put(Dst, 1241);
#line 1633 "../../src/codegen.in.c"
          } else {
            //| addsd xmm0, xmm1
            dasm_put(Dst, 1247);
#line 1635 "../../src/codegen.in.c"
          }
          return;
        case ND_SUB:
          if (is_float) {
            //| subss xmm0, xmm1
            dasm_put(Dst, 1253);
#line 1640 "../../src/codegen.in.c"
          } else {
            //| subsd xmm0, xmm1
            dasm_put(Dst, 1259);
#line 1642 "../../src/codegen.in.c"
          }
          return;
        case ND_MUL:
          if (is_float) {
            //| mulss xmm0, xmm1
            dasm_put(Dst, 1265);
#line 1647 "../../src/codegen.in.c"
          } else {
            //| mulsd xmm0, xmm1
            dasm_put(Dst, 1271);
#line 1649 "../../src/codegen.in.c"
          }
          return;
        case ND_DIV:
          if (is_float) {
            //| divss xmm0, xmm1
            dasm_put(Dst, 1277);
#line 1654 "../../src/codegen.in.c"
          } else {
            //| divsd xmm0, xmm1
            dasm_put(Dst, 1283);
#line 1656 "../../src/codegen.in.c"
          }
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          if (is_float) {
            //| ucomiss xmm1, xmm0
            dasm_put(Dst, 1289);
#line 1664 "../../src/codegen.in.c"
          } else {
            //| ucomisd xmm1, xmm0
            dasm_put(Dst, 1293);
#line 1666 "../../src/codegen.in.c"
          }

          if (node->kind == ND_EQ) {
            //| sete al
            //| setnp dl
            //| and al, dl
            dasm_put(Dst, 1298);
#line 1672 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            //| setp dl
            //| or al, dl
            dasm_put(Dst, 1307);
#line 1676 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1316);
#line 1678 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1320);
#line 1680 "../../src/codegen.in.c"
          }

          //| and al, 1
          //| movzx rax, al
          dasm_put(Dst, 1324);
#line 1684 "../../src/codegen.in.c"
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
          dasm_put(Dst, 1331);
#line 1697 "../../src/codegen.in.c"
          return;
        case ND_SUB:
          //| fsubrp st1, st0
          dasm_put(Dst, 1334);
#line 1700 "../../src/codegen.in.c"
          return;
        case ND_MUL:
          //| fmulp st1, st0
          dasm_put(Dst, 1337);
#line 1703 "../../src/codegen.in.c"
          return;
        case ND_DIV:
          //| fdivrp st1, st0
          dasm_put(Dst, 1340);
#line 1706 "../../src/codegen.in.c"
          return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
          //| fcomip st1
          //| fstp st0
          dasm_put(Dst, 1344);
#line 1713 "../../src/codegen.in.c"

          if (node->kind == ND_EQ) {
            //| sete al
            dasm_put(Dst, 1350);
#line 1716 "../../src/codegen.in.c"
          } else if (node->kind == ND_NE) {
            //| setne al
            dasm_put(Dst, 1354);
#line 1718 "../../src/codegen.in.c"
          } else if (node->kind == ND_LT) {
            //| seta al
            dasm_put(Dst, 1316);
#line 1720 "../../src/codegen.in.c"
          } else {
            //| setae al
            dasm_put(Dst, 1320);
#line 1722 "../../src/codegen.in.c"
          }

          //| movzx rax, al
          dasm_put(Dst, 1063);
#line 1725 "../../src/codegen.in.c"
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
        dasm_put(Dst, 1358);
#line 1744 "../../src/codegen.in.c"
      } else {
        //| add eax, RUTILd
        dasm_put(Dst, 1359);
#line 1746 "../../src/codegen.in.c"
      }
      return;
    case ND_SUB:
      if (is_long) {
        //| sub rax, RUTIL
        dasm_put(Dst, 1363);
#line 1751 "../../src/codegen.in.c"
      } else {
        //| sub eax, RUTILd
        dasm_put(Dst, 1364);
#line 1753 "../../src/codegen.in.c"
      }
      return;
    case ND_MUL:
      if (is_long) {
        //| imul rax, RUTIL
        dasm_put(Dst, 1368);
#line 1758 "../../src/codegen.in.c"
      } else {
        //| imul eax, RUTILd
        dasm_put(Dst, 1369);
#line 1760 "../../src/codegen.in.c"
      }
      return;
    case ND_DIV:
    case ND_MOD:
      if (node->ty->is_unsigned) {
        if (is_long) {
          //| mov rdx, 0
          //| div RUTIL
          dasm_put(Dst, 1373);
#line 1768 "../../src/codegen.in.c"
        } else {
          //| mov edx, 0
          //| div RUTILd
          dasm_put(Dst, 1386);
#line 1771 "../../src/codegen.in.c"
        }
      } else {
        if (node->lhs->ty->size == 8) {
          //| cqo
          dasm_put(Dst, 1396);
#line 1775 "../../src/codegen.in.c"
        } else {
          //| cdq
          dasm_put(Dst, 1397);
#line 1777 "../../src/codegen.in.c"
        }
        if (is_long) {
          //| idiv RUTIL
          dasm_put(Dst, 1399);
#line 1780 "../../src/codegen.in.c"
        } else {
          //| idiv RUTILd
          dasm_put(Dst, 1400);
#line 1782 "../../src/codegen.in.c"
        }
      }

      if (node->kind == ND_MOD) {
        //| mov rax, rdx
        dasm_put(Dst, 1405);
#line 1787 "../../src/codegen.in.c"
      }
      return;
    case ND_BITAND:
      if (is_long) {
        //| and rax, RUTIL
        dasm_put(Dst, 1409);
#line 1792 "../../src/codegen.in.c"
      } else {
        //| and eax, RUTILd
        dasm_put(Dst, 1410);
#line 1794 "../../src/codegen.in.c"
      }
      return;
    case ND_BITOR:
      if (is_long) {
        //| or rax, RUTIL
        dasm_put(Dst, 1024);
#line 1799 "../../src/codegen.in.c"
      } else {
        //| or eax, RUTILd
        dasm_put(Dst, 1025);
#line 1801 "../../src/codegen.in.c"
      }
      return;
    case ND_BITXOR:
      if (is_long) {
        //| xor rax, RUTIL
        dasm_put(Dst, 1414);
#line 1806 "../../src/codegen.in.c"
      } else {
        //| xor eax, RUTILd
        dasm_put(Dst, 1415);
#line 1808 "../../src/codegen.in.c"
      }
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      if (is_long) {
        //| cmp rax, RUTIL
        dasm_put(Dst, 1419);
#line 1816 "../../src/codegen.in.c"
      } else {
        //| cmp eax, RUTILd
        dasm_put(Dst, 1420);
#line 1818 "../../src/codegen.in.c"
      }

      if (node->kind == ND_EQ) {
        //| sete al
        dasm_put(Dst, 1350);
#line 1822 "../../src/codegen.in.c"
      } else if (node->kind == ND_NE) {
        //| setne al
        dasm_put(Dst, 1354);
#line 1824 "../../src/codegen.in.c"
      } else if (node->kind == ND_LT) {
        if (node->lhs->ty->is_unsigned) {
          //| setb al
          dasm_put(Dst, 1424);
#line 1827 "../../src/codegen.in.c"
        } else {
          //| setl al
          dasm_put(Dst, 1428);
#line 1829 "../../src/codegen.in.c"
        }
      } else if (node->kind == ND_LE) {
        if (node->lhs->ty->is_unsigned) {
          //| setbe al
          dasm_put(Dst, 1432);
#line 1833 "../../src/codegen.in.c"
        } else {
          //| setle al
          dasm_put(Dst, 1436);
#line 1835 "../../src/codegen.in.c"
        }
      }

      //| movzx rax, al
      dasm_put(Dst, 1063);
#line 1839 "../../src/codegen.in.c"
      return;
    case ND_SHL:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1440);
#line 1842 "../../src/codegen.in.c"
      if (is_long) {
        //| shl rax, cl
        dasm_put(Dst, 1445);
#line 1844 "../../src/codegen.in.c"
      } else {
        //| shl eax, cl
        dasm_put(Dst, 1446);
#line 1846 "../../src/codegen.in.c"
      }
      return;
    case ND_SHR:
      //| mov rcx, RUTIL
      dasm_put(Dst, 1440);
#line 1850 "../../src/codegen.in.c"
      if (node->lhs->ty->is_unsigned) {
        if (is_long) {
          //| shr rax, cl
          dasm_put(Dst, 1449);
#line 1853 "../../src/codegen.in.c"
        } else {
          //| shr eax, cl
          dasm_put(Dst, 1450);
#line 1855 "../../src/codegen.in.c"
        }
      } else {
        if (is_long) {
          //| sar rax, cl
          dasm_put(Dst, 1453);
#line 1859 "../../src/codegen.in.c"
        } else {
          //| sar eax, cl
          dasm_put(Dst, 1454);
#line 1861 "../../src/codegen.in.c"
        }
      }
      return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node* node) {
  switch (node->kind) {
    case ND_IF: {
      int lelse = codegen_pclabel();
      int lend = codegen_pclabel();
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| je =>lelse
      dasm_put(Dst, 1051, lelse);
#line 1877 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //| jmp =>lend
      //|=>lelse:
      dasm_put(Dst, 1055, lend, lelse);
#line 1880 "../../src/codegen.in.c"
      if (node->els)
        gen_stmt(node->els);
      //|=>lend:
      dasm_put(Dst, 1058, lend);
#line 1883 "../../src/codegen.in.c"
      return;
    }
    case ND_FOR: {
      if (node->init)
        gen_stmt(node->init);
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 1058, lbegin);
#line 1890 "../../src/codegen.in.c"
      if (node->cond) {
        gen_expr(node->cond);
        cmp_zero(node->cond->ty);
        //| je =>node->brk_pc_label
        dasm_put(Dst, 1051, node->brk_pc_label);
#line 1894 "../../src/codegen.in.c"
      }
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 1058, node->cont_pc_label);
#line 1897 "../../src/codegen.in.c"
      if (node->inc)
        gen_expr(node->inc);
      //| jmp =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1055, lbegin, node->brk_pc_label);
#line 1901 "../../src/codegen.in.c"
      return;
    }
    case ND_DO: {
      int lbegin = codegen_pclabel();
      //|=>lbegin:
      dasm_put(Dst, 1058, lbegin);
#line 1906 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->cont_pc_label:
      dasm_put(Dst, 1058, node->cont_pc_label);
#line 1908 "../../src/codegen.in.c"
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      //| jne =>lbegin
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1458, lbegin, node->brk_pc_label);
#line 1912 "../../src/codegen.in.c"
      return;
    }
    case ND_SWITCH:
      gen_expr(node->cond);

      for (Node* n = node->case_next; n; n = n->case_next) {
        bool is_long = node->cond->ty->size == 8;

        if (n->begin == n->end) {
          if (is_long) {
            //| cmp rax, n->begin
            dasm_put(Dst, 1463, n->begin);
#line 1923 "../../src/codegen.in.c"
          } else {
            //| cmp eax, n->begin
            dasm_put(Dst, 1464, n->begin);
#line 1925 "../../src/codegen.in.c"
          }
          //| je =>n->pc_label
          dasm_put(Dst, 1051, n->pc_label);
#line 1927 "../../src/codegen.in.c"
          continue;
        }

        // [GNU] Case ranges
        if (is_long) {
          //| mov RUTIL, rax
          //| sub RUTIL, n->begin
          //| cmp RUTIL, n->end - n->begin
          dasm_put(Dst, 1469, n->begin, n->end - n->begin);
#line 1935 "../../src/codegen.in.c"
        } else {
          //| mov RUTILd, eax
          //| sub RUTILd, n->begin
          //| cmp RUTILd, n->end - n->begin
          dasm_put(Dst, 1483, n->begin, n->end - n->begin);
#line 1939 "../../src/codegen.in.c"
        }
        //| jbe =>n->pc_label
        dasm_put(Dst, 1494, n->pc_label);
#line 1941 "../../src/codegen.in.c"
      }

      if (node->default_case) {
        //| jmp =>node->default_case->pc_label
        dasm_put(Dst, 1498, node->default_case->pc_label);
#line 1945 "../../src/codegen.in.c"
      }

      //| jmp =>node->brk_pc_label
      dasm_put(Dst, 1498, node->brk_pc_label);
#line 1948 "../../src/codegen.in.c"
      gen_stmt(node->then);
      //|=>node->brk_pc_label:
      dasm_put(Dst, 1058, node->brk_pc_label);
#line 1950 "../../src/codegen.in.c"
      return;
    case ND_CASE:
      //|=>node->pc_label:
      dasm_put(Dst, 1058, node->pc_label);
#line 1953 "../../src/codegen.in.c"
      gen_stmt(node->lhs);
      return;
    case ND_BLOCK:
      for (Node* n = node->body; n; n = n->next)
        gen_stmt(n);
      return;
    case ND_GOTO:
      //| jmp =>node->pc_label
      dasm_put(Dst, 1498, node->pc_label);
#line 1961 "../../src/codegen.in.c"
      return;
    case ND_GOTO_EXPR:
      gen_expr(node->lhs);
      //| jmp rax
      dasm_put(Dst, 1502);
#line 1965 "../../src/codegen.in.c"
      return;
    case ND_LABEL:
      //|=>node->pc_label:
      dasm_put(Dst, 1058, node->pc_label);
#line 1968 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1498, C(current_fn)->dasm_return_label);
#line 1994 "../../src/codegen.in.c"
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
    int bottom = 0;

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
    //
    // LinkFixup won't work as is
    // - offset is codeseg relative. it can be made a pointer for the
    // codeseg imports instead because it's recreated for each compile
    // anyway
    // - need to remap initializer_code_relocation to name, but...
    // actually we have the real address at this point now, so if the
    // data was allocated it can just be written directly i think rather
    // than deferred to a relocation

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
      dasm_put(Dst, 682, (r), offset);
#line 2316 "../../src/codegen.in.c"
      return;
    case 8:
      //| movsd qword [rbp+offset], xmm(r)
      dasm_put(Dst, 693, (r), offset);
#line 2319 "../../src/codegen.in.c"
      return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
    case 1:
      //| mov [rbp+offset], Rb(dasmargreg[r])
      dasm_put(Dst, 1506, (dasmargreg[r]), offset);
#line 2328 "../../src/codegen.in.c"
      return;
    case 2:
      //| mov [rbp+offset], Rw(dasmargreg[r])
      dasm_put(Dst, 1514, (dasmargreg[r]), offset);
#line 2331 "../../src/codegen.in.c"
      return;
      return;
    case 4:
      //| mov [rbp+offset], Rd(dasmargreg[r])
      dasm_put(Dst, 1515, (dasmargreg[r]), offset);
#line 2335 "../../src/codegen.in.c"
      return;
    case 8:
      //| mov [rbp+offset], Rq(dasmargreg[r])
      dasm_put(Dst, 1523, (dasmargreg[r]), offset);
#line 2338 "../../src/codegen.in.c"
      return;
    default:
      for (int i = 0; i < sz; i++) {
        //| mov [rbp+offset+i], Rb(dasmargreg[r])
        //| shr Rq(dasmargreg[r]), 8
        dasm_put(Dst, 704, (dasmargreg[r]), offset+i, (dasmargreg[r]));
#line 2343 "../../src/codegen.in.c"
      }
      return;
  }
}

#if X64WIN
extern int __chkstk(void);
#endif

static void emit_text(Obj* prog) {
  // Preallocate the dasm labels so they can be used in functions out of order.
  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    fn->dasm_return_label = codegen_pclabel();
    fn->dasm_entry_label = codegen_pclabel();
  }

  for (Obj* fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition || !fn->is_live)
      continue;

    //|=>fn->dasm_entry_label:
    dasm_put(Dst, 1058, fn->dasm_entry_label);
#line 2367 "../../src/codegen.in.c"

    C(current_fn) = fn;

    // outaf("---- %s\n", fn->name);

    // Prologue
    //| push rbp
    //| mov rbp, rsp
    dasm_put(Dst, 1531);
#line 2375 "../../src/codegen.in.c"

#if X64WIN
    // Stack probe on Windows if necessary. The MSDN reference for __chkstk says
    // it's only necessary beyond 8k for x64, but cl does it at 4k.
    if (fn->stack_size >= 4096) {
      //| mov rax, fn->stack_size
      dasm_put(Dst, 926, fn->stack_size);
#line 2381 "../../src/codegen.in.c"
      int fixup_location = codegen_pclabel();
      strintarray_push(&C(fixups), (StringInt){"__chkstk", fixup_location}, AL_Compile);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4310)  // dynasm casts the top and bottom of the 64bit arg
#endif
      //|=>fixup_location:
      //| mov64 r10, 0xc0dec0dec0dec0de
      dasm_put(Dst, 1536, fixup_location, (unsigned int)(0xc0dec0dec0dec0de), (unsigned int)((0xc0dec0dec0dec0de)>>32));
#line 2389 "../../src/codegen.in.c"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      //| call r10
      //| sub rsp, rax
      dasm_put(Dst, 1542);
#line 2394 "../../src/codegen.in.c"
    } else
#endif

    {
      //| sub rsp, fn->stack_size
      dasm_put(Dst, 617, fn->stack_size);
#line 2399 "../../src/codegen.in.c"
    }
    //| mov [rbp+fn->alloca_bottom->offset], rsp
    dasm_put(Dst, 1550, fn->alloca_bottom->offset);
#line 2401 "../../src/codegen.in.c"

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
      dasm_put(Dst, 1555, off, gp*8, off+4, fp * 8 + 48, off+8, off+8, off+16, off+16, off+24);
#line 2422 "../../src/codegen.in.c"

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
      dasm_put(Dst, 1582, off + 24, off + 32, off + 40, off + 48, off + 56, off + 64, off + 72, off + 80, off + 88, off + 96, off + 104, off + 112, off + 120, off + 128);
#line 2438 "../../src/codegen.in.c"
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
      dasm_put(Dst, 1655, 16, 24, 32, 40);
#line 2449 "../../src/codegen.in.c"
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
            store_gp(reg++, var->offset, MIN(8, ty->size));
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
      dasm_put(Dst, 734);
#line 2520 "../../src/codegen.in.c"
    }

    // Epilogue
    //|=>fn->dasm_return_label:
    //| mov rsp, rbp
    //| pop rbp
    //| ret
    dasm_put(Dst, 1672, fn->dasm_return_label);
#line 2527 "../../src/codegen.in.c"
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

  assign_lvar_offsets(prog);
  emit_text(prog);

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
  fld->codeseg_base_address = allocate_writable_memory(page_sized);
  // outaf("code_size: %zu, page_sized: %zu\n", code_size, page_sized);

  fill_out_text_exports(prog, fld->codeseg_base_address);

  free_link_fixups(fld);
  emit_data(prog);  // This needs to point into code for fixups, so has to go late-ish.
  fill_out_fixups(fld);

  dasm_encode(&C(dynasm), fld->codeseg_base_address);

  int check_result = dasm_checkstep(&C(dynasm), DASM_SECTION_MAIN);
  if (check_result != DASM_S_OK) {
    outaf("check_result: 0x%08x\n", check_result);
    ABORT("dasm_checkstep failed");
  }

  codegen_free();
}

// This can be called after a longjmp in update.
static void codegen_free(void) {
  if (C(dynasm)) {
    dasm_free(&C(dynasm));
  }
}
//
// END OF out/lr/codegen.l.c
//
#endif // !X64WIN
