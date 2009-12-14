#ifndef _RUNTIME_H
#define _RUNTIME_H

/* Symbol Types */
#define SYM_ANY 0L
#define SYM_WORLD 1L
#define SYM_OPERATOR 2L
#define SYM_INT 3L
#define SYM_IDENT 4L
#define SYM_BOOL 5L
#define SYM_REACTION 6L
#define SYM_REDEX 7L
#define SYM_REACTUM 8L
/* Types < 32 are reserved.  > 32 are free for custom use */

/* Kinds */
#define KIND_NODE 0L
#define KIND_COMPUTATION 1L
#define KIND_VALUE 2L
#define KIND_REACTION 3L
#define KIND_REDEX 4L
#define KIND_REACTUM 5L
/* Kinds < 32.  > 32 are free for custom use */

/* "hard" operators */
#define OPER_PLUS 1L
#define OPER_SUB  2L
#define OPER_MULT 3L
#define OPER_DIV  4L

#define TRUE 1
#define FALSE 0

/* File magic */
#define FILE_MAGIC 44442266

typedef long identifier_t;
typedef long operator_t;
typedef long symbol_type_t;
typedef long kind_t;

typedef struct {
	union symbol_data_t { 
		operator_t sym_operator;
		int sym_int;
	} data;
	symbol_type_t type;
	kind_t kind;	/* Only used in matching, not kind assignment */
} symbol_t;

typedef struct node_t {
	identifier_t node_id;
	symbol_t symbol;				// this will be extended to be an operator or name
	size_t num_children;
	struct node_t *match;
	struct node_t *children[0];
} node_t;

typedef struct reaction_t {
	const char *name;
	node_t *redex;
	node_t *reactum;
	node_t *scope;
	long count; /* for statistical purposes */
	struct reaction_t *next;
} reaction_t;

#endif
