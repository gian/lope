/**************************************************************************
* Lope Programming Language
* Copyright (c) 2009 Gian Perrone
* Runtime System
***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "runtime.h"

/* Hackety hackety hack */
node_t *idtable[2048] = {NULL};
kind_t *kindtable[2048] = {NULL};
int kindorder[2048] = {0};

long g_nid = 1;

identifier_t nodeid( void ) {
	return g_nid++;
}

int is_kind(symbol_type_t t, kind_t k)
{
	int i;
	for(i=0;i<kindorder[t];i++) {
		if(kindtable[t][i] == k) return TRUE;
	}

	return FALSE;
}

int symbol_compare(symbol_t s1, symbol_t s2)
{
	if(s1.type == SYM_OPERATOR && 
		s1.type == s2.type &&
		s1.data.sym_operator != s2.data.sym_operator)
		return FALSE;

	if(s1.type == SYM_INT && 
		s1.type == s2.type &&
		s1.data.sym_int != s2.data.sym_int)
		return FALSE;

	/* Kinding rules:
		s1_t	s1_k	s2_t	s2_k	cond
		ANY		NODE	ANY		NODE	K1 = K2 (true)
		ANY		NODE	ANY		K2		K1 = K2 = NODE
		ANY		NODE	t2		NODE	t2 < K1 (true)
		ANY		NODE	t2		K2		t2 < K1 (true)
		ANY		K1		ANY		NODE	K1 = K2 = NODE
		ANY		K1		ANY		K2		K1 = K2
		ANY		K1		t2		NODE	t2 < K1
		ANY		K1		t2		K2		t2 < K1
		t1		NODE	ANY		NODE	t1 < K2 (true)
		t1		NODE	ANY		K2		t1 < K2
		t1		NODE	t2		NODE	t1 = t2, t1 < K2, t2 < K1
		t1		NODE	t2		K2		t1 = t2, t1 < K2
		t1		K1		ANY		NODE	t1 < K2 (true)
		t1		K1		ANY		K2		t1 < K2
		t1		K1		t2		NODE	t1 = t2, t2 < K1
		t1		K1		t2		K2		t1 = t2. t1 < K2, t2 < K1
	
	The ugliest logical statement in the universe:
	*/
	if(s1.type == SYM_ANY) {
		if(s1.kind == KIND_NODE) {
			if(s2.type == SYM_ANY) {
				if(s2.kind == KIND_NODE) {
					return TRUE;
				} else {
					return s1.kind == s2.kind;
				}
			} else {
				return TRUE;
			}
		} else {
			if(s2.type == SYM_ANY) {
				return s1.kind == s2.kind;
			} else {
				return is_kind(s2.type,s1.kind);
			}
		}
	} else {
		if(s1.kind == KIND_NODE) {
			if(s2.type == SYM_ANY) {
				if(s2.kind == KIND_NODE) {
					return TRUE;
				} else {
					return is_kind(s1.type,s2.kind);
				}
			} else {
				if(s2.kind == KIND_NODE) {
					return s1.type == s2.type;
				} else {
					return s1.type == s2.type && is_kind(s1.type,s2.kind);
				}
			}
		} else {
			if(s2.type == SYM_ANY) {
				if(s2.kind == KIND_NODE) {
					return TRUE;
				} else {
					return is_kind(s1.type,s2.kind);
				}
			} else {
				if(s2.kind == KIND_NODE) {
					return s1.type == s2.type && is_kind(s2.type,s1.kind);
				} else {
					return s1.type == s2.type &&
							is_kind(s1.type,s2.kind) &&
							is_kind(s2.type,s1.kind);
				}
			}
		}
	}

	return FALSE;
}

void initialise_kinds()
{
	kindtable[SYM_ANY] = (kind_t *)malloc(sizeof(kind_t)*1);
	kindtable[SYM_ANY][0] = KIND_NODE;
	kindorder[SYM_ANY] = 1;

	kindtable[SYM_WORLD] = (kind_t *)malloc(sizeof(kind_t)*1);
	kindtable[SYM_WORLD][0] = KIND_NODE;
	kindorder[SYM_WORLD] = 1;

	kindtable[SYM_OPERATOR] = (kind_t *)malloc(sizeof(kind_t)*2);
	kindtable[SYM_OPERATOR][0] = KIND_NODE;
	kindtable[SYM_OPERATOR][1] = KIND_COMPUTATION;
	kindorder[SYM_OPERATOR] = 2;

	kindtable[SYM_INT] = (kind_t *)malloc(sizeof(kind_t)*3);
	kindtable[SYM_INT][0] = KIND_NODE;
	kindtable[SYM_INT][1] = KIND_COMPUTATION;
	kindtable[SYM_INT][2] = KIND_VALUE;
	kindorder[SYM_INT] = 3;

	kindtable[SYM_REACTION] = (kind_t *)malloc(sizeof(kind_t)*2);
	kindtable[SYM_REACTION][0] = KIND_NODE;
	kindtable[SYM_REACTION][1] = KIND_REACTION;
	kindorder[SYM_REACTION] = 2;

	kindtable[SYM_REDEX] = (kind_t *)malloc(sizeof(kind_t)*2);
	kindtable[SYM_REDEX][0] = KIND_NODE;
	kindtable[SYM_REDEX][1] = KIND_REDEX;
	kindorder[SYM_REDEX] = 2;

	kindtable[SYM_REACTUM] = (kind_t *)malloc(sizeof(kind_t)*2);
	kindtable[SYM_REACTUM][0] = KIND_NODE;
	kindtable[SYM_REACTUM][1] = KIND_REACTUM;
	kindorder[SYM_REACTUM] = 2;
}

void insert_id(identifier_t id, node_t *node)
{
	assert(id < 2048);

	idtable[id] = node;
}

node_t *lookup_id(identifier_t id)
{
	return idtable[id];
}

node_t *new_node(identifier_t id, symbol_t symbol, size_t num_children) 
{
	node_t *n = (node_t *)malloc(sizeof(node_t) + (num_children * sizeof(node_t *)));

	n->node_id = id;
	n->symbol = symbol;
	n->num_children = num_children;
	n->match = NULL;

	return n;
}

int match_node(node_t *graph, node_t *pattern)
{
	/* populate the "match" field of the pattern */
	
	/* Match the root of the pattern first. */
	if(pattern->symbol.type == SYM_ANY && pattern->num_children == 0) {
		/* the "anything" node with no children matches any subtree */
		pattern->match = graph;
		return TRUE;
	}

	if(pattern->num_children != graph->num_children) return FALSE;

	if(pattern->symbol.type == SYM_ANY) {
		pattern->match = graph;
		return TRUE;
	}

	if(symbol_compare(pattern->symbol, graph->symbol)) {
		pattern->match = graph;
		return TRUE;
	}

	return FALSE;
}

int match_graph(node_t *graph, node_t *pattern) {
	if(!match_node(graph,pattern)) {
		return FALSE;
	}

	int i;

	if(pattern->num_children > graph->num_children) return FALSE;

	for(i=0;i<pattern->num_children;i++) {
		if(!match_graph(graph->children[i],pattern->children[i])) return FALSE;
	}

	return TRUE;
}

node_t *match_graph_anywhere(node_t *graph, node_t *pattern)
{
	if(match_graph(graph, pattern)) return graph;

	int i;
	for(i=0;i<graph->num_children;i++) {
		node_t *m = match_graph_anywhere(graph->children[i],pattern);
		if(m != NULL) return m;
	}

	return NULL;
}

/* Apply a reactum at the root of graph, returning the rewritten graph. */
node_t *apply_reactum(node_t *reactum)
{
	// We need to copy the symbol from the original graph before replacing the node.
	if(reactum->symbol.type == SYM_ANY) {
		reactum->symbol.type = reactum->match->match->symbol.type;
		reactum->symbol.data = reactum->match->match->symbol.data;
	}

	if(reactum->match != NULL && reactum->match->num_children == 0 && reactum->num_children == 0 && reactum->match->match->num_children > 0) {
		printf("APPLY: %ld %ld %ld\n", reactum->match->num_children,
									reactum->num_children,
									reactum->match->match->num_children);

		/* Redex did not match children, so attach them to the new graph. */
		node_t *newreactum = new_node(nodeid(),
									reactum->symbol,
									reactum->match->match->num_children);
		newreactum->match = reactum->match;
		int i;
		for(i=0;i<reactum->match->match->num_children;i++) {
			newreactum->children[i] = reactum->match->match->children[i];
		}

		/*TODO: garbage collect this instead */
		free(reactum);
		reactum = newreactum;

	}
	
	reactum->match = NULL;

	int i = 0;
	for(i=0; i<reactum->num_children; i++) {
		apply_reactum(reactum->children[i]);
	}

	return reactum;
}

node_t *update_references(node_t *graph, node_t *src, node_t *dest)
{
	node_t *newhead = graph;
	if(graph == src) {
		newhead = dest;
	}

	int i;
	for(i=0;i<graph->num_children;i++) {
		graph->children[i] = update_references(graph->children[i],src,dest);
	}

	return newhead;
}

node_t *instantiate_reactum(node_t *reactum) 
{
	node_t *n = new_node(nodeid(),reactum->symbol,reactum->num_children);

	n->match = reactum->match;

	int i;
	for(i=0;i<reactum->num_children;i++) {
		n->children[i] = instantiate_reactum(reactum->children[i]);
	}

	return n;
}

node_t *apply_reaction(node_t *graph, reaction_t *reaction)
{
	node_t *reaction_site = match_graph_anywhere(graph, reaction->redex);

	/* This rule does not match anywhere in the graph. */
	if(reaction_site == NULL) {
		printf("[rule '%s' does not match]\n", reaction->name);
		return NULL;
	}

	printf("[applied rule '%s' at @%ld, rule count: %ld]\n", reaction->name, reaction_site->node_id, reaction->count+1);

	node_t *reactum = instantiate_reactum(reaction->reactum); 

	node_t *subgraph = apply_reactum(reactum);
	graph = update_references(graph,reaction_site,subgraph);

	reaction->count++;

	/*TODO: Add garbage collection to this. */
	return graph;
}

node_t *apply_all_reactions(node_t *graph, reaction_t *head)
{
	while(head != NULL) {
		printf("[attempting to apply '%s']\n", head->name);
		node_t *newgraph = apply_reaction(graph,head);
		if(newgraph != NULL) graph = newgraph;
		head = head->next;
	}

	return graph;
}

char *sym_to_string(symbol_t s) 
{
	char buf[1024];
	char tybuf[512];

	if(s.type == SYM_ANY) sprintf(tybuf,"_");

	if(s.type == SYM_OPERATOR) {
		switch(s.data.sym_operator) {
			case OPER_PLUS:
				sprintf(tybuf,"+");
				break;
			case OPER_SUB:
				sprintf(tybuf,"-");
				break;
			case OPER_MULT:
				sprintf(tybuf,"*");
				break;
			case OPER_DIV:
				sprintf(tybuf,"/");
				break;
			default:
				sprintf(tybuf,"<?>");
		}
	}

	if(s.type == SYM_INT) {
		sprintf(tybuf, "%d", s.data.sym_int);
	}

	if(s.kind != 0) {
		sprintf(buf, "(%s : %ld :: %ld)", tybuf, s.type, s.kind);
	} else {
		sprintf(buf, "(%s : %ld)", tybuf, s.type);
	}

	return strdup(buf);
}

void print_node(node_t *node) {
	printf("@%ld: %s", node->node_id, sym_to_string(node->symbol));
	int i;
	for(i = 0; i<node->num_children; i++) {
		printf(" %ld", node->children[i]->node_id);
	}

	if(node->match != NULL) {
		printf(" (@%ld)", node->match->node_id);
	}

	printf("\n");

	for(i = 0; i<node->num_children; i++) {
		print_node(node->children[i]);
	}
}

int save_node(FILE *fp, node_t *root)
{
	fwrite(&root->node_id, sizeof(long), 1, fp);
	fwrite(&root->num_children, sizeof(size_t), 1, fp);

	if(root->match == NULL) {
		identifier_t m = 0L;
		fwrite(&m, sizeof(identifier_t), 1, fp);
	} else {
		fwrite(&root->match->node_id, sizeof(identifier_t), 1, fp);
	}

	fwrite(&root->symbol, sizeof(symbol_t), 1, fp);

	int i;
	for(i=0;i<root->num_children; i++) {
		save_node(fp,root->children[i]);
	}

	return TRUE;
}

int save_graph(FILE *fp, node_t *root)
{
	int magic = FILE_MAGIC;

	fwrite(&magic, sizeof(int), 1, fp);

	return save_node(fp,root);
}

node_t *load_node(FILE *fp)
{
	long id;
	size_t arity;
	symbol_t symbol;
	long match_id;

	node_t *n;

	fread(&id, sizeof(long), 1, fp);
	fread(&arity, sizeof(size_t), 1, fp);
	fread(&match_id, sizeof(long), 1, fp);
	fread(&symbol, sizeof(symbol_t), 1, fp);

	n = new_node(id, symbol, arity);

	insert_id(id, n);

	if(match_id > 0) {
		n->match = lookup_id(match_id);
	}

	size_t i;
	for(i=0;i<arity;i++) {
		n->children[i] = load_node(fp);
	}

	return n;
}

node_t *load_graph(FILE *fp)
{
	node_t *root;

	int magic;

	fread(&magic, sizeof(int), 1, fp);

	if(magic != FILE_MAGIC) {
		fprintf(stderr, "Error: input is not a valid Lope binary.\n");
		return NULL;
	}

	root = load_node(fp);

	return root;
}

int main(int argc, char **argv)
{
	initialise_kinds();
	
	if(argc != 2) {
		fprintf(stderr, "Usage: %s file.lbc\n", argv[0]);
		return 1;
	}

	FILE *fp = fopen(argv[1], "r");
	node_t *root = load_graph(fp);

	fclose(fp);

	print_node(root);


/*	symbol_t s1;
	s1.type = SYM_OPERATOR;
	s1.data.sym_operator = OPER_PLUS;
	s1.kind = 0;

	symbol_t s2;
	s2.type = SYM_INT;
	s2.data.sym_int = 32;
	s2.kind = 0;

	symbol_t s3;
	s3.type = SYM_INT;
	s3.data.sym_int = 64;
	s3.kind = 0;

	node_t *root = new_node(nodeid(), s1, 2);
	root->children[0] = new_node(nodeid(), s2, 0);
	root->children[1] = new_node(nodeid(), s3, 0);

	print_node(root);


	save_graph(fp, root);

	fclose(fp);

	node_t *root2;

	FILE *fp2 = fopen("test.lbc", "r");
	
	root2 = load_graph(fp);

	if(root2 == NULL) return 1;

	fclose(fp2);

	print_node(root2);

	root2->symbol.kind = KIND_COMPUTATION;
	root2->symbol.type = SYM_ANY;
	print_node(root2);

	reaction_t reaction;

	reaction.name = "expand";
	reaction.count = 0;
	reaction.next = NULL;
	reaction.redex = root2;

	symbol_t s4;
	s3.type = SYM_ANY;
	
	node_t *reactum = new_node(nodeid(), s3, 0);
	reactum->match = root2;

	reaction.reactum = reactum;

	int i;
	for(i=0;i<5;i++) {
		node_t *newroot = apply_all_reactions(root, &reaction);
		if(newroot != NULL) root = newroot;
		print_node(root);
		printf("[iteration %d complete.]\n", i);
	}

	if(root != NULL) print_node(root);

*/

/*
	node_t *pat = new_node(nodeid(), 1, 0);

	reaction.redex = pat;

	node_t *reactum = new_node(nodeid(), 1, 1);
	reactum->match = pat;
	reactum->children[0] = new_node(nodeid(), 1, 0);
	
	reaction.reactum = reactum;

	reaction_t r2;

	r2.name = "contract";
	r2.count = 0;

	r2.redex = new_node(nodeid(), 1, 1);
	r2.redex->children[0] = new_node(nodeid(), 1, 1);
	r2.redex->children[0]->children[0] = new_node(nodeid(), 1, 0);

	r2.reactum = new_node(nodeid(),2,1);
	r2.reactum->match = r2.redex;
	r2.reactum->children[0] = new_node(nodeid(), 1, 0);

	r2.next = NULL;

	reaction.next = &r2; 

	int i;
	for(i=0;i<100;i++) {
		node_t *newroot = apply_all_reactions(root, &reaction);
		if(newroot != NULL) root = newroot;
		print_node(root);
		printf("[iteration %d complete.]\n", i);
	}

	if(root != NULL) print_node(root);*/

	return 0;
}

