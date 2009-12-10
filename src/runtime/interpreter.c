/******************************************************************************
* Lope Programming Language
* Copyright (c) 2009 Gian Perrone
* Runtime System
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define SYM_ANY 0L
#define TRUE 1
#define FALSE 0

typedef long identifier_t;
typedef long symbol_t;

long g_nid = 1;

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
	long count;
	struct reaction_t *next;
} reaction_t;

identifier_t nodeid( void ) {
	return g_nid++;
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
	if(pattern->symbol == SYM_ANY && pattern->num_children == 0) {
		/* the "anything" node with no children matches any subtree */
		pattern->match = graph;
		return TRUE;
	}

	if(pattern->num_children != graph->num_children) return FALSE;

	if(pattern->symbol == SYM_ANY) {
		pattern->match = graph;
		return TRUE;
	}

	if(pattern->symbol == graph->symbol) {
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
	if(reactum->symbol == SYM_ANY) {
		reactum->symbol = reactum->match->match->symbol;
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

node_t *apply_reaction(node_t *graph, reaction_t *reaction)
{
	node_t *reaction_site = match_graph_anywhere(graph, reaction->redex);

	/* This rule does not match anywhere in the graph. */
	if(reaction_site == NULL) return NULL;

	printf("[applied rule '%s' at @%ld, rule count: %ld]\n", reaction->name, reaction_site->node_id, reaction->count+1);

	node_t *subgraph = apply_reactum(reaction->reactum);
	graph = update_references(graph,reaction_site,subgraph);

	reaction->count++;

	/*TODO: Add garbage collection to this. */
	return graph;
}

node_t *apply_all_reactions(node_t *graph, reaction_t *head)
{
	while(head != NULL) {
		graph = apply_reaction(graph,head);
		head = head->next;
	}

	return graph;
}

char *sym_to_string(symbol_t s) 
{
	if(s == SYM_ANY) return "_";

	char b[16];

	sprintf(b, "%ld", s);

	return strdup(b);
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


int main(int argc, char **argv)
{
	node_t *root = new_node(nodeid(), 1, 2);
	root->children[0] = new_node(nodeid(), 2, 0); 
	root->children[1] = new_node(nodeid(), 3, 0);

	print_node(root);

	reaction_t reaction;

	reaction.name = "1to666";
	reaction.count = 0;
	reaction.next = NULL;

	node_t *pat = new_node(nodeid(), 1, 2);
	pat->children[0] = new_node(nodeid(), SYM_ANY, 0); 
	pat->children[1] = new_node(nodeid(), SYM_ANY, 0);

	reaction.redex = pat;

	node_t *reactum = new_node(nodeid(), 666, 2);
	reactum->match = pat;
	reactum->children[0] = new_node(nodeid(), SYM_ANY, 0);
	reactum->children[0]->match = pat->children[0];
	reactum->children[1] = new_node(nodeid(), SYM_ANY, 1);
	reactum->children[1]->match = pat->children[1];
	reactum->children[1]->children[0] = new_node(nodeid(), 333, 0);
	
	reaction.reactum = reactum;

	int i;
	for(i=0;i<100;i++) {
		root = apply_all_reactions(root, &reaction);
		print_node(root);
		printf("[iteration %d complete.]\n", i);
	}

	print_node(root);

	return 0;
}

