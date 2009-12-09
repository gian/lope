#include <stdio.h>
#include <stdlib.h>

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

int match_graph(node_t *graph, node_t *pattern)
{
	/* populate the "match" field of the pattern */


	return FALSE;
}

void print_node(node_t *node) {
	printf("%ld: %ld", node->node_id, node->symbol);
	int i;
	for(i = 0; i<node->num_children; i++) {
		printf(" %ld", node->children[i]->node_id);
	}

	if(node->match != NULL) {
		printf(" (%ld)", node->match->node_id);
	}

	printf("\n");

	for(i = 0; i<node->num_children; i++) {
		print_node(node->children[i]);
	}
}

void print_pattern(node_t *pattern)
{
	
}

void print_graph(node_t *graph)
{

}

int main(int argc, char **argv)
{
	node_t *root = new_node(nodeid(), 1, 2);
	root->children[0] = new_node(nodeid(), 2, 0); 
	root->children[1] = new_node(nodeid(), 3, 0);

	print_node(root);

	node_t *pat = new_node(nodeid(), 1, 2);
	pat->children[0] = new_node(nodeid(), SYM_ANY, 0); 
	pat->children[1] = new_node(nodeid(), SYM_ANY, 0);

	pat->children[1]->match = root->children[1];

	print_node(pat);

	printf("Match: %s\n", (match_graph(root,pat)) ? "true" : "false");	

	return 0;
}

