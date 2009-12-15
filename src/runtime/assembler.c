#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "runtime.h"

#define STACK_SIZE 1024

symbol_t *symbol_stack;
int stackptr = 0;
int lineno = 0;

void stack_push(symbol_t s) 
{
	printf("push: %d\n", stackptr);

	if(stackptr >= STACK_SIZE) {
		fprintf(stderr, "Error: stack overflow\n");
		exit(1);
	}

	symbol_stack[++stackptr] = s;
}

symbol_t stack_pop( void )
{
	printf("pop: %d\n", stackptr);

	if(stackptr < 0) {
		fprintf(stderr, "Error: stack underflow\n");
		exit(1);
	}

	return symbol_stack[stackptr--];
}

void instr_push(FILE *out, char *line)
{
	symbol_t s;
	sscanf(line,"%ld", &s.type);
	s.kind = 0;

	stack_push(s);
}

void instr_push_i(FILE *out, char *line)
{
	symbol_t s;
	sscanf(line,"%d", &s.data.sym_int);
	s.kind = 0;
	s.type = SYM_INT;
	stack_push(s);
}

void instr_push_o(FILE *out, char *line)
{
	symbol_t s;
	sscanf(line,"%ld", &s.data.sym_operator);
	s.kind = 0;
	s.type = SYM_OPERATOR;
	stack_push(s);
}

void instr_push_k(FILE *out, char *line)
{
	symbol_t s;
	sscanf(line,"%ld", &s.kind);
	s.type = SYM_ANY;
	stack_push(s);
}

void instr_reaction(FILE *out, char *line)
{
	symbol_t s;
	s.type = SYM_ANY;
	s.kind = KIND_REACTION;
	stack_push(s);
}

void instr_redex(FILE *out, char *line)
{
	symbol_t s;
	s.type = SYM_ANY;
	s.kind = KIND_REDEX;
	stack_push(s);
}

void instr_reactum(FILE *out, char *line)
{
	symbol_t s;
	s.type = SYM_ANY;
	s.kind = KIND_REACTUM;
	stack_push(s);
}

void instr_node(FILE *out, char *line)
{
	symbol_t s = stack_pop();

	long id;
	size_t arity;
	identifier_t m = 0L;

	sscanf(line, "%ld %ld", &id, &arity);

	fwrite(&id, sizeof(long), 1, out);
	fwrite(&arity, sizeof(size_t), 1, out);
	fwrite(&m, sizeof(identifier_t), 1, out);
	fwrite(&s, sizeof(symbol_t), 1, out);
}

void instr_nodem(FILE *out, char *line)
{
	symbol_t s = stack_pop();

	long id;
	long matchid;
	size_t arity;

	sscanf(line, "%ld %ld %ld", &id, &matchid, &arity);

	fwrite(&id, sizeof(long), 1, out);
	fwrite(&arity, sizeof(size_t), 1, out);
	fwrite(&matchid, sizeof(identifier_t), 1, out);
	fwrite(&s, sizeof(symbol_t), 1, out);
}

int assemble(FILE *in, FILE *out)
{
	/* Write magic */
	int magic = FILE_MAGIC;
	fwrite(&magic, sizeof(int), 1, out);
	char line[1024];

	while(fgets(line,1024,in) != NULL) {
		lineno++;

		if(line[0] == '#' || line[0] == '\n') {
			continue;
		} else if(!strncmp(line, "push ", 5)) {
			instr_push(out,line+5);
		} else if(!strncmp(line, "push_i ", 7)) {
			instr_push_i(out,line+7);
		} else if(!strncmp(line, "push_o ", 7)) {
			instr_push_o(out,line+7);
		} else if(!strncmp(line, "push_k ", 7)) {
			instr_push_k(out,line+7);
		} else if(!strncmp(line, "node ", 5)) {
			instr_node(out,line+5);
		} else if(!strncmp(line, "nodem ", 6)) {
			instr_nodem(out,line+6);
		} else if(!strncmp(line, "redex", 5)) {
			instr_redex(out,line+5);
		} else if(!strncmp(line, "reactum", 7)) {
			instr_reactum(out,line+7);
		} else if(!strncmp(line, "reaction", 8)) {
			instr_reaction(out,line+8);
		} else {
			fprintf(stderr, "Error: invalid instruction at line %d:\n%s",
				lineno, line);
			return FALSE;
		}
	}

	if(stackptr > 0) {
		fprintf(stderr, "Warning: spare tokens on symbol stack\n");
	}

	return TRUE;
}

char *make_filename(char *name)
{
	int i = strlen(name)-1;
	char newname[255];

	int pos = strlen(name);

	for(;i>0;i--) {
		if(name[i] == '.') {
			pos = i;
			break;
		}
	}

	strncpy(newname,name,pos);
	strcat(newname, ".lbc");

	return strdup(newname);
}

int main(int argc, char **argv)
{
	if(argc != 2) {
		fprintf(stderr, "Usage: %s asm-file\n", argv[0]);
		return 1;
	}

	FILE *fp = fopen(argv[1], "r");

	if(fp == NULL) {
		fprintf(stderr,"Error: cannot read from file '%s'\n", argv[1]);
		return 1;
	}

	char *outname = make_filename(argv[1]);
	FILE *outfp = fopen(outname, "w");

	if(outfp == NULL) {
		fprintf(stderr,"Error: cannot open output file '%s'\n", argv[1]);
		fclose(fp);
		return 1;
	}

	symbol_stack = (symbol_t *)malloc(sizeof(symbol_t)*STACK_SIZE);

	if(symbol_stack == NULL) {
		fprintf(stderr, 
		  "Error: could not allocate %ld bytes for symbol stack\n",
		  sizeof(symbol_t)*STACK_SIZE);
		fclose(fp);
		fclose(outfp);
		return 1;
	}

	if(!assemble(fp,outfp)) {
		return 1;
	}

	free(symbol_stack);
	fclose(fp);
	fclose(outfp);

	return 0;
}
/* Syntax:
push b
push c
push a
node 1 1
node 2 1
node 3 0

push_i 35
push_i 42
push_o +
node 1 2
node 2 0
node 3 0

redex
push_k Computation (or push_o 0, or push operator)
node 5 1
node 6

reactum
push_o *
node 7 1
nodem 8 6 0
*/


