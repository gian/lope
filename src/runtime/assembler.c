#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "runtime.h"

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


}
