/*****************************************************************************
File Name: platy_bt.c
Compiler: gcc
Author: Svillen Ranov, with modifications and additions by Gabriel Bourget
Course: CST8152 - Compilers
Assignment: A1 - The Buffer
Date: September 29, 2016
Professor: Svillen Ranov
Purpose: Contains and runs the main function for the overall buffer program.
Function List: main(), err_print(), display(), get_filesize()
Type Declarations: BufferDescriptor (aka Buffer, *pBuffer)
Constants: R_FAIL1, R_FAIL2, LOAD_FAIL, SET_R_FLAG
Macros:
*****************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard string library functions defined in string.h.
* The define directive does not have any effect on other compiler projects (gcc, Borland).
*/
#define _CRT_SECURE_NO_WARNINGS

/************** - CHANGES MADE TO MAIN FUNCTION - ***************/
/* -> Added command line arguments to handle specifying the     */
/*    buffer's initial capacity, increment factor and mode.     */
/*    -> param1: executable; param2: input file;                */
/*       param3: initial capacity; -> param4: increment factor; */
/*       param5: operational mode; -> param5: b_pack() flag     */
/* -> Not all big test files call b_pack(). Added a command     */
/*    line argument which will toggle the part of the main      */
/*    function that does this and displays the new statistics.  */
/*    Enter CLI argument 5 as 'y' to toggle b_pack() in main(). */
/****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "buffer.h"

/* constant definitions */
#define INIT_CAPACITY 200   /* initial buffer capacity */
#define INC_FACTOR 15       /* increment factor */

/*check for ANSI C compliancy */
#define ANSI_C 0
#if defined(__STDC__)
#undef ANSI_C
#define ANSI_C 1
#endif

/*  Declaration of an error printing function with
*  variable number of arguments
*/
void err_printf(char *fmt, ...);
/*  Declaration of a buffer contents display function */
void display(Buffer *ptr_Buffer);
long get_filesize(char *fname);

int main(int argc, char **argv){

	pBuffer ptr_Buffer;                            /* Pointer to Buffer structure */
	FILE *fi;                                      /* Input file handle */
	int loadsize = 0;                              /* The size of the file loaded in the buffer */
	int ansi_c = !ANSI_C;                          /* ANSI C compliancy flag */
	short cl_init_capacity = INIT_CAPACITY;        /* Initial capacity, read in from command line */
	short cl_inc_factor = INC_FACTOR;              /* Increment factor, read in from command line */

	/* Check if the compiler option is set to compile ANSI C */
	/* __DATE__, __TIME__, __LINE__, __FILE__, __STDC__ are predefined preprocessor macros*/
	if (ansi_c){
		err_printf("Date: %s  Time: %s", __DATE__, __TIME__);
		err_printf("ERROR: Compiler is not ANSI C compliant!\n");
		exit(1);
	}

	/* Missing file name or/and mode parameter */
	if (argc <= 2){
		err_printf("\nDate: %s  Time: %s", __DATE__, __TIME__);
		err_printf("\nRuntime error at line %d in file %s\n", __LINE__, __FILE__);
		err_printf("%s\b\b\b\b%s%s", argv[0], ": ", "Missing parameters.");
		err_printf("Usage: platybt source_file_name mode");
		exit(1);
	}

	if (argc == 6) {
		cl_init_capacity = (short)atoi(argv[2]);
		cl_inc_factor = (short)atoi(argv[3]);
	}

	/* Create a source code input buffer */
	switch (*argv[4]){
	case 'f': case 'a': case 'm': break;
	default:
		err_printf("%s%s%s", argv[0], ": ", "Wrong mode parameter.");
		exit(1);
	}

	/* Create the input buffer */
	ptr_Buffer = b_create(cl_init_capacity, (char)cl_inc_factor, *argv[4]);
	if (ptr_Buffer == NULL){
		err_printf("%s%s%s", argv[0], ": ", "Could not create buffer.");
		exit(1);
	}

	/* open the source file */
	if ((fi = fopen(argv[1], "r")) == NULL){
		err_printf("%s%s%s%s", argv[0], ": ", "Cannot open file: ", argv[1]);
		exit(1);
	}

	/* load a source file into the input buffer  */
	printf("Reading file %s ....Please wait\n", argv[1]);
	loadsize = b_load(fi, ptr_Buffer);
	if (loadsize == R_FAIL1) err_printf("%s%s%s", argv[0], ": ", "Error in loading buffer.");

	/* close the source file */
	fclose(fi);
	/*find the size of the file  */
	if (loadsize == LOAD_FAIL){
		printf("The input file %s %s\n", argv[1], "is not completely loaded.");
		printf("Input file size: %ld\n", get_filesize(argv[1]));
	}

	/* set a mark at the last char in the buffer*/
	b_setmark(ptr_Buffer, b_size(ptr_Buffer));

	/* display the contents of the input buffer */
	display(ptr_Buffer);

	if (*argv[5] == 'y') {
		/* pack the buffer, if possible, add end-of-file character (EOF) to the buffer display again */
		if (b_pack(ptr_Buffer)){
			if (!b_addc(ptr_Buffer, EOF))
				err_printf("%s%s%s", argv[0], ": ", "Error in writing to buffer.");
			display(ptr_Buffer);
		}
	}


	/* free the dynamic memory used by the buffer */
	b_free(ptr_Buffer);
	/* make the buffer invalid
	It is not necessary here because the function terminates anyway,
	but will prevent run-time errors and crashes in future expansions */
	ptr_Buffer = NULL;

	/*return success */
	return (0);
}

/* error printing function with variable number of arguments*/
void err_printf(char *fmt, ...){
	/*Initialize variable list */
	va_list ap;
	va_start(ap, fmt);

	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);

	/* Move to new line */
	if (strchr(fmt, '\n') == NULL)
		fprintf(stderr, "\n");
}

void display(Buffer *ptr_Buffer){
	printf("\nPrinting buffer parameters:\n\n");
	printf("The capacity of the buffer is:  %d\n", b_capacity(ptr_Buffer));
	printf("The current size of the buffer is:  %d\n", b_size(ptr_Buffer));
	printf("The operational mode of the buffer is:   %d\n", b_mode(ptr_Buffer));
	printf("The increment factor of the buffer is:  %u\n", b_incfactor(ptr_Buffer));
	printf("The current mark of the buffer is:  %d\n", b_mark(ptr_Buffer));
	/*printf("The reallocation flag is:   %d\n",b_rflag(ptr_Buffer));*/
	printf("\nPrinting buffer contents:\n\n");
	b_print(ptr_Buffer);
}

long get_filesize(char  *fname){
	FILE *input;
	long flength;
	input = fopen(fname, "r");
	if (input == NULL){
		err_printf("%s%s", "Cannot open file: ", fname);
		return 0;
	}
	fseek(input, 0L, SEEK_END);
	flength = ftell(input);
	fclose(input);
	return flength;
}

