/*****************************************************************************
File Name: stable.h
Compiler: gcc
Author: Gabriel Bourget
Course: CST8152 - Compilers
Assignment: A3 - Symbol Table
Date: September November 24th, 02016
Professor: Svillen Ranev
Purpose: Header file with supporting structures and function prototypes for a
symbol table.
Function List:  st_create(), st_install(), st_lookup(), st_update_value()
st_update_type(), st_get_type(), st_destroy(), st_print(), st_setsize(),
st_incoffset(), st_store(), st_sort()
Constants: ZEROS, SET_DEFAULT, CHK_LSB, SET_LSB, CHK_FLOAT, CHK_INT,
           CHK_STRING, SET0201_01, SET0201_10, SET0201_11, ALLBUTLSB,
		   REALLOC_SET, REALLOC_NSET, R_FAIL_ST, SUCCESS, VID_LEN
Type Declarations: InitialValue, SymbolTableVidRecord, SymbolTableDescriptor
*****************************************************************************/

#ifndef STABLE_H_
#define STABLE_H_

#include <malloc.h>
#include <string.h>
#include <stdio.h>
#include "buffer.h"

#define ZEROS        0x0000   /* 0000 0000 0000 0000 */
#define SET_DEFAULT  0xFFF8   /* 1111 1111 1111 1000 */
#define CHK_LSB      0x0001   /* 0000 0000 0000 0001 */
#define SET_LSB      0x0001   /* 0000 0000 0000 0001 */
#define CHK_FLOAT    0x0002   /* 0000 0000 0000 0010 */
#define CHK_INT      0x0004   /* 0000 0000 0000 0100 */
#define CHK_STRING   0x0006   /* 0000 0000 0000 0110 */
#define SET0201_01   0x0002   /* 0000 0000 0000 0010 */ /* FLOAT */
#define SET0201_10   0x0004   /* 0000 0000 0000 0100 */ /* INTEGER */
#define SET0201_11   0x0006   /* 0000 0000 0000 0110 */ /* STRING */
#define ALLBUTLSB    0XFFFE   /* 1111 1111 1111 1110 */

#define REALLOC_SET 1
#define REALLOC_NSET 0
#define R_FAIL_ST -1
#define SUCCESS 1
#define VID_LEN 8

typedef union InitialValue {
	int int_val;
	float fpl_val;
	int str_offset;
}InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* variable record status field */
	char * plex;    		     /* pointer to lexeme (VID) name in CA */
	int o_line; 				 /* line of first occurence */
	InitialValue i_value; 		 /* variable initial value */
	size_t reserved; 			 /* reserved for future use */
}STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;   /* Pointer to array of symbol table variable records */
	int st_size;   /* Total number of STVRs in the array */
	int st_offset; /* Offset to next STVR to be added to the array */
	Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
}STD;

/***********************/
/* Function Prototypes */
/***********************/
STD st_create(int);
int st_install(STD, char*, char, int);
int st_lookup(STD, char*);
int st_update_type(STD, int, char);
int st_update_value(STD, int, InitialValue);
char st_get_type(STD, int);
void st_destroy(STD);
int st_print(STD);
static void st_setsize();
static void st_incoffset();
int st_store(STD);
int st_sort(STD, char);
static int st_asc_comparator(const void*, const void*);
static int st_desc_comparator(const void*, const void*);

#endif
