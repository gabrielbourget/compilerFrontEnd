/*****************************************************************************
File Name: tableGB.h
Compiler: Visual Studio 2013 Compiler
Author: Gabriel Bourget
Course: Computer Engineering Technology
Assignment: Assignment 3 - Symbol Table
Date: November 24th, 2016
Professor: Svillen Ranev
Purpose: Support header file for scanner implementation
Function List: aafunc02(), aafunc03(), aafunc05(), aafunc08(), aafunc11(), 
               aafunc12()
Constants: ES, IS, TABLE_COLUMNS, ASWR, ASNR, NOAS, KWT_SIZES
Type Declarations: Token
*****************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#include "token.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/* REPLACE *ESN* WITH YOUR ERROR STATE NUMBER */
#define ES  12   /* Error state */
#define IS  13   /* Invalid state */

/* State transition table definition */

#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* |--------------------------------------------------------------------------------------| */
	/* |----------||----------------------INPUT SYMBOLS-----------------------||--------------| */
	/* |Currstate ||    [a-zA-Z]   0    [1-7]  [8-9]    .     %     other     || State Type   | */
	/* |--------------------------------------------------------------------------------------| */
	/* |   0      */{      1,      6,     4,     4,    IS,   IS,     IS    }, /*    NOAS      | */
	/* |   1      */{      1,      1,     1,     1,     2,    3,      2    }, /*    NOAS      | */
	/* |   2      */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /* AVID/KW/ASWR | */
	/* |   3      */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*  SVID/ASNR   | */
	/* |   4      */{     ES,      4,     4,     4,     7,    5,      5    }, /*    NOAS      | */
	/* |   5      */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*  DIL/ASWR    | */
	/* |   6      */{     ES,     ES,    10,    ES,     7,   ES,      5    }, /*    NOAS      | */
	/* |   7      */{      8,      7,     7,     7,     8,    8,      8    }, /*    NOAS      | */
	/* |   8      */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*   FPL/ASWR   | */
	/* |   9      */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*    NOAS      | */
	/* |   10     */{     11,     10,    10,    ES,    ES,   11,     11    }, /*    NOAS      | */
	/* |   11     */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*   OIL/ASWR   | */
	/* |   12     */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*   ES/ASNR    | */
	/* |   13     */{     IS,     IS,    IS,    IS,    IS,   IS,     IS    }, /*   ES/ASWR    | */
	/* |--------------------------------------------------------------------------------------| */
};

/* Accepting state table definition
REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS*/
#define ASWR     0  /* accepting state with retract */
#define ASNR     1  /* accepting state with no retract */
#define NOAS     2  /* not accepting state */

int as_table[] = {
	NOAS, /* State 0  */
	NOAS, /* State 1  */
	ASWR, /* State 2  */
	ASNR, /* State 3  */
	NOAS, /* State 4  */
	ASWR, /* State 5  */
	NOAS, /* State 6  */
	NOAS, /* State 7  */
	ASWR, /* State 8  */
	NOAS, /* State 9  */
	NOAS, /* State 10 */
	ASWR, /* State 11 */
	ASNR, /* State 12 */
	ASWR, /* State 13 */
};

/* Accepting action function declarations */

Token aa_func02(char *lexeme); /* AVID / KW */
Token aa_func03(char *lexeme); /* SVID */
Token aa_func05(char *lexeme); /* DIL  */
Token aa_func08(char *lexeme); /* FPL */
Token aa_func11(char *lexeme); /* OIL */
Token aa_func12(char *lexeme); /* ERR_TOK ASNR/ASWR */

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	NULL,
	aa_func11,
	aa_func12,
	NULL
};

/*HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
TO ACCEPTING FUNCTIONS. THE ARRAY HAS THE SAME SIZE AS as_table[ ].
YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
ACCEPTING FUNCTIONS (FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
THE REST OF THE ELEMENTS MUST BE SET TO NULL.*/

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table[] = {
	"ELSE",
	"IF",
	"INPUT",
	"OUTPUT",
	"PLATYPUS",
	"REPEAT",
	"THEN",
	"USING"
};
#endif         
