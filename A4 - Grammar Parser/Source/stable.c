/*****************************************************************************
File Name: stable.c
Compiler: gcc
Author: Gabriel Bourget
Course: CST8152 - Compilers
Assignment: A3 - Symbol Table
Date: September November 24, 02016
Professor: Svillen Ranev
Purpose: Creates a symbol table that keeps track of VIDs and their attributes.
Function List: st_create(), st_install(), st_lookup(), st_update_value()
st_update_type(), st_get_type(), st_destroy(), st_print(), st_setsize(),
st_incoffset(), st_store(), st_sort()
*****************************************************************************/

#include "stable.h" 

/* Global variables and functions */
extern STD sym_table; /* Defined in platy_tt.c */

/********************************st_create()**************************************
Purpose: Creates a new (and empty) symbol table.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: malloc(), b_create()
Parameters: -> st_size (Number of STVR elements to put in symbol table)
							 type: int
Return Value: STD, representing created symbol table descriptor
Algorithm: -> Declares a local STD.
				   -> Allocates dynamic memory for an array of STVR of size st_size.
				   -> Initialize the plsBD pointer to a new buffer structure.
				   -> If creation of STD is successful, set its size to st_size.
				   -> Return STD structure that was created.
*********************************************************************************/
STD st_create(int st_size) {

	/* Allocate local symbol table */
	STD sym_table;

	/* Allocate an array of STVR */
	if ((sym_table.pstvr = (STVR*)malloc(sizeof(STVR)*st_size)) == NULL) {
		return sym_table; /* DEV NOTE: WHAT TO RETURN HERE IF THERE IS AN ERROR? */
	}

	/* Create buffer descriptor for lexeme storage */
	if ((sym_table.plsBD = b_create(500, 15, 'a')) == NULL) { /* DEV NOTE: NOT SURE WHAT CREATION PARAMETERS ARE FOR THIS BUFFER STRUCTURE */
		return sym_table; /* DEV NOTE: WHAT TO RETURN HERE IF THERE IS AN ERROR? */
	} /* at least 500 chars of initial capacity, USE CONSTANTS NOT NUMBERS */

	/* Buffer creation successful, set st_size and initialize st_offset*/
	sym_table.st_size = st_size;
	sym_table.st_offset = 0;

	return sym_table;
}

/***********************************st_install()*****************************************
Purpose: Creates a new (and empty) symbol table.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: st_lookup(), b_addc(), b_rflag(), b_cbhead(), st_incoffset()
Parameters: -> sym_table (symbol table descriptor)
							 type: STD
						-> lexeme (lexeme to be installed in symbol table)
							 type: char*
						-> line (line number of first occurence)
							 type: int
Return Value: int, representing offset location of lexeme, or -1 if table is full
Algorithm: -> Call the st_lookup() function to search for the lexeme (var name) in
			  			the symbol table.
		       -> If the lexeme is not found, install the new one at the current st_offset.
		   		 -> Set plex and o_line to their corresponding values and that status field to
	            its default value.
		   		 -> Set the data type indicator to a value corresponding to variable type
		      	  represented by the formal parameter 'type'.
		   		 -> The value for the type parameter:
	 		        -> I for integer.
			   	 		-> F for floating point literal.
			   	 		-> S for string.
		       -> If the variable is of type 'string', set the update flag to 1.
		   		 -> Set the 'i_value' to 0 for integer/floating point literals, and to -1 for
		          strings.
		   		 -> Increment st_offset of global sym_table by one.
				   -> If the input lexeme is found, return the offset to its current STVR.
				   -> If the symbol table is full, return -1.
				   -> Return the current st_offset of the entry.
******************************************************************************************/
int st_install(STD sym_table, char* lexeme, char type, int line) {

	int lookupIndex = 0; /* Used to look up if lexeme is already stored */
	unsigned int i = 0; /* Iterator */
	int bufPos = 0; /* Iterator */
	int newOffset = sym_table.st_offset; /* Addition offset */
	int wordPos; /* Used for setting plex pointer */
	char reallocFlag = REALLOC_NSET; /* Used to handle dangling pointers */
	char *tempBuf = 0; /* Used to handle dangling pointers */
	char receivedChar = 0; /* Used to handle dangling pointers */	

	/* Guard against deallocated symbol table and full table */
	if (sym_table.pstvr == NULL
		|| (sym_table.st_offset == sym_table.st_size)) return R_FAIL_ST;

	lookupIndex = st_lookup(sym_table, lexeme); /* Offset returned when seeing if lexeme already in database */
	
	/* See if lexeme is already stored in STVR array */
	if (lookupIndex != R_FAIL_ST) return lookupIndex; /* Return index of matched STVR */

	/* Add lexeme into lexeme buffer */
	for (i = 0; i <= strlen(lexeme); i++) {
		b_addc(sym_table.plsBD, lexeme[i]);
		if (b_rflag(sym_table.plsBD)) reallocFlag = REALLOC_SET;
	}
	if (reallocFlag) {
		tempBuf = b_cbhead(sym_table.plsBD);

		for (i = 0; i <= sym_table.st_offset; i++) {
			sym_table.pstvr[i].plex = tempBuf;
			printf("Plex char is: %c", sym_table.pstvr[i]);
			bufPos = 0;
			while (receivedChar != '\0') {
				receivedChar = tempBuf[bufPos];
				++bufPos;
			}
			tempBuf = &(tempBuf[bufPos++]); /* One past the \0 character */
		}
	}
	wordPos = b_size(sym_table.plsBD) - strlen(lexeme) - 1;
	sym_table.pstvr[newOffset].plex = &(b_cbhead(sym_table.plsBD)[wordPos]);

	sym_table.pstvr[newOffset].o_line = line; /* Set line number */
	sym_table.pstvr[newOffset].status_field = ZEROS;
	sym_table.pstvr[newOffset].status_field |= SET_DEFAULT; /* Set status_field to default values */

	if (type == 'F') {
		/* Update type indicator to float */
		sym_table.pstvr[newOffset].status_field |= SET0201_01;
		/* Set i_value */
		sym_table.pstvr[newOffset].i_value.fpl_val = 0.0;
	}
	if (type == 'I') {
		/* Update type indicator to int */
		sym_table.pstvr[newOffset].status_field |= SET0201_10;
		/* Set i_value */
		sym_table.pstvr[newOffset].i_value.int_val = 0;
	}
	if (type == 'S') {
		/* Update type indicator to string */
		sym_table.pstvr[newOffset].status_field |= SET0201_11;
		/* Set i_value */
		sym_table.pstvr[newOffset].i_value.str_offset = -1;
		/* Set update flag so string can't be updated to different type */
		sym_table.pstvr[newOffset].status_field |= SET_LSB;
	}

	st_incoffset();
	return sym_table.st_offset; /* THIS IS A FLAW: sym_table should be passed in by reference */
}

/********************************st_lookup()**************************************
Purpose: Searches for a lexeme (variable name) in the symbol table.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: strcmp()
Parameters: -> sym_table (symbol table descriptor)
		       		 type: STD
						-> lexeme (lexeme to be installed in symbol table)
			   			 type: char*
Return Value: int, representing table position of lexeme
Algorithm: -> Start search backwards, beginning from last entry in the STVR array.
		   	 	 -> If the lexeme is found, return the offset of its location from the
			  			beginning of the array.
		   		 -> If not, return -1.
*********************************************************************************/
int st_lookup(STD sym_table, char *lexeme) {

	int i = 0; /* Iterator variable */

	/* Guard against deallocated or empty symbol table */
	if (sym_table.st_size == 0 || sym_table.st_offset == 0) return R_FAIL_ST;

	/* Start from end of array, return location if match found */
	for (i = (sym_table.st_offset - 1); i>=0; i--) {
		if (strcmp(sym_table.pstvr[i].plex, lexeme) == 0) return i;
	}

	/* No match found */
	return R_FAIL_ST;
}

/********************************st_update_type()**************************************
Purpose: Updates the data type indicator of the variable record indicated by vid_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> sym_table (symbol table descriptor)
						   type: STD
						-> vid_offset (position in symbol table to access)
					     type: int
						-> v_type (variable type to modify to)
						   type: char
Return Value: int, representing success or failure of update
Algorithm: -> Check update flag of variable record's status field.
				   -> If it is equal to 1, the type has been updated already, return -1.
				   -> If not, update the data type indicator of the status field.
				   -> Set the update flag.
				   -> Return vid_offset.
***************************************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type) {

	/* Guard against deallocated symbol table */
	/* Also, handle case where VID is of type String */
	if (sym_table.st_size == 0 || v_type == 'S') return R_FAIL_ST;

	/* Check if update flag is set */
	if ((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == 0) {
		if (v_type == 'F') {
			/* If type is already string, can't update it */
			if ((sym_table.pstvr[vid_offset].status_field & CHK_STRING) == 0x0006) return R_FAIL_ST;
			/* Update type indicator to float */
			sym_table.pstvr[vid_offset].status_field |= SET0201_01;
		}
		if (v_type == 'I') {
			/* If type is already string, can't update it */
			if ((sym_table.pstvr[vid_offset].status_field & CHK_STRING) == 0x0006) return R_FAIL_ST;
			/* Update type indicator to int */
			sym_table.pstvr[vid_offset].status_field |= SET0201_10;
		}
		/* Set update flag */
		sym_table.pstvr[vid_offset].status_field |= SET_LSB;
	}
	return R_FAIL_ST;
}

/********************************st_update_value()**************************************
Purpose: Update the i_value of the variable record indicated by vid_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> sym_table (symbol table descriptor)
					     type: STD
						-> vid_offset (position in symbol table to access)
						   type: int
					  -> i_value (value to change record to)
						   type: InitialValue
Return Value: int, representing success or failure of update
Algorithm: -> Check update flag of variable record's status field.
		       -> If it is equal to 1, the type has been updated already, return -1.
				   -> If not, update the data type indicator of the status field.
				   -> Set the update flag.
				   -> Return vid_offset.
************************************************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value) {

	/* Guard against deallocated symbol table */
	/* Also check to see if the status field's update flag is set */
	if (sym_table.st_size == 0
		|| ((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == 1)) return R_FAIL_ST;

	sym_table.pstvr[vid_offset].i_value = i_value; /* DEV NOT: SKEPTICAL THAT THAT'S ALL THERE IS TO IT */
	sym_table.pstvr[vid_offset].status_field |= SET_LSB;
	return vid_offset;
}

/********************************st_get_type()**************************************
Purpose: Returns the type of the variable record indicated by vid_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> sym_table (symbol table descriptor)
					   type: STD
				    -> vid_offset (position in symbol table to access)
					   type: int
Return Value: char, representing variable record type
Algorithm: -> Return F for floating point, I for integer type, S for string type.
				   -> Upon failure, return -1.
***************************************************************************************/
char st_get_type(STD sym_table, int vid_offset) {

	unsigned short tempStatus = 0;

	/* Guard against deallocated symbol table */
	if (sym_table.st_size == 0) return R_FAIL_ST; /* DEV NOTE: DOES THIS GET CAST AS A CHAR? */	

	tempStatus |= sym_table.pstvr[vid_offset].status_field;
	/* Turn off LSB if it's set */
	tempStatus &= ALLBUTLSB;

	if (tempStatus == (SET_DEFAULT | CHK_INT)) return 'I';

	if (tempStatus == (SET_DEFAULT | CHK_FLOAT)) return 'F';

	if (tempStatus == (SET_DEFAULT | CHK_STRING)) return 'S';

	return R_FAIL_ST;
}

/********************************st_destroy()**************************************
Purpose: Frees dynamic memory areas associated with the symbol table.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: free(), st_setsize(), b_free()
Parameters: -> sym_table (symbol table descriptor)
							 type: STD
Return Value: ---
Algorithm: -> Free dynamic memory associated with the symbol table.
		   		 -> Set st_size of STD to 0.
***************************************************************************************/
void st_destroy(STD sym_table) {

	/* Guard against deallocated symbol table */
	if (sym_table.st_size == 0) return;

	/* Free lexeme buffer */
	b_free(sym_table.plsBD);
	/* Release memory holding entire VSTR array */
	free(sym_table.pstvr); 
	st_setsize();
}

/********************************st_print()**************************************
Purpose: Prints the contents of the symbol table.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: fprintf()
Parameters: -> sym_table (symbol table descriptor)
							 type:  STD
Return Value: int, number of entries or failure indicator
Algorithm: -> Print the contents of the symbol table to the standard output.
		   		 -> Return the number of entries or return -1 upon failure.
***************************************************************************************/
int st_print(STD sym_table) {

	int i = 0; /* Loop iterator */

	/* Guard against deallocated symbol table */
	if (sym_table.st_size == 0) return R_FAIL_ST;

	fprintf(stdout, "\nSymbol Table\n____________\n\n");
	fprintf(stdout, "Line Number Variable Identifier\n");
	for (i = 0; i<sym_table.st_offset; i++) {
		fprintf(stdout, "%2d          %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
	}

	return sym_table.st_size;
}

/***********************************st_setsize()***************************************
Purpose: Internal function that sets st_size to 0.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: ---
Return Value: ---
Algorithm: -> Set symbol table's st_size to 0.
***************************************************************************************/
static void st_setsize() { sym_table.st_size = 0; }

/*********************************st_incoffset()***************************************
Purpose: Internal function that increments st_offset by 1.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: ---
Return Value: ---
Algorithm: -> Increment symbol table's st_offset by 1.
***************************************************************************************/
static void st_incoffset() { ++sym_table.st_offset; }

/************************************st_store()*****************************************
Purpose: Stores the symbol table into a text file named $stable.ste.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: fopen(), fprintf(), strlen(), fclose()
Parameters: -> sym_table (symbol table descriptor)
			  			 type: STD
Return Value: int, number of records stored or failure indicator
Algorithm: -> Open a file in the current directory, overwriting if it already exists.
				   -> Print out the symbol table size.
				   -> For each record in the symbol table database...
				   -> Print the status field in hex format.
				   -> Print the length of the lexeme.
				   -> Print the lexeme itself.
				   -> Print the line number it first appeared on.
				   -> Print its initial value.
				   -> Increment the number of records stored.
				   -> Close the file.
				   -> Print out "Symbol table stored."
				   -> Return the number of entries or return -1 upon failure.
***************************************************************************************/
int st_store(STD sym_table) {

	int i = 0; /* Iterator variable */
	FILE *oFile; /* File pointer to text file being written */
	char varType = '\0'; /* Records variable type for each record */
	int numRecordsStored = 0; /* Keeps track of the number of records stored */

	if (sym_table.st_size == 0) return R_FAIL_ST;

	if ((oFile = fopen("$stable.ste", "w")) == NULL) return R_FAIL_ST;

	fprintf(oFile, "%d", sym_table.st_size);

	for (i = 0; i<sym_table.st_size; i++) {
		fprintf(oFile, " %X", sym_table.pstvr[i].status_field);    /* Status field */
		fprintf(oFile, " %lu", strlen(sym_table.pstvr[i].plex));   /* Length of lexeme */
		fprintf(oFile, " %s", sym_table.pstvr[i].plex);            /* Lexeme */
		fprintf(oFile, " %d", sym_table.pstvr[i].o_line);          /* Line number */

		/* Initial value */
		varType = st_get_type(sym_table, i);
		if (varType == 'F') fprintf(oFile, " %.2f", sym_table.pstvr[i].i_value.fpl_val);
		else if (varType == 'I') fprintf(oFile, " %d", sym_table.pstvr[i].i_value.int_val);
		else if (varType == 'S') fprintf(oFile, " %d", sym_table.pstvr[i].i_value.str_offset);

		++numRecordsStored; /* Increment number of records stored */
	}

	fclose(oFile); /* Close file stream */
	fprintf(stdout, "Symbol Table stored.\n");
	return numRecordsStored;
}

/********************************st_asc_comparator()**************************************
Purpose: Comparator function for the VIDs stored in the symbol table database. Passed to
qsort() function eventually for use in sorting entries in an ascending fashion.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: strcmp()
Parameters: -> a (pointer to element a)
		  			   type: const void*
				    -> b (pointer to element b)
			  		   type: const void*
Return Value: int, comparison result
Algorithm: -> Conduct lexicographic comparison of two strings.
******************************************************************************************/
static int st_asc_comparator(const void *a, const void *b) {
	return (strcmp(((STVR*)a)->plex, ((STVR*)b)->plex));
}

/********************************st_desc_comparator()**************************************
Purpose: Comparator function for the VIDs stored in the symbol table database. Passed to
qsort() function eventually for use in sorting entries in an descending fashion.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: strcmp()
Parameters: -> a (pointer to element a)
						   type: const void*
						-> b (pointer to element b)
						   type: const void*
Return Value: int, comparison result
Algorithm: -> Conduct lexicographic comparison of two strings.
*******************************************************************************************/
static int st_desc_comparator(const void *a, const void *b) {
	return (strcmp(((STVR*)b)->plex,((STVR*)a)->plex));
}

/************************************st_sort()*****************************************
Purpose: Sorts the contents of the STVR array.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: qsort()
Parameters: -> sym_table (symbol table descriptor)
						   type: STD
						-> s_order (sorting order, ascending or descending)
						   type: char
Return Value: int, sort status
Algorithm: -> Call qsort function with context of order being asked for.
***************************************************************************************/
int st_sort(STD sym_table, char s_order) {
	return 0;
}

