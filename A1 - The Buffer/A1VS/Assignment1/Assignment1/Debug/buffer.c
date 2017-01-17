/*****************************************************************************
File Name: buffer.c
Compiler: gcc
Author: Gabriel Bourget
Course: CST8152 - Compilers 
Assignment: A1 - The Buffer
Date: September [INSERT DATE HERE WHEN SUBMITTING], 2016
Professor: Svillen Ranov	
Purpose: Creates a buffer of characters along with a structure that manages its 
         size, along with other important paramaters.
Function List:  b_create(), b_addc(), b_reset(), b_free(), b_isfull(), b_size(),
			    b_capacity(), b_setmark(), b_mark(), b_mode, b_incfactor(),	b_load(), 
			    b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(),
				b_retract(), b_retract_to_mark(), b_getcoffset(), b_cbhead()
*****************************************************************************/

#include "buffer.h"

/* ======================= TEMP CODE REMOVE THIS EVENTUALLY ============================= */

/*  File name: platy_bt.c
 *  Purpose:This is the main program for Assignment #1, CST8152, Fall 16
 *  Version: 1.16.2
 *  Author: Svillen Ranev
 *  Date: 6 September 2016
 */   

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard string library functions defined in string.h.
 * The define directive does not have any effect on other compiler projects (gcc, Borland).
 */
#define _CRT_SECURE_NO_WARNINGS

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

#define DEBUG 0
#define MAIN_DEBUG 0

/*  Declaration of an error printing function with
 *  variable number of arguments
 */
void err_printf(char *fmt, ...);
/*  Declaration of a buffer contents display function */
void display (Buffer *ptr_Buffer); 
long get_filesize(char *fname);

int main(int argc, char **argv){
	
   pBuffer ptr_Buffer;   /* pointer to Buffer structure */
   FILE *fi;             /* input file handle */
   int loadsize = 0;     /*the size of the file loaded in the buffer */
   int ansi_c = !ANSI_C; /* ANSI C compliancy flag */

   if (MAIN_DEBUG) fprintf(stdin,"--- IN MAIN FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

    
 /* Check if the compiler option is set to compile ANSI C */
 /* __DATE__, __TIME__, __LINE__, __FILE__, __STDC__ are predefined preprocessor macros*/
  if(ansi_c){
    err_printf("Date: %s  Time: %s",__DATE__, __TIME__);
    err_printf("ERROR: Compiler is not ANSI C compliant!\n");
    exit(1);
  }

 /* missing file name or/and mode parameter */
  if (argc <= 2){

    err_printf("\nDate: %s  Time: %s",__DATE__, __TIME__);
    err_printf("\nRuntime error at line %d in file %s\n", __LINE__, __FILE__);
	  err_printf("%s\b\b\b\b%s%s",argv[0],": ","Missing parameters.");
	  err_printf("Usage: platybt source_file_name mode");
	  exit(1);
	}
	
 /* create a source code input buffer */		
	switch(*argv[2]){
	 case 'f': case 'a': case 'm': break;
	 default:
	  err_printf("%s%s%s",argv[0],": ","Wrong mode parameter.");
	  exit(1);
	}

  if (MAIN_DEBUG) printf("--- BEFORE ENTERING B_CREATE FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
 /*create the input buffer */
    ptr_Buffer = b_create(INIT_CAPACITY,INC_FACTOR,*argv[2]);
	if (ptr_Buffer == NULL){
		err_printf("%s%s%s",argv[0],": ","Could not create buffer.");
		exit(1);
	}

 if (MAIN_DEBUG) printf("--- BEFORE OPENING THE SOURCE FILE TO FILE POINTER ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
 /* open the source file */
	if ((fi = fopen(argv[1],"r")) == NULL){
		err_printf("%s%s%s%s",argv[0],": ", "Cannot open file: ",argv[1]);
		exit (1);
	}

 if (MAIN_DEBUG) printf("--- LOADING SOURCE FILE INTO BUFFER USING B_LOAD --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
 /* load a source file into the input buffer  */
     printf("Reading file %s ....Please wait\n",argv[1]);
     loadsize = b_load (fi,ptr_Buffer);
     if(loadsize == R_FAIL1)
       err_printf("%s%s%s",argv[0],": ","Error in loading buffer.");

 /* close the source file */	
 	fclose(fi);
 /*find the size of the file  */
    if (loadsize == LOAD_FAIL){
     printf("The input file %s %s\n", argv[1],"is not completely loaded.");
     printf("Input file size: %ld\n", get_filesize(argv[1]));
    } 
 /* set a mark at the last char in the buffer*/
  b_setmark(ptr_Buffer,b_size(ptr_Buffer));

 /* display the contents of the input buffer */	  
   display(ptr_Buffer);

 /* pack the buffer 
  * if possible, add end-of-file character (EOF) to the buffer
  * display again
  */
 if (MAIN_DEBUG) printf("--- BEFORE ENTERING B_PACK FUNCTION ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
  if(b_pack(ptr_Buffer)){         
    if(!b_addc(ptr_Buffer, EOF))
      err_printf("%s%s%s",argv[0],": ","Error in writing to buffer.");
    display(ptr_Buffer);
  }  

 if (MAIN_DEBUG) printf("--- BEFORE ENTERING B_FREE FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
 /* free the dynamic memory used by the buffer */  
  b_free(ptr_Buffer);
 /* make the buffer invalid
   It is not necessary here because the function terminates anyway,
   but will prevent run-time errors and crashes in future expansions
 */
  ptr_Buffer = NULL;
 /*return success */

  if (MAIN_DEBUG) printf("--- BEFORE EXITING MAIN FUNCTION ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
  return (0);
}

/* error printing function with variable number of arguments*/
void err_printf( char *fmt, ... ){
/*Initialize variable list */   
  va_list ap;
  va_start(ap, fmt);
     
  (void)vfprintf(stderr, fmt, ap);
   va_end(ap);

  /* Move to new line */
  if( strchr(fmt,'\n') == NULL )
     fprintf(stderr,"\n");
}

void display (Buffer *ptr_Buffer){
  printf("\nPrinting buffer parameters:\n\n");
  printf("The capacity of the buffer is:  %d\n",b_capacity(ptr_Buffer));
  printf("The current size of the buffer is:  %d\n",b_size(ptr_Buffer));
  printf("The operational mode of the buffer is:   %d\n",b_mode(ptr_Buffer));
  printf("The increment factor of the buffer is:  %lu\n",b_incfactor(ptr_Buffer));
  printf("The current mark of the buffer is:  %d\n",b_mark(ptr_Buffer));
/*printf("The reallocation flag is:   %d\n",b_rflag(ptr_Buffer));*/
  printf("\nPrinting buffer contents:\n\n");
  b_print(ptr_Buffer);
}

long get_filesize(char  *fname){
   FILE *input;
   long flength;
   input = fopen(fname, "r");
   if(input == NULL){
      err_printf("%s%s","Cannot open file: ",fname);  
	  return 0;
   }
   fseek(input, 0L, SEEK_END);
   flength = ftell(input);   
   fclose(input);
   return flength;
}

/* ======================= TEMP CODE REMOVE THIS EVENTUALLY ============================= */


/* DEV NOTE: GO THROUGH FUNCTIONS AFTER FIRST DRAFT DONE AND CHECK TO IF I CAN MAKE MORE */
/* EFFICIENT BY CALLING OTHER FUNCTIONS I'VE WRITTEN WITHIN OTHER ONES					 */
/* -> GO THROUGH FUNCTIONS AND CHECK/FIX DANGLING POINTERS AFTER FREEING MEMORY */


/****************************** - b_create() - ********************************
Purpose: Creates a BufferDescriptor structure, along with allocating the memory needed for
		 an array of characters. Based on the input parameters for the function, b_create 
		 assigns an initial capacity to the buffer (which decides on the size of the char 
		 array), along with its operational mode and increment factor for resizing.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: calloc(), malloc(), perror(), free(), sizeof().
Parameters: -> init_capacity
				type: short
			   	range: any short > 0
			-> inc_factor
				type: char
				range: 0 for fixed mode
					   1-255 for additive self-incrementing mode
					   1-100 for multiplicative self-incrementing mode
			-> o_mode
				type: char
				range: 'f' for fixed mode
					   'a' for additive self-incrementing mode
					   'm' for multiplicative self-incrementing mode
Return Value: Buffer* (Pointer of type Buffer* to the new BufferDescriptor structure created)
Algorithm: -> Makes sure that the init_capacity is >= 0
		   -> Allocates BufferDescriptor structure
		   -> Allocates char array
		   -> Based on inc_factor and o_mode, sets the increment factor and operational mode 
		      of the BufferDescriptor structure.
*****************************************************************************/
Buffer* b_create(short init_capacity,char inc_factor,char o_mode) {

	/* Pointer to buffer descriptor structure that will be allocated */
	Buffer* bDescriptor = NULL;

	if (DEBUG) printf("--- INSIDE B_CREATE ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check that init_capacity provided by user meets expectations */
	if (init_capacity < 0) return bDescriptor;
	
	if (DEBUG) printf("--- BEFORE ALLOCATING BUFFER DESCRIPTOR --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Allocate memory for one BufferDescriptor structure */
	if ((bDescriptor = (Buffer*)calloc(1,sizeof(Buffer))) == NULL){
		perror("calloc buffer descriptor");
		return bDescriptor;
	}

	if (DEBUG) printf("--- BEFORE ALLOCATING CHARACTER ARRAY --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Allocate memory for the character buffer array */
	if ((bDescriptor->cb_head = (char*)malloc(sizeof(char)*(init_capacity))) == NULL){
		perror("malloc buffer array");	
		free(bDescriptor);	
		bDescriptor = NULL;
		return bDescriptor;
	}

	if (DEBUG) printf("--- BEFORE SETTING OPERATIONAL MODE AND INITIAL INCREMENT --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Set the operational mode for the buffer, along with increment factor */
	if (o_mode == 'f') {
		bDescriptor->inc_factor = 0;
		bDescriptor->mode = 0;
	}
	else if (o_mode == 'a') {
		if ((inc_factor >= 1) && ((unsigned char)inc_factor <= 255)) {
			bDescriptor->inc_factor = inc_factor;
			bDescriptor->mode = 1;
		}
		else {
			printf("Increment factor must be inclusively between 1 and 255.\n");
			free(bDescriptor->cb_head);
			free(bDescriptor);
			bDescriptor = NULL;
			return bDescriptor;
		}
	}
	else if (o_mode == 'm') {
		if ((inc_factor >= 1) && (inc_factor <= 100)) {
			bDescriptor->inc_factor = inc_factor;
			bDescriptor->mode = -1;
		}
		else {
			printf("Increment factor must be inclusively between 1 and 100.\n");
			free(bDescriptor->cb_head);
			free(bDescriptor);
			bDescriptor = NULL;
			return bDescriptor;
		}
	}
    /*	else {
		printf("Something wrong in your command line input, try again.\n");
		free(bDescriptor->cb_head); 
		free(bDescriptor); 
		bDescriptor = NULL;
		return bDescriptor;
	}*/

	/* Buffer capacity is init_capacity provided by user */
	bDescriptor->capacity = init_capacity*sizeof(char);

	return bDescriptor;
}

/****************************** - b_addc() - *********************************
Purpose: Adds a character fed into the function, into the buffer. The function does this
		 in the context of the buffer's increment factor and incrementation mode. It then
		 returns a pointer to the BufferDescriptor structure.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: realloc(), perror()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const pBuffer
			-> symbol
				type: char
Return Value: pBuffer (Pointer of type pBuffer to the BufferDescriptor structure)
Algorithm: -> If addc_offset is smaller than the buffer capacity, the character is added
			  to the buffer at the location of the addc_offset.
			   -> Addc_offset is then incremented by one and pBD is returned.
		   -> If the buffer is at capacity...
		       -> If the buffer is in fixed mode, return NULL.
		       -> If the buffer is in additive mode...
		           -> If capacity is at max buffer size, return NULL.
		           -> Calculate new capacity
		           -> If new capacity is bigger than max buffer size, new capacity is 
		              changed to be the max buffer size.
		           -> Reallocate buffer according to new capacity.
		           -> Check if memory location of buffer has changed.
		               -> If so, set r_flag to 1.	
		           -> Add the character to the buffer at the location of the addc_offset.
		           -> Increment addc_offset
		           -> Update the BufferDescriptor with the new capacity.
			   -> If the buffer is in multiplicative mode...
		           -> If capacity is at max buffer size, return NULL.
		           -> Calculate new capacity
		           -> If new capacity is bigger than max buffer size, new capacity is 
		              changed to be the max buffer size.
		           -> Reallocate buffer according to new capacity.
		           -> Check if memory location of buffer has changed.
		               -> If so, set r_flag to 1.	
		           -> Add the character to the buffer at the location of the addc_offset.
		           -> Increment addc_offset
		           -> Update the BufferDescriptor with the new capacity.
		   -> Return pBD.        
*****************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {

	short numBytesToAdd = 0;
	short availableSpace = 0;
	short newCapacity = 0;
	char* memLocationCheck = NULL;

	if (DEBUG) printf("--- INSIDE B_ADDC FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	pBD->r_flag = 0;

	/* Add new character in if there's space in the buffer */ 
	if (pBD->addc_offset < pBD->capacity) {
		if (DEBUG) printf("--- ADDING NEW CHARACTER IN (THERE IS CAPACITY) AT POSITION %d --- \n",pBD->addc_offset); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset++;
		return pBD;
	}

	/* If there isn't calculate how much more you need and resize it, depending on operational mode */
	else {
		/********************
		 * -- FIXED MODE -- *
		 ********************/
		if (pBD->mode == 0) {
			return NULL;
		}

		/*****************************************
		 * -- ADDITIVE SELF-INCREMENTING MODE -- *
		 *****************************************/
		else if (pBD->mode == 1) {

			/* If capacity is already at max size, can't add new character. */
			if (pBD->capacity == SHRT_MAX) return NULL;

			/* Calculate new capacity */
			newCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;

			/* This accounts for the fact that c characters are signed, could overflow */
			if (newCapacity <= 0) return NULL;

			if (newCapacity > SHRT_MAX) newCapacity = SHRT_MAX;

			if (DEBUG) printf("--- BEFORE REALLOC IN ADDITIVE SELF INCREMENTING MODE --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

			/* Record current memory location of the buffer descriptor for later comparison after realloction */
			memLocationCheck = pBD->cb_head;

			if ((pBD->cb_head = (char*)realloc(pBD->cb_head,newCapacity)) == NULL){ 
				perror("realloc buffer array");
				return NULL;
			}

			/* Compare memory location of preRecorded pointer to new location of the character buffer. */
			/* If they are different, set r_flag to 1												   */
			if (memLocationCheck != pBD->cb_head) pBD->r_flag = SET_R_FLAG;				
			
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset++;
			pBD->capacity = newCapacity;

		}
		/***********************************************
		 * -- MULTIPLICATIVE SELF-INCREMENTING MODE -- *
		 ***********************************************/
		else if (pBD->mode == -1) {

			/* If capacity is already at max size, can't add new character. */
			if (pBD->capacity == SHRT_MAX) return NULL; 

			availableSpace = SHRT_MAX - pBD->capacity;
			numBytesToAdd = (availableSpace*(unsigned char)pBD->inc_factor)/100;
			newCapacity = pBD->capacity + numBytesToAdd;

			if (newCapacity > SHRT_MAX) newCapacity = SHRT_MAX;

			if (DEBUG) printf("--- BEFORE REALLOC IN MULTIPLICATIVE SELF INCREMENTING MODE --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

			/* Record current memory location of the buffer descriptor for later comparison after realloction */
			memLocationCheck = pBD->cb_head;

			if ((pBD->cb_head = (char*)realloc(pBD->cb_head,newCapacity)) == NULL) {
				perror("realloc buffer array");
				return NULL;
			}

			/* Compare memory location of preRecorded pointer to new location of the character buffer. */
			/* If they are different, set r_flag to 1												   */
			if (memLocationCheck != pBD->cb_head) pBD->r_flag = SET_R_FLAG;				
			
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset++;
			pBD->capacity = newCapacity;
		}
	}
	return pBD;
}


/*****************************************************************************
Purpose: Keeps the memory content of the buffer intact, but modifies the buffer descriptor
         paramaters to make it appear that the buffer is empty, such that a proceeding 
         call to b_addc() will put the character at the start of the array.
Author: Gabriel Bourget	
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const pBuffer
Return Value: int (status int representing success or failure)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Set addc_offset to 0
		   -> Set getc_offset to 0
*****************************************************************************/
int b_reset(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_RESET FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_reset.\n");
		return R_FAIL1;
	}
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	return 0; 
}

/*****************************************************************************
Purpose: Deallocates memory in the heap associated with the buffer array and the buffer
         descriptor structure.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: free()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: --- (void)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return.
		   -> free pBD->cb_head (array mem)
           -> free pBD (buff descriptor mem)
*****************************************************************************/
void b_free(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_FREE FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	if ((pBD == NULL) || (pBD->cb_head == NULL)) return;

	if (MAIN_DEBUG) printf("Address of character array: %p\n",(void*)pBD->cb_head); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Release memory for character array and BufferDescriptor */
	free(pBD->cb_head);

	if (MAIN_DEBUG) printf("--- GOT PAST FREE(PBD->CB_HEAD) ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	if (MAIN_DEBUG) printf("Address of buffer descriptor structure: %p\n",(void*)pBD);

	free(pBD);

	if (MAIN_DEBUG) printf("--- GOT PAST FREE(PBD) --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */
}

/*****************************************************************************
Purpose: Determines if the buffer capacity has reached the maximum buffer size.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: integer (status int reporting if buffer is full or not)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> If buffer capacity is smaller than max buffer size, return 0.
           -> If it's equal or bigger, return 1.
*****************************************************************************/
int b_isFull(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_ISFULL FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_isFull.\n");
		return R_FAIL1;
	}

	if (pBD->capacity < SHRT_MAX) return 0;

	else return 1;
}

/*****************************************************************************
Purpose: Determines and retrieves the current size of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: short (current size of the buffer in characters)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Return the current addc_offset
*****************************************************************************/
short b_size(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_SIZE FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_size.\n");
		return R_FAIL1;
	}

	return pBD->addc_offset; /* DEV NOTE: NOT SURE ABOUT THIS, MIGHT HAVE TO RETURN ADDC_OFFSET+1 ??? */
}

/*****************************************************************************
Purpose: Determines and retrieves the current capacity of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: short (current capacity of the buffer in bytes)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Return the current capacity of the buffer.
*****************************************************************************/
short b_capacity(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_CAPACITY FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_capacity.\n");
		return R_FAIL1;
	}

	return pBD->capacity;
}

/*****************************************************************************
Purpose: Sets the mark_offset to the spot specified in the function parameters.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
			-> mark (Spot where mark_offset is to be placed)
			    type: short
			    range: > 0 && < addc_offset
Return Value: short (returns the newly placed mark_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Do bounds checking on the inputted mark parameter.
               -> If out of bounds, return R_FAIL1.
           -> Set mark_offset to mark parameter.
           -> Return newly placed mark_offset.
*****************************************************************************/
short b_setmark(Buffer* const pBD,short mark) {

	if (DEBUG) printf("--- INSIDE B_SETMARK FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_setmark.\n");
		return R_FAIL1;
	}

	/* Check validity of 'mark' input parameter */
	if ((mark < 0) || (mark > pBD->addc_offset)) {
		printf("Problem with entered mark parameter.\n");
		return R_FAIL1;
	}

	pBD->mark_offset = mark;
	return pBD->mark_offset;
}

/*****************************************************************************
Purpose: Determines and retrieves the mark_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: short (returns the current mark_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Return the current mark_offset of the buffer descriptor.
*****************************************************************************/
short b_mark(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_MARK FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mark.\n");
		return R_FAIL1;
	}

	return pBD->mark_offset;
}

/*****************************************************************************
Purpose: Determines and retrieves the current incrementation mode of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: int (representing the current incrementation mode of the buffer)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
           -> Return the current incrementation mode of the buffer.
*****************************************************************************/
int b_mode(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_MODE FUNCTION --- \n");

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mode.\n");
		return R_FAIL1;
	}

	return pBD->mode;
}

/*****************************************************************************
Purpose: Determines and retrieves the current increment factor of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: size_t (representing the current incrementation factor of the buffer)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return 256.
		   -> Return the current incrementation factor of the buffer.
*****************************************************************************/
size_t b_incfactor(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_INCFACTOR FUNCTION ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_incfactor.\n");
		return 256;
	}

	return pBD->inc_factor; 
}

/*****************************************************************************
Purpose: Loads in a file character-by-character into the buffer until the
		 end of file is reached, or the buffer can no longer add any more content.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf(), feof(), fgetc(), b_addc()
Parameters: -> fi (pointer to the file being read in)
			    type: FILE*
		    -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: int (representing the number of characters added to the buffer)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
           -> While the end of file hasn't been reached...
		       -> Get next character from the input file.
		       -> Attempt to add it to the buffer.
		           -> If this is unsuccessful, return LOAD_FAIL.
		       -> Increment the number of characters added by one.
		   -> Return the number of characters added to the array.
*****************************************************************************/
int b_load(FILE* const fi,Buffer* const pBD) {
	
	char currentChar = '\0';
	int numCharactersAdded = 0;
	pBuffer b_addcReturnPointer = NULL;

	if (DEBUG) printf("--- INSIDE B_LOAD FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mode.\n");
		return R_FAIL1;
	}

	while (feof(fi) == 0) {
		currentChar = fgetc(fi);
		b_addcReturnPointer = b_addc(pBD,currentChar);
		if (b_addcReturnPointer == NULL) {
			return LOAD_FAIL;
		}
		numCharactersAdded++;
	}
	return numCharactersAdded;
}

/*****************************************************************************
Purpose: Checks to see if the buffer is empty. 
Author: Gabriel Bourget	
History/Version: v1.0	
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: int (status int representing whether buffer is empty)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> If addc_offset is 0, return 0.
		   -> If addc_offset is anything else, return 1.
*****************************************************************************/
int b_isempty(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_ISEMPTY FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mode.\n");
		return R_FAIL1;
	}

	if (pBD->addc_offset == 0) return 0;
	else return 1;
}

/*****************************************************************************
Purpose: Indicates whether or not the end of the buffer content has been reached. If
		 eob is 1, b_getc() should not be called until getc_offset has been reset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: int (representing the eob flag (1 or 0))
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
           -> Return the buffer descriptor's eob flag.
*****************************************************************************/
int b_eob(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_EOB FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mode.\n");
		return R_FAIL1;
	}

	return pBD->eob;
}

/*****************************************************************************
Purpose: Gets the character from the array being indicated by getc_offset. The 
         function also considers whether getc_offset has reached the end of the
         buffer content.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: char (representing the character retrieved from the buffer)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL2.
		   -> If getc_offset has reached addc_offset, set the eob flag and return -1.
		   -> Assign the character located by getc_offset into a temporary char variable.
		   -> Increment getc_offset by 1.
		   -> Return the retrieved character.
*****************************************************************************/
char b_getc(Buffer* const pBD) {

	char returnChar = '\0';

	if (DEBUG) printf("--- INSIDE B_GETC FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mode.\n");
		return R_FAIL2;
	}

	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = 1;
		return -1;
	}

	pBD->eob = 0; /* DEV NOTE: ON FIRST GLANCE, I DON'T KNOW WHY THIS IS THERE, VERY SUSPICIOUS LINE OF CODE */
	returnChar = pBD->cb_head[pBD->getc_offset]; /* DEV NOTE: PROBLEMS MIGHT FIX BY CHANGING THIS TO pBD->cb_head[pBD->getc_offset] */
	pBD->getc_offset++;
	return returnChar;
}

/*****************************************************************************
Purpose: Goes through the buffer and prints out its contents, character by character, 
         starting at the location of getc_offset, and going all the way to the end of 
         buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf(), b_eob(), b_getc()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: int (representing the number of characters printed out)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> If the buffer has no content, print out that it's empty.
		   -> Store the current getc_offset into a temporary variable.
		       -> Set getc_offset to 0.
		   -> While b_eob() doesn't return 1...
		       -> Print a character by calling b_getc() inside printf().
		       -> Increment the number of characters printed in a temporary variable.
		   -> Print a newline to distinguish the buffer content from further output.
		   -> Reset getc_offset to where it was before printing.
		   -> Return the number of characters that were printed out.
*****************************************************************************/
int b_print(Buffer* const pBD) {
	
	short bufferSize = 0;
	int endOfBuffer = 0;
	short getcOffsetTemp = 0;
	int numCharsPrinted = 0;

	if (DEBUG) printf("--- INSIDE B_PRINT FUNCTION --- \n");

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_print.\n");
		return R_FAIL1;
	}

	/* Check to see if the buffer is empty. */
	if ((bufferSize = b_size(pBD)) == 0) {
		printf("The buffer is empty.\n\n");
	}

	/* Store current getc_offset in temp variable */
	getcOffsetTemp = pBD->getc_offset;
	pBD->getc_offset = 0;

	/* Loop from start to end of content in buffer, */
	/* printing out each character one at a time.   */
	while ((endOfBuffer = b_eob(pBD)) != 1) {
		printf("%c",b_getc(pBD));
		/*if (SPECIAL) printf("%c =[ GETC_OFFSET = %d ]= - =[ADDC_OFFSET = %d]=\n",b_getc(pBD),pBD->getc_offset,pBD->addc_offset);*/
		numCharsPrinted++;
	}
	printf("\n");

	/* Restore original value of getc_offset */
	pBD->getc_offset = getcOffsetTemp;

	return numCharsPrinted;
}


/*****************************************************************************
Purpose: Packs the capacity of the buffer down to the location of addc_offset + 1, 
         reallocating the memory for the character buffer accordingly.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf() perror(), sizeof()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: Buffer* (Pointer of type Buffer* to the new BufferDescriptor structure created)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
           -> If addc_offset + 1 <= max buffer size...
           	   -> Record the current memory location of the buffer into a temporary pointer.
               -> Reallocate buffer to have a size of (addc_offset + 1)*(sizeof(char)).
               -> Set new capacity in the buffer descriptor.
           -> If addc_offset + 1 > max buffer size...
               -> Record the current memory location of the buffer into a temporary pointer.
               -> Reallocate buffer to have a size of (addc_offset)*(sizeof(char)).
           -> Check if memory location of buffer has changed.
		       -> If so, set r_flag to 1.	
		   -> Set the eob flag to 0.
		   -> Return pBD.
*****************************************************************************/
Buffer* b_pack(Buffer* const pBD) {

	char* memLocationCheck = NULL;

	if (DEBUG) printf("--- INSIDE B_PACK FUNCTION ---\n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_pack.\n");
		return NULL;
	}

	/* Check to see if you can pack buffer to addc_offset + 1. If so, proceed */
	if (pBD->addc_offset + 1 <= SHRT_MAX) {

	    memLocationCheck = pBD->cb_head;

		if ((pBD->cb_head = realloc(pBD->cb_head,(pBD->addc_offset + 1)*sizeof(char))) == NULL) { /* DEV NOTE: MAY NEED TO REALLOCATE IN TERMS OF BYTES, NOT CHARS */
			perror("realloc buffer array");
			return NULL;
		}

		/* Set new capacity */
		pBD->capacity = pBD->addc_offset + 1;
	}

	else {

		memLocationCheck = pBD->cb_head;

		if ((pBD->cb_head = realloc(pBD->cb_head,pBD->addc_offset*sizeof(char))) == NULL) { /* DEV NOTE: MAY NEED TO REALLOCATE IN TERMS OF BYTES, NOT CHARS */
			perror("realloc buffer array");									   /* ALSO, MIGHT NOT EVEN NEED TO REALLOCATE HERE */
			return NULL;
		}
	}

	if (memLocationCheck != pBD->cb_head) pBD->r_flag = SET_R_FLAG;

	pBD->eob = 0; /* DEV NOTE: INVESTIGATE THIS LINE FURTHER */


	return pBD;
}


/*****************************************************************************
Purpose: Determines and retrieves the current r_flag from the buffer descriptor.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: char (representing the current r_flag)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Return the current r_flag.
*****************************************************************************/
char b_rflag(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_RFLAG FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_rflag.\n");
		return R_FAIL1;
	}

	return pBD->r_flag;
}

/*****************************************************************************
Purpose: Decrements getc_offset by one.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: short (representing the decremented getc_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> If the current getc_offset is already 0, return it.
		   -> Decrement getc_offset.
		   -> Return getc_offset.
*****************************************************************************/
short b_retract(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_RETRACT FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_retract.\n");
		return R_FAIL1;
	}

	if (pBD->getc_offset == 0) return pBD->getc_offset;

	pBD->getc_offset--;
	return pBD->getc_offset;
}

/*****************************************************************************
Purpose: Retracts getc_offset to the current value of the mark_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: short (representing the newly positioned getc_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> If the current getc_offset is equal to mark_offset, return getc_offset.
		   -> Assign getc_offset to the value of mark_offset.
		   -> Return getc_offset.
*****************************************************************************/
short b_retract_to_mark(Buffer* const pBD) {

	/* DEV NOTE: WHAT IF THE MARK OFFSET IS GREATER THAN THE GETC OFFSET? */

	if (DEBUG) printf("--- INSIDE B_RETRACT_TO_MARK FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_retract_to_mark.\n");
		return R_FAIL1;
	}

	if (pBD->getc_offset == pBD->mark_offset) return pBD->getc_offset;

	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}

/*****************************************************************************
Purpose: Determines and retrieves the current getc_offset from the buffer descriptor.
Author: Gabriel Bourget 
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: short (representing the current getc_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
		   -> Return getc_offset.
*****************************************************************************/
short b_getcoffset(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_GETCOFFSET FUNCTION --- \n"); 

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_getcoffset.\n");
		return R_FAIL1;
	}

	return pBD->getc_offset;
}


/*****************************************************************************
Purpose: Determines and retrieves the current memory location of the buffer array.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: printf()
Parameters: -> pBD (pointer to BufferDescriptor structure)
			    type: const Buffer*
Return Value: char* (representing the current address of the buffer array)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return NULL.
		   -> Return cb_head.
*****************************************************************************/
char* b_cbhead(Buffer* const pBD) {

	if (DEBUG) printf("--- INSIDE B_CBHEAD FUNCTION --- \n"); /* DEBUG COMMENT - DELETE BEFORE HANDING IN ASSIGNMENT */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) { 
		printf("Run time error encountered in b_mode.\n");
		return NULL;
	}

	return pBD->cb_head;
}












