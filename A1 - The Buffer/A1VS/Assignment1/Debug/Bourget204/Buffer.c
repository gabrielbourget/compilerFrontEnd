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
Buffer* b_create(short init_capacity, char inc_factor, char o_mode) {

	/* Pointer to buffer descriptor structure that will be allocated */
	Buffer* bDescriptor = NULL;

	/* Check that init_capacity provided by user meets expectations */
	if (init_capacity < 0) return bDescriptor;

	/* Allocate memory for one BufferDescriptor structure */
	if ((bDescriptor = (Buffer*)calloc(1, sizeof(Buffer))) == NULL){
		perror("calloc buffer descriptor");
		return bDescriptor;
	}

	/* Allocate memory for the character buffer array */
	if ((bDescriptor->cb_head = (char*)malloc(sizeof(char)*(init_capacity))) == NULL) {
		perror("malloc buffer array");
		free(bDescriptor);
		bDescriptor = NULL;
		return bDescriptor;
	}

	/* Set the operational mode for the buffer, along with increment factor */
	/* Fixed mode */
	if (o_mode == 'f' || inc_factor == 0) {

		/* Anticipates a case from the big test files. */
		if (init_capacity == 0) {
			free(bDescriptor->cb_head);
			free(bDescriptor);
			bDescriptor = NULL;
			return bDescriptor;
		}

		bDescriptor->inc_factor = 0;
		bDescriptor->mode = 0;
	}
	/* Additive mode */
	else if (o_mode == 'a') {
		/* inc_factor in range for additive mode */
		if (((unsigned char)inc_factor >= 1) && ((unsigned char)inc_factor <= 255)) {
			bDescriptor->inc_factor = inc_factor;
			bDescriptor->mode = 1;
		}
		/* It's not. */
		else {
			free(bDescriptor->cb_head);
			free(bDescriptor);
			bDescriptor = NULL;
			return bDescriptor;
		}
	}
	/* Multiplicative mode */
	else if (o_mode == 'm') {
		/* inc factor in range for multiplicative mode */
		if ((inc_factor >= 1) && (inc_factor <= 100)) {
			bDescriptor->inc_factor = inc_factor;
			bDescriptor->mode = -1;
		}
		/* It's not. */
		else {
			free(bDescriptor->cb_head);
			free(bDescriptor);
			bDescriptor = NULL;
			return bDescriptor;
		}
	}

	/* Buffer capacity is set to init_capacity provided by user */
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

	short numBytesToAdd = 0;  /* Tracks how many bytes there are to add to the buffer */
	short availableSpace = 0; /* Represents the amount of available space there is in the buffer */
	unsigned short newCapacity = 0;    /* Represents the new, updated capacity after a resize */
	char* memLocationCheck = NULL; /* Used to see if the memory location of the buffer changes upon a resize */
	char* temp = NULL; /* Temp pointer to guard against dangling pointers */

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;

	/* Set r_flag to 0 */
	pBD->r_flag = 0;

	/* If there's space, add new character in the buffer, then increment addc_offset*/
	if (pBD->addc_offset < pBD->capacity) {
		pBD->cb_head[pBD->addc_offset++] = symbol;
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

			/* If new capacity is bigger than max buff size, return NULL */
			if (newCapacity > SHRT_MAX) return NULL;

			/* Record current memory location of the buffer descriptor for later comparison after realloction */
			memLocationCheck = pBD->cb_head;

			/* Reallocate the buffer according to the new capacity */
			if ((temp = (char*)realloc(pBD->cb_head, newCapacity*sizeof(char))) == NULL){
				perror("realloc buffer array");
				return NULL;
			}

			/* Compare memory location of preRecorded pointer to new location of the character buffer. */
			/* If they are different, set r_flag to 1												   */
			if (memLocationCheck != temp) pBD->r_flag = SET_R_FLAG;

			/* Assign cb_head to new address, add new character in the buffer, then increment addc_offset*/
			pBD->cb_head = temp;
			pBD->cb_head[pBD->addc_offset++] = symbol;
			pBD->capacity = newCapacity*sizeof(char);

		}
		/***********************************************
		* -- MULTIPLICATIVE SELF-INCREMENTING MODE -- *
		***********************************************/
		else if (pBD->mode == -1) {

			/* If capacity is already at max size, can't add new character. */
			if (pBD->capacity == SHRT_MAX) return NULL;

			/* Calculate new capacity */
			availableSpace = SHRT_MAX - pBD->capacity;
			numBytesToAdd = (availableSpace*(unsigned char)pBD->inc_factor) / 100;
			if (numBytesToAdd == 0) newCapacity = SHRT_MAX;
			else newCapacity = pBD->capacity + numBytesToAdd;

			/* If new capacity is bigger than max buff size, set new capacity to max buff size */
			if (newCapacity > SHRT_MAX) newCapacity = SHRT_MAX;

			/* Record current memory location of the buffer descriptor for later comparison after realloction */
			memLocationCheck = pBD->cb_head;

			/* Reallocate the buffer according to the new capacity */
			if ((temp = (char*)realloc(pBD->cb_head, newCapacity*sizeof(char))) == NULL) {
				perror("realloc buffer array");
				return NULL;
			}

			/* Compare memory location of preRecorded pointer to new location of the character buffer. */
			/* If they are different, set r_flag to 1												   */
			if (memLocationCheck != temp) pBD->r_flag = SET_R_FLAG;

			/* Assign cb_head to new address, add new character in the buffer, then increment addc_offset*/
			pBD->cb_head = temp;
			pBD->cb_head[pBD->addc_offset++] = symbol;
			pBD->capacity = newCapacity*sizeof(char);
		}
	}
	return pBD;
}


/********************************b_reset()**************************************
Purpose: Keeps the memory content of the buffer intact, but modifies the buffer descriptor
paramaters to make it appear that the buffer is empty, such that a proceeding
call to b_addc() will put the character at the start of the array.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const pBuffer
Return Value: int (status int representing success or failure)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Set addc_offset to 0
-> Set getc_offset to 0
*****************************************************************************/
int b_reset(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;

	/* Reset relevant buffer descriptor parameters to give appearance of empty buffer. */
	pBD->mark_offset = 0;
	pBD->r_flag = 0;
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	return 0;
}

/*********************************b_free()************************************
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

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) return;

	/* Release memory for character array and BufferDescriptor */
	free(pBD->cb_head);
	free(pBD);
}

/******************************b_isFull()*************************************
Purpose: Determines if the buffer capacity has reached the maximum buffer size.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: integer (status int reporting if buffer is full or not)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> If buffer capacity is smaller than max buffer size, return 0.
-> If it's equal or bigger, return 1.
*****************************************************************************/
int b_isFull(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	if (pBD->addc_offset < pBD->capacity) return 0;

	else return 1;
}

/*******************************b_size()**************************************
Purpose: Determines and retrieves the current size of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: short (current size of the buffer in characters)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return the current addc_offset
*****************************************************************************/
short b_size(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->addc_offset;
}

/*******************************b_capacity()**********************************
Purpose: Determines and retrieves the current capacity of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: short (current capacity of the buffer in bytes)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return the current capacity of the buffer.
*****************************************************************************/
short b_capacity(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->capacity;
}

/******************************b_setmark()************************************
Purpose: Sets the mark_offset to the spot specified in the function parameters.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
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
short b_setmark(Buffer* const pBD, short mark) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;

	/* Check validity of 'mark' input parameter */
	if ((mark < 0) || (mark > pBD->addc_offset)) return R_FAIL1;

	pBD->mark_offset = mark;
	return pBD->mark_offset;
}

/*******************************b_mark()**************************************
Purpose: Determines and retrieves the mark_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: short (returns the current mark_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return the current mark_offset of the buffer descriptor.
*****************************************************************************/
short b_mark(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->mark_offset;
}

/*********************************b_mode()************************************
Purpose: Determines and retrieves the current incrementation mode of the buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: int (representing the current incrementation mode of the buffer)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return the current incrementation mode of the buffer.
*****************************************************************************/
int b_mode(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->mode;
}

/******************************b_incfactor()**********************************
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

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return 256;
	return (unsigned char)pBD->inc_factor;
}

/********************************b_load()*************************************
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
int b_load(FILE* const fi, Buffer* const pBD) {

	/* Holds the current character to be added */
	char currentChar = '\0';
	/* Tracks how many characters were added to the buffer */
	int numCharactersAdded = 0;
	/* Tracks the return value of calling b_addc() */
	pBuffer b_addcReturnPointer = NULL;

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) return R_FAIL1;

	/* Loop through file and get each character for the buffer. */
	while (feof(fi) == 0) {
		currentChar = (char)fgetc(fi);

		if (currentChar == EOF) break;

		b_addcReturnPointer = b_addc(pBD, currentChar);
		if (b_addcReturnPointer == NULL) return LOAD_FAIL;

		++numCharactersAdded;
	}
	return numCharactersAdded;
}

/*******************************b_isempty()***********************************
Purpose: Checks to see if the buffer is empty.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: int (status int representing whether buffer is empty)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> If addc_offset is 0, return 0.
-> If addc_offset is anything else, return 1.
*****************************************************************************/
int b_isempty(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;

	if (pBD->addc_offset == 0) return 1;
	else return 0;
}

/********************************b_eob()**************************************
Purpose: Indicates whether or not the end of the buffer content has been reached. If
eob is 1, b_getc() should not be called until getc_offset has been reset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: int (representing the eob flag (1 or 0))
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return the buffer descriptor's eob flag.
*****************************************************************************/
int b_eob(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->eob;
}

/*********************************b_getc()************************************
Purpose: Gets the character from the array being indicated by getc_offset. The
function also considers whether getc_offset has reached the end of the
buffer content.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: char (representing the character retrieved from the buffer)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL2.
-> If getc_offset has reached addc_offset, set the eob flag and return R_FAIL1.
-> Assign the character located by getc_offset into a temporary char variable.
-> Increment getc_offset by 1.
-> Return the retrieved character.
*****************************************************************************/
char b_getc(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if (pBD == NULL) return R_FAIL2;

	/* See if you've reached end-of-buffer. */
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = 1;
		return R_FAIL1;
	}

	pBD->eob = 0;

	return pBD->cb_head[pBD->getc_offset++];
}

/*********************************b_print()************************************
Purpose: Goes through the buffer and prints out its contents, character by character,
starting at the location of getc_offset, and going all the way to the end of
buffer.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: b_eob(), b_getc()
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

	short getcOffsetTemp = 0;	/* Records current location of getc_offset */
	int numCharsPrinted = 0;    /* Records the number of characters printed */
	char tempChar = '\0';       /* Records the character coming in from b_getc() */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) return R_FAIL1;

	/* Check to see if the buffer is empty. */
	if (b_isempty(pBD) == 1) {
		printf("The buffer is empty.\n");
		return numCharsPrinted;
	}

	/* Store current getc_offset in temp variable */
	getcOffsetTemp = pBD->getc_offset;
	pBD->getc_offset = 0;

	/* Loop from start to end of content in buffer, */
	/* printing out each character one at a time    */
	/* until the end of the buffer is reached.      */
	while (b_eob(pBD) != 1) {

		/* Store retrieved character in temp char var and check to see if b_getc() worked as expected. */
		tempChar = b_getc(pBD);
		if (tempChar == R_FAIL2) return R_FAIL1;

		/* Verify that you have not reached the end of buffer before printing the character */
		if (b_eob(pBD) != 1) printf("%c", tempChar);
		++numCharsPrinted;
	}
	printf("\n");

	/* Restore original value of getc_offset */
	pBD->getc_offset = getcOffsetTemp;

	return numCharsPrinted;
}


/*******************************b_pack()**************************************
Purpose: Packs the capacity of the buffer down to the location of addc_offset + 1,
reallocating the memory for the character buffer accordingly.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: perror(), sizeof()
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

	char* memLocationCheck = NULL; /* Used to see if the memory location of the buffer changes upon a resize */
	char* temp = NULL; /* Temp pointer to guard against dangling pointers */

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) return NULL;

	/* Check to see if you can pack buffer to addc_offset + 1. If so, proceed */
	if (pBD->addc_offset + 1 <= SHRT_MAX) {

		memLocationCheck = pBD->cb_head;

		/* Record current memory location of the buffer descriptor for later comparison after realloction */
		if ((temp = realloc(pBD->cb_head, (pBD->addc_offset + 1)*sizeof(char))) == NULL) {
			perror("realloc buffer array");
			return NULL;
		}

		/* Set new capacity */
		pBD->capacity = pBD->addc_offset + 1;
	}
	else {

		/* Record current memory location of the buffer descriptor for later comparison after realloction */
		memLocationCheck = pBD->cb_head;

		if ((temp = realloc(pBD->cb_head, pBD->addc_offset*sizeof(char))) == NULL) {
			perror("realloc buffer array");
			return NULL;
		}
	}

	/* Compare memory location of preRecorded pointer to new location of the character buffer. */
	/* If they are different, set r_flag to 1												   */
	if (memLocationCheck != pBD->cb_head) pBD->r_flag = SET_R_FLAG;

	/* Assign cb_head to new address, add new character in the buffer, then increment addc_offset*/
	pBD->cb_head = temp;

	pBD->eob = 0;

	return pBD;
}


/********************************b_rflag()************************************
Purpose: Determines and retrieves the current r_flag from the buffer descriptor.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: char (representing the current r_flag)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return the current r_flag.
*****************************************************************************/
char b_rflag(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->r_flag;
}

/******************************b_retract()************************************
Purpose: Decrements getc_offset by one.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: short (representing the decremented getc_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> If the current getc_offset is already 0, return it.
-> Decrement getc_offset.
-> Return getc_offset.
*****************************************************************************/
short b_retract(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;

	/* Can't have getc_offset be < 0. */
	if (pBD->getc_offset == 0) return pBD->getc_offset;

	/* Decrement getc_offset. */
	pBD->getc_offset--;
	return pBD->getc_offset;
}

/***************************b_retract_to_mark()*******************************
Purpose: Retracts getc_offset to the current value of the mark_offset.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: short (representing the newly positioned getc_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> If the current getc_offset is equal to mark_offset, return getc_offset.
-> Assign getc_offset to the value of mark_offset.
-> Return getc_offset.
*****************************************************************************/
short b_retract_to_mark(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;

	/* Nothing is to be done is getc_offset is already equal to mark_offset */
	if (pBD->getc_offset == pBD->mark_offset) return pBD->getc_offset;

	/* Retract to mark_offset */
	pBD->getc_offset = pBD->mark_offset;

	return pBD->getc_offset;
}

/****************************b_getcoffset()***********************************
Purpose: Determines and retrieves the current getc_offset from the buffer descriptor.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: short (representing the current getc_offset)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return R_FAIL1.
-> Return getc_offset.
*****************************************************************************/
short b_getcoffset(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor is null first. */
	if (pBD == NULL) return R_FAIL1;
	return pBD->getc_offset;
}


/********************************b_cbhead()***********************************
Purpose: Determines and retrieves the current memory location of the buffer array.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: ---
Parameters: -> pBD (pointer to BufferDescriptor structure)
type: const Buffer*
Return Value: char* (representing the current address of the buffer array)
Algorithm: -> If the pointer to the buffer structure or buffer are NULL, return NULL.
-> Return cb_head.
*****************************************************************************/
char* b_cbhead(Buffer* const pBD) {

	/* Check to see if pointer to BufferDescriptor or array is null first. */
	if ((pBD == NULL) || (pBD->cb_head == NULL)) return NULL;
	return pBD->cb_head;
}












