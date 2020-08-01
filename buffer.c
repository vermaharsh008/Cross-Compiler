/*
File name: Buffer.c
Compiler: MS Visual Studio 2015
Author: Harshdeep Verma - 040880851
Course: CST 8152 – Compilers, Lab Section: 13
Assignment: 1
Date: 01/10/2018
Professor: Sv. Ranev
Purpose: This file implements a buffer that can operate in three different modes:
fixed, additive and multiplicative buffer. It can read input form a
file as well as print output to a file.
Function list: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(),
b_capacity(), b_mark(), b_mode(), b_incfactor(), b_load(),
b_isempty(), b_getc(), b_eob(), b_print(), b_compact(),
b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(),
b_location()
*/
#include "buffer.h"
#include <stdio.h>
#include <limits.h> 
/*
Purpose: This function validates the capacity, inc_factor and mode if they
are valid then it intializes all the variables of the struct Buffer,
it also mallocs space for the actual buffer.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: Calloc(), Malloc(), Sizeof()
Parameters: Short int_capacity - The capacity of the buffer,
Char inc_factor - Increment factor by which the sapce is
increased if the buffer is full,
Char o_mode - The mode for the type of buffer created.
Return value: Returns a pointer to the Buffer struct created.
Algorithm: Checks if the capacity is between 0 bytes and the maximum value allowed
for a short - 1 bytes,
Checks if the increment factor is between 0 bytes and 255 bytes,
Checks if the capacity for fixed mode is greater than 0 bytes else
NULL is returned,
Assigns additive mode inc_factor to the parameter value,
Checks if the inc_factor is for multiplicative is more than 100 then
NULL is returned,
Space for a Buffer Struct is calloced according to the size of the Struct,
Space for the char buffer is malloced according to the capacity.
*/

Buffer *b_allocate(short int_capacity, char inc_factor, char o_mode) {
	char inc_factor_temp = 0;
	/*Stores the temporary value of inc_factor so that it can be validated.*/
	char mode_temp = 0;
	/*Stores the temporary value of mode so that it can be validated.*/
	char* head_temp;
	/*Stores the temporary character buffer*/
	Buffer *buffer;
	/*Pointer to a buffer struct*/
	if (int_capacity > MAX_VAL || int_capacity < MIN_VAL) {
		/*Capacity is checked if it lies between 0 and Max_val.*/
		return NULL;
	}
	if ((unsigned char)inc_factor == 0 || o_mode == 'f') {
		/*If the inc_factor is 0 then the mode is by default fixed.*/
		o_mode = 'f';
		mode_temp = FIXED_MODE;
	}
	switch (o_mode) {
	case 'f': /*If the mode is fixed then the inc_factor is set to 0.*/
		if (int_capacity == MIN_VAL) {
			return NULL;
		}
		break;
	case 'a':
		mode_temp = ADDITIVE_MODE;
		/*If the mode is additive then the casted inc_facotr is set to a temp variable.*/
		inc_factor_temp = (unsigned char)inc_factor;
		break;
	case 'm':
		if ((unsigned char)inc_factor > MAX_INC_FACTOR_M) {
			/*Checking is the inc_factor is not more than 100 for
			multiplicative mode.*/
			return NULL;
		}
		mode_temp = MULTIPLICATIVE_MODE;
		inc_factor_temp = (unsigned char)inc_factor;
		break;
	default:
		return NULL;
	}
	buffer = (Buffer *)calloc(1, sizeof(Buffer));
	/*A buffer is calloced equal to the space of the buffer struct.*/
	if (buffer == NULL) { /*The process was not success then NULL is returned.*/
		return NULL;
	}
	buffer->flags = DEFAULT_FALGS; /*All the buffer's variable are initialized.*/
	buffer->capacity = int_capacity;
	buffer->inc_factor = inc_factor_temp;
	buffer->mode = mode_temp;
	head_temp = (char *)malloc(int_capacity);
	/*Space for the actual character buffer is malloced according to
	the capacity.*/
	if (head_temp == NULL) {
		free(buffer);
		return NULL;
	}
	buffer->cb_head = head_temp;
	head_temp = NULL;
	return buffer;
}

/*
Purpose: This function adds the character passed to the buffer. if the buffer
is full then based on the mode it increases the size of the buffer
and then adds the character. The capacity is not changed for the
fixed mode.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: Realloc()
Parameters: pBuffer const pBD - a pointer to the Buffer struct,
Char symbol - the character which is to be added to the buffer.
Return value: A pointer to the Buffer struct is returned Or NULL.
Algorithm: Checks if the buffer is present,
If the add_offset is less than the capacity then the character
is simply added to the buffer,
If the mode is fixed the function returns NULL,
If the mode is additive then the inc_factor is added to the
capacity as long as its less the the maximum allowed value
for short - 1,
If the mode is multiplicative then the capacity is incremented
based on a calculation,
A temp buffer is realloced with the new capacity,
If the location of the buffer differs from the orignal location
then the r_flag is set,
Then the capacity and the head is set to the new variables.
NULL is returned.
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	short new_cap;  /*Stores the new capacity.*/
	char *newBuffer; /*Stores the new temporary buffer.*/
	if (pBD == NULL) {
		return NULL;
	}
	if (pBD->capacity == MAX_VAL + 1) {
		return NULL;  // CHECK IF THIS FIXES THE COMPACT AFTER PROBLEM.
	}
	pBD->flags &= RESET_R_FLAG;
	if ((pBD->addc_offset * sizeof(char)) < (unsigned short)pBD->capacity) {
		/*If addc_offset is less than capacity then the parameter symbol is
		added at the offset.*/
		pBD->cb_head[pBD->addc_offset] = symbol;
		++pBD->addc_offset; /*Offset is incremented*/
		return pBD;
	}
	/*If the end of buffer is reached then the character is added and the
	capacity is changed based on the mode and the inc_factor.*/
	switch (pBD->mode) {
	case FIXED_MODE: /*If the mode is fixed then no more character can be
					 added.*/
		return NULL;
	case ADDITIVE_MODE:
		new_cap = pBD->capacity + (unsigned char)pBD->inc_factor;
		/*New capacity is the sum of old capacity and inc_factor.*/
		if (new_cap < MIN_VAL) {
			/*If the capacity becomes negeative due to overflow then NULL
			is returned.*/
			return NULL;
		}
		if (new_cap > MAX_VAL) { /*If the new capacity is equal to Short_max then
								 the capacity is set to Max_val (short_max -1).*/
			new_cap = MAX_VAL;
			break;
		}
		break;
	case MULTIPLICATIVE_MODE:
		new_cap = pBD->capacity;
		if (new_cap == MAX_VAL) {
			/*If the capacity has reached the maximum value then null is returned.*/
			return NULL;
		}
		new_cap = pBD->capacity + (((MAX_VAL - pBD->capacity) *
			(unsigned char)pBD->inc_factor) / PERCENTAGE);
		/*The new capacity is the sum of old capacity and the product
		of available space, inc_factor and 1/100.*/
		if (new_cap == pBD->capacity) {
			/*if the above calculation result no change in the
			value then the capacity is set to the maximum value.*/
			new_cap = MAX_VAL;
			break;
		}
		break;
	default:
		return NULL;
	}
	newBuffer = realloc(pBD->cb_head, new_cap);
	/*A buffer is realloced with the new capacity.*/
	if (newBuffer == NULL) {
		return NULL;
	}
	if (newBuffer != pBD->cb_head) {
		/*The the buffer has moved due to realloc then the r_flag is set to 1.*/
		pBD->cb_head = newBuffer;
		pBD->flags |= SET_R_FLAG;
	}
	/*All the new values are set.*/
	pBD->capacity = new_cap;
	pBD->cb_head[pBD->addc_offset] = symbol;
	/*After the increment the character is added at the next space.*/
	++pBD->addc_offset;
	return pBD;
	//return NULL;
}

/*
Purpose: Sets the addc_offset, getc_offset, markc_offset of the Buffer struct
to 0. When this fuction is run the buffer returns to the state when it
is first created. However the contents of the buffer are not cleared.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: Returns an int - -1 if there is an error
Algorithm:
*/
int b_clear(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*All the offsets are set to zero and all the flags are reset.*/
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags &= RESET_EOB;
	pBD->flags &= RESET_R_FLAG;
	return FUNCT_ENDED_NORMALLY;
}

/*
Purpose: The function free the character array and the buffer structure.
after their presence is comfirmed to prevent memory violations.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: free()
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: None
Algorithm: None
*/
void b_free(Buffer *const pBD) {
	if (pBD != NULL) {
		/*To prevent access voilation error, pBD is checked for
		NULL before calling free on both.*/
		free(pBD->cb_head);
		free(pBD);
	}
}

/*
Purpose: Checks if the the capacity is equal to the add_offset. If yes, then the
buffer is full.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: Returns an int - 1 if buffer is full, 0 is not, -1 if there is an
error
Algorithm:
*/
int b_isfull(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	if ((pBD->addc_offset * sizeof(char)) == (unsigned short)pBD->capacity) {
		/*If addc_offset is equal to capacity then the buffer is full.*/
		return FULL;
	}
	return RUN_TIME_ERROR;
}

/*
Purpose: Returns the current limit of the character buffer. The current limit is
the amount of sapce measured in chars that is currently being used.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: short
Algorithm:
*/
short b_limit(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return pBD->addc_offset;
}

/*
Purpose: The function returns the cpacity of the buffer.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: short
Algorithm:
*/
short b_capacity(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return pBD->capacity;
}

/*
Purpose: The function sets mark_offset to mark. The mark must be between 0 bytes
and addc_offset
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct,
Short mark - the position of the marked character
Return value: short
Algorithm: Checks if the mark is between 0 bytes and addc_offset included.
*/
short b_mark(Buffer *const pBD, short mark) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	if (MIN_VAL <= mark && mark <= pBD->addc_offset) {
		/*As long as mark is between 0 and addc_offset the value is set and
		returned.*/
		return pBD->markc_offset = mark;
	}
	return RT_FAIL_1;
}

/*
Purpose: The function returns the value of mode to the calling function
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: int - returns -2 is there is a failure as -1 is a valid mode.
Algorithm:
*/
int b_mode(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_2; /*Failure*/
	}
	return pBD->mode;
}

/*
Purpose: The function returns a non-negative valur of the inc_factor
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: size_t - unsigned int
Algorithm: inc_factor is casted to unsigned char before returning.
*/
size_t b_incfactor(Buffer *const pBD) {
	if (pBD == NULL) {
		return INCFACTOR_ERROR;
	}
	return (unsigned char)(pBD->inc_factor);
	/*Inc factor is casted to an unsigned char before returning.*/
}

/*
Purpose: The function reads the file provided character by character into the
buffer using b_addc() function.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: b_addc()
Parameters: FILE *const fi - Pointer to a File,
Buffer *const pBD - a pointer to the Buffer struct
Return value: int - -2 when the function can't load the character into the
buffer.
Algorithm: It used a while loop with feof() function as the expression to
read characters,
Fget() function is used to read the file character by character,
Uses b_addc() function to add the character to the char buffer,
If the b_addc() is unable to add the character then unget()
function is used to display that character and returns -2.
*/
int b_load(FILE *const fi, Buffer *const pBD) {
	char symbol;
	int unget;
	/*It is used to count the number of characters read.*/
	int count = 0;
	if (fi == NULL || pBD == NULL) {
		/*Checks if the file is actually there.*/
		return RT_FAIL_1;
	}

	/*While the end of file has not been reached the loop continues.*/
	while (1) {
		/*It is used to store the output of fgetc()*/
		symbol = (char)fgetc(fi);
		/*fget is used to get the nect character and then is cast to
		char as the function returns an int.*/
		if (!feof(fi)) {
			if (b_addc(pBD, symbol) == NULL) {
				unget = ungetc(symbol, fi);
				/*If a character cannot be added to the buffer becuase
				it is full then that character is printed.*/
				printf("The last character read from the file is: %c %d\n",
					(char)unget, unget);
				return LOAD_FAIL;
			}
			++count;
		}
		else {
			break;
		}
	}
	return count;
}

/*
Purpose: If the addc_offset is 0, the function returns 1; otherwise it returns 0
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: int
Algorithm:
*/
int b_isempty(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == EMPTY_BUFFER) {
		/*If addc_offset is equal to 0 then the buffer is empty.*/
		return EMPTY;
	}
	return RUN_TIME_ERROR;
}

/*
Purpose: The function is used to retrieve the character being pointed by
the getc_offset. And if it reaches the end of the file then
sets the EOB flag.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value:
Algorithm: Checks if addc_offset is equal to getc_offset, that means the end of
buffer is reached,
Resets the EOB flag,
Retrives the character from the buffer array and returns it.
*/
char b_getc(Buffer *const pBD) {
	char charOffset;
	if (pBD == NULL) {
		return RT_FAIL_2;
	}
	if (pBD->addc_offset == pBD->getc_offset) {
		/*If addc_offset and getc_offset are equal then the end of buffer has
		been reched and the flag is set to 1.*/
		pBD->flags |= SET_EOB;
		return END_OF_BUFFER;
	}
	pBD->flags &= RESET_EOB;
	/*The end of buffer flag is reset and the character at getc_offset is
	returned.*/
	charOffset = pBD->cb_head[pBD->getc_offset];
	/*charOffset is used to store the character present at the getc_offset*/
	++(pBD->getc_offset);
	return charOffset;
}

/*
Purpose: Returns the EOB bit value i.e. 0 or 1
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD- a pointer to the Buffer struct
Return value: int
Algorithm:
*/
int b_eob(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return (pBD->flags & CHECK_EOB);
}

/*
Purpose: This function prints the contents of the buffer till the end of buffer
is reached. It also checks if the buffer is empty.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: b_eob()
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: int
Algorithm: Checks if the buffer is empty,
If the add_offset is 0 bytes then prints empty buffer,
Uses b_getc() meathod to retrive the charcater to be printed,
Checks the EOB flag, if 1 byte then breaks the loop,
Prints the retrieved character.
*/
int b_print(Buffer *const pBD) {
	int count = 0; /*Used to count how many characters are read.*/
	char cTemp; /*Used to read the output from b_getc method.*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == EMPTY_BUFFER) {
		/*If add_offset is zero then the buffer is empty.*/
		printf("Empty buffer!\n");
		return RT_FAIL_1;
	}
	while (1) {
		cTemp = b_getc(pBD);
		/*b_get() method is called to retrieve the character in the buffer.*/
		if (b_eob(pBD) != 0) {
			/*If the End of Buffer is reached loop is broken.*/
			break;
		}
		printf("%c", cTemp);  /*The character which was retrieved is printed.*/
		++count;
	}
	printf("\n");
	return count;
}

/*
Purpose: The function changes the capacity of the buffer to addc_offset + 1,
reallocs a new buffer and sets
the last character of the new buffer to symbol.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: realloc()
Parameters: Buffer *const pBD - a pointer to the Buffer struct,
Char symbol - Character to be added to the buffer
Return value: Buffer *
Algorithm: Checks if the pBD or the head is NULL,
Changes the capacity to addc_offset + 1 if it lies
between 0 bytes and SHRT_MAX,
Reallocs a temporary buffer with the new capacity,
If the location of the buffer has changed then it sets the r_flag,
Updates all the fields and the head to the new buffer.
*/
Buffer *b_compact(Buffer *const pBD, char symbol) {
	if (pBD == NULL) {
		return NULL;
	}
	if (!(MIN_VAL <= (pBD->addc_offset + 1) <= (MAX_VAL + 1))) {
		return NULL;
	}
	/*If the new capacity (addc_offset + 1) lies between 0 and Short_max
	inclusive.*/
	char *temp = realloc(pBD->cb_head, (pBD->addc_offset + 1));
	/*The cb_head is realloced with the new capacity.*/
	if (temp == NULL) {
		return NULL;
	}
	pBD->capacity = (pBD->addc_offset + 1);
	/*The capacity of the buffer is set.*/
	if (temp != pBD->cb_head) {
		/*If the new buffer has moved due to reallocing then the R_FLAG
		is set to zero.*/
		pBD->cb_head = temp;
		pBD->flags |= SET_R_FLAG;
	}
	pBD->cb_head[pBD->addc_offset] = symbol;
	/*The symbol is set as the last character in the buffer.*/
	++(pBD->addc_offset);
	temp = NULL;
	return pBD;
}

/*
Purpose: The function returns the r_flag bit value to the calling function.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: char
Algorithm:
*/
char b_rflag(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return (pBD->flags & CHECK_R_FLAG);
	/*Returns the value of R_flag but does not alter the value.*/
}

/*
Purpose: The function decrements getc_offset by 1.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: short
Algorithm:
*/
short b_retract(Buffer *const pBD) {
	if (pBD == NULL || pBD->getc_offset == 0) {
		return RT_FAIL_1;
	}
	return --pBD->getc_offset;
}
/*
Purpose: The function sets getc_offset to the value of the
current markc_offset.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: short
Algorithm:
*/
short b_reset(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	pBD->getc_offset = pBD->markc_offset;
	/*Sets getc_offset to the value of markc_offset.*/
	return pBD->getc_offset;
}

/*
Purpose: Returns the value of getc_offset to the calling function.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: short
Algorithm:
*/
short b_getcoffset(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return pBD->getc_offset;
}

/*
Purpose: The function sets addc_offset and markc_offset to 0 bytes, so
that the buffer can be read again.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct
Return value: int
Algorithm:
*/
int b_rewind(Buffer *const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	pBD->getc_offset = 0;
	/*Both getc_offset and markc_offset are set to 0 bytes so that the buffer can be
	reread.*/
	pBD->markc_offset = 0;
	return FUNCT_ENDED_NORMALLY;
}

/*
Purpose: The function returns a pointer to a location of the
character buffer indicated by loc_offset.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions:
Parameters: Buffer *const pBD - a pointer to the Buffer struct, Short loc_offset -
Return value:
Algorithm: Checks if pBD or pBD->head are not null
Checks if loc_offset is within 0 bytes and add_offset.
returns the location of the character at loc_offset.
*/
char* b_location(Buffer *const pBD, short loc_offset) {
	if (pBD == NULL) {
		return NULL;
	}
	if (MIN_VAL <= loc_offset < pBD->capacity) {
		/*Checking if the loc_offset is between 0 and Capacity where 0 is
		inclusive.*/
		return (pBD->cb_head + loc_offset);
		/*The address of the character at loc_offset is returned.*/
	}
	return NULL;
}