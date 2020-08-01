/*
File name: Buffer.h
Compiler: MS Visual Studio 2015
Author: Harshdeep Verma - 040880851
Course: CST 8152 – Compilers, Lab Section: 13
Assignment: 1
Date: 01/10/2018
Professor: Sv. Ranev
Purpose: This file contains all the constants, buffer discriptor struct
and prototypes.
Function list: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(),
b_limit(), b_capacity(), b_mark(), b_mode(),
b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(),
b_print(), b_compact(), b_rflag(), b_retract(), b_reset(),
b_getcoffset(), b_rewind(), b_location()
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) */
/*to enforce C89 type comments  - to make //comments an warning */
/*#pragma warning(error:4001)*/
/* to enforce C89 comments - to make // comments an error */
/* standard header files */

#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1 /* fail return value */
#define RT_FAIL_2 -2 /* fail return value */
#define LOAD_FAIL -2 /* load fail return value */
#define MAX_VAL (SHRT_MAX -1) /*Maximum value allowe*/
#define MIN_VAL 0 /*Minimum Possible Value*/
#define EMPTY 1 /*Returned when the buffer is empty*/
#define FULL 1 /*Returned when the buffer is full*/
#define RUN_TIME_ERROR 0 
#define EMPTY_BUFFER 0
#define END_OF_BUFFER 0
#define FUNCT_ENDED_NORMALLY 0 
#define FIXED_MODE 0
#define ADDITIVE_MODE 1
#define MULTIPLICATIVE_MODE -1
#define INCFACTOR_ERROR 0x100
#define MAX_INC_FACTOR_A 255 /*Maximum Increment Factor for Additive mode*/
#define MAX_INC_FACTOR_M 100 /*Maximum Increment Factor for Multiplicative mode*/
#define PERCENTAGE 100 /*Used to calculate new capacity in Multiplicative mode*/
#define DEFAULT_FALGS  0xFFFC /*default flags value*/
#define SET_EOB  0x0001       /*set eob mask - OR*/ 
#define RESET_EOB  0xFFFE     /*reset eob mask - AND*/
#define CHECK_EOB  0x0001     /*check eob mask - AND*/
#define SET_R_FLAG  0x0002    /*set r_flag mask - OR*/
#define RESET_R_FLAG  0xFFFD  /*reset r_flag mask - AND*/
#define CHECK_R_FLAG  0x0002  /*check r_flag mask - AND*/

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array
					 (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes)
					   allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  mode;       /* operational mode indicator*/
	unsigned short flags; /* contains character array reallocation flag
						  and end-of-buffer flag */
} Buffer, *pBuffer;

Buffer *b_allocate(short int_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer *const pBD);
void b_free(Buffer *const pBD);
int b_isfull(Buffer *const pBD);
short b_limit(Buffer *const pBD);
short b_capacity(Buffer *const pBD);
short b_mark(Buffer *const pBD, short mark);
int b_mode(Buffer *const pBD);
size_t b_incfactor(Buffer *const pBD);
int b_load(FILE *const fi, Buffer *const pBD);
int b_isempty(Buffer *const pBD);
char b_getc(Buffer *const pBD);
int b_eob(Buffer *const pBD);
int b_print(Buffer *const pBD);
Buffer *b_compact(Buffer *const pBD, char symbol);
char b_rflag(Buffer *const pBD);
short b_retract(Buffer *const pBD);
short b_reset(Buffer *const pBD);
short b_getcoffset(Buffer *const pBD);
int b_rewind(Buffer *const pBD);
char *b_location(Buffer *const pBD, short loc_offset);
#endif
#pragma once
