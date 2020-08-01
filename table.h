/*
File name: table.h
Compiler: MS Visual Studio 2015
Author: Harshdeep Verma - 040880851
Course: CST 8152 – Compilers, Lab Section: 13
Assignment: 2
Date: 08/11/2018
Professor: Sv. Ranev
Purpose: This file contains the constants, typedefs, accepting function declarations,
finite automoton, accepting function pointer array and Keyword array.
Function list: aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11()
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define NOT_A_KEYWORD -1 /*Returned when the keyword doesn't match*/
#define HEAD 0 /*Used to access head of the buffer*/
#define LETTER 0 /*Column number for Letters*/
#define ZERO 1 /*Column number for Zero*/
#define DIGIT 2 /*Column number for Digits*/
#define PERIOD 3 /*Column number for Period*/
#define DOLLAR 4 /*Column number for Dollar Sign*/
#define QUOTE 5 /*Column number for Quote*/
#define SEOF 6 /*Column number for SEOF Characters*/
#define OTHER 7 /*Column number for anything other than mentioned above */
#define ASWR -4  /*accepting state with retract */
#define ASNR -5 /*accepting state with no retract*/
#define NOAS -6  /*not an accepting state*/  
#define SEOF_1 '\0' 
#define SEOF_2 255
#define NULL_TERMINATOR '\0'
#define MAX 32767
#define FIXED 'F' /*Mode for the lex_buf*/
#define scerrnumC 2000 /*A non negative constant for run time errors*/
#define KWT_SIZE  10
#define ES 11 /*Comment - Error state  with no retract */
#define ER 12 /*Comment - Error state  with retract */
#define IS -1 /*Comment - Inavalid state */
#define TABLE_COLUMNS 8

int  st_table[][TABLE_COLUMNS] = {
	/*0*/{ 1,6,4,ES,ES,9,IS,ES },
	/*1*/{ 1,1,1,2,3,2,2,2 },
	/*2*/{ IS,IS,IS,IS,IS,IS,IS,IS },
	/*3*/{ IS,IS,IS,IS,IS,IS,IS,IS },
	/*4*/{ ES,4,4,7,5,ES,5,5 },
	/*5*/{ IS,IS,IS,IS,IS,IS,IS,IS },
	/*6*/{ ES,6,ES,7,ES,ES,5,5 },
	/*7*/{ 8,7,7,8,8,8,8,8 },
	/*8*/{ IS,IS,IS,IS,IS,IS,IS,IS },
	/*9*/{ 9,9,9,9,9,10,ER,9 },
	/*10*/{ IS,IS,IS,IS,IS,IS,IS,IS },
	/*11*/{ IS,IS,IS,IS,IS,IS,IS,IS },
	/*12*/{ IS,IS,IS,IS,IS,IS,IS,IS },
};

int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR,
NOAS, ASNR, ASNR, ASWR };

Token aa_func02(char *lexeme); /*AVID Funtion*/
Token aa_func03(char *lexeme); /*SVID Funtion*/
Token aa_func05(char *lexeme); /*IL Funtion*/
Token aa_func08(char *lexeme); /*FPL Function*/
Token aa_func10(char *lexeme); /*SL Function*/
Token aa_func11(char *lexeme); /*ES Function*/
void incrementLine(char c);
void checkSymbolTable(char* lexeme, char flags);

typedef Token(*PTR_AAF)(char *lexeme);
/*PTR_AAF is a pointer to a function that takes in a char* or string and returns a token*/

variable **symbolTable;
int symbolTableCounter;

PTR_AAF aa_table[] = { NULL, NULL, aa_func02, aa_func03, NULL, aa_func05, NULL, NULL,
aa_func08, NULL, aa_func10, aa_func11, aa_func11 };

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
