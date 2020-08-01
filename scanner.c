/*
File name: Scanner.c
Compiler: MS Visual Studio 2015
Author: Harshdeep Verma - 040880851
Course: CST 8152 – Compilers, Lab Section: 13
Assignment: 2
Date: 07/11/2018
Professor: Sv. Ranev
Purpose: This file implements a scanner that reads the input file one character at a time
and generates tokens appropriately.
Function list: scanner_init(), malar_next_token(), char_class()
get_next_state(), iskeyword(), aa_func02(),
aa_func03(), aa_func05(), aa_func08(), aa_func10(),
aa_func11()
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

#define NDEBUG        
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

						 /* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
					   /* No other global variable declarations/definitiond are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */

										/*
										Purpose: This function initilaizes the sc_buff buffer to the buffer passed in as parameter.
										It makes sure that the buffer has some content in it. And also removes all content
										from the string literal buffer
										Author: Harshdeep Verma
										History/Versions: 1.0
										Called functions: b_isempty(), b_rewind(), b_clear()
										Parameters: Buffer *psc_buf - a pointer to the Buffer struct
										Return value: int - It returns 1 if the function ran succesfully
										It returns 0 if the function failed
										Algorithm: Calls b_isempty() to check if the buffer has any content in it.
										Calls b_rewind() to set getc_offset and markc_offset to zero,
										so that the buffer can be read from the start.
										Calls b_clear() to delete the contents of the string literal buffer.
										It sets line variable to 1.
										It set sc_buff to the parameter passed.
										*/
int scanner_init(Buffer * psc_buf) {
	if (b_isempty(psc_buf))
		return EXIT_FAILURE;/*1*/
							/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
						/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
Purpose: This function reads a character from the sc_buf and determines what symbol it is.
Part 1 of the function gererates token for symbols, logical and relational operators,
comments. Part 2 of the function runs when part 1 is unable to match a character. It
is the finite automoton that determines if the sequence of symbols is an AVID, SVID,
Integer Literal, Floating Point Literal, String Literal or an Error.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_getc(), b_mark(), b_reset(), b_retract(), get_next_state(), b_allocate(),
b_getcoffset(), b_addc(), b_free
Parameters: void
Return value: Token -
Algorithm: The function is a while loop that runs till a token is returned.
Calls getc() to read a character.
Checks for white Spaces and skips then.
Checks for braces, parentheses, semicolon, pound and comma and returns a token
Checks for = and generates an Assignment token
Checks for ==, <>, <, > and generates a relation operator token
Checks for +, - , *, \ and generates an arithmatic operator token
Checks for .AND. and .OR. and generates a logical operator token
Checks for comments (!!) and skips all characters till it finds a newline.
It also skips all the character incase the comment symbol was wrong.
It Checks for '\0' and 255 and generates a SEOF token.
If it doesnt match any symbols mentioned above then, it sets  a mark at that
symbol(lexstart)
Calls get_next_state() to determine the next stage.
Calls b_getc() to get another character.
It loops till it reaches an accepting stage.
It retracts if the accepting function calls for it.
It sets lexend at the next character to be read.
It allocates a new buffer called lex_buf.
It gives a run time error if it is unable to create the buffer
it resets the sc_buf to mark (lexstart) so that it can be read again
It copies all the charcater between lexstart and lexend from sc_buf to lex_buf
It uses funtion pointer to call the accepting function according to the
accepting stage determines earlier.
It frees the lex_buf and returns the token.

*/
Token malar_next_token(void) {
	Token t = { 0 };
	/* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart; /*start offset of a lexeme in the input char buffer (array) */
	short lexend; /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	int comment = 0; /*Acts as a boolean for knowing if the comment symbol was correct*/
	char tempC = 0; /*Stores the sceond charater when the comment symbol is wrong*/
	while (1) {
		comment = 0;
		c = b_getc(sc_buf);
		switch (c) {
		case ' ':
		case '\f':
		case '\v':
		case '\t':
			/*White spaces are skiped*/
			continue;
			/*Line number is incremented if a new line is processed*/
		case '\n':
			++line;
			continue;
		case '\r':
			c = b_getc(sc_buf);
			if (c != '\n')
				b_retract(sc_buf);
			++line;
			continue;
		case ';':
			t.code = EOS_T;
			return t;
		case '{':
			t.code = LBR_T;
			return t;
		case '}':
			t.code = RBR_T;
			return t;
		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;
		case '#':
			t.code = SCC_OP_T;
			return t;
		case ',':
			t.code = COM_T;
			return t;
		case '=':
			c = b_getc(sc_buf);
			if (c == '=') {
				/*Checks if the next charater is = (==, which is a relational operator)*/
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}
			else {
				b_retract(sc_buf);
				t.code = ASS_OP_T;
			}
			return t;
		case '<':
			c = b_getc(sc_buf);
			if (c == '>') {
				/*Checks if the next charater is > (<>, which is a relational operator)*/
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
			}
			else {
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
			}
			return t;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		case '.':
			b_mark(sc_buf, b_getcoffset(sc_buf));
			/*Sets a mark at the character right after the . so that buffer can be
			reset at that point so that it can be read again*/
			c = b_getc(sc_buf);
			if (c == 'A') {
				/*Checks if the token evaluates to .AND.*/
				c = b_getc(sc_buf);
				if (c == 'N') {
					c = b_getc(sc_buf);
					if (c == 'D') {
						c = b_getc(sc_buf);
						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
				/*If the token is not .AND. then sc buffer resets to the mark so
				that it can read again*/
				b_reset(sc_buf);
			}
			else if (c == 'O') {
				/*Checks if the token evaluates to .OR.*/
				c = b_getc(sc_buf);
				if (c == 'R') {
					c = b_getc(sc_buf);
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}
			b_reset(sc_buf);
			/*The sc buffer is reset to the mark (character after the .)*/
			c = '.';
			/*c = *b_location(sc_buf,b_getcoffset(sc_buf) - 1);*/
			/*So that the . can be processed again the char c is set to . again*/
			break;
		case '!':
			c = b_getc(sc_buf);
			/*If the next character is !, then the comment is valid and the int is set to 1*/
			if (c == '!')
				comment = 1;
			else
				tempC = c;
			/*In case the comment is wrong the wrong character is stored so that it
			can be placed in the error token*/
			if (c == '\n' || c == SEOF_1 || c == SEOF_2) {
				/*If ! is followed by white space or SEOF then it is an error token*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[2] = NULL_TERMINATOR;
				return t;
			}
			do {
				c = b_getc(sc_buf);
				if (c == SEOF_1 || c == SEOF_2) {
					comment = 2;
					break;
				}
				/*Whether the comment is wrong or right it is ignored till the newline or SEOF*/
			} while (c != '\n' && c != '\r' && c != SEOF_1 && c != SEOF_2);
			if (c == SEOF1 || c == SEOF2)
				b_retract(sc_buf);
			if (c == '\r') {
				c = b_getc(sc_buf);
				if (c != '\n') {
					b_retract(sc_buf);
				}
			}
			if (comment != 2)
				++line;
			/*If the comment symbol was right then it contines*/
			if (comment == 1)
				continue;
			/*If the comment symbol was faulty then it is stored in the err_lex*/
			if (comment == 2)
				tempC = '!';
			t.code = ERR_T;
			t.attribute.err_lex[0] = '!';
			t.attribute.err_lex[1] = tempC;
			t.attribute.err_lex[2] = NULL_TERMINATOR;
			return t;
		case SEOF_1:
			t.code = SEOF_T;
			t.attribute.seof = SEOF1;
			return t;
		case SEOF_2:
			t.code = SEOF_T;
			t.attribute.seof = SEOF2;
			return t;
		}
		/*A mark is set at the charcter before the getc_offset so the
		the character can be copied to the lex_buf*/
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
		/*Finite Automaton is run until an accepting stage is reached*/
		while (accept == NOAS) {
			state = get_next_state(state, c, &accept);
			if (accept != NOAS) {
				break;
			}
			c = b_getc(sc_buf);
		}
		/*if the accepting function calls for retract then it is retracted*/
		if (accept == ASWR) {
			//err_printf("here");
			b_retract(sc_buf);
		}
		/*lexend is set at the character that is to be read*/
		lexend = b_getcoffset(sc_buf);
		/*A new fixed buffer is allocated*/
		lex_buf = b_allocate((lexend - lexstart + 1) * sizeof(char), 0, FIXED);
		if (lex_buf == NULL) {
			/*if the buffer is unable to allocate then it is a run time error*/
			t.code = RTE_T;
			char * runMes = "RUN TIME ERROR: ";
			/*The length of the message is less than the capacity of err_lex*/
			strncpy(t.attribute.err_lex, runMes, strlen(runMes));
			/*A non-negative number is stored in scerrnum*/
			scerrnum = scerrnumC;
			return t;
		}
		/*The buffer is reset to the mark set at lex_start*/
		b_reset(sc_buf);
		for (int i = 0; i < (lexend - lexstart); i++) {
			/*The contents of sc_buf that lie betweeb lex_start and lex_end are
			copied to lex_buf*/
			c = b_getc(sc_buf);
			b_addc(lex_buf, c);
		}
		/*A terminator is added to the lex_buf*/
		b_addc(lex_buf, NULL_TERMINATOR);
		/*According to the state, an accepting is called*/
		t = aa_table[state](b_location(lex_buf, HEAD));
		b_free(lex_buf);
		return t;
	}
}

/*
Purpose: This function determines the next stage of the finite automoton.
The function uses char_class() to determines the which column it belongs to
and then gets the next stage from st_table. Also it sets the variable
accept.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: char_class()
Parameters:	int state - Current State of the finite automoton
char c - Char for which the next stage is determined
int *accept - Stores whether the next stage is accepting or not
Return value: int - Returns the next stage based on the parameters
Algorithm: Calls function char_class() to get the column number.
Gets the value from the two dimentional array st_table with state as the row
and col as the column to get the next stage
It asserts the the next state retrieved is not Illegal State.
Retrives the value from as_table array using next as the row and sets accept value.
*/
int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
Purpose: This function determines whether the lexeme passes as a parameter is
a keyword defines according to the grammer.
Author: Harshdeep Verma
History/Versions: 1.1
Called functions: strcmp()
Parameters: char* kw_lexeme - The lexeme string read from the sc_buf
Return value: int - Returns the index of the keyword stored in the kw_table array
-1 if the lexeme is not a keyword.
Algorithm: Sequentialy searchs (for loop) the keyword in the kw_table array.
Uses strcmp to match the the lexeme with each element of the array
*/
int iskeyword(char* kw_lexeme) {
	/*It compares whether the lexeme is present in the keyword table*/
	for (int i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_table[i], kw_lexeme) == 0) {
			return i;
		}
	}
	return NOT_A_KEYWORD;
}

/*
Purpose: It determines the column number for the given parameter.
The column number belongs to the st_table table. It returns
the same coloumn number for all the letters and all the digits.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions:
Parameters: char c - The character read from the sc_buf
Return value: int - Returns a non-negative index (Between 0 adn 7 inclusive)
Algorithm: If the c is an uppercase or a lowercase letter then returns 0
If the c is a '0' then returns 1
If the c is a digit between 1 and 9 inclusive then returns 2
If the c is a '.'  then returns 3
If the c is a '$' then returns 4
If the c is a " (double quote) then returns 5
If the c is a SEOF then returns 6
If the c is anything else then returns 7
*/
int char_class(char c)
{
	/*Returns the index for the state to which the char belongs*/
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) { return LETTER; }
	else if (c == '0') { return ZERO; }
	else if (c >= '1' && c <= '9') { return DIGIT; }
	else if (c == '.') { return PERIOD; }
	else if (c == '$') { return DOLLAR; }
	else if (c == '"') { return QUOTE; }
	else if (c == SEOF_2 || c == SEOF_1) { return SEOF; }
	else { return OTHER; }
}

/*
Purpose: This is an accepting function for AVID and Keywords.
It uses the lexeme to determine if it is a Keyword otherwise
It returns the token for AVID accordingly. It stores the AVID
in vid_lex.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: iskeyword(), b_limit(), checkSymbolTable()
Parameters: char *lexeme - It is the cb_head of the lex_buf.
It contains the word passed.
Return value: Token - It returns the AVID Token or KW_T Token
Algorithm: Calculates the length of the Lexeme using b_limit().
Calls the funtion iskeyword() to check if the lexeme is a keyword.
If it is then returns a keyword token and sets the index anbnd exits the function.
It sets the token code to AVID.
If the length of the lexeme is longer than the VID_LEN then it truncates it
and copies it to vid_lex and then sticks a null terminator at the end.
Returns the AVID token.
*/
Token aa_func02(char *lexeme) {
	Token t = { 0 };
	short length = b_limit(lex_buf) - 1;
	/*Stores the index from the isKeyword method*/
	int key = iskeyword(lexeme);
	/*Returns a keyword token if the index is not -1*/
	if (key != NOT_A_KEYWORD) {
		t.code = KW_T;
		t.attribute.kwt_idx = key;
		return t;
	}
	/*It is checking the first letter of the AVID variable, if it it i, d, n, h then it is an integer else a float*/
	t.code = AVID_T;
	if (lexeme[0] == 'i' || lexeme[0] == 'd' || lexeme[0] == 'n' || lexeme[0] == 'h') {
		/*If it is an int it sets the flag to '0' and the default value to 0*/
		t.avid_attribute.flags = '0';
		t.avid_attribute.values.int_value = 0;
	}
	else {
		/*If it a float then it sets the flag to '1' and the default value to 0.0*/
		t.avid_attribute.flags = '1';
		t.avid_attribute.values.flt_value = 0.0;
	}
	/*If the length of the AVID is more than 8 (VID_LEN) characters then only 8
	(VID_LEN) characters are stored*/
	if (length * sizeof(char) > VID_LEN) {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN * sizeof(char));
		length = VID_LEN;
	}
	else {
		strncpy(t.attribute.vid_lex, lexeme, length * sizeof(char));
	}
	/*A null terminator is added at the end*/
	t.attribute.vid_lex[length] = NULL_TERMINATOR;
	/*Calls the checkSymbolTable method to check if the variable exists in the table if not then it adds it in*/
	checkSymbolTable(lexeme, t.avid_attribute.flags);
	return t;
}

/*
Purpose: This is an accepting function for SVID.
It returns the token for SVID. It stores the SVID
in vid_lex.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_limit(), checkSymbolTable()
Parameters: char *lexeme - It is the cb_head of the lex_buf.
It contains the word passed.
Return value: Token - It returns the SVID Token
Algorithm: Calculates the length of the Lexeme using b_limit().
It sets the token code to SVID.
If the length of the lexeme is longer than the VID_LEN - 1 then it truncates it
and copies it to vid_lex and then sticks a $ and a null terminator at the end.
Returns the AVID token.
*/
Token aa_func03(char *lexeme) {
	Token t = { 0 };
	short length = b_limit(lex_buf) - 1;
	t.code = SVID_T;
	/*Sets the avid flag to 2 and sets default as an empty string*/
	t.avid_attribute.flags = '2';
	t.avid_attribute.values.str_locator = "";
	/*If the length of the SVID is more than 7 (VID_LEN - 1) characters then
	only 7 (VID_LEN - 1) characters are stored*/
	if (length * sizeof(char) > VID_LEN - 1) {
		strncpy(t.attribute.vid_lex, lexeme, (VID_LEN - 1) * sizeof(char));
		length = VID_LEN;
	}
	else {
		strncpy(t.attribute.vid_lex, lexeme, length * sizeof(char));
	}
	/*$ and null ternimator are added in the end*/
	t.attribute.vid_lex[length - 1] = '$';
	t.attribute.vid_lex[length] = NULL_TERMINATOR;
	/*Calls the checkSymbolTable method to check if the variable exists in the table if not then it adds it in*/
	checkSymbolTable(lexeme, t.avid_attribute.flags);
	return t;
}

/*
Purpose: It is an accepting function for Integer Literals.
It converts the lexeme to an Int and returns the Int token.
It checks for int overflow and leading zeros. If such error
occurs then an error token is returned which stores the lexeme
in its err_lex.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_limit(), atoi()
Parameters: char *lexeme - It is the cb_head of the lex_buf.
It contains the word passed.
Return value: Token - It returns the INT_L Token
Algorithm: Calculates the length of the Lexeme using b_limit().
It converts the lexeme to an int using atoi
Checks for overflow (less than 65535) and for leading zeros
If it fails the condition then returns an error token and stores
the lexeme in err_lex.
If the length of the lexeme is longer than the ERR_LEN then it
adds three '.' and a null terminator
It sets the token code to INL_T and stores the int.

*/

Token aa_func05(char *lexeme) {
	Token t = { 0 };
	short length = b_limit(lex_buf) - 1;
	long temp = atol(lexeme);
	/*Checks for overflow and leading zeros in the int, if true then returns an error*/
	if (temp > MAX || (temp != 0 && lexeme[0] == '0')) {
		t.code = ERR_T;
		/*If the length is greater then ERR_LEN the  only stores ERR_LEN - 3 characters
		and adds three .*/
		if (length * sizeof(char) > ERR_LEN) {
			length = ERR_LEN - 3;
			t.attribute.err_lex[ERR_LEN - 3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
		}
		strncpy(t.attribute.err_lex, lexeme, length * sizeof(char));
		t.attribute.err_lex[ERR_LEN] = NULL_TERMINATOR;
		return t;
	}
	t.code = INL_T;
	t.attribute.int_value = (int)temp;
	return t;
}

/*
Purpose: It is an accepting function for Floating Point Literals.
It converts the lexeme to a Float and returns the float token.
It checks for float overflow, underflow and leading zeros. If such error
occurs then an error token is returned which stores the lexeme
in its err_lex.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_limit(), atof()
Parameters: char *lexeme - It is the cb_head of the lex_buf.
It contains the word passed.
Return value: Token - It returns the FPL_T Token
Algorithm: Calculates the length of the Lexeme using b_limit().
It converts the lexeme to an double using atof
Checks for overflow, underflow and for leading zeros
It also makes sure that 0.0 is a valid input
If it fails the condition then returns an error token and stores
the lexeme in err_lex.
If the length of the lexeme is longer than the ERR_LEN then it
adds three '.' and a null terminator
It sets the token code to FPL_T and stores the double as float.
Casting is safe as the range has been verified
*/
Token aa_func08(char *lexeme) {
	Token t = { 0 };
	short length = b_limit(lex_buf) - 1;
	double temp = atof(lexeme);
	/*Checks for the range of float between FLT_MAX and FLT_MIN, also checks the leading zeros*/
	if ((temp > FLT_MAX || temp < FLT_MIN) || (temp >= 1 && lexeme[0] == '0')) {
		/*Checks for zero else returns an error token*/
		if (temp != 0.0) {
			t.code = ERR_T;
			if (length > ERR_LEN) {
				length = ERR_LEN - 3;
				t.attribute.err_lex[ERR_LEN - 3] = '.';
				t.attribute.err_lex[ERR_LEN - 2] = '.';
				t.attribute.err_lex[ERR_LEN - 1] = '.';
			}
			strncpy(t.attribute.err_lex, lexeme, length);
			t.attribute.err_lex[ERR_LEN] = NULL_TERMINATOR;
			return t;
		}
	}
	t.code = FPL_T;
	/*Since the range for float was confirmed it is safe to cast double to a float*/
	t.attribute.flt_value = (float)temp;
	return t;
}

/*
Purpose: This is an accepting function for String Literal.
It returns the token for STR_T. It stores the STR_T
in str_LTBL.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_limit(), incrementLine()
Parameters: char *lexeme - It is the cb_head of the lex_buf.
It contains the word passed.
Return value: Token - It returns the STR_L Token
Algorithm: Calculates the length of the Lexeme using b_limit().
It sets the token code to STR_T;
It sets the str_offset to the addc_offset of str_LTBL
It copies the lexeme to the str_LTBL.
If it encounter \n then it increments the line counter
Then it adds a null terminator at the end and returns the token.
*/
Token aa_func10(char *lexeme) {
	Token t = { 0 };
	short length = b_limit(lex_buf) - 1;
	t.code = STR_T;
	/*Sets the t.attribute.str_offset to addc_offset of str_LTBL*/
	t.attribute.str_offset = b_limit(str_LTBL);
	for (int i = 1; i < (length - 1); i++) {
		b_addc(str_LTBL, lexeme[i]);
		incrementLine(lexeme[i]);
	}
	/*Adds the null terminator at the end*/
	b_addc(str_LTBL, NULL_TERMINATOR);
	return t;
}

/*
Purpose: This is an accepting function for Errors.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_limit(), incrementLine()
Parameters: char *lexeme - It is the cb_head of the lex_buf.
It contains the word passed.
Return value: Token - It returns the ERR_T Token
Algorithm: Calculates the length of the Lexeme using b_limit().
It sets the token code to ERR_T;
If the length of the lexeme is longer than the ERR_LEN then it
adds three '.' and a null terminator
It copies the lexeme to the err_lex.
If it encounter \n then it increments the line counter
It returns the token
*/
Token aa_func11(char *lexeme) {
	Token t = { 0 };
	short length = b_limit(lex_buf) - 1;
	int err = 0;
	t.code = ERR_T;
	/*If the length of the lexeme is more than ERR_LEN then three periods are added
	in the end with a null terminator*/
	if (length * sizeof(char) > ERR_LEN) {
		err = 1;
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = NULL_TERMINATOR;
	}
	else {
		t.attribute.err_lex[length] = NULL_TERMINATOR;
	}
	for (int i = 0; i < length; i++) {
		if (err == 0 || (err == 1 && i < 17))
			t.attribute.err_lex[i] = lexeme[i];
		/*Increments the line counter if the character is a liner terminator*/
		incrementLine(lexeme[i]);
	}
	return t;
}

/*
Purpose: It checks if the character passed is a newline or a carige return, if
		 is then it increments the line counter
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: b_getc(), b_retract()
Parameters: char c
Return value: 
Algorithm:
*/
void incrementLine(char c)
{
	/*Line is incremented if the read \n, \r or \r\n*/
	if (c == '\n')
		++line;
	if (c == '\r') {
		c = b_getc(sc_buf);
		if (c != '\n')
			b_retract(sc_buf);
		++line;
	}
}

/*
Purpose: This function creates a symbol table and stores all the variable names and their types.
		 It also checks if the variables already exists or not.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: realloc(), malloc(), strcpy()
Parameters: char lexeme - it contains the name of the VID
			char flag - it cointains the flag that defines the type of variable (0, 1, 2)
Return value: void
Algorithm:
It checks the size of the symbol table, if its modulus by 5 is zero it reserves space for 5 more symbols
It matches the lexeme passed against all the symbols in the table, it returns if the match is succesful
Else it reserves a space for the struct which contains the VID information
It sets the flag that is passed as a parameter
It reserves space for the name and copies the lexeme in it and increments the symbol counter.
*/
void checkSymbolTable(char* lexeme, char flag) {
	int length = strlen(lexeme);
	char* string = NULL;
	/*It checks the count, if it a multiple of 5 then it reallocs space for 5 more. Works with 0 as well*/
	if (symbolTableCounter % 5 == 0) {
		symbolTable = (variable**)realloc(symbolTable, (5 + symbolTableCounter) * sizeof(variable*));
	}
	if (flag == '2') {
		string = lexeme;
		string[strlen(lexeme) - 1] = '_';
		string[strlen(lexeme)] = '\0';
	}
	int match = 1;
	for (int i = 0; i < symbolTableCounter; i++) {
		if (flag == '2') 
			match = strcmp(string, symbolTable[i]->name);
		else
			match = strcmp(lexeme, symbolTable[i]->name);
		/*If match is successful that means the variable already exists in the table and function is returned*/
		if (match == 0)
			return;
	}
	/*Reserves space for a struct variable so that it can be added to the symbol table*/
	symbolTable[symbolTableCounter] = (variable*)malloc(sizeof(variable));
	/*Sets the flags in the symbolTable list to the flag value passed*/
	symbolTable[symbolTableCounter]->flagType = flag;
	/*Mallocs space for thr variable name*/
	
	symbolTable[symbolTableCounter]->name = (char *)malloc(length+1);
	/*Sets the last character as the null terminator*/
	symbolTable[symbolTableCounter]->name[length] = '\0';
	/*Copies the value from leseme to the symbolTable list*/
	strcpy(symbolTable[symbolTableCounter]->name, lexeme);
	symbolTableCounter++;
	return;
}
