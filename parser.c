/*
File name: Parser.c
Compiler: MS Visual Studio 2015
Author: Harshdeep Verma - 040880851
Course: CST 8152 – Compilers, Lab Section: 13
Assignment: 3
Date: 07/12/2018
Professor: Sv. Ranev
Purpose: This file implements a parser that reads tokens to determines whether they are syntacitically correct or not.
Function list:
parser( ), match(), syn_eh(), syn_printe( ), gen_incode(), program( ),  opt_statements( ),  statements( ), statements_p( ),
statement( ),  assignment_statement( ), assignment_expression( ), selection_statement( ), pre_condiiton( ), iteration_statement( ),
input_statement( ), variable_list( ), variable_list_p( ), variable_identifier( ), output_statement( ), output_list( ),
arithmetic_expression( ), unary_arithmetic_expression( ), additive_arithmetic_expression( ), additive_arithmetic_expression_p( ),
multiplicative_arithmetic_expression( ), multiplicative_arithmetic_expression_p( ), primary_arithmetic_expression( ), string_expression( ),
string_expression_p( ), primary_string_expression( ), conditional_expression( ), logical_OR_expression( ), logical_OR_expression_p( ),
logical_AND_expression( ), logical_AND_expression_p( ), relational_expression( ), sign( ), primary_a_relational_expression( ),
primary_s_relational_expression( )
*/
#define _CRT_SECURE_NO_WARNINGS
#include "parser.h"
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

/*
Purpose: It is the starting fuction. It calls the first production.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: malar_next_token(), program(), match(), gen_incode()
Parameters: void
Return value: void
Algorithm:
*/
void parser(void) {
	char *str = malloc(256);
	int ch;
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed", 0);
	/*The file stream c is closed after being written to and is opened again in read mode*/
	fclose(c);
	c = fopen("temp.txt", "r");
	fprintf(code, "#include <stdio.h>\n#include <stdlib.h>\nint main() {");
	/*This loop prints(declares in according to ANCI C) all the variables in symbol table and also prints their type based on the flag with a semicolon at the end*/
	for (int i = 0; i < symbolTableCounter; i++) {
		fprintf(code, "\n");
		if (symbolTable[i]->flagType == '0')
			fprintf(code, "int ");
		else if (symbolTable[i]->flagType == '1')
			fprintf(code, "float ");
		else
			fprintf(code, "char* ");
		fprintf(code, "%s", symbolTable[i]->name);
		if (symbolTable[i]->flagType == '0')
			fprintf(code, " = 0;");
		else if (symbolTable[i]->flagType == '1')
			fprintf(code, " = 0.0;");
		else
			fprintf(code, " = \"\";");
	}
	fprintf(code, "\n");
	/*This loop extracts rest of the characters from c stream and prints it in the code stream*/
	while (1) {
		ch = fgetc(c);
		if (ch == EOF)
			break;
		else
			putc((char)ch, code);
	}
	/*This loop deletes all the names of the variable stored in the struct in the symbol table*/
	for (int i = 0; i < symbolTableCounter; i++) {
		free(symbolTable[i]->name);
	}
	/*This loop deletes all the struct in the symbol table*/
	for (int i = 0; i < symbolTableCounter; i++) {
		free(symbolTable[i]);
	}
	free(str);
	free(symbolTable);
	free(flags);
	/*This loop deletes all the names of the variable stored in the variable dynamic array*/
	for (int i = 0; i < finalCountVariables; i++) {
		free(variables[i]);
	}
	free(variables);
}

/*
Purpose: The match() function matches two tokens: the current input token (lookahead) and the
token required by the parser. The token required by the parser is represented by two
integers - the token code and the token attribute.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: malar_next_token(), syn_eh(), syn_printe()
Parameters: int pr_token_code, int pr_token_attribute
Return value: void
Algorithm: If the lookahead token code and the parameter token code (pr_token_code) match then
It checks if the code is a Keyword, Logical, Arithmetic or Relation operator then it tries to match the attribute
If the match was successful then it sets the int match as 1
If the match was successful the token is SEOF_T then it returns else it calls malar_next_token()
If the match was unsuccessful then it calls syn_eh() with the expected token code
If the token code was an ERR_T then it ncrements synerrno, calls syn_printe() and malar_next_token()
*/
void match(int pr_token_code, int pr_token_attribute)
{
	int match = 0;
	if (lookahead.code == pr_token_code)
		switch (lookahead.code) {
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (pr_token_attribute == lookahead.attribute.get_int)
				match = 1;
			break;
		default:
			match = 1;
		}
	if (match) {
		if (lookahead.code == SEOF_T)
			return;
		lookahead = malar_next_token();
	}
	else {
		syn_eh(pr_token_code);
		return;
	}
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

/*
Purpose: This method deals with error recovery, it skips to the token that the parser expects.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions: exit(), malar_next_token()
Parameters: int sync_token_code
Return value: void
Algorithm: Calls syn_printe()
Increments synerrno
Loop infinetly till the token if found
calls malar_next_token() to get the next token
If token is found then gets the next token and then returns
If the token expected was SEOF_T then it simply returns
If the expected token is not found, insted SEOF_T is found then it calls exit()
*/
void syn_eh(int sync_token_code)
{
	syn_printe();
	synerrno++;
	while (1) {
		lookahead = malar_next_token();
		/*If the token requires is not SEOF_T but is found then the program exits*/
		if (sync_token_code != SEOF_T && lookahead.code == SEOF_T)
			exit(synerrno);
		/*If the token is found then malar_next_token() is called unless the token is SEOF_T*/
		if (lookahead.code == sync_token_code) {
			if (sync_token_code == SEOF_T)
				return;
			lookahead = malar_next_token();
			return;
		}
	}
}

void syn_printe() {
	//printf("Token Code have - %d\n", lookahead.code);
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;
	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}

/*
Purpose: It prints the parameter with a newline.
Author: Harshdeep Verma
History/Versions: 1.0
Called functions:
Parameters: char* string - The string to be printed.
Return value: void
Algorithm:
*/
void gen_incode(char* string, int c_code)
{
	/*Prints messages to the out file*/
	if (c_code == 0)
		printf("%s\n", string);
	else
		fprintf(c,"%s", string);
	/*Prints c code to a temporary text file */
}

/*
<program> → PLATYPUS {<opt_statements>}
First(program) = {KW_T(PLAYTYPUS)}
*/
void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("return 1;\n}", 1);
	gen_incode("PLATY: Program parsed", 0);
}

/*
<opt_statements> → <statements> | ε
First(opt_statements) = {First(statements), ε}
= {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ε}
*/
void opt_statements() {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed", 0);
	}
}

/*
<statements> → <statement><statements_p>
First(statements) = {First(statement)}
= {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statements(void)
{
	statement();
	statements_p();
}

/*
<statements_p> → <statement><statements_p> | ε
First(statements_p) = {First(statement), ε}
= {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ε}
*/
void statements_p(void)
{
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		statements_p();
		break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statements();
			statements_p();
			break;
		}
	default:
		break;
	}
}

/*
<statement> → <assignment_statement> | <selection_statement> | <iteration_statement> | <input_statement> | <output_statement>
First(statement) = {First(assignment_statement), First(selection_statement), First(iteration_statement), First(input_statement), First(output_statement)}
= {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statement(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF:
			selection_statement();
			return;
		case WHILE:
			iteration_statement();
			return;
		case READ:
			input_statement();
			return;
		case WRITE:
			output_statement();
			return;
		}
	default:
		syn_printe();
		break;
	}
}

/*
<assignment statement> -> <assignment expression>;
First(assignment_statement) = {First(assignment_expression)}
= {AVID_T, SVID_T}
*/
void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode(";\n", 1);
	gen_incode("PLATY: Assignment statement parsed", 0);
}

/*
<assignment expression> -> AVID = <arithmetic_expression> | SVID = <string_expression>
First(assignment_expression) = {AVID_T, SVID_T}
*/
void assignment_expression(void)
{
	char* string = NULL;
	if (lookahead.code == AVID_T) {
		gen_incode(lookahead.attribute.vid_lex, 1); 
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		gen_incode("=", 1);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed", 0);
	}
	else if (lookahead.code == SVID_T) {
		/*The vid_lex is stored in the variable string, the $ sign is removed and a null terminator is added at the end*/
		string = lookahead.attribute.vid_lex;
		string[strlen(string) - 1] = '_';
		string[strlen(string)] = '\0';
		gen_incode(string, 1); 
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		gen_incode("=", 1);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed", 0);
	}
	else {
		syn_printe();
	}
}

/*
<selection_statement> → IF <pre-condition> (<conditional_expression>) THEN {<opt_statements>} ELSE {<opt_statements>};
First(selection_statement) = {KW_T(IF)}
*/
void selection_statement(void)
{
	match(KW_T, IF);
	gen_incode("if(", 1);
	pre_condiiton();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	/*If the pre-condition is false it surrounds the conditional expression with !()*/
	if (ifFalse == 1)
		gen_incode(")", 1);
	/*Resets the ifFalse variable*/
	gen_incode(") {\n", 1);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR); 
	gen_incode("}\nelse {\n", 1);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("}\n", 1);
	gen_incode("PLATY: Selection statement parsed", 0);
}

/*
<pre-condition> → TRUE | FALSE
First(pre-condition) = {KW_T(TRUE), KW_T(FALSE)}
*/
void pre_condiiton(void)
{
	if (lookahead.code == KW_T && lookahead.attribute.get_int == TRUE)
		match(KW_T, TRUE);
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == FALSE) {
		match(KW_T, FALSE);
		/*Surrounds the conditional statement with !() and sets the variable if false to true*/
		gen_incode("!(", 1);
		ifFalse = 1;
	}
	else
		syn_printe();
}

/*
<iteration_statement> → WHILE <pre-condition> (<conditional_expression>) REPEAT {<statements>};
First(iteration_statement) = {KW_T(WHILE)}
*/
void iteration_statement(void)
{
	match(KW_T, WHILE);
	gen_incode("while(", 1);
	pre_condiiton();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	/*If the pre-condition is false it surrounds the conditional expression with !()*/
	if (ifFalse == 1)
		gen_incode(")", 1);
	/*Resets the ifFalse variable*/
	ifFalse = 0;
	gen_incode(") {\n", 1);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("}\n", 1);
	gen_incode("PLATY: Iteration statement parsed", 0);
}

/*
<input_statement> → READ (<variable list>);
First(input_statement) = {KW_T(READ)}
*/
void input_statement(void)
{
	write = 2;
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	gen_incode("scanf(\"", 1);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode(");\n", 1);
	gen_incode("PLATY: Input statement parsed", 0);
}

/*
<variable_list> → <variable_identifier><variable_list_p>
First(variable_list) = {First(variable_identifier)}
= {AVID_T, SVID_T}
*/
void variable_list(void)
{
	countVariables = 0;
	variable_identifier();
	variable_list_p();
	/*Prints the %d, %f or %s based on the flag in printf and scanf*/
	for (int i = 0; i < countVariables; i++) {
		if(flags[i] == '0')
			gen_incode("%d ", 1);
		else if (flags[i] == '1')
			gen_incode("%f ", 1);
		else 
			gen_incode("%s ", 1);
	}
	gen_incode("\"", 1);
	for (int i = 0; i < countVariables; i++) {
		gen_incode(" ,", 1);
		/*If the statemnt is a write statement (scanf) then puts & before the variable name */
		if(write == 2)
			gen_incode("&", 1);
		gen_incode(variables[i], 1);
	}
	gen_incode("PLATY: Variable list parsed", 0);
	finalCountVariables = countVariables;
	countVariables = 0;
}

/*
<variable_list_p> → ,<variable_identifier><variable_list_p> | ε
First(variable_list_p) = {COM_T, ε}
*/
void variable_list_p(void)
{
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
}

/*
<variable_identifier> → AVID | SVID
First(variable_identifier) = {AVID_T, SVID_T}
*/
void variable_identifier(void)
{
	char* string = NULL;
	/*It checks the variable count, if it a multiple of 5 then it reserves space for 5 more. Works with 0 as well*/
	if (countVariables % 5 == 0) {
		flags = (char*)realloc(flags, (5 + countVariables)* sizeof(char));
		variables = (char**)realloc(variables, (5 + countVariables )* sizeof(char*));
	}
	if (lookahead.code == AVID_T) {
		/*Sets the flags in the variable list table to the flag value of the lookahead*/
		flags[countVariables] = lookahead.avid_attribute.flags;
		/*Mallocs space for thr variable name*/
		variables[countVariables] = (char *)malloc(strlen(lookahead.attribute.vid_lex) + 1);
		/*Sets the last character as the null terminator*/
		variables[countVariables][strlen(lookahead.attribute.vid_lex)] = '\0';
		/*Copies the value from string variable to the variable list table*/
		strcpy(variables[countVariables],lookahead.attribute.vid_lex);
		match(AVID_T, NO_ATTR);
		countVariables++;
	}
	else if (lookahead.code == SVID_T) {
		/*The vid_lex is stored in the variable string, the $ sign is removed and a null terminator is added at the end*/
		string = lookahead.attribute.vid_lex;
		string[strlen(string) - 1] = '_';
		string[strlen(string)] = '\0';
		/*Sets the flags in the variable list table to the flag value of the lookahead*/
		flags[countVariables] = lookahead.avid_attribute.flags;
		/*Mallocs space for thr variable name*/
		variables[countVariables] = (char *)malloc(strlen(lookahead.attribute.vid_lex) + 1);
		/*Copies the value from string variable to the variable list table*/
		strcpy(variables[countVariables], string);
		match(SVID_T, NO_ATTR);
		countVariables++;
	}
	else
		syn_printe();
}

/*
<output_statement> → WRITE (<output_list>);
First(output_statement) = {KW_T(WRITE)}
*/
void output_statement(void)
{
	/*It lets the variable list know if the list is for write or read*/
	write = 1;
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	gen_incode("printf(\"", 1);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode(");\n", 1);
	gen_incode("PLATY: Output statement parsed", 0);
}

/*
<output_list> → <variable_list> | < STR_T > | ε
First(output_list) = {First(variable_list), STR_T, ε}
= {AVID_T, SVID_T, STR_T, ε}
*/
void output_list(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		gen_incode(b_location(str_LTBL, lookahead.attribute.str_offset), 1);
		gen_incode("\"", 1);
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed", 0);
		break;
	default:
		gen_incode("\"", 1);
		gen_incode("PLATY: Output list (empty) parsed", 0);
	}
}

/*
<arithmetic_expression> → <unary_arithmetic_expression> | <additive_arithmetic_expression>
First(arithmetic_expression) = {First(unary_arithmetic_expression), First(additive_arithmetic_expression)}
= {ART_OP_T(PLUS), ART_OP_T(MINUS), AVID_T, FPL_T, INL_T, LPR_T}
*/
void arithmetic_expression(void)
{
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.get_int == PLUS || lookahead.attribute.get_int == MINUS)
			unary_arithmetic_expression();
		else {
			/*If the operator is MULTIPY or DIVIDE*/
			syn_printe();
			return;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed", 0);
}

/*
<unary_arithmetic_expression> → -<primary_arithmetic_expression> | +<primary_arithmetic_expression>
First(unary_arithmetic_expression) = {ART_OP_T(PLUS), ART_OP_T(MINUS)}
*/
void unary_arithmetic_expression(void)
{
	/*The function arithmetic_expression() call unary_arithmetic_expression() only if the operators is PLUS or MINUS*/
	if (lookahead.attribute.get_int == PLUS) {
		match(ART_OP_T, PLUS);
		gen_incode("+", 1);
	}
	else {
		match(ART_OP_T, MINUS);
		gen_incode("-", 1);
	}
	primary_arithmetic_expression();
	gen_incode("PLATY: Unary arithmetic expression parsed", 0);
}

/*
<additive_arithmetic_expression> → <multiplicative_arithmetic_expression><additive_arithmetic_expression_p>
First(additive_arithmetic_expression) = {First(multiplicative_arithmetic_expression)}
= {AVID_T, FPL_T, INL_T, LPR_T}
*/
void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*
<additive_arithmetic_expression_p> → +<multiplicative_arithmetic_expression><additive_arithmetic_expression_p>
| -<multiplicative_arithmetic_expression><additive_arithmetic_expression_p> | ε
First(additive_arithmetic_expression_p) = {ART_OP_T(PLUS), ART_OP_T(MINUS), ε}
*/
void additive_arithmetic_expression_p(void)
{
	if (lookahead.code == ART_OP_T) {
		if (lookahead.attribute.get_int == PLUS) {
			match(ART_OP_T, PLUS);
			gen_incode("+", 1);
		}
		else if (lookahead.attribute.get_int == MINUS) {
			match(ART_OP_T, MINUS);
			gen_incode("-", 1);
		}
		else
			return;
		multiplicative_arithmetic_expression();
		additive_arithmetic_expression_p();
		gen_incode("PLATY: Additive arithmetic expression parsed", 0);
	}
}

/*
<multiplicative_arithmetic_expression> → <primary_arithmetic_expression><multiplicative_arithmetic_expression_p>
First(multiplicative_arithmetic_expression) = {First(primary_arithmetic_expression)}
= {AVID_T, FPL_T, INL_T, LPR_T}
*/
void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*
<multiplicative_arithmetic_expression_p> → *<primary_arithmetic_expression><multiplicative_arithmetic_expression_p>
| \<primary_arithmetic_expression><multiplicative_arithmetic_expression_p> | ε
First(multiplicative_arithmetic_expression_p) = {ART_OP_T(MULT), ART_OP_T(DIV), ε}
*/
void multiplicative_arithmetic_expression_p(void)
{
	if (lookahead.code == ART_OP_T) {
		if (lookahead.attribute.get_int == MULT) {
			match(ART_OP_T, MULT);
			gen_incode("*", 1);
		}
		else if (lookahead.attribute.get_int == DIV) {
			match(ART_OP_T, DIV);
			gen_incode("/", 1);
		}
		else
			return;
		primary_arithmetic_expression();
		multiplicative_arithmetic_expression_p();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed", 0);
	}

}

/*
<primary_arithmetic_expression> → AVID_T | FPL_T | INL_T | (<additive_arithmetic_expression>)
First(primary_arithmetic_expression) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void primary_arithmetic_expression(void)
{
	/*It is used to store the converted number*/
	char buffer[64];
	switch (lookahead.code) {
	case AVID_T:
		gen_incode(lookahead.attribute.vid_lex, 1);
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		/*This method converts the floating point number into a string and stores it into the buffer*/
		snprintf(buffer, sizeof(buffer), "%f", lookahead.attribute.flt_value);
		gen_incode(buffer, 1);
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		/*This method converts the integer literal into a string and stores it into the buffer*/
		snprintf(buffer, sizeof(buffer), "%d", lookahead.attribute.int_value);
		gen_incode(buffer, 1);
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		gen_incode("(", 1);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		gen_incode(")", 1);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed", 0);
}

/*
<string_expression> → <primary_string_expression><string_expression_p>
First(string_expression) = {First(primary_string_expression)}
= {SVID_T, STR_T}
*/
void string_expression(void)
{
	gen_incode("\"", 1);
	primary_string_expression();
	string_expression_p();
	gen_incode("\"", 1);
	gen_incode("PLATY: String expression parsed", 0);
}

/*
<string_expression_p> → # <primary_string_expression><string_expression_p> | ε
First(string_expression_p) = {SCC_OP_T, ε}
*/
void string_expression_p(void)
{
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	}
}

/*
<primary_string_expression> → SVID_T | STR_T
First(primary_string_expression) = {SVID_T, STR_T}
*/
void primary_string_expression(void)
{
	char* string = NULL;
	if (lookahead.code == SVID_T) {
		/*The vid_lex is stored in the variable string, the $ sign is removed and a null terminator is added at the end*/
		string = lookahead.attribute.vid_lex;
		string[strlen(string) - 1] = '_';
		string[strlen(string)] = '\0';
		gen_incode(string, 1);
		match(SVID_T, NO_ATTR);
	}
	else if (lookahead.code == STR_T) {
		/*The string literal is surrounded with quotes if the expression parsed is a primary string relation expression*/
		if(pri_s_rel)
			gen_incode("\"", 1);
		gen_incode(b_location(str_LTBL, lookahead.attribute.str_offset),1);
		if (pri_s_rel)
			gen_incode("\"", 1);
		match(STR_T, NO_ATTR);
	}
	else
		syn_printe();
	gen_incode("PLATY: Primary string expression parsed", 0);
}

/*
<conditional_expression> → <logical_OR_expression>
First(conditional_expression) = {First(logical_OR_expression)}
= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void)
{
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed", 0);
}

/*
<logical_OR_expression> → <logical_AND_expression><logical_OR_expression_p>
First(logical_OR_expression) = {First(logical_AND_expression)}
= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_OR_expression(void)
{
	logical_AND_expression();
	logical_OR_expression_p();
}

/*
<logical_OR_expression_p> → .OR.<logical_AND_expression><logical_OR_expression_p> | ε
First(logical_OR_expression_p) = {LOG_OP_T(OR), ε}
*/
void logical_OR_expression_p(void)
{
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == OR) {
		match(LOG_OP_T, OR);
		gen_incode(" || ", 1);
		logical_AND_expression();
		logical_OR_expression_p();
		gen_incode("PLATY: Logical OR expression parsed", 0);
	}
}

/*
<logical_AND_expression> → <relational_expression><logical_AND_expression_p>
First(logical_AND_expression) = {Frist(relational_expression)}
= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_AND_expression(void)
{
	relational_expression();
	logical_AND_expression_p();
}

/*
<logical_AND_expression_p> → .AND.<relational_expression><logical_AND_expression_p> | ε
First(logical_AND_expression_p) = {LOG_OP_T(AND)., ε}
*/
void logical_AND_expression_p(void)
{
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == AND) {
		match(LOG_OP_T, AND);
		gen_incode(" && ", 1);
		relational_expression();
		logical_AND_expression_p();
		gen_incode("PLATY: Logical AND expression parsed", 0);
	}
}

/*
<relational_expression> → <primary_a_relational_expression><sign><primary_a_relational_expression>
| <primary_s_relational_expression><sign><primary_s_relational_expression>
First(relational_expression) = {First(primary_a_relational_expression), First(primary_s_relational_expression)}
= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		sign();
		primary_a_relational_expression();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		sign();
		primary_s_relational_expression();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed", 0);
}

/*
<sign> →    == | <> | > | <
First(sign) = {REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT)}
*/
void sign(void)
{
	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.get_int) {
		case EQ:
			match(REL_OP_T, EQ);
			gen_incode("==", 1);
			return;
		case NE:
			match(REL_OP_T, NE);
			gen_incode("!=", 1);
			return;
		case GT:
			match(REL_OP_T, GT);
			gen_incode(">", 1);
			return;
		case LT:
			match(REL_OP_T, LT);
			gen_incode("<", 1);
			return;
		default:
			syn_printe();
		}
	}
	syn_printe();
}

/*
<primary_a_relational_expression> →   AVID_T | FPL_T | INL_T
First(primary_a_relational_expression) = {AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void)
{
	/*It is used to store the converted number*/
	char buffer[64];
	switch (lookahead.code) {
	case AVID_T:
		gen_incode(lookahead.attribute.vid_lex, 1);
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		/*This method converts the floating point number into a string and stores it into the buffer*/
		snprintf(buffer, sizeof(buffer), "%f", lookahead.attribute.flt_value);
		gen_incode(buffer, 1);
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		/*This method converts the integer literal into a string and stores it into the buffer*/
		snprintf(buffer, sizeof(buffer), "%d", lookahead.attribute.int_value);
		gen_incode(buffer, 1);
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed", 0);

}

/*
<primary_s_relational_expression> → <primary_string_expression>
First(primary_s_relational_expression) = {First(primary_string_expression)}
= {SVID_T, STR_T}
*/
void primary_s_relational_expression(void)
{
	/*Sets the pri_s_rel to 1 so that the string literal can be surrounded with quotes*/
	pri_s_rel = 1;
	primary_string_expression();
	/*Resets the variables pri_s_rel to 0*/
	pri_s_rel = 0;
	gen_incode("PLATY: Primary s_relational expression parsed", 0);
}


