/*
File name: parser.h
Compiler: MS Visual Studio 2015
Author: Harshdeep Verma - 040880851
Course: CST 8152 – Compilers, Lab Section: 13
Assignment: 3
Date: 07/12/2018
Professor: Sv. Ranev
Purpose: This file contains all the constants and protypes for parser.
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
#pragma once
#include "buffer.h"
#include "token.h"

#define NO_ATTR -8 /*No Attribute*/
#define ELSE 0  /*Kewword ELSE index*/
#define FALSE 1 /*Kewword FALSE index*/
#define IF 2 /*Kewword IF index*/
#define PLATYPUS 3 /*Kewword PLATYPUS index*/
#define READ 4 /*Kewword READ index*/
#define REPEAT 5 /*Kewword REPEAT index*/
#define THEN 6 /*Kewword THEN index*/
#define TRUE 7 /*Kewword TRUE index*/
#define WHILE 8 /*Kewword WHILE index*/
#define WRITE 9 /*Kewword WRITE index*/
#define AND 0 /*Logical Operator AND enum*/
#define OR 1  /*Logical Operator OR enum*/
#define EQ 0 /*Relational Operators Equal enum*/
#define NE 1 /*Relational Operator Not Equal enum*/
#define GT 2 /*Relational Operator Greater Than enum*/
#define LT 3 /*Relational Operator Less Than enum*/
#define PLUS 0 /*Arithemtic Operator PLUS enum*/
#define MINUS 1 /*Arithemtic Operator MINUS enum*/
#define MULT 2 /*Arithemtic Operator MULT enum*/
#define DIV 3/*Arithemtic Operator DIV enum*/

/*Lookahead Token is staic and is wideley used*/
static Token lookahead;
/*Keyword Index Table defined in table.h*/
extern char * kw_table[];
/*Function defined in Scanner.c*/
extern Token malar_next_token(void);
/*Buffer defined in platy.c*/
extern Buffer* str_LTBL;
/*Integer defined in Scanner.c*/
extern int line;
/*Used to calculate number of syntax error*/
int synerrno;
/*Used to determine whether the pre-condition was false or not*/
int ifFalse;
/*Used to determine whether selection statement was selected or iteration*/
int ifStatement;
/*Used to determine whether write statement was selected or read*/
int write;
/*File to which the c code is printed*/
extern FILE* c;
extern FILE* code;
/*It is a dynamic storage for the variable type flag used in variable list*/
char* flags;
/*It is a dynamic storage for the variable names used in variable list*/
char** variables;
/*Used to count how many variables are present in the variable list*/
int countVariables;
/*Used to count how many variables are present in the variable list last time the function is run so that all the dynamic memory can be freed*/
int finalCountVariables;
/*Used to know whether it is a primary_s_relational_expression*/
int pri_s_rel;
/*Symbol table is used to store all the varibales used in the program*/
extern variable **symbolTable;
/*Used to keep count of all the structs inside the symbol table*/
extern int symbolTableCounter;

void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(char * string, int c_code);
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void pre_condiiton(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void sign(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);