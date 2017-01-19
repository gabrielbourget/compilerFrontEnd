#ifndef PARSER_H_
#define PARSER_H_

#include "buffer.h"
#include "token.h"

#define SYN_ERR_INIT 0 /* Used to keep track of errors found during parsing of syntax */

/* Token Attributes */
#define NO_ATTR -1 
#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7

/* For debugging */
#define DEBUG
#undef DEBUG

#define MATCH_DBG
#undef MATCH_DBG

/* Extern variables */
extern char* kw_table[];

/*******************************/
/* --- FUNCTION PROTOTYPES --- */
/*******************************/
void match(int, int);
void parser(Buffer*);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char*);

/* Production functions */
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_p(void);

void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);

void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_p(void);

void output_statement(void);
void opt_variable_list(void);
void string_literal(void);
void output_list(void);

void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void additive_operators(void);
void multiplicative_operators(void);

void string_expression(void);
void primary_string_expression(void);
void string_expression_p(void);

void conditional_expression(void);
void logical_OR_expression(void);
void logical_AND_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression_p(void);

void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void rel_ops(void);

#endif
