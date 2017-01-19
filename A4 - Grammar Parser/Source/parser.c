/*****************************************************************************
File Name: parser.c
Compiler: gcc
Author: Gabriel Bourget
Course: CST8152 - Compilers
Assignment: A3 - Symbol Table
Date: December 08, 02016
Professor: Svillen Ranev
Purpose: Manages the parsing process for a program written in the PLATYPUS
         language.
Function List: parser(), tokenMatched(), match(), syn_eh(), syn_printe(),
               gen_incode(), program(), 
*****************************************************************************/

#include "parser.h"
#include "stable.h"
#include "token.h"

static Token lookahead; /* Global token used for predictive parsing*/
static Buffer* sc_buf; /* Used for parsing */
extern Buffer* str_LTBL; /* Keeps track of string contents of source file */
int synerrno = SYN_ERR_INIT; /* Error status int for parsing syntax */
extern STD sym_table; /* Keeps track of VIDs recognized during compilation */
extern int line; /* Keeps track of line number from source file */
extern Token mlwpar_next_token(Buffer* sc_buf); 

/********************************parser()**************************************
Purpose: Triggers predictive PLATYPUS parsing process. Recursively moves through
         parsing rules of the PLATYPUS grammar productions up until the SEOF_T 
		 token is found.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: mlwpar_next_token(), program(), match(), gen_incode()
Parameters: -> in_buf (Pointer to Buffer structure)
               type: Buffer*
Return Value: ---
Algorithm: -> bind local buffer pointer sc_buf to address pointed to by in_buf.
           -> Gather next token from the buffer.
				   -> Call program() function.
				   -> When recursive call structure exits program() call match(),
				      anticipating EOF.
				   -> Generate an incode saying that the source file is parsed.
*********************************************************************************/
void parser(Buffer* in_buf) {

#ifdef DEBUG
	printf("In parser() function \n");
#endif

	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program();
	match(SEOF_T, NO_ATTR); 
	gen_incode("PLATY: Source file parsed");
}

/********************************match()**************************************
Purpose: 
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: tokenMatched(), syn_eh()
Parameters: -> pr_token_code (Represents token type)
						   type: int
						-> pr_attribute_code (Represents token attribute)
						   type: int
Return Value: ---
Algorithm: 
*****************************************************************************/
void match(int pr_token_code, int pr_attribute_code) {

#ifdef MATCH_DBG
	printf("In match function with t_code %s and attribute code %s\n", tokenCodeToString(pr_token_code),
		                                                              tokenAttributeToString(lookahead));
#endif

	if (lookahead.code != pr_token_code) {syn_eh(pr_token_code); return;}
	switch (pr_token_code) {
	/* Keywords */
	case KW_T:
		if (lookahead.attribute.kwt_idx == pr_attribute_code) break;
		else {syn_eh(pr_token_code); return;}
	/* Logical operators */
	case LOG_OP_T:
		if (lookahead.attribute.log_op == pr_attribute_code) break;
		else {syn_eh(pr_token_code); return;}
	/* Arithmetic operators */
	case ART_OP_T:
		if (lookahead.attribute.arr_op == pr_attribute_code) break;
		else {syn_eh(pr_token_code); return;}
	/* Relational operators */
	case REL_OP_T:
		if (lookahead.attribute.rel_op == pr_attribute_code) break;
		else {syn_eh(pr_token_code); return;}
	/* Handles all other cases, where pr_attribute_code is NO_ATTR */
	default: break;
	}
	/* Token is end of file marker */
	if (lookahead.code == SEOF_T) return;
	else lookahead = mlwpar_next_token(sc_buf);
	/* Next token is an error token */
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = mlwpar_next_token(sc_buf);
		++synerrno;
		return;
	}
}

/********************************syn_eh()**************************************
Purpose: Error handling function for the parser. Implements panic-mode recovery.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: syn_printe(), exit(), mlwpar_next_token()
Parameters: -> sync_token_code (Representing token required by the parser)
               type: int
Return Value: ---
Algorithm: -> Call syn_printe().
           -> Increment synerrno error counter.
				   -> While lookahead token != sync_token_code...
				      -> If lookahead token is SEOF_T and sync_token_code != SEOF_T,
				         exit() with parameter synerrno.
				      -> Grab next token into lookahead slot.
				   -> If lookahead token is SEOF_T and sync_token_code is SEOF_T, return.
				   -> Before returning, step forward lookahead token once more.
*******************************************************************************/
void syn_eh(int sync_token_code) {
	syn_printe();
	++synerrno;
	while (lookahead.code != sync_token_code) {
		if ((lookahead.code == SEOF_T) && (sync_token_code != SEOF_T)) {
			exit(synerrno);
		}
		lookahead = mlwpar_next_token(sc_buf);
	}
	if ((lookahead.code == SEOF_T) && (sync_token_code == SEOF_T)){
		return;
	}
	lookahead = mlwpar_next_token(sc_buf);
}

/********************************syn_printe()**************************************
Purpose: Error printing function for the parser.
Author: Svillen Ranev
History/Version: v1.0
Called Functions: printf(), 
Parameters: ---
Return Value: ---
Algorithm: -> Determine what the lookahead token is.
           -> Print a type-specific error message for it.
**********************************************************************************/
void syn_printe(){
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_cbhead(str_LTBL) + t.attribute.str_offset);
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
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
	} /* end switch */
} /* end syn_printe()*/


/********************************gen_incode()**************************************
Purpose: Generates incodes after parsing productions.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: fprintf()
Parameters: incode (Code to be printed)
            type: char*
Return Value: ---
Algorithm: -> Print incode passed into the function.
**********************************************************************************/
void gen_incode(char* incode) {
	printf("%s\n", incode);
}

/********************************program()**************************************
Purpose: Production function for the <program> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), opt_statements(), gen_incode()
Parameters: ---
Return Value: ---
Production: <program> -> PLATYPUS{<opt_statements>}
					  FIRST(<program>) = {PLATYPUS}
Algorithm: -> Follow production rule.
********************************************************************************/
void program(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <program>\n");
#endif

	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/********************************opt_statements()**************************************
Purpose: Production function for the <opt_statements> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: statements()
Parameters: ---
Return Value: ---
Production: <opt_statements> -> <statements> | e
		    		FIRST(<opt_statements>) = {AVID,SVID,IF,USING,INPUT,OUTPUT,e}
Algorithm: -> Follow production rule.
Algorithm: -> Print incode passed into the function.
***************************************************************************************/

void opt_statements(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <opt_statements>\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if ((lookahead.attribute.kwt_idx != PLATYPUS)
			&&(lookahead.attribute.kwt_idx != ELSE)
			&&(lookahead.attribute.kwt_idx != THEN)
			&& (lookahead.attribute.kwt_idx != REPEAT)) {
			statements();
			break;
		}
	default:
		gen_incode("PLATY: Opt_statements parsed");
		break;
	}
}

/********************************statements()**************************************
Purpose: Production function for the <statements> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: statements()
Parameters: ---
Return Value: ---
Production: <statements> -> <statement> <statements'>
		    		FIRST(<statements>) = {AVID,SVID,IF,USING,INPUT,OUTPUT}
Algorithm: -> Follow production rule.
************************************************************************************/
void statements(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <statements>\n");
#endif

	statement(); statements_p();
}

/********************************statement()**************************************
Purpose: Production function for the <statement> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: assignment_statement(), selection_statement(), iteration_statement(),
                  input_statement(), output_statement()
Parameters: ---
Return Value: ---
Production: <statement> -> <assignment statement> | <selection statement> |
                           <iteration statement> | <input statement> |
						  						 <output statement>
		    		FIRST(<statement>) = {AVID,SVID,IF,USING,INPUT,OUTPUT}
Algorithm: -> Follow production rule.
************************************************************************************/
void statement(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <statement>\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement(); break;
	case KW_T:
		if (lookahead.attribute.kwt_idx == IF) {
			selection_statement(); break;
		}
		if (lookahead.attribute.kwt_idx == USING) {
			iteration_statement(); break;
		}
		if (lookahead.attribute.kwt_idx == INPUT) {
			input_statement(); break;
		}
		if (lookahead.attribute.kwt_idx == OUTPUT) {
			output_statement(); break;
		}
	default: syn_printe();
	}
}

/********************************statements_p()**************************************
Purpose: Production function for the <statements'> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: statement(), statements_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <statements'> -> <statement><statements'> | e
            FIRST(<statements'>) = {AVID,SVID,IF,USING,INPUT,OUTPUT,e}
Algorithm: -> Follow production rule.
************************************************************************************/
void statements_p(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <statements'>\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T : statement(); statements_p(); break;
	case KW_T:
		if (lookahead.attribute.kwt_idx == IF) {
			statement(); statements_p(); break;
		}
		if (lookahead.attribute.kwt_idx == USING) {
			statement(); statements_p(); break;
		}
		if (lookahead.attribute.kwt_idx == INPUT) {
			statement(); statements_p(); break;
		}
		if (lookahead.attribute.kwt_idx == OUTPUT) {
			statement(); statements_p(); break;
		}
	default: break;
	}
}

/********************************assignment_statement()**************************************
Purpose: Production function for the <assignment_statement> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: arithmetic_expression(), string_expression(), syn_printe()
Parameters: ---
Return Value: ---
Production: <assigmnment statement> -> <assignment expression>;
						FIRST(<assignment statement>) = {AVID,SVID}
Algorithm: -> Follow production rule.
**********************************************************************************************/
void assignment_statement(void) {
#ifdef DEBUG
	printf("------------------------------------------------------> <assignment statement>\n");
#endif

	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/********************************assignment_expression()**************************************
Purpose: Production function for the <assignment_expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: arithmetic_expression(), string_expression(), syn_printe()
Parameters: ---
Return Value: ---
Production: <assigmnment expression> -> AVID = <arithmetic expression> 
                                       |SVID = <string expression>
            FIRST(<assignment expression>) = {AVID,SVID}
Algorithm: -> Follow production rule.
**********************************************************************************************/
void assignment_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <assignment_expression>\n");
#endif

	switch (lookahead.code) {
	case AVID_T: 
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression(); 
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T: 
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression(); 
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe();
	}
}

/********************************selection_statement()**************************************
Purpose: Production function for the <selection statement> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), conditional_expression(), opt_statements(), gen_incode()
Parameters: ---
Return Value: ---
Production: <selection statement> -> IF(<conditional_expression>)THEN<opt_statements>
                                     ELSE(<opt_statements>);
            FIRST(<selection statement>) = {IF}
Algorithm: -> Follow production rule.
*********************************************************************************************/
void selection_statement(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <selection statement>\n");
#endif

	match(KW_T, IF);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);;
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}

/********************************iteration statement()**************************************
Purpose: Production function for the <iteration statement> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), assignment_expression(), conditional_expression(), gen_incode()
Parameters: ---
Return Value: ---
Production: <iteration statement> -> USING(<assignment expression>,<conditional expression>,
                                           <assignment expression>)
									 									 REPEAT{<opt_statements>};
						FIRST(<iteration statement>) = {USING}
Algorithm: -> Follow production rule.
********************************************************************************************/
void iteration_statement(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <iteration statement>");
#endif

	match(KW_T, USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}

/********************************intput_statement()**************************************
Purpose: Production function for the <input statement> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), variable_list(), gen_incode()
Parameters: ---
Return Value: ---
Production: <input statement> -> INPUT(<variable list>);
            FIRST(<input statement>) = {INPUT}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void input_statement(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <input statement>");
#endif

	match(KW_T, INPUT);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}

/********************************variable_list()**************************************
Purpose: Production function for the <variable list> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: variable_identifier(), variable_list_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <variable list> -> <variable identifier><variable list'>
						FIRST(<variable list>) = {AVID_T,SVID_T}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void variable_list(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <variable list>\n");
#endif

	variable_identifier(); variable_list_p(); 
	gen_incode("PLATY: Variable list parsed");
}

/********************************variable_identifier()**************************************
Purpose: Production function for the <variable identifier> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <variable identifier> -> AVID_T | SVID_T
		   		  FIRST(<variable identifier>) = {AVID_T,SVID_T}
Algorithm: -> Follow production rule.
*******************************************************************************************/
void variable_identifier(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <variable identifier>\n");
#endif

	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case SVID_T: match(SVID_T, NO_ATTR); break;
	default: syn_printe();
	}
}

/********************************variable_list_p()**************************************
Purpose: Production function for the <variable list'> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <variable list'> -> ,<variable identifier> <variable list'> | e
						FIRST(<variable list'>) = {,,e}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void variable_list_p(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <variable list'>\n");
#endif

	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
}

/********************************output_statement()**************************************
Purpose: Production function for the <output statement> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), output_list(), gen_incode()
Parameters: ---
Return Value: ---
Production: <output statement> -> OUTPUT(<output list>);
						FIRST(<output statement>) = {OUTPUT}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void output_statement(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <output statement>\n");
#endif

	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}

/********************************opt_variable_list()**************************************
Purpose: Production function for the <opt_variable list> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: variable_list(), gen_incode()
Parameters: ---
Return Value: ---
Production: <opt_variable list> -> <variable list> | e
		 		    FIRST(<opt_variable list>) = {AVID_T,SVID_T,e}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void opt_variable_list(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <opt_variable list>\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: variable_list(); break;
	default: break;
	}
}

/********************************string_literal()**************************************
Purpose: Production function for the <string literal> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match()
Parameters: ---
Return Value: ---
Production: <string literal> -> STR_T
					 	FIRST(<string literal>) = {STR_T}
Algorithm: -> Follow production rule.
***************************************************************************************/
void string_literal(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <string literal>\n");
#endif

	match(STR_T, NO_ATTR);
}

/********************************output_list()**************************************
Purpose: Production function for the <output list> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), opt_variable_list(), gen_incode()
Parameters: ---
Return Value: ---
Production: <output list> -> <opt_variable list> | e
						FIRST(<output list>) = {AVID_T,SVID_T,STR_T,e}
Algorithm: -> Follow production rule.
************************************************************************************/
void output_list(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <output_list>\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		opt_variable_list(); 
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default: gen_incode("PLATY: Output list (empty) parsed");
	}
}

/********************************arithmetic_expression()**************************************
Purpose: Production function for the <arithmetic expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: additive_arithmetic_expression(), unary_arithmetic_expression(),
                  syn_printe()
Parameters: ---
Return Value: ---
Production: <arithmetic expression> -> <unary arithmetic expression> 
								   	   | <additive arithmetic expression>
						FIRST(<arithmetic expression>) = {AVID_T,FPL_T,INL_T,(,+,-}
Algorithm: -> Follow production rule.
**********************************************************************************************/
void arithmetic_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <arithmetic expression>\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: 
		additive_arithmetic_expression(); 
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case (ART_OP_T):
		if (lookahead.attribute.arr_op == PLUS
			||(lookahead.attribute.arr_op == MINUS)) {
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		}
	default: syn_printe();
	}
}

/********************************unary_arithmetic_expression()**************************************
Purpose: Production function for the <unary arithmetic expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: additive_operators(), primary_arithmetic_expression(), gen_incode()
Parameters: ---
Return Value: ---
Production: <unary arithmetic expression> -> <additive operators> <primary arithmetic expression>
						FIRST(<unary arithmetic expression>) = {+,-}
Algorithm: -> Follow production rule.
****************************************************************************************************/
void unary_arithmetic_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <unary_arithmetic_expression>\n");
#endif

	additive_operators(); primary_arithmetic_expression(); 
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/********************************additive_arithmetic_expression()**************************************
Purpose: Production function for the <additive arithmetic expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: multiplicative_arithmetic_expression(), additive_arithmetic_expression_p(),
                  gen_incode()
Parameters: ---
Return Value: ---
Production: <additive arithmetic expression> -> <multiplicative arithmetic expression> 
																													<additive arithmetic expression'>
						FIRST(<additive arithmetic expression>) = {AVID_T,FPL_T,INL_T,(}
Algorithm: -> Follow production rule.
******************************************************************************************************/
void additive_arithmetic_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <additive arithmetic expression>\n");
#endif

	multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
}

/********************************additive_arithmetic_expression_p()**************************************
Purpose: Production function for the <additive arithmetic expression'> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: additive_operators(), multiplicative_arithmeti_expression(), 
                  additive_arithmetic_expression_p()
Parameters: ---
Return Value: ---
Production: <additive arithmetic expression'> -> <additive operators> 
                                                 <multiplicative arithmetic expression>
																								 <additive arithmetic expression'> | e
						FIRST(<additive arithmetic expression'>) = {+,-,e}
Algorithm: -> Follow production rule.
********************************************************************************************************/
void additive_arithmetic_expression_p(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <additive_arithmetic_expression'>\n");
#endif

	switch (lookahead.code) {
	case ART_OP_T:
		if ((lookahead.attribute.arr_op == PLUS)
			|| (lookahead.attribute.arr_op == MINUS)) {
			additive_operators();
			multiplicative_arithmetic_expression(); 
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	default:
		break;
	}
}

/********************************primary_arithmetic_expression()**************************************
Purpose: Production function for the <primary arithmetic expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), arithmetic_expression(), syn_printe()
Parameters: ---
Return Value: ---
Production: <primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
						FIRST(<additive arithmetic expression>) = {AVID_T,FPL_T,INL_T,(}
Algorithm: -> Follow production rule.
******************************************************************************************************/
void primary_arithmetic_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <primary_arithmetic_expression>\n");
#endif

	switch (lookahead.code) {
	case AVID_T: 
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case INL_T: 
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	default: syn_printe();
	}
}

/********************************multiplicative_arithmetic_expression()**************************************
Purpose: Production function for the <multiplicative arithmetic expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: primary_arithmetic_expression(), multiplicative_arithmetic_expression_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <multiplicative arithmetic expression> -> <primary arithmetic expression>
													    <multiplicative arithmetic expression'>
						FIRST(<multiplicative arithmetic expression>) = {AVID_T,FPL_T,INL_T,(}
Algorithm: -> Follow production rule.
*************************************************************************************************************/
void multiplicative_arithmetic_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <multiplicative arithmetic expression>\n");
#endif

	primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
}

/********************************multiplicative_arithmetic_expression_p()**************************************
Purpose: Production function for the <multiplicative arithmetic expression'> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: multiplicative_operators(), primary_arithmetic_expression(), 
                  multiplicative_arithmetic_expression_p()
Parameters: ---
Return Value: ---
Production: <multiplicative arithmetic expression'> -> <multiplicative operators>
													   <primary arithmetic expression>
                                                       <multiplicative arithmetic expression'> | e
						FIRST(<additive arithmetic expression>) = {*,/,e}
Algorithm: -> Follow production rule.
**************************************************************************************************************/
void multiplicative_arithmetic_expression_p(void) {
	
#ifdef DEBUG
	printf("------------------------------------------------------> <multiplicative arithmetic expression>\n");
#endif

	switch (lookahead.code) {
	case ART_OP_T:
		if ((lookahead.attribute.arr_op == MULT)
			|| (lookahead.attribute.arr_op == DIV)) {
			multiplicative_operators();
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	default: break;
	}
}

/********************************additive_operators()**************************************
Purpose: Production function for the <additive operators> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <additive operators> -> + | -
						FIRST(<additive operators>) = {+,-}
Algorithm: -> Follow production rule.
*******************************************************************************************/
void additive_operators(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <additive operators>\n");
#endif

	if (lookahead.attribute.arr_op == PLUS) match(ART_OP_T, PLUS);
	else if (lookahead.attribute.arr_op == MINUS) match(ART_OP_T, MINUS);
	else syn_printe();
}

/********************************multiplicative_operators()**************************************
Purpose: Production function for the <multiplicative operators> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <multiplicative operators> -> * | /
						FIRST(<multiplicative operators>) = {*,/}
Algorithm: -> Follow production rule.
************************************************************************************************/
void multiplicative_operators(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <multiplicative operators>\n");
#endif

	if (lookahead.attribute.arr_op == MULT) match(ART_OP_T, MULT);
	else if (lookahead.attribute.arr_op == DIV) match(ART_OP_T, DIV);
	else syn_printe();
}

/********************************string_expression()**************************************
Purpose: Production function for the <string expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: primary_string_expression(), string_expression_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <string expression> -> <primary string expression> <string expression'>
						FIRST(<string expression>) = {SVID_T,STR_T}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void string_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <string expression>\n");
#endif

	primary_string_expression(); string_expression_p();
	gen_incode("PLATY: String expression parsed");
}

/********************************primary_string_expression()**************************************
Purpose: Production function for the <string expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <primary string expression> -> SVID_T | STR_T
						FIRST(<primary string expression>) = {SVID_T,STR_T}
Algorithm: -> Follow production rule.
*************************************************************************************************/
void primary_string_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <primary string expression>\n");
#endif

	switch (lookahead.code) {
	case SVID_T: 
		match(SVID_T, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	default: syn_printe();
	}

}

/********************************string_expression_p()**************************************
Purpose: Production function for the <string expression'> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: primary_string_expression(), string_expression_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <string expression'> -> # <primary string expression> <string expression'> | e
						FIRST(<string expression'>) = {#,e}
Algorithm: -> Follow production rule.
*******************************************************************************************/
void string_expression_p(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <string expression'>\n");
#endif

	switch (lookahead.code) {
	case SCC_OP_T: 
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
		break;
	default:
		break;
	}
}

/********************************conditional_expression()**************************************
Purpose: Production function for the <conditional expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: logical_OR_expression()
Parameters: ---
Return Value: ---
Production: <conditional expression> -> <logical OR expression>
						FIRST(<conditional expression>) = {AVID_T,FPL_T,INL_T,SVID_T,STR_T}
Algorithm: -> Follow production rule.
*****************************************************************************************/
void conditional_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <conditional expression>\n");
#endif

	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/********************************logical_OR_expression()**************************************
Purpose: Production function for the <logical OR expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: logical_AND_expression(), logical_OR_expression_p()
Parameters: ---
Return Value: ---
Production: <logical OR expression> -> <logical AND expression> <logical OR expression'>
						FIRST(<logical OR expression>) = {AVID_T,FPL_T,INL_T,SVID_T,STR_T}
Algorithm: -> Follow production rule.
*********************************************************************************************/
void logical_OR_expression(void) {
	
#ifdef DEBUG
	printf("------------------------------------------------------> <logical OR expression>\n");
#endif

	logical_AND_expression(); logical_OR_expression_p();
}

/********************************logical_AND_expression()**************************************
Purpose: Production function for the <logical AND expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: relational_expression(), logical_AND_expression_p()
Parameters: ---
Return Value: ---
Production: <logical AND expression> -> <relational expression> <logical AND expression'>
						FIRST(<logical AND expression>) = {AVID_T,FPL_T,INL_T,SVID_T,STR_T}
Algorithm: -> Follow production rule.
*********************************************************************************************/
void logical_AND_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <logical AND expression>\n");
#endif

	relational_expression(); logical_AND_expression_p();
}

/********************************logical_OR_expression_p()**************************************
Purpose: Production function for the <logical OR expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), logical_AND_expression(), logical_OR_expression_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <logical OR expression'> -> .OR. <logical AND expression> <logical OR expression'> | e
						FIRST(<logical OR expression'>) = {.OR.,e}
Algorithm: -> Follow production rule.
***********************************************************************************************/
void logical_OR_expression_p(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <logical OR expression'>\n");
#endif

	switch (lookahead.code) {
	case LOG_OP_T: 
		if (lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_p();
			gen_incode("PLATY: Logical OR expression parsed");
		}
	default: break;
	}
}

/********************************logical_AND_expression_p()**************************************
Purpose: Production function for the <logical AND expression'> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), relational_expression(), logical_AND_expression_p(), gen_incode()
Parameters: ---
Return Value: ---
Production: <logical AND expression'> -> .AND. <relational expression> <logical AND expression'> | e
						FIRST(<logical AND expression'>) = {.AND.,e}
Algorithm: -> Follow production rule.
***********************************************************************************************/
void logical_AND_expression_p(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <logical_AND_expression'>\n");
#endif

	switch (lookahead.code) {
	case LOG_OP_T:
		if (lookahead.attribute.log_op == AND) {
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_p();
			gen_incode("PLATY: Logical AND expression parsed");
		}
	default: break;
	}
}

/********************************relational_expression()**************************************
Purpose: Production function for the <relational expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: primary_a_relational_expression(), rel_ops(), primary_s_relational_expression(),
				  syn_printe()
Parameters: ---
Return Value: ---
Production: <relational expression> -> <primary a_relational expression> 
									   <rel_ops>
									   <primary a_relational expression>
									   |
									   <primary s_relational expression>
									   <rel_ops>
									   <primary s_relational expression>
						FIRST(<relational expression>) = {AVID_T,FPL_T,INL_T,SVID_T,STR_T}
Algorithm: -> Follow production rule.
***********************************************************************************************/
void relational_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <relational expression>\n");
#endif

	if ((lookahead.code == AVID_T)
		|| (lookahead.code == FPL_T)
		|| (lookahead.code == INL_T)) {
		primary_a_relational_expression();
		rel_ops();
		primary_a_relational_expression();
		//gen_incode("PLATY: Primary a_relational expression parsed");
		gen_incode("PLATY: Relational expression parsed");
	}
	else if ((lookahead.code == SVID_T)
		|| (lookahead.code == STR_T)) {
		primary_s_relational_expression();
		gen_incode("PLATY: Primary s_relational expression parsed");
		rel_ops();
		primary_s_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
	}
	else syn_printe();
}

/********************************primary_a_relational_expression()**************************************
Purpose: Production function for the <primary a_relational expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <primary a_relational expression> -> AVID_T | FPL_T | INL_T
						FIRST(<primary a_relational expression>) = {AVID_T,FPL_T,INL_T}
Algorithm: -> Follow production rule.
********************************************************************************************************/
void primary_a_relational_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <primary a_relational expression>\n");
#endif

	switch (lookahead.code) {
	case AVID_T: 
		match(AVID_T, NO_ATTR); 
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case INL_T: match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	default: syn_printe();
	}
	//gen_incode("PLATY: Primary a_relational expression parsed");
}

/********************************primary_s_relational_expression()**************************************
Purpose: Production function for the <primary s_relational expression> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <primary s_relational expression> -> SVID_T | STR_T
						FIRST(<primary s_relational expression>) = {SVID_T,STR_T}
Algorithm: -> Follow production rule.
********************************************************************************************************/
void primary_s_relational_expression(void) {

#ifdef DEBUG
	printf("------------------------------------------------------> <primary s_relational expression>\n");
#endif

	switch (lookahead.code) {
	case SVID_T:
		match(SVID_T, NO_ATTR);
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;
	default: syn_printe();
	}
}

/**********************************rel_ops()****************************************
Purpose: Production function for the <rel_ops> nonterminal
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: match(), syn_printe()
Parameters: ---
Return Value: ---
Production: <rel_ops> -> == | <> | > | <
						FIRST(<rel_ops>) = {==,<>,>,<}
Algorithm: -> Follow production rule.
************************************************************************************/
void rel_ops(void) {
	if (lookahead.attribute.rel_op == EQ) match(REL_OP_T, EQ); 
	else if (lookahead.attribute.rel_op == NE) match(REL_OP_T, NE);
	else if (lookahead.attribute.rel_op == GT) match(REL_OP_T, GT);
	else if (lookahead.attribute.rel_op == LT) match(REL_OP_T, LT);
	else syn_printe();
}

char* tokenCodeToString(int code) {
	switch (code) {
	case 0: return "ERR_T";
	case 1: return "SEOF_T";
	case 2: return "AVID_T";
	case 3: return "SVID_T";
	case 4: return "FPL_T";
	case 5: return "INL_T";
	case 6: return "STR_T";
	case 7: return "SCC_OP_T";
	case 8: return "ASS_OP_T";
	case 9: return "ART_OP_T";
	case 10: return "REL_OP_T";
	case 11: return "LOG_OP_T";
	case 12: return "LPR_T";
	case 13: return "RPR_T";
	case 14: return "LBR_T";
	case 15: return "RBR_T";
	case 16: return "KW_T";
	case 17: return "COM_T";
	case 18: return "EOS_T";
	default: return "NA";
	}
}

char* tokenAttributeToString(Token lookahead) {
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.kwt_idx) {
		case 0: return "ELSE";
		case 1: return "IF";
		case 2: return "INPUT";
		case 3: return "OUTPUT";
		case 4: return "PLATYPUS";
		case 5: return "REPEAT";
		case 6: return "THEN";
		case 7: return "USING";
		}
		break;
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case 0: return "PLUS";
		case 1: return "MINUS";
		case 2: return "MULT";
		case 3: return "DIV";
		}
		break;
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case 0: return "AND";
		case 1: return "OR";
		}
		break;
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case 0: return "EQ";
		case 1: return "NE";
		case 2: return "GE";
		case 3: return "LE";
		}
	default: return "NO_ATTR";
	}
}