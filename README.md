# PLATYPUS Compiler Project
Front end of a compiler built to process a custom computer language for a course at Algonquin College.

##Introduction
During the course of a semester at Algonquin college in the CST8152 Compilers class, I built the front end of a compiler to process a custom language designed by our professor Svillen Ranev. This was a project in which I learnt about and applied knowledge associated with:
  - Programming in C for a large-scale project
  - Natural language processing
  - Bitwise manipulation of variables
  - Dynamic memory management strategies
  - Dynamic, adaptable data structures (buffers)
  - Token generation from character analysis (scanner)
  - Creation and indexing of metadata for a computer program (symbol table)
  - Analysis and categorization of series of tokens into a set grammar (parser)
  
##Content Summary
The project took part in four major parts:
  1. Buffer Structure
  2. Character Scanner
  3. Symbol Table
  4. Grammar Parser

##Part 1 - The Buffer
In assignment 1, I was tasked with creating a dynamic data structure that could establish an initial capacity and growth method. Then, as it took in the characters of a text file, it would store them into the structure's allocated memory and either grow as it reached its initial capacity by a set increment, or not at all if it was fixed to a static size. The buffer can be set to grow by a specific capacity when full (additive mode), by a certain percentage of its current size (multiplicative mode), or to stay the same mode (fixed mode). The structure consisted of a character array to store the characters themselves, along with a handling structure that recorded and remembered different offsets, a pointer to the character array, and flags for mode and end-of-buffer. This structure can be found in the buffer.h header file.

###Compilation and Testing
The source files for this project are:
  - buffer.c
  - buffer.h
  - platy_bt.c
 
Source files are to be compiled into one executable, and the main function is contained in platy_bt.c. To run the program and process some of the test files, the command line template is:

  - ./(executable name) (path to input file) (operation mode ('f', 'a', or 'm')) > (path to output file)

##Part 2 - The Scanner
In assignment 2, myself and a team member built a scanner structure that could take in the stored characters in the previous buffer structure one-by-one, and classify them into tokens. These tokens involved the different types of symbols involved in the Platypus language, such as different variable identifiers, operators, and separator characters, along with useful metadata, such as values of numeric and string variables, values of the variable names, and index in a table of keywords. This structure can be further inspected in the token.h header file. The main program that accompanies the code for this project prints out the tokens as they are returned, but they can be stored for later analysis, as they are in later projects.

The scanner was both token driven and then transition-table driven later on. At first, it discounted the possibility an incoming set of characters being either a:
  - Newline character
  - Either a space, tab, vertical tab character
  - Separator character
  - Concatenation, assignment, arithmetic, or conditional operator
  - Logical operator
  - String literal
  
Following this, the logic structure enters a finite state machine, the transitions from state to state being governed by the transition table layed out in the table.h header file.

When and end-of-file marker is reached in the source file, the scanner ceases to operate and returns control flow to the main function.

###Compilation and Testing
The source files for this project are:
  - buffer.c
  - buffer.h
  - scanner.c
  - table.h
  - token.h
  - platy_st.c
  
Source files are to be compiled into one executable, and the main function is contained in platy_st.c. To run the program and process some of the test files, the command line template is:

  - ./(executable name) (path to input file) > (path to output file)

##Part 3 - The Symbol Table
In assignment 3, I was tasked with creating a symbol table data structure, which was a managed database of the variable identifiers identified during the scanning process, along with other useful information, such as the variable name, type, initial value, and line number. The symbol table descriptor is a container structure which holds a pointer to the database of symbol table variable records, a count of how many are stored, an offset number to next STVR to be added, and a pointer to a buffer structure holding all the variable names.

To capture this information at the most efficient time possible, the scanner code was modified slightly. With these modifications, when the scanner code identified a token that could be identified as either an arithmetic or string variable identifier, it was incorporated into the symbol table as the scanner token was generated. These changes can be seen in scanner.c, in the the accepting state functions for arithmetic and string variable identifiers (aa_funct02() and aa_func03()).

###Compilation and Testing
The source files for this project are:
  - buffer.c
  - buffer.h
  - scanner.c
  - table.h
  - token.h
  - stable.c
  - stable.h
  - platy_tt.c
  
Source files are to be compiled into one executable, and the main function is contained in platy_tt.c. To run the program and process some of the test files, the command line template is:

  - ./(executable name) (path to input file) > (path to output file)

##Part 4 - The Parser
In assignment 4, the goal was to build a predictive, recursive descent LL(1) parser modeled after the Grammar of the Platypus language. The first part of the assignment involved taking this set of grammar rules, and modifying them (LR->LL transformations, left-factoring, and removal of left recursion) so that they may be coded in such a way to work within the specified parser specifications. Further reading on language grammars and parsing can be found in the lecture notes that I created during the course of the semester (stored in the root folder of this repository).

Once the grammar was appropriately modified, I translated the production rules into functions (parser.c) which allowed the parser to move from token to token, identifying larger structures like if/else statements, for loops, and relational expressions. The tokens entered the parser structure upon encountering the starting token (the keyword PLATYPUS), and it recursively processed them until encountering the end-of-file token.

###Compilation and Testing
The source files for this project are:
  - buffer.c
  - buffer.h
  - scanner.c
  - table.h
  - token.h
  - stable.c
  - stable.h
  - parser.c
  - parser.h
  - platy.c
  
Source files are to be compiled into one executable, and the main function is contained in platy.c. To run the program and process some of the test files, the command line template is:

  - ./(executable name) (path to input file) > (path to output file)
