# PLATYPUS Compiler Project
Front end of a compiler built to process a custom computer language for a course at Algonquin College.

##Introduction
During the course of a semester at Algonquin college in the CST8152 Compilers class, I built the front end of a compiler to process a custom language designed by our professor Svillen Ranev. This was a project in which I learnt about and applied knowledge associated with:
  - Programming in C for a large-scale project.
  - Natural language processing.
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
In assignment 1, I was tasked with creating a dynamic data structure that could establish an initial capacity and growth method. Then, as it took in the characters of a text file, it would store them into the structure's allocated memory and either grow as it reached its initial capacity by a set increment, or not at all if it was fixed to a static size. The structure consisted of a a character array to store the characters themselves, along with a handling structure that recorded and remembered different offsets, a pointer to the character array, and flags for mode and end-of-buffer. This structure can be found in the buffer.h header file.

###Compilation and Testing

##Part 2 - The Scanner
In assignment 2, myself and a team member built a scanner structure that could take in the stored characters in the previous buffer structure one-by-one, and classify them into tokens. These tokens involved the different types of symbols involved in the Platypus language, such as different variable identifiers, operators, and separator characters, along with useful metadata, such as values of numeric and string variables, values of the variable names, and index in a table of keywords.

The scanner was both token driven and then transition-table driven later on. At first, it discounted the possibility an incoming set of characters being either a:
  - Newline character
  - Either a space, tab, vertical tab character
  - Separator character
  - Concatenation, assignment, arithmetic, or conditional operator
  - Logical operator
  - String literal
