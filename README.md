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
In assignment 1, I was tasked with creating a dynamic data structure that could establish an initial capacity and growth method. Then, as it took in the characters of a text file, it would store them into the structure's allocated memory and either grow as it reached its initial capacity by a set increment, or not at all if it was fixed to a static size.

###Compilation and Testing
