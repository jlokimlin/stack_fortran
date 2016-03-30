# **stack\_fortran**

This Fortran project implements the **stack** data structure based on the principle of Last In First Out (LIFO).

-----------------------------------------------------------------------------

## Requirements
* The GNU Make tool https://www.gnu.org/software/make/
* The GNU gfortran compiler https://gcc.gnu.org/wiki/GFortran

-----------------------------------------------------------------------------

## To build the project

Type the following command line arguments
```
git clone https://github.com/jlokimlin/stack_fortran.git

cd stack_fortran; make all
```

-----------------------------------------------------------------------------

## Result

```

	The stack is empty
	size of stack =   2
	The stack is:   4 ->  3 -> null()
	size of stack =   3
	The stack is:   5 ->  4 ->  3 -> null()
	Pop operation..............
	Poped value is   5
	size of stack =   2
	The stack is:   4 ->  3 -> null()
	size of stack =   3
	The stack is:   6 ->  4 ->  3 -> null()
	Pop operation..............
	Poped value is   6
	size of stack =   2
	The stack is:   4 ->  3 -> null()
	Pop operation..............
	Poped value is   4
	size of stack =   1
	The stack is:   3 -> null()
	Pop operation..............
	Poped value is   3
	The stack is empty


```