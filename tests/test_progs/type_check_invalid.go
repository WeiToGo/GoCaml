/*
Symbol table: 

Pre-declared: 

ID			Type
true		bool
false		bool

*/

// Var declarations:

var a float64 = (3*4/5)
var b int = 3.4
var c, d string = "str1", 6
var {
	x1, x2 rune = `str`, 'r1'
	y1, y2 string = `raw`, 'str' 
	z int = (4+5)/6.0	
	x1 string
}

// type declarations:

type num int
var e num = 4  //should be var e num = num(4)


// function decl



// STATEMENTS

// return statement


func f()int {
	return 
}

func f() {
	var x string
	return x 
}

func f()[4]int {
	var x [4]rune 
	return x
}

// short declaration


// declaration

// assignment

// op-assignment

// block

// print/println

// for loop

// if statement

// switch statement

// EXPRESSIONS

// identifiers

// unary expression

// binary expression

// function call

// indexing

// field selection

// append

// type cast

