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


func f1(a int, a string) {
}


func f1(a int, b int) {
	var b string
}
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

func short_decl() {
	a ,b := 5, 10
	x := "hello"
	a, b := 0, 1
}

func short_decl() {
	a := 5.5 
	x := "hello"
	a, b := 0, 1 //a was assigned float
}

// declaration

// assignment

func assign_stmts() {
	var c [2]int
	var point struct { x float64 }
	c[0], point.x = "hi", 3.1415; 
}


func assign_stmts() {
	var a,b int;
	a, b = 0, 1.5

}
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

