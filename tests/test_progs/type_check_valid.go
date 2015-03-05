/* Type declarations */

type point struct {
	x,y float64
}
type g point 


/* Var declarations */

var a float64 = (3.0*4/5.3)
var b int = 3
var c, d rune = 'str1', 'str2'
var {
	x1, x2 rune = `str`, 'r1'
	y1, y2 string = `raw`, "str" 
	z int = (4+5)/6.0	
	x1 string
}


// type declarations:

type num int
var e num = num(4)

// function decl

func f1(a int, b int) {
}

func f2(a int, f1 int) {	
}

func f3(a int, b int) {
	var f3 string
}

func f4(a, b int, c, d bool) {
}

// STATEMENTS

// return statement

func f() {
	return 
}

func f()string {
	var x string
	return x 
}

func f()[]rune {
	var x []rune 
	return x
}

// short declaration
func short_decl() {
	a := 5
	x := "hello"
	a, b := 0, 1
}

// declaration

// assignment
func assign_stmts() {
	var a,b int;
	var c [2]string
	var point struct { x float64 }
	a, b = 0, 1
	c[0], point.x = "hi", 3.1415; 
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

