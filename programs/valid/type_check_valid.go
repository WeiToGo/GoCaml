/* Type declarations */
package main 

type point struct {
	x,y float64
}
type g point 


/* Var declarations */

var a float64 = (3.0*4.0/5.3)
var b int = 3
var c, d rune = 't', 's'

var (
	x1, x2 rune = 'u', 'r'
	y1, y2 string = `raw`, "str" 
	z int = (4+5)/6	
)


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

func foo() {
	return 
}

func foo1()string {
	var x string
	return x 
}

func foo2()[]rune {
	var x []rune 
	return x
}

// short declaration
func short_decl() {
	a := 5
	x := "hello"
	a, b := 0, 1
}


// assignment
func assign_stmts() {
	var a,b int;
	var c [2]string
	var point struct { 
		x float64 
	}
	a, b = 0, 1
	c[0], point.x = "hi", 3.1415; 
}



// block

func block() {
	x := 5
	println(x)
	{
		x := 10
		println(x) 
	}
	x++
	println(x)
}

// print/println

func printing(){
	a, b, c := "str1", 'r', 14
	print(4+(5*6)/2)
	println(a,b,c)

}

// aliases of basic types
func printing2(){
	type num int
	type name rune
	var a num = num(5)
	var b name = name('w')
	print(a,b)
	println(b,a)

}
// for loop
func for_stmts() { 
	var a int = 0
	for {
		(420/45+3)/3
		break		// break statement
	}

	// "while" loop
	for 4 < 5 {
		continue	// continue statement
	}
	for ;; {

	}

	// three-part loop, init statement only
	for a := 0; ; {
	}

	// three-part loop, expression only
	for ; a < 10; {
	}

	// three-part loop, update statement only
	for ; ; a++ {
	}

	// three-part loop, init statement and expression
	for a := 0; a < 10; {
	}

	// three-part loop, init and update statements
	for a := 10; ; a++ {
	}

	// three-part loop, expr and update statements
	for ; a < 10; a++ {
	}

	// three-part loop, all parts
	for a := 0; a < 10; a++ {
	}
}
// if statement

// switch statement

// EXPRESSIONS

// identifiers

// unary expression
func unary(){
	var a = +(5)
	a1 := +(46.5/8.0)
	a2 := +('r')
	a3  := -(5)
	a4 := -(46.5/8.0)
	a5 := -('r')
	a6 := !((34<=32) && (4.6>=23.4) )
	a7 := !(34 != 5)
	a8 := !((4<5) || (5.7 > 4.6))
	a9 := ^(45)
	a10  := ^('r')
}

// binary expression

// function call

// indexing

// field selection

// append

// type cast
