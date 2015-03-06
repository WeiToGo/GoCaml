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
	a, b, c := "str1", 'rune1', 14
	print(4+(5*6)/2)
	println(a,b,c)

}

// aliases of basic types
func printing2(){
	type num int
	type name string
	var a num = 5
	var b name = "asiefa"
	print(a,b)
	println(b,a)

}
// for loop
func for_stmts() { 
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
+(5)
+(46.5/8.0)
+('r')
-(5)
-(46.5/8.0)
-('r')
!((34<=32) && (4.6>=23.4) )
!(34 != 5)
!((4<5) || (5.7 > 4.6))
^(45)
^('r')

// binary expression

// function call

// indexing

// field selection

// append

// type cast

