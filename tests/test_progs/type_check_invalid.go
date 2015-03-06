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
func printing(){
	type t4 struct {
		x float64
		y, z float64
	}
	var arr[4]rune
	arr[0] = 'r1'
	println(t4.x)
	print(arr[0])
}

// for loop

func for_stmts() { 
	for {
		5 + 6.7
		break		
	}

	// "while" loop
	for 5+2 {
		continue	
	}
	for a < b {
		300.5 % 4
		continue	
	}
	for ;; {
		"etvd" + 'rv'
	}

	// three-part loop, init statement only
	for ; ; {
		45.6 * "srr"
	}

	// three-part loop, expression only
	for 34.5 + 'a'; a < 10; {
	}

	// three-part loop, update statement only
	for ; ; 10.4++ {
	}

	// three-part loop, init statement and expression
	for a := 0; a = 10; {
	}

	// three-part loop, init and update statements
	for a := 10; ; 3.4++ {
	}

	// three-part loop, expr and update statements
	for ; a = 10; a++ {
	}

	// three-part loop, all parts
	for a := 0; a < 10; "srt"++ {
	}
}
// if statement

func if_stmts() {
	// if/then
	// if true {
	// 	a
	// }

	// // if/then with empty init statement
	// if ; true {
	// 	a
	// 	b
	// }

	// init has wrong type
	if x < 0; x == 0 {
		return
	}

	// expr not bool
	if  x:= 2 {
		print(x)
	} else {
		print(x+1)
	}

	// if/then/else with empty init statement
	if ; x+1 {
		return
	} else {
		return
	}

	// if/then/else with init statement
	if x; true {
		a
	} else {
		b
		c
	}

	// if/else-if/else
	if false {
		1
	} else if false {
		2
	} else {
		3
	}

	// if/else-if/else with init statement
	if false {
		1
	} else if x; false {
		2
	} else {
		3
	}
}

// switch statement

// EXPRESSIONS

// identifiers

// unary expression
+(5+5.6)
+("terf")
+('rser')
+(5+5.6)
+("terf")
+('rser')
!((34 <= 32.5) && (4 >= 23.4) )
!((34==3) != 5)
!((4<5.5) || (5.7 > 'r'))
^(45.5)
^("ewr")

// binary expression

// function call

// indexing

// field selection

// append

// type cast

