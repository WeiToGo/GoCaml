/*
Symbol table: 

Pre-declared: 

ID			Type
true		bool
false		bool

*/

// Var declarations:

// var a float64 = (3*4/5)
var b int = 3.4
var c, d string = "str1", 6
// var {
// 	x1, x2 rune = `str`, 'r1'
// 	y1, y2 string = `raw`, 'str' 
// 	z int = (4+5)/6.0	
// 	x1 string
// }

// type declarations:

// type num int
// var e num = 4  //should be var e num = num(4)


// function decl

// too many return arguments
// func f0(){
// 	return(3)
// }

// duplicate argument
// func f1(a int, a string) {
// }

// var already declared
// func f2(a int, b int) {
// 	var b string
// }

// no return value
// func f3() int {
// 	return 
// }

// statement ill-typed
// func f4(a int) {
// 	a = (3.4/4)
// }


// STATEMENTS

// func f5(a int, b int) {}
// func f6(a, b int) {}

// // function call

// f5(5.4, 3) //wrong arg type
// f(3, 4, 5) // too many arguments


// return statement

// func f()[4]int {
// 	var x [4]rune 
// 	return x
// }

// ill-typed expression in return
// func foo()rune {
// 	return 'rune'
// }


// short declaration

// func short_decl() {
// 	a ,b := 5, 10
// 	x := "hello"
// 	a, b := 0, 1
// }

// func short_decl() {
// 	a := 5.5 
// 	x := "hello"
// 	a, b := 0, 1 //a was assigned float
// }

// declaration

// assignment

// 1
// func assign_stmts() {
// 	var c [2]int
// 	var point struct { x float64 }
// 	c[0], point.x = "hi", 3.1415; 
// }

//2
// func assign_stmts() {
// 	var a,b int;
// 	a, b = 0, 1.5

// }
// op-assignment

// block

// print/println
// func printing(){
// 	type t4 struct {
// 		x float64
// 		y, z float64
// 	}
// 	var arr[4]rune
// 	arr[0] = 'r1'
// 	println(t4.x)
// 	print(arr[0])
// }

// for loop

func for_stmts() { 
	// for {
	// 	5 + 6.7
	// 	break		
	// }

	// "while" loop
	// for 5+2 {
	// 	continue	
	// }
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
	// for 34.5 + 'a'; a < 10; {
	// }

	// three-part loop, update statement only
	// for ; ; 10.4++ {
	// }

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

	var x string
	// init has wrong type
	// if x = 0; x == 0 {
	// 	return
	// }

	// expr not bool
	// if  x:= 2 {
	// 	print(x)
	// } else {
	// 	print(x+1)
	// }

	// empty init statement, expr not bool
	if ; x+1 {
		return
	} else {
		return
	}

	// statement body wrong type of x
	// if x = x; true {
	// 	print((x+1)/5)
	// } else {
	// 	return
	// }

	// expr in else not bool
	// if false {
	// 	print((4+3)/2)
	// } else if y++ {
	// 	return
	// } else {
	// 	return
	// }

	// statement body in else if has wrong type
	// if false {
	// 	return
	// } else if x = x; false {
	// 	print(x+6.4)
	// } else {
	// 	return
	// }
	// statement body in else doesn't type check
	// if false {
	// 	return
	// } else if x = x; false {
	// 	print(x)
	// } else {
	// 	print(x+'def')
	// }
}

// switch statement
var y int = 1
func switch_stmts() {
	// init does not type check
	// switch (3+0.5); y {
	// case 0: "zero"
	// default: "valid"
	// case 1, 3, 5: "odd"
	// }

	// expr not well typed
	// switch y+"str" {
	// case 0: "zero"
	// case 1, 3: "odd" 
	// }

	// no expr, case e1, e2 not bool
	// switch z:=0; {
	// case 0: "zero"
	// case 1: "odd" 
	// }

	// case e1, e2 not well-typed
	// switch z:=0; {
	// case y < 0.5: "str1"
	// case y > 1.6: "str2"
	// default: "str"
	// }

	// case e1, e2 well-typed but not the same type then expr
	// switch z:=0; (x+z){
	// case y < 0: "str1"
	// case y > 1: "str2"
	// default: "str"
	// }

	// statement not well typed
	// switch {
	// case y < 0: print("string" + 3)
	// case y > 0: print(4+1)
	// default: "zero"
	// }

}
// EXPRESSIONS

// identifiers

// unary expression
+(5+5.6)
+("terf")
// +("string")
+(5+5.6)
+("terf")
+('rser')
!((34 <= 32.5) && (4 >= 23.4) )
!((34==3) != 5)
!((4<5.5) || (5.7 > 'r'))
^(45.5)
^("ewr")


// binary expression
// defining comparable variables
var a, b int 0,1
var c, d float64 4.5, 6.4
var e, f string "s1", 's2'
var g, h [2]int
type st1, st2 struct {
	x,y int
}
(y<4) || (y++)
(y--) && (y==0)


// indexing

// field selection

// append

// type cast

