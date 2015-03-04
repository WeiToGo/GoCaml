// Package declaration
package main

// Var declarations

var a1 int
var a2 float64 = a1
var a6 = 12
var a3, a4, a5 string
var a, _ int

var x struct { }
var y [][]int
var z [2]struct{}

// "Distributive" var declaration
var (
	b1 int
	b2 float64 = b1
	b3, b4, b5 string
	k struct{
		a,b float64
		c string
	}
)

// Type alias
type t1 int

// "Distributive" type aliases
type (
	t2 rune
	t3 bool
)

var s1[]t1
var s2[4]t2


// struct type
type t4 struct {
	x float64
	y, z float64
}
//empty struct
type t5 struct {
}

// Slices
type t6 []int			// base type
type t7 []point			// type id
type t8 []struct {		// composite type
	a int
}
type t9 [][]float64		// matrix

// Arrays
type t10 [4]int			// base type
type t11 [4]point		// type id
type t12 [8]struct {		// composite type
	a int
}
type t13 [3][3]float64         	// matrix
// no params, no return value
func f1() {
}

// no params, one return value
func f2() int {
	var x int = 0
	return x
}

// one param, no return value
func f3(a int) {
}

// two params (long form), no return value
func f4(a int, b int) {
}

// two params (short form), no return value
func f5(a, b int) {
}


// two sets of short form params
func f6(a, b int, c, d bool) {
}

func for_stmts() { // ERROR WHEN HAVE FOR  , , 
	// Infinite loop
	for {
		x + y
		break		// break statement
	}

	// "while" loop
	for a < b {
		continue	// continue statement
	}

	// three-part loop, all parts missing
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


func if_stmts() {
	// if/then
	if true {
		a
	}

	// if/then with empty init statement
	if ; true {
		a
		b
	}

	// if/then with init statement
	if x := 0; x == 0 {
		a
		b
		c
	}

	// if/then/else
	if false {
		a
	} else {
		b
	}

	// if/then/else with empty init statement
	if ; true {
		a
	} else {
		b
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


func switch_stmts() {
	// switch, no expression
	switch {
	case x < 0: "negative"
	case x > 0: "positive"
	default: "zero"
	}

	// switch, expression, no default
	switch x {
	case 0: "zero"
	case 1, 3, 5, 7, 9: "odd" // expression list
	}

	// switch, expression, complex expressions
	switch c; {
	case x < 0: "negative"
	case x > 0: "positive"
	default: "zero"
	}

	// switch, default in the middle
	switch c; x {
	case 0: "zero"
	default: "something else"
	case 1, 3, 5, 7, 9: "odd"
	}
}

func return_stmts() {
	// Return statements
	return
	return 17
	return f8()

}

func decl_stmts() {
	// Var declarations
	var a1 int			// type, no expr (initialized to zero)
	var a2 float64 = a1		// type, expr
	var a6 = 12			// no type, expr (type inferred)
	var a3, a4, a5 string		// list of ids

	// Multiple var declarations
	var (
		b1 int
		b2 float64 = b1
		b3, b4, b5 string
	)

	// Type alias
	type t1 int

	// Multiple type aliases
	type (
		t2 rune
		t3 bool
	)

	// struct type
	type t4 struct {
		x float64
		y, z float64
	}

	// empty struct
	type t5 struct {
	}

	// Slices
	type t6 []int			// base type
	type t7 []point			// type alias
	type t8 []struct {		// composite type
		a int
	}
	type t9 [][]float64		// matrix

	// Arrays
	type t10 [4]int			// base type
	type t11 [4]point		// type alias
	type t12 [8]struct {		// composite type
		a int
	}
}


func assign_stmts() {
	// Assignment
	a = 0
	a, b = 0, 1
	a[0], point.x = "hi", 3.1415;  // ##################### NEED SEMCOL

	// Op assign
	a += 1
	a -= 1
	a *= 1
	a /= 1
	a %= 1
	a &= 1
	a |= 1
	a ^= 1
	a &^= 1
	a <<= 1
	a >>= 1
}



func short_decl() {
	x := "hello"
	a, b := 0, 1
}


func block() {
	println(1)
	{
		println(2) 
		println(4)
	}
	println(3)
}


func exprs() {
	// Integers
	n = 255
	n = 0xff
	n = 0377

	// Floats 
	x = 12.0
	x = 12.
	x = .12

	// Rune
	c = 'L'
	c = '\\'
	c = '\''
	c = '\a'
	c = '\b'
	c = '\f'
	c = '\n'
	c = '\r'
	c = '\t'
	c = '\r'

	// Interpreted strings
	s = ""
	s = "compilers!"
	s = "\a \b \f \n \r \t \v \\ \""

	// Raw strings
	r = ``
	r = `compilers!`
	r = `\a \b \f \n \r \t \v \\ \"`

	// ids
	abc = 0
	_var = 0
	Var = 0
	Var1234 = 0

	// blank id
	_ = 0

	// Unop
	-x
	+x
	!x
	^x

	// Binop
	x + 1
	x - 1
	x * 1
	x / 1
	x % 1
	x & 1
	x | 1
	x ^ 1
	x &^ 1
	x << 1
	x >> 1
	x && y
	x || y
	x < y
	x <= y
	x == y
	x != y
	x >= y
	x > y

	// Function calls
	f()
	f(a, b+c, d[0])

	// Indexing slices and arrays
	x[0]
	x[i + 1]
	x[i][j][k]

	// Selecting elements from structs
	x.y
	x[0].y.z
}





