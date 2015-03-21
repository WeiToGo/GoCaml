package main

func arith() {
	var (
		i int = 42
		f float64 = 3.141592
		r rune = 'V'
		s string = "golite"
	)

	// Addition
	println(i + i)
	println(f + f)
	println(r + r)
	println(s + s)

	// Subtraction
	println(i - i)
	println(f - f)
	println(r - r)

	// Multiplication
	println(i * i)
	println(f * f)
	println(r * r)

	// Division
	println(i / i)
	println(f / f)
	println(r / r)

	// Modulo
	println(i % i)
	println(r % r)

	// Bit-and
	println(i & i)
	println(r & r)

	// Bit-or
	println(i | i)
	println(r | r)

	// Bit-xor
	println(i ^ i)
	println(r ^ r)

	// Bit-clear
	println(i &^ i)
	println(r &^ r)

	// Left shift
	println(i << i)
	println(r << r)

	// Right shift
	println(i >> i)
	println(r >> r)
}

func logical() {
	var b bool = true

	// Logical and/or
	println(b && b)
	println(b || b)

}


func comparison() {
	var (
		b bool = true
		i int = 42
		f float64 = 3.141592
		r rune = 'V'
		s string = "golite"
	)

	// Equality
	println(b == b)
	println(i == i)
	println(f == f)
	println(r == r)
	println(s == s)

	// Inequality
	println(b != b)
	println(i != i)
	println(f != f)
	println(r != r)
	println(s != s)

	// Less than
	println(i < i)
	println(f < f)
	println(r < r)
	println(s < s)

	// Less/equal than
	println(i <= i)
	println(f <= f)
	println(r <= r)
	println(s <= s)

	// Greater than
	println(i > i)
	println(f > f)
	println(r > r)
	println(s > s)

	// Greater/equal than
	println(i >= i)
	println(f >= f)
	println(r >= r)
	println(s >= s)
}


func type_aliases() {
	type t int
	var x, y t

	// Arithmetic
	println(x + y)
	println(x - y)
	println(x * y)
	println(x / y)
	println(x % y)

	// Bitwise ops
	println(x & y)
	println(x | y)
	println(x ^ y)
	println(x &^ y)
	println(x << y)
	println(x >> y)

	// Comparison ops
	println(x == y)
	println(x != y)
	println(x < y)
	println(x <= y)
	println(x >= y)
	println(x > y)
}
