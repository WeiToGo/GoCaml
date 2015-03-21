package main

func main() {
	// `var` declares 1 or more variables.
	var a string = "initial"
	println(a)

	// You can declare multiple variables at once.
	var b, c int = 1, 2
	println(b, c)

	// Go will infer the type of initialized variables.
	var d = true
	println(d)

	// Variables declared without a corresponding
	// initialization are _zero-valued_. For example, the
	// zero value for an `int` is `0`.
	var e int
	println(e)

	// The `:=` syntax is shorthand for declaring and
	// initializing a variable, e.g. for
	// `var f string = "short"` in this case.
	f := "short"
	println(f)
}
