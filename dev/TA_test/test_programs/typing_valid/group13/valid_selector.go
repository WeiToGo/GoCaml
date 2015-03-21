package main

/*
 * Valid example of field selection
 */
func main() {
	type test2 struct {
		x string
	}

	type test struct {
		a int
		b int
		c float64
		d [5]int
		e test2
	}

	var t test;

	print(t.a, t.b, t.c, t.d[1], t.e.x)
}
