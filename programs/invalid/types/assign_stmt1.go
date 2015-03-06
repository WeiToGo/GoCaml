package main 

func assign_stmts() {
	var c [2]int
	var point struct { x float64 }
	c[0], point.x = "hi", 3.1415; 
}
