package main

func main() {
	var a [7]int;
	
	a[1.0] = 8; //ERROR: Should take an int as index
}
