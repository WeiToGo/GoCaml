package main

func main() {	
	var str1, str2 string = "hello", "world"
	// Invalid since assignment operator accepts numeric types only
	str1 -= str2
	println(str1)
}
