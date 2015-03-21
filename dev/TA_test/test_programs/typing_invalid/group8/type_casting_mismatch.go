package main

type digit int;
type num float64;
type Circle struct {
	radius float64;
	area float64;
}

func main() {
	var int_one = 5;
	var digit_one = digit(int_one);
	print(digit_one)
	var num_one = num(int_one);
	print(num_one)
	var circle_one = Circle(int_one);
	print(circle_one);
	
}
