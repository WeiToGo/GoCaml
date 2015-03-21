package main

func main() {
	var int_one int;
	var int_two int;
	var int_three int;
	var int_four int;
	var float_one float64;
	var float_two float64;
	
	//These assignments should all work.
	int_one = 5;
	int_two = 10;
	int_three = 15;
	int_four = 20;
	
	float_one = 25.0;
	float_two = 30.0;
	
	//This should work.
	int_one = int_two;
	
	//Type mismatches in a list of assignments.
	float_two, int_two, int_four = float_one, int_three, float_one;	
}
