package main

type digit int;
type num float64;
type Circle struct {
	radius int;
	diameter float64;
}

func main() {
	
	var circle_one Circle;
	circle_one.radius = "Blah blah";
	circle_one.diameter = 10.0;
	print(circle_one.radius);
	
}
