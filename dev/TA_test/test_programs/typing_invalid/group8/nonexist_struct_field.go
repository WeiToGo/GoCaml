package main

type digit int;
type num float64;
type Circle struct {
	radius float64;
	diameter float64;
}

func main() {
	
	var circle_one Circle;
	circle_one.radius = 5.0;
	circle_one.diameter = 10.0;
	circle_one.area = 2*3.14*25.0;
	print(circle_one.area);
	
}
