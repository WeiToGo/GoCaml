package main

func main() {
	type dog bool;
	var one dog;
	var two bool;
	
	switch one {
	case two:
		print("works");
	default:
		print("default");
	}
}
