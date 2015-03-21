package main

func test() bool {
	x := 3;
	y := 3.0;
	if x < 3 {
		if y == 2.2 {
			return true;
		}
	} else {
		return false;
	}
}

func main() {
	test();
}
