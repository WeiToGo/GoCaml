package main

func main() {
	type st struct {
		b bool;
		f float64;
		i int;
		s []int;
	}

	var st1, st2 st;

	print(st1 == st2)
}
