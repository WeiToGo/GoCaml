package main;

func f() { 
	type 	s  struct {
		x int;
	};
	var (
		a s;
	);
	print (a/* s */ .x/* int */ );
};
