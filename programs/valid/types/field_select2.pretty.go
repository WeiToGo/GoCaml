package main;

func f() { 
	var (
		k  int;
	);
	var (
		a  struct {
			x, y float64;
			z string;
		};
	);
	print (a/* struct { x: float64, y: float64, z: string } */ .x/* float64 */ );
};
