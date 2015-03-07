package main;

func f() { 
	var (
		a  struct {
			x, y float64;
		};
	);
	print ( int(a/* struct { x: float64, y: float64 } */ .x/* float64 */ )/* int */ );
};
