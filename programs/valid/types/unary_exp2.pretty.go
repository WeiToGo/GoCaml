package main;

var (
	x  struct {
		a, b int;
	};
);
var (
	c  = ( + x/* struct { a: int, b: int } */ .b/* int */ )/* int */ ;
);
