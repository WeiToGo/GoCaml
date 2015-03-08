package main

var a int;
var b int;
var c int;
var d int;


func main() {
	b = 1;
	c = 1;

	println(b);
	println(c);

	a = 100;

	for a != 0 {
		println(b + c);
		d = b;
		b = c;
		c = c + d;
		a = a - 1;
	}
}
