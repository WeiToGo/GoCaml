package main

var x int;
var y int;
var m int;
var n int;
var k int;
type forselector struct{
	q int
}
func other(a int, b int){

	println ("called function done.");
	return;
}
func main(){
	m = 4;
	x, y = 1, 2;
	n = +(x-y);
	n = -n;
	n = 1 <<2;
	m = 3 << 4;
	var t bool = true;
	println(!t);	
	println(^m);
	if m == 4 && x > y {
		println("and");
	}
	if m != 4 || x < y {
		println("or");
	}
	if m >= n && x <= y {
		x = 10;
	}
	x = k+-2;
	other(x, y);

	//append
	var slice []int;
	slice = append(slice, 4);

	//array call
	var array [2]int;
	array[0] = 11;
	array[1] = 22;
	x = array[1+0];

	var p forselector;
	p.q = 5;
	var floattest float64; 
	floattest = 3.14;
	var runetest rune;
	runetest = 'r';
	var stringtest string = "This is a string.";

	var b int;
	m = y + b;
	m = b - 4;
	m = b | 15;
	m = y ^ 16;
	n = k + b * n;
	y = n / 4;
	m = n % 4;
	b = m & 5;
	b = m &^ 6;
	b = m >> 4;
	b = m << 3;

	println (anInt, floattest, runetest, stringtest);
	return;
}
