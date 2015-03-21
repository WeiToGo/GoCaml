package main

func main () {
	type t1 int
	type t2 int
	type t3 bool
	var a,b t1
	var c t3 = t3(true)
	c = c && t3(true)
	c = c || t3(true)
	c = !c

	var d bool = true;
	d = d && false;
	a = a + t1(1)
	a += t1(1)
	a ^= t1(1)
	a -= t1(1)
	a /= t1(1)
	a *= t1(1)
	a %= t1(1)
	a <<= t1(1)

	a = a & t1(3)
	a = a ^ t1(3)
	a = a | t1(3)
	a = ^a
	a ^= a
	
	a &= t1(3)
	a &^= t1(3)
	a &= t1(3)
	a |= t1(3)
	a ^= t1(3)

	var x int = 3
	x &= 3
	a = a + t1(1)


	var f struct{x,y int;}
	f.x = 3
}

