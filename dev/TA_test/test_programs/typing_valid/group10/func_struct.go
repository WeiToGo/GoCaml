package main

func f(a struct{x,y int;}) struct{x,y int;}{
	a.x = 3
	return a
}

type t1 struct{
	t1 struct {x,y int;}
}

func main () {
	var y struct{x,y int;}
	y = f(y)

	var z t1;
	var t2 struct{x,y int;}
	t2 = z.t1
	z.t1 = t2
	t2.x = 3
	z.t1.x = 2


	var t3 [3]struct{x,y int;}
	t3[0].x = 3

	var t4 [2][3]struct{x,y int;}
	
	t4[0] = t3

	type s [3]struct{x,y int;}

	var t5 []s
	var t6 s
	append(t5,t6)

}

