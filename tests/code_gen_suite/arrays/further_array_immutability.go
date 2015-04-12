package main 

func main () {
	var fa [2]float64;
	var sa [2]string;
	var ra [2]rune;
	var ba [2]bool;
	var aa [2][2]int;
	var sra [2]struct { a float64; }

	var fa2 = fa
	var sa2 = sa
	var aa2 = aa
	var ra2 = ra
	var ba2 = ba
	var sra2 = sra

	fa[0] = 0.5
	sa[0] = "sdfsfd"

	var x [2]int
	aa[0] = x
	ra[1] = 'v'
	ba[1] = true
	x[1] = 5
	aa[1][1] = 7
	var strkt struct { a float64;}
	sra[0] = strkt
	strkt.a = 0.75
	sra[1].a = 0.2

	println(fa[0] == 0.5)
	println(fa2[0] == 0.0)
	println(sa[0])
	println(sa2[0])
	println(aa[0][1])
	println(x[1])
	println(aa2[1][1])
	println(aa[1][1])
	println(ba[1])
	println(ra[1])
	println(ba2[1])
	println(ra2[1])
	println(strkt.a == 0.75)
	println(sra[0].a == 0.0)
	println(sra[1].a == 0.2)
	println(sra2[1].a == 0.0)
}
