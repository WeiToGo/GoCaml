package main

func main(){
	var a float64
	var d string
	a = 2.0
	b:=int(a)
	c:=float64(a)
	
	d="aa"
	//d:=bool(a)
	e:=rune(a)
	
	println(a,b,c,d,e)
	
	z:=rune(d)
	print(z)
}
