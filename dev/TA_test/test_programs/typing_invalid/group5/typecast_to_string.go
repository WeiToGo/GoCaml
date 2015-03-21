package main

func main(){
	var a float64
	a = 2.0
	b:=int(a)
	c:=float64(a)
	
	//d:=bool(a)
	e:=rune(a)
	
	println(a,b,c,e)
	
	z:=string(a)
	print(z)
}
