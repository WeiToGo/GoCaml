package main

func plusminus(a,b int, c bool) int {
	var result int
	if c {
	result=a+b
	} else {
	result=a-b
	}

	return result
}

func main(){
	printThis:=plusminus(5,10,true)
	println(printThis)
	printThis=plusminus(5,10,false)
	println(printThis)
}
