package main

/*Declarations 1.2& 1.3*/

func main(){
	
	type num int
	var a num = 2
	print(a)
	//type redeclaration in inner scope is ok
	{
		type num bool
		var b num = true
		println(b)
	}
	
	//error: redeclaration in the same scope
	type num bool
	var b num = true
	print(b)
}
