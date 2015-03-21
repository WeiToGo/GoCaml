package main

//error: adder shouldnt have return argument
func adder (a int, b int) {
    var c = a+b
    print(c)
    return c
}

func main(){
	adder(2,3)
}
