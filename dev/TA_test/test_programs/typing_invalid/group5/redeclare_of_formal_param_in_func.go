
package main

func main(){

	f(2)
}

func f(f int){
	// error: f redeclared in this block
	var f bool
	f = true;
}
