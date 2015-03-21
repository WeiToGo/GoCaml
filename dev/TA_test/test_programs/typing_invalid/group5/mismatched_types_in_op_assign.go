package main

/*2. Statements */
func main(){

	/*2.8 Op-assignmet*/
	var a float64
	var b int
	a = 1.0
	b = 2
	//error: invalid operation: a *= b (mismatched types float64 and int)
	a*=b
	print(a,b)
	
}
