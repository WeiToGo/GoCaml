package main

func main () {
	
	var a int = 5
	
	//didn't include infinite loop
	
	for a>0 {
		println(a)
		a--
	}
	
	//error: a used as value
	for a {
		println(a)
	}
	

}
