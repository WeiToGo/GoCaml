package main 

func main () {

// bool 
	println(true || true)  
	println((6<9) || (4>5)) //T F
	println((4!=4) || (4>5)) // F F
	println((10<9)|| (5==5)) // F T
	println((2>4) && (3==4)) // F F
	println((4!=5) && (4<=9)) //T T
	println((4!=5) && (3==4)) // T F
	println((3>=9) && (5!=6)) // F T

}


