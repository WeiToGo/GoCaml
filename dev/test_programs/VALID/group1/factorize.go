package main

func factorial(x int) int {
	var total int = 1;
	var iter int;
	for iter = 2; iter <= x; iter++ {
		total *= iter;
	} 
	return total;
}
func fac(x int) bool {
	var result bool = true;
	var iter = x;
	for iter=2; iter < x; iter++ {
		if x % iter == 0 {
			result = false;
			println (iter, "is the smallest factor of ", x);
			break;
		}else{
			//print(iter, "-");
		}
	}
	return result;
}

func main (){
	var largenumber int;
	largenumber = factorial(7);/*want large number greater than 100,
	for purporses of being interesting.
	*/
	var prime bool;

	prime = fac(largenumber);
	if prime {
		println(largenumber, "is prime.")
	}else{
		println(largenumber, "is not prime.")
	}
	prime = fac(largenumber-1);
	switch prime {
		case true:
			println(largenumber-1, "is prime.");
		case false:
			println(largenumber-1, "is not prime.")
		default:
			println(largenumber, "ERROR: shouldn't reach this case.");
	}
	prime = fac(largenumber+89);
	if prime {
		println(largenumber+89, "is prime.")
	}
	if !prime {
		println(largenumber+89, "is not prime.");
	}
	return;
}
