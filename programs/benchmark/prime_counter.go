package main

func main() {
	var sum = 1 // including 2 already
	
	// took around 26 seconds to run in java
	for i := 2; i < 400000; i++{
		for j := 2; j < i; j++{ // naive check for primes
			if i % j == 0 {
				break
			}else if j + 1 == i{
				sum++
			}
		}
	}
	print (sum)
}
