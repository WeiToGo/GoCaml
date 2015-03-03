package main

type pair struct {
	a, b int
}

// computes sqrt(n)
func sqrt(x int) int {
	r := x
	for i := 0; i < 10; i++ {
		r = r - (r*r-x)/(2*r)
	}
	return r
}

// determines if n is prime
func isPrime(n int) bool {
	if n <= 1 || n%2 == 0 || n%3 == 0 {
		return false
	}
	i := 5
	for i <= sqrt(n) {
		if n%i == 0 || n%(i+2) == 0 {
			return false
		}
		i += 6
	}
	return true
}

// prints the number of pairs of twin primes between lo and hi
// prints the twin primes if printPrimes is true
func twinPrimes(lo, hi int, printPrimes bool) {
	var count, k int
	if hi <= 1 || hi <= lo {
		return
	}
	var twins []pair
	if lo <= 3 && hi >= 5 {
		var p pair
		p.a, p.b = 3, 5
		twins = append(twins, p)
		count = 1
	}
	if lo%6 == 0 {
		k = lo / 6
	} else {
		k = (lo + (6 - lo%6)) / 6
	}
	for (6*k + 1) <= hi {
		if isPrime(6*k-1) && isPrime(6*k+1) {
			//println(6*k-1, 6*k+1)
			var p pair
			p.a, p.b = 6*k-1, 6*k+1
			twins = append(twins, p)
			count += 1
		}
		k++
	}
	println("There are", count, "pairs of twin primes between", lo, "and", hi)
	if printPrimes {
		for i := 0; i < count; i++ {
			println(twins[i].a, twins[i].b)
		}
	}
}

func main() {
	twinPrimes(1, 100, true)
}

