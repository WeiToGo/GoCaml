// mismatched types int and float64

package main

func main() {
	a, b := 0, 0.0
	if a>=b {
		print(a, b)
	}
}

