// int and rune are not comparable

package main

func main() {
	x, y := 1, '1'
	if x == y {
		print(y)
	}
}

