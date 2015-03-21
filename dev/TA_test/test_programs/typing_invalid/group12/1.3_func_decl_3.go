// f should have a return statement on every execution path
package main

func f (x int) int {
	if x>0 {
		print(x)
	} else {
		return x
	}
}

func main() {

	print(f(2))

}
