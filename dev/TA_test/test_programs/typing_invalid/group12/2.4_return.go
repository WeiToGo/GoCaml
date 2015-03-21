// f has a return type but return statement has no expression

package main

func f() int {
	return
}

func main() {
	print(f())
}
