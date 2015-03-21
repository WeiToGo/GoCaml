// cannot append string to int slice

package main

func main() {
	var a []int
	a = append(a, "s")
}
