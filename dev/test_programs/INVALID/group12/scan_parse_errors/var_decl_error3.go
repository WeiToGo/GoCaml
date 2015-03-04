// only one type allowed per VarSpec

package main

func main() {
	var x int, y float64 = 0, 0.
	if false {
		println(x, y)
	}
}
