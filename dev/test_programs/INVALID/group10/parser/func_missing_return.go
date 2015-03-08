package main;

// '<-' operator and chan are not supported in GoLite
func findMarker(c <-chan int) int {
	for i := range c {
		if x := <-c; isMarker(x) {
			return x
		}
	}
	// invalid: missing return statement.
}


func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);
	}
	println("The Square root of ", square, " is roughly ", x);

}
