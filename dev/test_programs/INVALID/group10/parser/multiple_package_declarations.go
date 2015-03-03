package main;
package main2;

var (
	square = 1764
)


func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);

	}
	println("The Square root of ", square, " is roughly ", x);

}
