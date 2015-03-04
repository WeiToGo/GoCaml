package main;


var (
	square = 1764
)

var y []int = []int{1,2,3};

func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);
	}
	println("The Square root of ", square, " is roughly ", x);

}
