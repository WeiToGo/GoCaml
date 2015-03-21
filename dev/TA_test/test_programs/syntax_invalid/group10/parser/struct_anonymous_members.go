package main;


var (
	square = 1764
)

type point struct{
    x,y int
}

type p struct {
    int
    *point
}


func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);
	}
	println("The Square root of ", square, " is roughly ", x);

}
