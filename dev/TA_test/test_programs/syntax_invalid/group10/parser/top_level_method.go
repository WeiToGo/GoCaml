package main;

var (
	square = 1764
)

type Point struct{
	x float64
	y float64
}

func (p *Point) Length() float64 {
	return math.Sqrt(p.x * p.x + p.y * p.y)
}

func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);

	}
	println("The Square root of ", square, " is roughly ", x);

}
