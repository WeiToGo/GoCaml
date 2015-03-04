package main;


var (
	square = 1764
)

type s struct {
	microsec  uint64 "field 1"
	serverIP6 uint64 "field 2"
	process   string "field 3"
}

func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);
	}
	println("The Square root of ", square, " is roughly ", x);

}
