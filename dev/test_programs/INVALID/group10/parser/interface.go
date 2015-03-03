package main;


var (
	square = 1764
)

var z [3]int

type Locker interface {
    Lock()
    Unlock()
}
func Lock(){
    return
}
func Unlock(){
    return
}

func main() {
	x := float64(square/2)
	for i:= 0; i < 20; i++ {
		x = x - (x*x - float64(square))/(2*x);
	}
	println("The Square root of ", square, " is roughly ", x);

}
