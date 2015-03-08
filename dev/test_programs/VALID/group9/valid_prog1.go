package main

var (
	rollingAvr float64 = 0
	windowSize = 10;
	n int = 0
	maxN int = 100
	ratio float64 = 0.9
	z string = "here"
)

func main() {
	for n = 1 ; n < windowSize ;  n++{ // initialize rollingAvr
		rollingAvr += float64(n);
	}
	rollingAvr /= float64(windowSize);
	
	print("Rolling Average initialized to: ")
	println(rollingAvr)
	
	for ; n < maxN; n++ {
		rollingAvr *= float64(windowSize) * ratio;
		rollingAvr += float64(n);
		rollingAvr /= float64(windowSize)
	}
	
	print("Final rolling average is ")
	println(rollingAvr)
}
