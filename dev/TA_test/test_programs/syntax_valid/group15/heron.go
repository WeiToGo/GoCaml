// Calculate (squared) area of a triangle by Heron's formula

package main

// import "fmt";

func areasqrd(x, y, z float64) float64 {
   var semi = (x + y + z) / 2
   var asqrd = semi * (semi - x) * (semi - y) * (semi - z)
   return asqrd
}

func main() {
   var x, y, z float64
   x = 3
   y = 4
   z = 5
   //fmt.Scanf("%f %f %f", x, y, z)
   println("Area squared =", areasqrd(x, y, z))
}
