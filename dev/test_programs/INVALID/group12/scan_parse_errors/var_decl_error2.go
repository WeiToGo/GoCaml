// invalid short declaration

package main

func main() {
var (
a, b int
x := 0
)
if false {
println(a, b, x)
}
}