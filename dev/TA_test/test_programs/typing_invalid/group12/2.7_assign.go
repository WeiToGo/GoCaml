// cannot assign unnamed type (int) to named type (integer)
// note: this typechecks in Go

package main

func main() {
    type integer int
    var x, y bool
    var z integer
    x, y, z = true, false, 5
    print(x, y, z)
}
