// mismatched types int and float64
// Note: this typechecks in Go

package main

func main() {
    println(int(0) + 1.0)
}
