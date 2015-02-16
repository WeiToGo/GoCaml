package main

import "fmt"

func main() {
    x := 5;
    y := 2;
    x,y = 6, x;  // y gets the old value of x, not the new value
    fmt.Println(x) // 6
    fmt.Println(y) // 5
}

