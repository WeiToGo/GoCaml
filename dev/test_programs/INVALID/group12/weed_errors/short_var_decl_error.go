package main

type point struct {
    x, y, z int
}

func main() {
    var y point
    x, y.x := 0, 2
    println(x,y.x)
}