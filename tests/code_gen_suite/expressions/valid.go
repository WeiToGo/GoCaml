package main

type point int

func boo(a point) {
    return
}

func main() {
    var a point;
    a = point(34);
    boo(a)
}
