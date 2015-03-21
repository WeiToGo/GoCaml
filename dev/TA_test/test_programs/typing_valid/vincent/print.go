package main

type salary float64

type employee struct {
	id int
	name string
	sex rune
	wage salary
}

func main() {
	println(3, 3.141592, "hello", 'c', true)

	var e employee
	println(e.id, e.name, e.sex, e.wage)
}
