package main

func main() {
	var a int = 2
	var b int = 5
	var c bool = false
	var d bool = false

	for a < b {
		if c {
			a += 1
		} else if d {
			print("d was true")
		} else {
			b -= 1
		}
	}
	print(b)
}

