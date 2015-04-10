
package main 


type letter bool

func main () {
	var y2 , y3 = true, false
	var z letter = letter(y2)
	var a letter = letter(y3)
	println(z || a)
}
