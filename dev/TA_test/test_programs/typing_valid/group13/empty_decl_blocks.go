package main

var ()
type ()
/* Type checking empty blocks shouldn't result in an error */
func main() {
	var ()
	type ()
}
