//ERROR: operand should be of type in/rune/float64
package main;

func main () {
	var string_var string = "string";

	//ERROR: will fail for every one of these
	//for the following operators, only the operands should only be int, rune & float64
	string_var -=  "string";
	string_var *=  "string";
	string_var /=  "string";
	string_var %=  "string";
}
