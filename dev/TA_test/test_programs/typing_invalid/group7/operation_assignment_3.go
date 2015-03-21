//ERROR: operand are not of type
package main;

func main () {
	var int_var int = 1;
	var rune_var rune = 'a';

	//ERROR: operands are of different types
	int_var -=  rune_var;

}
