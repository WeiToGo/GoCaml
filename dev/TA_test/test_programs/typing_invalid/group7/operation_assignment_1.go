//ERROR: operand should be of type in/rune
package main;

func main () {
	var float_var float64 = 1.0;

	//ERROR: will fail for every one of these
	//for the following operators, only the operands should only be int & rune
	float_var |= 1.0;
	float_var &= 1.0;
	float_var <<= 1.0;
	float_var >>= 1.0;
	float_var &^= 1.0;
	float_var ^= 1.0;

}
