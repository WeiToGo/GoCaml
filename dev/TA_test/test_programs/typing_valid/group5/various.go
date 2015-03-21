/* Package declaration, golite 2.2 */
package main

/*Functions Declarations 1.3, Unary expr 3.3 */
func f(f int) int {
	f *= (-f)
	return f
}

func regularPower(a int, b int) int {
	var res int
	res = 1
	/*For loop 2.11*/
	for b > 0 {
		/*Op-assignment 2.8*/
		res *= a
		b--
	}
	return res
}

func main() {

	/*Variables Declarations 1.1 */
	var num1 int
	num1 = 12
	var num2 int = num1*1 + 2

	/*Block 2.9*/
	{
		var num3 = num1 * num1
		println("In the block. num3 should be 144:", num3)
	}

	var num3 = num1 * 3
	println("Three numbers should be 12 14 36:", num1, num2, num3)

	/*Short Declaration 2.5*/
	/*Assignment 2.7*/
	num4, num5 := num1+1, num2*2
	var num6, num7 int
	num6, num7 = num4-num1, num2/num5
	println("Should be 13 28 1 0:", num4, num5, num6, num7)
	println("13<=28 ", num4 <= num5, " 1<<2", num6<<2)

	var flag1, flag2 bool
	flag1, flag2 = true, true
	/*runeType, Binary expr 3.4*/
	println('a', 'b', flag1 || !flag2, " ", flag1 && !flag2, " ", num4 != num5)
	/*Function Call 3.5*/
	println("3*(-3) is:", f(3))
	/*Types Declarations 1.2 */
	type a int
	var x a = a(2)
	println("x is of type a and it should be 2:", x)

	/*redeclare type in inner scope*/
	{
		type a bool
		var x a = a(false)
		println(x)
	}

	var res int = regularPower(2, 3)
	println("2^3 is:", res)

	/*For Loop 2.11*/
	for y := 2; y > 0; y-- {
		x *= x
	}
	println("should be 16 :", x)

	/*If statement 2.12, Type cast*/
	if z := "error"; x > a(10) {
		println(x, "is bigger then 10")
	} else {
		println(z + "should be bigger than 10")
	}

}
