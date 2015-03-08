/* Package declaration, golite 2.2 */
package main

func main() {

	/* Type casts, golite 2.9.8 */
	a := 7.1415726
	b := int64(a)                               // losing the decimals here
	println("should be 7"+" and we have...", b) //should be 7

	//initialize sum to 0
	var i, j, k, sum1, sum2, sum3 int

	/* For statements, golite 2.8.11 */
	/* Break and continue statements, golite 2.8.12 */
	/* Increment and decrement statements, golite 2.8.6 */

	// Infinite loop
	for {
		sum1 = sum1 + i
		i++
		if i <= 10 {
			continue
		} else {
			break
		}
	}

	// "While" loop
	for j < 11 {
		sum2 += j
		j -= (-1)
	}

	// Three-part loop_c
	for k = 10; k >= 0; k-- {
		sum3 += k
	}

	println("the three results should be equal and we have...", "\n\nloop a has", sum1, "\nloop b has", sum2, "\nloop c jas", sum3)
	result := sum1 == sum2 && sum2 == sum3 && sum3 == 55
	println(result)

}
