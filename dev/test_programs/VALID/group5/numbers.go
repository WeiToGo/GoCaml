/* Package declaration,


 golite 2.2 */
package main

func main() {
	n := 5
	funky(n)

	number(35)

	/* Type declaration, golite 2.5 */
	/*2 forms of type declaration*/

	type (
		a     int
		point struct {
			x, y string
		}
	)

	var z a
	z = 2
	//wanted to println point{"what","what"} also, but fmt.Pringln is not supported and println doesn't support struct.
	println(z)

}

/* Append, golite 2.9.7 */
/* Switch statements, golite 2.8.10 */
func funky(n int) {

	var s0 []int
	s0 = append(s0, 0)
	s0 = append(s0, 1%5)

	s1 := append(s0, 2)
	s2 := append(s1, 3+5)

	switch n {
	default:
		println(s2[1])
	case 1, 2, 3, 4:
		println(s0[1])
	case 5, 6:
		println(s2[3])
	}

	return
}

func number(a int) int {

	/* If statements, golite 2.8.9  */
	/* Print and println statements, golite 2.8.7 */

	if a < 0 {
		println(a, "is negative.")
	}

	if a%2 == 0 {
		println(a, "is even.")
	} else {
		println(a, "is odd.")
	}

	/*Short declaration statements, golite 2.8.5 */
	b := 50
	c := b - a

	if c > 0 {
		println(a, "<50.")
	} else /*commentttttttt*/ if c < 0 {
		println(a, ">50.")
	} else /*blockkkkk commentttt
	tttt*/{
		println(a, "=50.")
	}

	return a
}
