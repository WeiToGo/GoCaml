/* Package declaration, golite 2.2 */
package parser_invalid1

func funky2(){

	n := 5
	var s0 []int
	s0 = append(s0, 0)
	s0 = append(s0, 1)
	s0 = append(s0, 3)
	s0 = append(s0, 5)

	s1 := append (s0,7)
	s1 = append(s1, 9)

	s2 := append (s0,11)

	/* Switch, golite 2.8.10 */
	switch i:=n-1;i {
	default: println(s2)
	case 1,2.0: println(s0[1])
	case 3,4: println(s1[1])

	/*error:  multiple defaults in switch */
	/*need a weeding phase to make sure that there is only one default case.*/
	default: println(s0[1])
	}

    /* Return statements, golite 2.8.7 */
	return
}
