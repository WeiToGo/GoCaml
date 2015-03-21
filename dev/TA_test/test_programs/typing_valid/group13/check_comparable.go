package main

/*
 * Checks if comparable is working correctly. Both lhs and rhs must have the same type.
 * Scalars are comparable, strings are comparable, structs are comparable if all fields
 * are comparable and arrays are comparable if their elements are comparable.
 */
func main() {
	type st struct {
		b bool;
		f float64;
		i int;
	}

	type at int;

	var i1, i2 int;
	var r1, r2 rune;
	var b1, b2 bool;
	var f1, f2 float64;
	var s1, s2 string;
	var arr1, arr2 [5]int;
	var st1, st2 st;
	var a1, a2 at;

	print(i1 != i2)
	print(i1 == i2)
	print(r1 == r2)
	print(b1 == b2)
	print(f1 == f2)
	print(s1 == s2)
	print(arr1 == arr2)
	print(st1 == st2)
	print(a1 == a2)
}
