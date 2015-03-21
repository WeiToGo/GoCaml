//ERROR: LHS(Primitive type) = RHS(Custom type) fails

package main

func main() {
	type dog int;
	var chihuahua dog;
	
	var cat int;
	cat = chihuahua; //ERROR
}
