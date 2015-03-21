//ERROR: LHS type does NOT match RHS type

package main;

func main(){
	var a,b,c int;
	a,b,c,d := 1,2,3,4;

	d,e := 5.0 ,6; //ERROR:fails because expresion for varibale "d" is type float46 instead of int
}
