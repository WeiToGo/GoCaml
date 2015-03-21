//ERROR: no new variable on LHS of short declaration

package main;

func main(){
	var a,b,c int;
	a,b,c := 1,2,3,4; //ERROR: fails because there is no new variables on the LHS
}
