/* Package declaration, golite 2.2 */
package parser_invalid2


func main(){

/*Type casts, golite 2.9.8*/
/* example from the given GoLite Syntax Specification*/

type num int
var x int = 3
var y num = num(x)
var z float64 = float64(x)

/*error: cannot convert x (type int) to type bool */
var know bool = bool(x)

/*error: cannot convert x (type int) to type string */
/*go compiler accepts this tho.*/
var know2 string = string(x)


}
