
package main;


var (
    square = 1764
)

func main() {
    var y bool
    switch i := y.(type) {
    case nil:
        println("x is nil")                // type of i is type of x (interface{})
    case int:
        println(i)                            // type of i is int
    case float64:
        println(i)                        // type of i is float64
    case func(int) float64:
        println(i)                       // type of i is func(int) float64
    case bool, string:
        println("type is bool or string")  // type of i is type of x (interface{})
    default:
        println("don't know the type")     // type of i is type of x (interface{})
    }


    x := float64(square/2)
    for i:= 0; i < 20; i++ {
        x = x - (x*x - float64(square))/(2*x);
    }
    println("The Square root of ", square, " is roughly ", x);
