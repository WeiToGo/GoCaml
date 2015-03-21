package main

var hell  = "hello"
var int_array [2]int;

func greetings(wrld string) string { 
    return (hell + " " + wrld)
}

func main() { 
    int_array[0] = 42
    int_array[1] = 10

    for i:= 1; i >=0; i-- {
        println(int_array[i])
    }

    println(1 + 45 * 23)
    print(greetings("world!"))

} 
