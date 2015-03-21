/* Trying to print num, but num is a type, not a term */

package main

func main() {
     type num int
     println("The number is ", num)
}
