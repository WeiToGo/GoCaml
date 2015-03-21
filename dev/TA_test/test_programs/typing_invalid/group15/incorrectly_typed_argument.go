/* ret_int expects an integer, but receives a boolean */

package main

func ret_int(n int) int {
   return n
}

func main() {
     ret_int(true)
     return   
}
