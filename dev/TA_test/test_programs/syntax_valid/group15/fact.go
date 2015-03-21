package main

// Assumes nonnegative n
func fact(n int) int {
   if n == 0 {
      return 1
   } else {
      return n * fact(n - 1)
   }
}

func main() {
   
}
