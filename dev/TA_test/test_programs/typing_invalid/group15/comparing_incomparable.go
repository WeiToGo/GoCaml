/* x and y are slices, which are incomparable. Thus, we cannot do x==y */

package main

func main() {
     var x []int 
     x = append(x,1)
     var y []int
     y = append(y,1)
     x==y
     return   
}
