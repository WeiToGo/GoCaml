package parser_invalid4

//func goto package if else return
//case defer go map struct

func main() {
  x:=28

//error:even(x) used as value
//y:=even(x) 

  even(x)
  print

//error:too many arguments to return
  return x
}





func even(x int) {
if x%2==0 {goto skip}
else {goto msg}

msg:
  print("odd number")
skip:
  print("even number")
}
