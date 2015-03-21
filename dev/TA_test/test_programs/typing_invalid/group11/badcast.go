package main

func main(){
var x bool=true
var y string
y=string(x)
}

//This program will fail the type check because you can't cast from a bool to a string. We check for this in castK under typeFactor in GoType.c
