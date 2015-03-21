/* This program is tries to select an undeclared field from a struct */

package main

func main() {
	var x struct
	{
		y int
	}
	x.z = 4
}
