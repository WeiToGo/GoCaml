// Go's _select_ lets you wait on multiple channel
// operations. Combining goroutines and channels with
// select is a powerful feature of Go.

package main

//import is not supported in GoLite

import "time"
import "fmt"

func main() {

    // For our example we'll select across two channels.
    c1 := make(chan string)
    c2 := make(chan string)

   
}
