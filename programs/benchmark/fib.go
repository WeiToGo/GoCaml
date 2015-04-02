package main

func fib(n int)int{
	if (n < 2){
		return n;
	}else{
		return fib(n-1) + fib(n-2)
	}
}

func main() {
	print(fib(45))
} 
