package main;

func main() {
	var list [5]int
	list[0] = 1
	list[1] = 2
	list[2] = 3
	list[3] = 4
	list[4] = 5
	
	var result = calculate(list)
	
	var sum = result.a
	var diff = result.b;
	
	println(sum);
	println(diff);
}

func calculate(list [5]int) struct{
	a, b int
} {
	
	var (	
		res struct {
			a, b int;
		}	
		i int = 0		
	)
	
	for i < 5 {
		res.a += list[i]
		res.b -= list[i]
		i++;
	}
	
	return res
}
