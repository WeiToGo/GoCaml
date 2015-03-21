package main

func main() {
	type digit int; 
	{
		var digit_one digit = digit(1);
		{
			type digit float64;
			var digit_two digit = digit(1.0);
			print((digit_one == digit_two))
		}
	}
}
