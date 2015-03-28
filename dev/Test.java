public class Test{

	public static String hello = "hello";
	public static int[] int_array = new int[2];


public static String greetings(String wrld) {
	return (hello + " " + wrld);
}

public static void main (String[] args) {
	int_array[0] = 42;
	int_array[1] = 10;

	for (int i = 1; i >= 0; i--){
		System.out.println(int_array[i]);
	}

	System.out.println(1 + 45 * 23);
	System.out.println(greetings("world!"));	
}

}
