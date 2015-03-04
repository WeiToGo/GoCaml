package main;

var (
	A = makeMatrix(2,2);
	B = makeMatrix(1,2);
)

/* makeMAtrix makes a matrix of dimensions x by y
 * uses append, cause that's what you do when you don't have make...
 */
func makeMatrix(x, y int) [][]int{
	var matrix [][]int
	var sub []int
	for i := 0; i < y; i++ {
		sub = append(sub, 0)
	}
	for i := 0; i < x; i++ {
		matrix = append(matrix, sub) 
	}
	return matrix
}

func initA() {
	A[0][0] = 2
	A[0][1] = 4
	A[1][0] = 8
	A[1][1] = 1
}

func initB() {
	B[0][0] = 3
	B[0][1] = 4
}

func MatrixMultiply(M1, M2 [][]int) [][]int {
	var C [][]int
	C = makeMatrix(len(M2), len(M1[0]))
	for i := 0; i < len(C); i++ {
		for j := 0; j < len(C[i]); j++ {
			val := 0
			for k := 0; k < len(M1); k++ {
				val += M1[k][j]*M2[i][k];
			}
			C[i][j]=val
		}
	}
	return C;
}

func printMatrix(matrix [][]int) {
	for i:= 0; i < len(matrix[0]); i++ {
		for j := 0; j < len(matrix); j++ {
			print(matrix[j][i], "\t")
		}
		print("\n")
	}
}

func main() {
	initA();
	initB();
	C := MatrixMultiply(A, B);
	println("A:")
	printMatrix(A)
	println("B:")
	printMatrix(B)
	println("AB=C:")
	printMatrix(C)
}
