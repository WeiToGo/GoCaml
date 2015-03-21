package main

/* Valid rune assignment example, testing with all escaped characters */
func main() {
	var r1, r2, r3, r4, r5, r6, r7, r8, r9 rune;
	r1 = '\a'
	r2 = '\b'
	r3 = '\f'
	r4 = '\n'
	r5 = '\r'
	r6 = '\t'
	r7 = '\v'
	r8 = '\\'
	r9 = '\''
	print(r1, r2, r3, r4, r5, r6, r7, r8, r9)
}
