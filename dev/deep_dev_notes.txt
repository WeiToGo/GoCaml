Expressions
	- Unary
		+ ((1))
		+ ((0x0))
		+ ((023))
		+ ((-1))
		+ ((+ + -0.0))
		+ ""
		+ "string"
		+ 'rune'
		+ ''
		+ ("string")
		+ x
		+ (+ -x)
		+ 0.
		+ .1
		+ foo()
		+ foo(x, y) 
		+ (- foo(x,y))
		+ foo[(-21)]
		+ foo()[1]
		+ (foo(x[1]))
		+ (foo(-0x0))
		+ append(x, foo())
		+ append(y, (- + append(foo, foo(x, y))))
		+ foo().sdf
		+ a.foo()  [ a is a struct ]
		+ a[0]()   [ a is an array of functions ]
		+ int(foo(34))
		+ +int(34)
		+ append(x, y)[34]
		+ (x)[1]  <------ Probably failing
	- Binary
		+ 1 + 1
		+ -1 + 1
		+ 1 + -1
		+ 1 + -0.0
		+ 1 + 2 * 3
		+ 1 * 2 + 3
		+ (1 + 2) * 3
		+ 1 * 2 + 4 || 5
		+ 1 || 5 && 4
		+ 34 + 34 = 23 + true
		+ a | b || c | d
		+ 34 + foo()
		+ (x + foo)()     [Totally bogus, but should pass parser]
		+ x[34] + x[foo()]

Statements
	- Assignments
		+ a,b,c = 1,2,3
		+ a = 4
		+ _ = foo()    [ This should be blankid ]
		+ __ = foo()   [ This is not blankid ]
		+ a, _ = 34, 35
		+ _, _ = 1, 2
		+ x += 1
		+ x |= 34
		+ x[1] = foo()
		+ x.y = foo(cv)
		+ x.y[1] = 34
		+ (x.y)[1] = 34  <-- failing
		+ (x)[0] = 34

	- Declaration Statements

		+ var x = 1
		+ var x,y = 1, foo()
		+ var x int
		+ var x Point
		+ var x int = 1
		+ var x,y int
		+ var x,y int = 1,2
		+ var x func (int, int) int
		+ var x,y func()
		+ var x func(int)
		+ var x { a int; b string; }
		+ var x { a,b int; }
		+ var x []int
		+ var x [2]int

		+ type Money int
		+ type ComplexPoint {
			x [2]int
			y [2]int
		}
		+ type (x int; point float;)


	- ShortVarDecl
		+ x := 1
		+ x,y := 12, foo()[23]

	- IncDecr
		+ x++
		+ x[23]++
		+ x.y--

	- print/println/continue/break/return
		+ print(foo()[23])
	 	+ println(x,y[10])
	 	+ print()
	 	+ continue
	 	+ break
	 	+ return
	 	+ return 23

	- Block:
		+ { a = 23; 
			var a bool;
		  }
	- If:
		+ if x < 3 { foo (); }
		+ if 3 < 4 == true { foo(); } else { bar(); }
		+ if true { foo(); } else if false { bar(); } else { woof(); }
		+ if ; true { foo(); }
		+ if 2;3 { foo(); }
	  + if x { }
		+ if x:= sdf.fd(); (2 < 3) {
			sdf()
		} else if x {
			lsdkj()
		}	else if x { 
			slkdfj
		}	else {}
		+ if x { s1; s2; } else { t1; t2}  (* Check order in ast *)


	- Switch:
		+ 	switch x:= "test2"; x {
					case "test", "test2": print(1)
					case "test3": print(2)
					default: print(3)
				}
		+ 	switch x:= 2; {
					case x < 3: print(1)
					case  x > 4: print(2)
				}
		+ switch { }
		+ switch 1; { }
		+ switch 1 { }
		+ switch ; { }
		+ switch { case 1: 34; }= 
		+ 	switch false {
					case 1 < 2: print("1 < 2")
					default: print("1 !< 2")
				} 
		+ 	switch  {
						case 1 < 2: println("1 < 2");
							    			print("23");
						default: print("1 !< 2")
				} 
		+ switch true { case true: case false: print(1)} (* This should not print 1 *)


	- For:
		+ for { }
		+ for ;; { }
		+ for x := 34;; { }
		+ for x; a + foo(); z { }
		Check order of s1, s2 for the following
		+ for { s1; s2; }
		+ for x <1 { s1; s2; }
		+ for ;; { s1; s2; } 
		+ for ;x; { s1; s2; }

Packege Decl:
	- package main

	should fail: 
	- package 34

TopDecl:
	- var x int
	- var x,y int
	- var x int = 34
	- var (x1,x2 int)  (* Check order *)
	- var()
	- var (x1 int; x2 int;) (* check order *)

	- type x int
	- type (x1 int; x2 int;)
	- type ()

	should fail:
	- var x
	- type x1 int, x2 int
	= x:= 1



should fail:
 		+ append(34, )
 		+ a.foo()
 		+ var x int, y int = 1,2
 		+ foo()--
 		+ switch { foo(); }



weeder todo:
- Break and continue outside loops are not allowed.  
- Weeding for assignment, variable decl , and inc/decr
We no longer have lvalues since that was a massive parsing PITA
(In particular, I could not get `(x)[0] = 1` to parse).
So weeder needs to take care of lvalue checking.

- Weed out short variable decl in for loop post statement
- Weed out blank statement rule.  For example, these are not allowed:
	x = _
	x := _
	var x int = _
There might be more such rules. 

- Note that in the real go compiler, this passes fine: 
	```
	func foo(_, _ int) { } 
	```
while this has an duplicate argument error:
	```
	func foo(a, a int) { } 
	```
The second one will need to be done by weeding. 



typechecking: 
+ Change ast node for typecasting of custom type


Known issue:
+ There is no semicolon insertion if there is no new line at the end of file. So the following won't parse:
```
package main

var x int
%no newline at of file
```

