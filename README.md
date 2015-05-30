# GoCaml


This compiler compiles Go-Lite (a subset of Go) to Java bytecode. 

The compilation happens in several steps. The main compiler compiles a go file to Jasmin code. The .j files are then compiled to bytecode using the jasmin assembler. There are several pre-compiled jvm classes that provide runtime support to implement go data structures, and in the last step these classes are copied over to accompany the class files produced from go. 

## Dependencies
- OCaml (along with ocamllex and ocamlbuild)
- menhir (For parsing)
- JDK 7 or higher (because we use java reflection)
- Make
- Jasmin (provided in the repo)

## Compiling the compiler

Note that you need JDK 7 or above to successfully use the compiler. You can check your jdk version using `java -version` and `javac -version`.

To compile, simply run:

    make clean
    make

This will produce all the compiled ocaml files in the _build directory, and also compile some supported java classes and put them in `_build/staticlib`directory. There is one handcoded jasmin file (`RuntimeSupport.j`), which will also be copied to `_build/staticlib`. Finally, an executable `main.byte` is produced in the base directory. 

## Running the compiler

`./main.byte` simply produces a set of jasmin files, which cannot be run by themselves. They need to be assembled to bytecode, and then given the company of its supporting classes. The recommended way to run the compiler is through the `golite` script provided in the base directory:

    ./golite <path/to/go/file>

The list of supported flags can be obtained through `./golite -h`

## Note about jasmin 
The version of jasmin assembler we used is provided as a jar file in the repository. The presence of that jar file is important for the assembler phase to work.

## Optional test of codegen 
There is a test script in the `tests/` directory that compiles a suite of test go files (provided in `tests/code_gen_suite/`) first with go, and then with our compilers, and takes the diff to check for identical output. To run these tests:

    # Make sure you compiled the compiler first
    cd tests
    ./run_code_gen_suite.sh "$(dirname $(pwd))/jasmin.jar" -all  # The second argument is absolute path to jasmin jar.

If the gocaml god is feeling generous, all tests should pass.
