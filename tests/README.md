# Running code gen tests

To run the tests, execute 
    ./run_code_gen_suite.sh path/to/your/jasmin-jar

You can add a go files in the `code_gen_suite` directory. The test script will copy the go file to the `_build` directory, and then run the file using both the go compiler and our compiler. If there is a difference in output, the test fails, and an error message will be printed indicating the location of the output logs. 
