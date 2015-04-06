# Running code gen tests

To run the tests, execute 
```
./run_code_gen_suite.sh path/to/your/jasmin-jar
```

You can add a go files in the `code_gen_suite` directory. The test script will copy the go file to the `_build` directory, and then run the file using both the go compiler and our compiler. If there is a difference in output, the test fails, and an error message will be printed indicating the location of the output logs. 

**NOTE:** Even though you can put files in subdirectories in `code_gen_suite`, please do not have the same filename for two tests even though they are in different directories. This will mess up error reporting, although it won't give false positives / false negatives. You can fix this issue if you want, but I don't think it's quite worth the effort. 
