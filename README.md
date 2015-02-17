cs520: Group 9
==============

Group Members
Wei Gao (#260356731)
Omar Gonzalez (#260427991)
Deepanjan Roy (#260469677)


## Dependencies
- menhir
- Make
- oUnit (for running tests) 


## How to run code 
    make clean
    make
    ./main.native <your golite file path> 

## Running tests
You can run the unittests using `make test` command.



###Milestone 1:

Scanner and Parser implemented, although lots of further testing is required. Quite a few bugs that have not been ironed out. The ast is constructed, but pretty printing code is not functional yet. 