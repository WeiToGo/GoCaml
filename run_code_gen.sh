#!/bin/bash


if [[ $# -eq 1 ]]; then
	if [[ "$1" = "-v" ]]; then
		echo "version -- gocaml1.0"
	elif [[ "$1" = "-h" ]]; then
	echo "usage: ./run_code_gen.sh -h for help 
	./run_code_gen.sh -v for version number
	./run_code_gen.sh <PATH_TO_JASMIN_JAR> 
		to generate code for all test files"
	else 
		JASMIN_JAR=$1 
		cd tests/
		./run_code_gen_suite.sh $JASMIN_JAR -all
	fi
else
	echo "usage: ./run_code_gen.sh -h for help 
	./run_code_gen.sh -v for version number
	./run_code_gen.sh <PATH_TO_JASMIN_JAR> 
		to generate code for all test files"
fi

