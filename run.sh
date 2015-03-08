#!/bin/bash

# check the arguments to call main with proper arguments.
if [ $# -lt 1 ]; then
	echo "Not enough arguments. 
	USAGE: ./run.sh <FLAG1> <FLAG2> <FILENAME>,
	where FLAG1, FLAG2 are optional"
fi

arg1=$1
if [ "$arg1" = "-pptype" ]; then
	if [ $# -lt 2 ]; then
		echo "Not enough arguments. 
		USAGE: ./run.sh <FLAG1> <FLAG2> <FILENAME>,
		where FLAG1, FLAG2 are optional"
	else
		arg2=$2
	fi
	if [ "$arg2" = "-dumpsymtab" ]; then
		if [ $# -lt 3 ]; then
		echo "Not enough arguments. 
		USAGE: ./run.sh <FLAG1> <FLAG2> <FILENAME>, 
		where FLAG1, FLAG2 are optional"
		else 
			file=$3
			./main.byte t t $file
		fi
	else 
		file=$arg2
		./main.byte t f $file		
	fi
elif [ "$arg1" = "-dumpsymtab" ]; then
	if [ $# -lt 2 ]; then
		echo "Not enough arguments. 
		USAGE: ./run.sh <FLAG1> <FLAG2> <FILENAME>,
		where FLAG1, FLAG2 are optional"
	else
		arg2=$2
	fi
	if [ "$arg2" = "-pptype" ]; then
		if [ $# -lt 3 ]; then
		echo "Not enough arguments. 
		USAGE: ./run.sh <FLAG1> <FLAG2> <FILENAME>,
		where FLAG1, FLAG2 are optional"
		fi
		file=$3
		./main.byte t t $file
	else
		file=$arg2
		./main.byte f t $file
	fi
else 
	file=$arg1
	./main.byte f f $file 

fi
