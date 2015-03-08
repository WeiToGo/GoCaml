#!/bin/bash

count=0
for file in */*.go; do
	echo "***********************************************"
	echo "test $count:"
	echo "go run $file: " 
    ../../src/golite $file
    count=$((count + 1))
done

exit 0
