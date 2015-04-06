#!/bin/sh

echo "Go output: "
/usr/local/go/bin/go run /Users/droy/code/compilers/cs520_group9/dev/scratch/partial_dev_program.go
echo "\n==============================\n"

java -jar /Users/droy/code/compilers/jasmin-2.4/jasmin.jar goprog.j
java GoProg
