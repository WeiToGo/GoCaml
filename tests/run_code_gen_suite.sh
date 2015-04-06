#!/bin/bash
set -o nounset
JASMIN_JAR=$1
a=$(find code_gen_suite -type f)
while read -r line; do
   echo "Running test on $line"
   filename=$(basename $line)
   pushd . &>/dev/null
   gen_dir="_build/gen_$filename" 
   mkdir -p $gen_dir 
   cp $line $gen_dir
   cd $gen_dir
   go run $filename &> gooutput.txt  # Go run prints to stderr on my mac 
   ../../../main.byte f f $filename
   java -jar $JASMIN_JAR GeneratedBytecode.j  >/dev/null
   java GeneratedBytecode > gocaml.txt
   diff gooutput.txt gocaml.txt
   if [[ $? != 0 ]]; then
       echo "Failed at $line"
       echo "Look at $gen_dir"
       popd &>/dev/null
       exit 1
   fi
   popd &>/dev/null
done <<< "$a"

echo "--------------------------------------"
echo "| All tests pass. Life is beautiful! | "
echo "--------------------------------------"