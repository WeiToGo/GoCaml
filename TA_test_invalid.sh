#!/bin/bash
a=$(find dev/TA_test/test_programs/typing_invalid -type f)
while read -r line; do
   echo "Running test on $line "
   ./run.sh $line
   if [[ $? != 0 ]]; then
       echo "Failed at $line"
       echo "$1"
       echo "**************"
   else 
   	echo 'VALID'
   fi
done <<< "$a"
