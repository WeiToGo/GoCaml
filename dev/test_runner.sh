#!/bin/bash
a=$(find dev/test_programs/VALID -type f)
while read -r line; do
   echo "Running test on $line"
   ./main.byte $line
   if [[ $? != 0 ]]; then
       echo "Failed at $line"
       exit 1
   fi
done <<< "$a"
