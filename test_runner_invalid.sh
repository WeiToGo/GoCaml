#!/bin/bash
a=$(find dev/debug_test -type f)
while read -r line; do
   echo "Running test on $line "
   ./run.sh $line
   if [[ $? != 0 ]]; then
       echo "Failed at $line"
       echo "$1"
       echo "**************"
   fi
done <<< "$a"
