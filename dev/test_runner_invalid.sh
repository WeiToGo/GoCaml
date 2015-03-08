#!/bin/bash
a=$(find programs/invalid/types -type f)
while read -r line; do
   echo "Running test on $line "
   ./main.byte $line
   if [[ $? != 0 ]]; then
       echo "Failed at $line"
       echo "$1"
       echo "**************"
   fi
done <<< "$a"
