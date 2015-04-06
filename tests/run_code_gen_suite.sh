#!/bin/bash
set -o nounset
JASMIN_JAR=$1

if [[ "$2" == "-f" ]]; then
  keep_going="true"
else
  keep_going="false"
fi

a=$(find code_gen_suite -type f)

let total_ran=0
let passed=0
let failed=0

while read -r line; do
  echo "Running test on $line"
  let total_ran=total_ran+1
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
  diff gooutput.txt gocaml.txt >/dev/null
  if [[ $? != 0 ]]; then
    echo -ne "\033[1;1m"  # bold
    echo "Failed at $line"
    echo "Look at $gen_dir"
    echo -ne "\033[1;0m"  # reset
    popd &>/dev/null
    if [[ "$keep_going" == "false" ]]; then 
      exit 1
    else 
      let failed=failed+1
    fi
  fi
  popd &>/dev/null
  let passed=passed+1
done <<< "$a"

function print_report { 
echo "| Total tests executed : $total_ran"
echo "| Total passed         : $passed"
echo "| Total failed         : $failed"
}

if [[ "$failed" == "0" ]]; then 
echo "-----------------------------------------"
echo "| All tests pass. Life is beautiful! "
print_report
echo "-----------------------------------------"
else
echo "-----------------------------------------"
echo "| Some test cases failed :( "
print_report
echo "-----------------------------------------"
fi
