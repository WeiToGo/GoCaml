#!/bin/bash
set -o nounset
JASMIN_JAR=$1
PASS_REGISTRY="_build/previous_passes"

if [[ $# > 1 && "$2" == "-all" ]]; then
  run_all_tests="true"
  rm -f $PASS_REGISTRY
else
  run_all_tests="false"
fi

a=$(find code_gen_suite -type f -regex ".*\.go")

let total_ran=0
let passed=0
let failed=0
touch $PASS_REGISTRY

while read -r line; do
  if grep --quiet "^$line\$" $PASS_REGISTRY; then
    continue
  fi
  echo -n "Running test on $line"
  let total_ran=total_ran+1
  filename=$(basename $line)
  pushd . &>/dev/null
  gen_dir="_build/gen_$filename" 
  mkdir -p $gen_dir 
  cp $line $gen_dir
  expected_output_file="${line}camlout"
  if [[ -e "$expected_output_file" ]]; then
    echo " (expected output provided in $expected_output_file)"
    cp $expected_output_file ${gen_dir}/gooutput.txt
    cd $gen_dir
  else 
    echo ""
    cd $gen_dir
    go run $filename &> gooutput.txt  # Go run prints to stderr on my mac 
  fi
  ../../../main.byte f f $filename
  rm *.j
  java -jar $JASMIN_JAR *.j  >/dev/null
  java GeneratedBytecode > gocaml.txt
  diff gooutput.txt gocaml.txt >/dev/null
  if [[ $? != 0 ]]; then
    echo -ne "\033[1;1m"  # bold
    echo "Failed at $line"
    echo "Look at $gen_dir"
    echo -ne "\033[1;0m"  # reset
    popd &>/dev/null
    if [[ "$run_all_tests" == "false" ]]; then 
      exit 1
    else 
      let failed=failed+1
      popd &>/dev/null
    fi
  else
    let passed=passed+1
    popd &>/dev/null
    echo "$line" >> $PASS_REGISTRY
  fi
done <<< "$a"

function print_report { 
echo "| Total tests executed : $total_ran"
echo "| Total passed         : $passed"
echo "| Total failed         : $failed"
if [[ $run_all_tests == "false" ]]; then
  echo "| Note: Only new tests executed. "
  echo "| Use the \"./run_code_gen_suite.sh \$JASMIN_PATH -all\" to run all test cases"
fi
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
