#!/bin/bash
set -o nounset
set -e
UTOP_DEPS="utils.cmo parser.cmo scan.cmo ast.cmo jasminAst.cmo codeGen.cmo codeEmitter.cmo ../dev/printer.cmo ../dev/dev_tools.cmo symtable.cmo typecheck.cmo"
pushd . 

echo "###### Go output: "
go run $1

echo "------------------"

echo "Compiling with gocaml... "
cd "_build/src"
cat "../../$2" | utop $UTOP_DEPS -stdin
echo "###### Gocaml output: "
cd $(dirname ../../$1)
java -jar /Users/droy/code/compilers/jasmin-2.4/jasmin.jar GeneratedBytecode.j
java GeneratedBytecode

popd 