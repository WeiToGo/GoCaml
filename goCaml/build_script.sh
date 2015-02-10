#!/bin/bash
menhir --only-tokens token.mly && ocamlc token.mli token.ml && ocamllex scan.mll && ocamlc scan.ml
ocamlc scan.cmo main.ml
