#!/bin/bash
grep -v ocamlfind $1 | grep -v menhir | grep -v ocamllex.opt | grep -v "Parallel statistics" | grep -v "input sentences" | grep -v "No parallelism done" | grep -v "Command exited with code 2" | awk '!seen[$0]++' || true
grep Warning $1 > .log_warn || true
grep Error $1 > .log_err || true

x=$(wc -l .log_warn | head -n1 | cut  -d " " -f1 )
y=$(wc -l .log_err | head -n1 | cut  -d " " -f1 )
echo "$y Errors $x Warnings"
rm .log_warn .log_err
