#!/bin/bash

# (Re-)Generates the parsing test-suite derived from test262. The
# parse trees are generated using SpiderMonkey JS Parser API (assumes
# spidermonkey is 'js').

for tc in $(find ../test-data/test262-es5/test/suite/ -name *.js) 
do
   fn=${tc#*suite/}
   mkdir -p ../test-data/t262/${fn%/*}
   cp $tc ../test-data/t262/$fn
   cat ../test-data/t262/$fn | ./testgen > ../test-data/t262/${fn%.js}.json
done
