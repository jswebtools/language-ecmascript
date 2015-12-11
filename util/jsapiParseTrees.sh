#!/bin/bash

# (Re-)Generates the parsing test-suite derived from test262. The
# parse trees are generated using SpiderMonkey JS Parser API. Assumes
# spidermonkey is 'js24' and language-ecmascript-testgen is built and
# available in the current dir as "testgen".
mkdir -p ../test-data/t262/

for tcn in $(find ../test-data/test262-es5/test/suite/ -name *.js) 
do
   fn=$(basename $tcn)
   if LANG=C grep -m 1 -F "${fn}" ../test-data/test262ignore; then
       continue
   else
       cp $tcn ../test-data/t262/${fn}
       echo "Converting $fn"
       js24 -e "try{var p = Reflect.parse(read(\"../test-data/t262/${fn}\"));
                print(JSON.stringify(p))}
                catch (ex) {print(\"FAIL\")}" > ../test-data/t262/${fn%.js}.json
       ./testgen ../test-data/t262/${fn%.js}.json > ../test-data/t262/${fn%.js}.parse
   fi
done
