#!/bin/bash
set -e
set -u
mkdir -p gen
find ../../submodules/test262/test/suite/ -iname "*.js" -type f | xargs -n1 -ijsfile ./js24_parse.sh "jsfile"
