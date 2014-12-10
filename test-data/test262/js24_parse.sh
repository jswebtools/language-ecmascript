#!/bin/bash
set -e
set -u
js24 -e "print(JSON.stringify(Reflect.parse(read(\"$1\"))))" > gen/$(basename $1)
