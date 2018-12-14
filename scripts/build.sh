#!/bin/bash

set -e

rm -rf ./dist
NODE_ENV=production webpack -p
file=`find dist -name "*.js"`
npx uglifyjs $file --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | npx uglifyjs --mangle --output=$file
