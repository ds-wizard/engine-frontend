#!/bin/bash

set -e

rm -rf ./dist/engine-$COMPONENT
NODE_ENV=production ./node_modules/.bin/webpack
file=`find dist/engine-$COMPONENT -name "*.js"`
./node_modules/.bin/uglifyjs $file --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | ./node_modules/.bin/uglifyjs --mangle --output $file

$(dirname $0)/build_info.sh
