#!/bin/bash

set -e

FILES=("package.json" "package-lock.json")

for FILE in ${FILES[@]}; do
  sed 's/^  "version": ".*"/  "version": "'$1'"/' $FILE > $FILE.new && mv $FILE.new $FILE
done

npm install
