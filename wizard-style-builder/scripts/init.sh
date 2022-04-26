#!/bin/bash

set -e

# Prepare src
rm -rf ./src
mkdir src
cp -r ../node_modules/@fortawesome ./src/~@fortawesome
cp -r ../node_modules/bootstrap ./src/~bootstrap
cp -r ../engine-wizard/scss ./src/scss

# Create version based on git tag or branch
branch=`git rev-parse --abbrev-ref HEAD`
commit=`git rev-parse --short HEAD`
version="$branch~$commit"
gittag=`git tag -l --contains HEAD | head -n 1`
if test -n "$gittag"
then
    version="$gittag~$commit"
fi

# Replace version
VERSION_FILE=./src/scss/_version.scss
sed -i.bak "s#{version}#$version#" $VERSION_FILE && rm $VERSION_FILE".bak"
