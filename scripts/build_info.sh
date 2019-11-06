#!/bin/bash

set -e


# File with build info
BUILD_INFO_FILE=$(dirname $0)/../dist/engine-$COMPONENT/main.*.js


# Create version based on git tag or branch
branch=`git rev-parse --abbrev-ref HEAD`
commit=`git rev-parse --short HEAD`
version="$branch~$commit"
gittag=`git tag -l --contains HEAD | head -n 1`
if test -n "$gittag"
then
    version="$gittag~$commit"
fi


# Get build timestamp
builtAt=`date +"%Y/%m/%d %TZ"`


# Replace values
sed -i.bak "s#{version}#$version#" $BUILD_INFO_FILE && rm $BUILD_INFO_FILE".bak"
sed -i.bak "s#{builtAt}#$builtAt#" $BUILD_INFO_FILE && rm $BUILD_INFO_FILE".bak"
