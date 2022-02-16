#!/bin/bash

set -e


# File with build info
FILE=$(dirname $0)/../engine-$COMPONENT/scss/_version.scss


# Create version based on git tag or branch
branch=`git rev-parse --abbrev-ref HEAD`
commit=`git rev-parse --short HEAD`
version="$branch~$commit"
gittag=`git tag -l --contains HEAD | head -n 1`
if test -n "$gittag"
then
    version="$gittag~$commit"
fi


# Replace values
sed -i.bak "s#{version}#$version#" $FILE && rm $FILE".bak"
