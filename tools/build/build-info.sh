#!/bin/bash

set -e


# File with build info
BUILD_INFO_FILE=$(dirname $0)/../../dist/$COMPONENT/main.*.js
STYLE_FILE=$(dirname $0)/../../dist/$COMPONENT/main.*.css


# Create version based on git tag or branch
# In CI the checkout is a detached HEAD: the branch is the one the run is for.
branch=${GITHUB_REF_NAME:-`git rev-parse --abbrev-ref HEAD`}
commit=`git rev-parse --short HEAD`
version="$branch~$commit"
gittag=$(git tag --points-at HEAD | head -n 1)
if test -n "$gittag"
then
    version="$gittag~$commit"
fi


# Get build timestamp
builtAt=`date -u +"%Y-%m-%dT%H:%M:%SZ"`


# Replace values
sed -i.bak "s#{version}#$version#" $BUILD_INFO_FILE && rm $BUILD_INFO_FILE".bak"
sed -i.bak "s#{builtAt}#$builtAt#" $BUILD_INFO_FILE && rm $BUILD_INFO_FILE".bak"


# A release tag (vX.Y.Z) points the default guide links at its guide (X.Y)
# instead of develop; a deployment's own guideLinks still override them.
releasetag=$(git tag --points-at HEAD | grep -E '^v[0-9]+\.[0-9]+\.[0-9]+$' | head -n 1 || true)
if test -n "$releasetag"
then
    guideversion=$(echo "$releasetag" | sed -E 's#^v([0-9]+\.[0-9]+)\..*#\1#')
    sed -i.bak -E "s#(https://guide\.[a-z.-]+/en/)develop/#\1$guideversion/#g" $BUILD_INFO_FILE && rm $BUILD_INFO_FILE".bak"
fi
