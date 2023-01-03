#!/bin/bash

set -e

# Bump sass for the frontend
npm install -S sass@$1

# Bump sass in the dockerfile
DOCKERFILE=engine-wizard/docker/Dockerfile
sed -i.bak -E "s#dart-sass-([0-9\.]+)-linux#dart-sass-$1-linux#g ; s#download/([0-9\.]+)/dart#download/$1/dart#g" $DOCKERFILE
rm $DOCKERFILE".bak"

# Bump sass for the style builder
cd wizard-style-builder
npm install -S sass@$1
