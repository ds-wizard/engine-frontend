#!/bin/bash

set -e

# Bump sass in the frontend dockerfile
DOCKERFILE=engine-wizard/docker/Dockerfile
sed -i.bak -E "s#install -g sass@([0-9\.]+)#install -g sass@$1#g" $DOCKERFILE
rm $DOCKERFILE".bak"


# Bump sass for the frontend
npm install -S sass@$1

# Bump sass for the style builder
cd wizard-style-builder
npm install -S sass@$1
