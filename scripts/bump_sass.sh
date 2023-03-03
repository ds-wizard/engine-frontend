#!/bin/bash

set -e

# Bump sass for the frontend
npm install -S sass@$1

# Bump sass for the style builder
cd wizard-style-builder
npm install -S sass@$1
