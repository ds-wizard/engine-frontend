#!/bin/bash

set -e

zip -r wizard-style-builder-lambda node_modules src core.js lambda.js template.yml
