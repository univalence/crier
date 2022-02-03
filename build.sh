#!/bin/bash

# BASH STRICT MODE
set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd $SCRIPT_DIR;

sbt stage

./core/target/universal/stage/bin/crier