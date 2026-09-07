#!/bin/zsh
set -e
python3 build.py "$1" /dev/null
python3 rebase.py "$2"
