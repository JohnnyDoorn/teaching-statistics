#!/bin/zsh
# palette -> geometry/bullets -> shared master -> re-base
set -e
python3 applypalette.py "$1" p1.pptx
python3 bake.py p1.pptx p2.pptx > /dev/null
python3 build.py p2.pptx /dev/null
python3 rebase.py "$2"
