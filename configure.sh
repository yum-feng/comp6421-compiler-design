#!/usr/bin/env bash

# DISCLAIMER: not sure if it's appropriate to call such a purposed
# file as 'configure.sh' but yolo.

if [ -d ~/.config/common-lisp/source-registry.conf.d ]; then
  printf "(:tree \"$(pwd)\")" > ~/.config/common-lisp/source-registry.conf.d/comp6421-compiler-design.conf
else
  printf 'directory does not exist: ~/.config/common-lisp/source-registry.conf.d'
fi
