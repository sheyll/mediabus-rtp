#!/usr/bin/zsh

set -e


stack build --no-test --no-library-profiling --no-executable-profiling --no-haddock
