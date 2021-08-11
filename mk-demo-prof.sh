#!/usr/bin/env bash

set -e

$HERE=$(realpath $(dirname "$0"))

$HERE/mk-demo.sh --arg withProfiling true
