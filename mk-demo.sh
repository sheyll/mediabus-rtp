#!/usr/bin/env bash

set -e

nix-build -A mediabus-rtp.components.exes.mediabus-demo-rtp-alaw-player $@
