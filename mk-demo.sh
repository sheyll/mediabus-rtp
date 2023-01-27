#!/usr/bin/env bash

set -e

nix build .\#mediabus-rtp:exe:mediabus-rtp-demo $@
