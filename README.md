# RTP Source/Sink for [mediabus](https://github.com/lindenbaum/mediabus)

[![Build Status](https://travis-ci.org/lindenbaum/mediabus-rtp.svg?branch=master)](https://travis-ci.org/lindenbaum/mediabus-rtp)
[![Hackage](https://img.shields.io/hackage/v/mediabus-rtp.svg)](http://hackage.haskell.org/package/mediabus-rtp)

## Overview

Currently this module provides support for reception of RTP Audio packets with a focus on low-fi audio codecs.

## RTP-Audio Demo

Either use `nix develop` **or** make sure you have **sox** and **gstreamer >= 1.0** installed.

Run an RTP sender:

    cd mediabus-rtp-demo/rtp-sender
    ./send-pcm16.sh 10000 127.0.0.1

This sends the demo/test content.

Now start the demo-receiver in a second shell:

    nix run .#mediabus-rtp-demo -- sync

or

    ./mk-demo.sh
    ./result/bin/mediabus-rtp-demo sync

or
    nix develop
    cabal run -- sync
