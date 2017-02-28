# RTP Source/Sink for [mediabus](https://github.com/lindenbaum/mediabus)

[![Build Status](https://travis-ci.org/lindenbaum/mediabus-rtp.svg?branch=master)](https://travis-ci.org/lindenbaum/mediabus-rtp)
[![Hackage](https://img.shields.io/hackage/v/mediabus-rtp.svg)](http://hackage.haskell.org/package/mediabus-rtp)

## Overview

Currently this module provides support for reception of RTP packets.

## RTP-Alaw Demo

Make sure you have **sox** and **gstreamer >= 1.0** installed.

Check out this project and in shell execute these commands:

    cd mediabus-demo-rtp-alaw-player/rtp-sender
    ./send.sh 10000 127.0.0.1

This starts the RTP sender, that sends the demo/test content.

Now start the demo-receiver in a second shell:

    ./build-mediabus-demo-rtp-alaw-player.sh
    stack exec mediabus-demo-rtp-alaw-player
