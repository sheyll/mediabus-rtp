#!/usr/bin/zsh

set -e


outdir=mediabus-demo-rtp-alaw-player-profile-output-$(date --iso-8601=seconds)
mkdir $outdir
stack exec mediabus-demo-rtp-alaw-player -- +RTS -hy  -M2M -xt -p -P
hp2ps -c mediabus-demo-rtp-alaw-player.hp
ps2pdf mediabus-demo-rtp-alaw-player.ps
mv mediabus-demo-rtp-alaw-player.aux mediabus-demo-rtp-alaw-player.hp mediabus-demo-rtp-alaw-player.prof mediabus-demo-rtp-alaw-player.pdf mediabus-demo-rtp-alaw-player.ps $outdir
