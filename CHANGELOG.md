# Changes

## 2.0.1

* Update to mediabus-2.0.1

## 2.0.0

* Introduce `Data.MediaBus.Rtp.PcmAudioSource` for directly receiving PCM 16 bit 16 kHz audio
* Introduce `rtpPayloadDispatcher` and `rtpParserC`
* Rename a lot of stuff

## 1.0.0

* Update logging to always set a `LogSource` for this library
* Change the examples to include some segmentation.

## 0.7.0

* Remove the static segmentation from `udpRtpAlaw16kHzS16SourceC`
  so the caller can decide the segmentation.

## 0.6.2

* Update to mediabus-0.9.0

## 0.6.1

* Remove unused dependencies

## 0.6.0

* Update to mediabus-0.8.0

## 0.5.0.1

* Update to mediabus-0.5.0.1

## Version 0.3.2.0

* Adapt to mediabus-0.3.2.0
* Add Logging
