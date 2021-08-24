module Data.MediaBus.Rtp.SourceSpec (spec) where

import Conduit
import Control.Lens
import Control.Monad.Logger
import qualified Data.ByteString as B
import Data.Conduit.List
import Data.MediaBus
import Data.MediaBus.Rtp
import Data.Proxy
import Data.Word
import Test.Hspec

spec :: Spec
spec = rtpSourceSpec >> rtpPayloadDemuxSpec

rtpSourceSpec :: Spec
rtpSourceSpec = describe "rtpSource" $ do
  let countStarts :: [Series a b] -> Int
      countStarts =
        foldr
          ( \x n -> case x of
              Start _ -> n + 1
              Next _ -> n
          )
          0
      runTestConduit inputs =
        runNoLogging $
          runConduit
            ( sourceList inputs
                .| annotateTypeCIn
                  (Proxy :: Proxy (Stream Int Int Int () B.ByteString))
                  rtpSource
                .| consume
            )

  it "yields 'Start' when only when the first payload packet arrives" $
    let inputs = [MkStream (Start (MkFrameCtx 0 0 0 ()))]
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 0

  it "yields 'Start' when the first packet arrives" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 0 0
          ]
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 1

  it "yields 'Start' when the ssrc changes" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 8 9 10 ())),
            mkTestRtpPacket ssrc0 0 0,
            mkTestRtpPacket ssrc0 0 0,
            mkTestRtpPacket ssrc1 0 0,
            mkTestRtpPacket ssrc1 0 0,
            mkTestRtpPacket ssrc0 0 0,
            mkTestRtpPacket ssrc1 0 0,
            mkTestRtpPacket ssrc1 0 0
          ]
        ssrc0 = 123
        ssrc1 = 345
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 4

  it "yields 'Start' when the sequence numbers change too much" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 01 0,
            mkTestRtpPacket 0 02 0,
            mkTestRtpPacket 0 03 0,
            mkTestRtpPacket 0 24 0,
            mkTestRtpPacket 0 25 0,
            mkTestRtpPacket 0 06 0,
            mkTestRtpPacket 0 07 0
          ]
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 3

  it "yields 'Start' when the sequence numbers change too much" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 01 0,
            mkTestRtpPacket 0 02 0,
            mkTestRtpPacket 0 03 0,
            mkTestRtpPacket 0 24 0,
            mkTestRtpPacket 0 25 0,
            mkTestRtpPacket 0 06 0,
            mkTestRtpPacket 0 07 0
          ]
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 3

  it "yields no 'Start' when the sequence number wraps around" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 (negate 3) 0,
            mkTestRtpPacket 0 (negate 2) 0,
            mkTestRtpPacket 0 (negate 1) 0,
            mkTestRtpPacket 0 0 0,
            mkTestRtpPacket 0 1 0
          ]
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 1

  it "yields no 'Start' when the timestamp wraps around" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 0 (negate 300),
            mkTestRtpPacket 0 0 (negate 200),
            mkTestRtpPacket 0 0 (negate 100),
            mkTestRtpPacket 0 0 0,
            mkTestRtpPacket 0 0 100
          ]
     in countStarts (_stream <$> runTestConduit inputs)
          `shouldBe` 1

  it "can handle broken packets without crashing" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 0 777,
            mkTestRtpPacket 0 0 777,
            mkBrokenTestRtpPacket,
            mkBrokenTestRtpPacket,
            mkTestRtpPacket 0 0 777,
            mkTestRtpPacket 0 0 777,
            mkBrokenTestRtpPacket,
            mkTestRtpPacket 0 0 777,
            mkTestRtpPacket 0 0 777,
            mkTestRtpPacket 0 0 777
          ]
     in length (runTestConduit inputs)
          `shouldBe` 8

runNoLogging :: NoLoggingT Identity a -> a
runNoLogging = runIdentity . runNoLoggingT

rtpPayloadDemuxSpec :: Spec
rtpPayloadDemuxSpec = describe "rtpPayloadDemux" $ do
  let runTestConduit inputs payloadHandlers fallback =
        runNoLogging $
          runConduit
            ( sourceList inputs
                .| rtpSource
                .| rtpPayloadDemux payloadHandlers fallback
                .| consume
            )

  it "always yields the fallback element if the payload table contains no handler" $
    let inputs =
          [ MkStream (Start (MkFrameCtx 0 0 0 ())),
            mkTestRtpPacket 0 0 0
          ]
        outs = preview eachFramePayload <$> runTestConduit inputs [] ()
     in outs `shouldBe` [Nothing, Just ()]
  it "always yields the fallback element if the payload table contains no handler for the payload type" $
    let inputs =
          MkStream (Start (MkFrameCtx 0 0 0 ())) :
            [ mkTestRtpPacketWithPayload
                0
                0
                0
                (mkTestPayload (MkRtpPayloadType p))
              | p <- [0 .. 128]
            ]
        fallback :: Audio (Hz 8000) Mono (Raw S16)
        fallback = mempty
        outs =
          preview eachFramePayload
            <$> runTestConduit
              inputs
              [ ( MkRtpPayloadType 129,
                  over (framePayload . eachChannel) decodeALawSample
                    . alawPayloadHandler
                )
              ]
              fallback
     in outs `shouldBe` Nothing :
          [ Just fallback
            | _ <- [0 .. 128 :: Word8]
          ]
  it "invokes the first matching payload handler" $
    let inputs =
          MkStream (Start (MkFrameCtx 0 0 0 ())) :
          [ mkTestRtpPacketWithPayload 0 0 0 (mkTestPayload 8),
            mkTestRtpPacketWithPayload 0 0 0 (mkTestPayload 0)
          ]
        outputs =
          preview eachFramePayload
            <$> runTestConduit
              inputs
              [ ( 8,
                  framePayload
                    .~ "first 8 handler"
                ),
                ( 8,
                  framePayload
                    .~ "second 8 handler"
                ),
                ( 0,
                  framePayload
                    .~ "first 0 handler"
                ),
                ( 0,
                  framePayload
                    .~ "second 0 handler"
                )
              ]
              "bad"
     in outputs
          `shouldBe` [Nothing, Just "first 8 handler", Just "first 0 handler"]

mkBrokenTestRtpPacket :: Stream Int Int Int () B.ByteString
mkBrokenTestRtpPacket = MkStream (Next (MkFrame 0 0 (B.pack [0, 0, 0])))

mkTestPayload :: RtpPayloadType -> RtpPayload
mkTestPayload pt = MkRtpPayload pt (mediaBufferFromList [0, 0, 0])

mkTestRtpPacket ::
  RtpSsrc ->
  RtpSeqNum ->
  RtpTimestamp ->
  Stream Int Int Int () B.ByteString
mkTestRtpPacket ssrc sn ts =
  mkTestRtpPacketWithPayload ssrc sn ts (mkTestPayload 0)

mkTestRtpPacketWithPayload ::
  RtpSsrc ->
  RtpSeqNum ->
  RtpTimestamp ->
  RtpPayload ->
  Stream Int Int Int () B.ByteString
mkTestRtpPacketWithPayload ssrc sn ts p =
  MkStream
    ( Next
        ( MkFrame
            0
            0
            ( serialize
                ( MkRtpPacket
                    ( MkRtpHeader
                        2
                        False
                        False
                        sn
                        ts
                        ssrc
                        []
                        Nothing
                    )
                    p
                )
            )
        )
    )
