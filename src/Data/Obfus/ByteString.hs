module Data.Obfus.ByteString where

import Pipes
import qualified Data.ByteString as B
import Data.Bits (xor)

xorStream :: Monad m => B.ByteString -> Pipe B.ByteString B.ByteString m ()
xorStream key
  | B.length key == 0 = error "xorStream: empty key"
  | otherwise = goPipe 0
 where
  goPipe keyOffset = do
    bs <- await
    let (newKeyOffset, bs') = goXor keyOffset bs
    yield bs'
    goPipe newKeyOffset

  goXor i bs =
    let i' = (i + B.length bs) `rem` B.length key
        bs' = B.pack $ zipWith xor (B.unpack bs) (drop i infKey)
    in (i', bs')

  infKey = cycle $ B.unpack key
