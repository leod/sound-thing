module Main where

import Data.Maybe (listToMaybe)
import Data.Int (Int16)
import Foreign.Storable (sizeOf, pokeByteOff, peekByteOff)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Control.Concurrent (threadDelay)

import Sound.OpenAL as AL

type SampleType = Int16
sampleBytes = sizeOf (0 :: SampleType)

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

numSeconds = 5
sampleRate = 22050

sine :: Int -> Double -> Int -> Double
sine sampleRate freq t = sin (2.0 * pi * freq / (fromIntegral sampleRate) * fromIntegral t)

manysine :: Int -> Int -> Double
manysine sampleRate t = (sine sampleRate 440.0 t + sine sampleRate 660.0 t + sine sampleRate 880.0 t) / 3.0

amplitudeToInt :: Double -> Int16
amplitudeToInt x =
    let clip = min 1.0 $ max (-1.0) x
        shift = clip * 0.5 + 0.5
    in floor $ 32760.0 * clip

main :: IO ()
main = do
   deviceName <- fmap listToMaybe $ AL.get AL.allDeviceSpecifiers
   device <- check "openDevice" $ AL.openDevice deviceName

   context <- check "createContext" $ AL.createContext device []
   AL.currentContext $= Just context

   buffer <- AL.genObjectName

   let numSamples = numSeconds * sampleRate

   let arraySize = numSamples * sampleBytes
   array <- fmap castPtr $ mallocBytes arraySize

   mapM_ (\t -> pokeByteOff array (t*sampleBytes) (amplitudeToInt $ manysine sampleRate t)) [0..numSamples]
   AL.bufferData buffer $= AL.BufferData (MemoryRegion array (fromIntegral arraySize))
                                         AL.Mono16
                                         (fromIntegral sampleRate)

   source <- AL.genObjectName
   AL.buffer source $= Just buffer
   AL.play [source]

   threadDelay (numSeconds*1000*1000)

   check "closeDevice" $ fmap boolToMaybe $ AL.closeDevice device
