module Main where

import Data.Maybe (listToMaybe)
import Data.Int (Int16)
import Foreign.Storable (sizeOf, pokeByteOff, peekByteOff)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Control.Concurrent (threadDelay)

import Sound.OpenAL as AL

type BufferType = Int16

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

numSeconds = 5
sampleRate = 44100

sound :: Int -> Int -> Double
sound sampleRate t = sin (2.0 * pi * 220.0 / (fromIntegral sampleRate) * fromIntegral t)

amplitudeToInt :: Double -> Int16
amplitudeToInt x =
    let clip = min 1.0 $ max (-1.0) x
        shift = clip * 0.5 + 0.5
    in ceiling $ clip * 65535.0 - 32768.0

main :: IO ()
main = do
   deviceName <- fmap listToMaybe $ AL.get AL.allDeviceSpecifiers
   device <- check "openDevice" $ AL.openDevice deviceName

   context <- check "createContext" $ AL.createContext device []
   AL.currentContext $= Just context

   buffer <- AL.genObjectName

   let numSamples = numSeconds * sampleRate

   let arraySize = numSamples * sizeOf (0 :: BufferType)
   array <- fmap castPtr $ mallocBytes arraySize

   mapM_ (\t -> pokeByteOff array t (amplitudeToInt $ sound sampleRate t)) [0..numSamples]
   AL.bufferData buffer $= AL.BufferData (MemoryRegion array (fromIntegral arraySize))
                                         AL.Mono16
                                         (fromIntegral sampleRate)

   source <- AL.genObjectName
   AL.buffer source $= Just buffer
   AL.play [source]

   threadDelay (numSeconds*1000*1000)

   check "closeDevice" $ fmap boolToMaybe $ AL.closeDevice device
