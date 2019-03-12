module Main where

import Data.Maybe (listToMaybe)
import Data.Int (Int16)
import Foreign.Storable (sizeOf, pokeElemOff)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Control.Concurrent (threadDelay)

import qualified Sound.OpenAL as AL
import Sound.OpenAL (($=))

import Sample

amplitudeToInt :: Double -> Int16
amplitudeToInt x =
    let clip = min 1.0 $ max (-1.0) x
        shift = clip * 0.5 + 0.5
    in floor $ 32760.0 * clip

samplesToBuffer :: Int -> [Double] -> IO AL.Buffer
samplesToBuffer sampleRate xs = do
    let arraySize = length xs * (sizeOf (0 :: Int16))
    array <- fmap castPtr $ mallocBytes arraySize

    let mapping = zip [0..] (map amplitudeToInt xs)
    mapM_ (uncurry $ pokeElemOff array) mapping

    let memoryRegion = AL.MemoryRegion array (fromIntegral arraySize)
    let bufferData = AL.BufferData memoryRegion AL.Mono16 (fromIntegral sampleRate)

    buffer <- AL.genObjectName
    AL.bufferData buffer $= bufferData

    free array

    return buffer

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

manysine :: Sampler
manysine = mix [sine 440.0, sine 660.0, sine 880.0]

sampleRate = 22050

sample' = sample sampleRate

evens (x:y:zs) = x:(evens zs)
evens [x] = [x]
evens [] = []

notes = (evens . chromatic 12) (freq C) ++ major (freq C) ++ minor (freq C)

play :: Double -> [Double]
play f = sample' 3.0 (fade 1.0 hann_window 3.0 |*| sine f)

pause :: Double -> [Double]
pause duration = sample' duration (const 0.0)

samples :: [Double]
samples = concatMap play notes

main :: IO ()
main = do
    deviceName <- fmap listToMaybe $ AL.get AL.allDeviceSpecifiers
    device <- check "openDevice" $ AL.openDevice deviceName

    context <- check "createContext" $ AL.createContext device []
    AL.currentContext $= Just context

    buffer <- samplesToBuffer sampleRate samples

    source <- AL.genObjectName
    AL.buffer source $= Just buffer

    putStrLn "Playing"
    AL.play [source]

    threadDelay (ceiling $ duration sampleRate (length samples)*1000*1000)

    check "closeDevice" $ fmap boolToMaybe $ AL.closeDevice device
