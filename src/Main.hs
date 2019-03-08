module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Maybe (listToMaybe)
import System.Environment ( getArgs )
import Sound.OpenAL as AL

type DeviceSpecifier = Maybe String

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

main :: IO ()
main = do
   d <- fmap listToMaybe $ AL.get AL.allDeviceSpecifiers
   device <- check "openDevice" $ AL.openDevice d
   threadDelay 1000000
   check "closeDevice" $ fmap boolToMaybe $ AL.closeDevice device
