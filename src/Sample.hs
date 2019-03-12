module Sample where

type Sampler = Double -> Double

sine :: Double -> Sampler
sine f t = sin (2.0 * pi * f * t)

wmix :: [Double] -> [Sampler] -> Sampler
wmix ws gs t = sum . map (uncurry (*)) $ zip ws (map ($ t) gs)

mix :: [Sampler] -> Sampler
mix gs =
    let weight = 1.0 / (fromIntegral (length gs))
    in wmix (repeat weight) gs

sample :: Int -> Double -> Sampler -> [Double]
sample sampleRate duration g =
    let n = fromIntegral . ceiling $ duration * fromIntegral sampleRate
        ts = map ((/ (fromIntegral sampleRate)) . fromIntegral) [0..n-1] 
    in map g ts

whole_ratio = 2.0 ** (1.0 / 6.0)
half_ratio = 2.0 ** (1.0 / 12.0)

data Note = A | B | C | D | E | F | G

freq :: Note -> Double
freq A = 440.0
freq B = freq A + whole_ratio
freq C = freq B + half_ratio
freq D = freq C + whole_ratio
freq E = freq D + whole_ratio
freq F = freq E + half_ratio
freq G = freq F + whole_ratio

major :: Double -> [Double]
major f = scanl (*) f scale
    where w = whole_ratio
          h = half_ratio
          scale = [w, w, h, w, w, w, h]

minor :: Double -> [Double]
minor f = scanl (*) f scale
    where w = whole_ratio
          h = half_ratio
          scale = [w, h, w, w, h, w, w]

chromatic :: Int -> Double -> [Double]
chromatic n f =
    -- f * ratio^n = 2 * f
    -- => ratio = 2^(1/n)
    let ratio = 2.0 ** (1.0 / fromIntegral n)
    in take (n+1) $ iterate (* ratio) f
