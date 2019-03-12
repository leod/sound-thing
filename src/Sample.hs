module Sample where

type Sampler = Double -> Double

infixl 8 |*| 

(|*|) :: Sampler -> Sampler -> Sampler
(|*|) g h t = g t * h t

sine :: Double -> Sampler
sine f t = sin (2.0 * pi * f * t)

wmix :: [Double] -> [Sampler] -> Sampler
wmix ws gs t = sum . map (uncurry (*)) $ zip ws (map ($ t) gs)

mix :: [Sampler] -> Sampler
mix gs =
    let weight = 1.0 / (fromIntegral (length gs))
    in wmix (repeat weight) gs

nsamples :: Int -> Double -> Int
nsamples sampleRate duration =
    let n = fromIntegral . ceiling $ duration * fromIntegral sampleRate
    in n + 1

duration :: Int -> Int -> Double
duration sampleRate n = fromIntegral (sampleRate * n)

sample :: Int -> Double -> Sampler -> [Double]
sample sampleRate duration g =
    let n = nsamples sampleRate duration 
        ts = map ((/ fromIntegral sampleRate) . fromIntegral) [0..n-1] 
    in map g ts

type Window = Double -> Sampler

hann_window :: Window
hann_window t_end t = (sin (pi * t / t_end)) ** 2.0

hamming_window :: Window
hamming_window t_end t =
    let a_0 = 0.54
    in a_0 - (1 - a_0) * cos (2 * pi * t / t_end)

fade :: Double -> Window -> Double -> Sampler
fade duration window t_end t =
    if t >= 0.0 && t <= duration then window (duration * 2.0) t
    else if t > duration && t < t_end - duration then 1.0
    else if t >= t_end - duration && t <= t_end then window (duration * 2.0) (t_end - t)
    else 0.0

whole_ratio = 2.0 ** (1.0 / 6.0)
half_ratio = 2.0 ** (1.0 / 12.0)

data Note = A | B | C | D | E | F | G

freq :: Note -> Double
freq A = 440.0
freq B = freq A * whole_ratio
freq C = freq B * half_ratio
freq D = freq C * whole_ratio
freq E = freq D * whole_ratio
freq F = freq E * half_ratio
freq G = freq F * whole_ratio

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

majorChord :: Double -> [Double]
majorChord f = [f, f * h^4, f * h^7]
    where h = half_ratio

minorChord :: Double -> [Double]
minorChord f = [f, f * h^3, f * h^7]
    where h = half_ratio

--progression :: [Double] -> [Int] -> [[Double]]
--progression f ns = 

fixToScale :: Double -> Double -> Double
fixToScale f_scale f =
  if f > 2.0 * f_scale then f / 2.0
  else if f < f_scale then f * 2.0
  else f

chromatic :: Int -> Double -> [Double]
chromatic n f =
    -- f * ratio^n = 2 * f
    -- => ratio = 2^(1/n)
    let ratio = 2.0 ** (1.0 / fromIntegral n)
    in take (n+1) $ iterate (* ratio) f
