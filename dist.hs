{-# LANGUAGE DuplicateRecordFields #-}

-- Library for working with distribution functions

-- Numerical integration using midpoint rule
integral :: [Double] -> (Double -> Double) -> Maybe Double
integral []  _ = Nothing
integral [x] _ = Nothing
integral xs d  = Just $ sum $ zipWith (*) heights widths
  where
    pairs = zip xs $ drop 1 xs
    widths = map (\(x, y) -> y - x) pairs
    midpoints = map (\(x, y) -> (y + x) / 2) pairs
    heights = map d midpoints

-- Wrapper function that implements CDF
-- Should do something more sophisticated than integrating from -100
cdf :: ParametrizedDensity -> Double -> Double -> Maybe Double
cdf d w x = d >>= integral [(-100),(-100+w)..x]


-- Density functions --

-- Density typeclass
data Density = Density { parameters :: [Double -> Bool]
                       , support    :: Double -> Bool
                       , pdf        :: [Double] -> Double -> Double
                       } deriving ()

data ParametrizedDensity = ParametrizedDensity { support :: Double -> Bool
                                               , pdf     :: Maybe (Double -> Double)
                                               } deriving ()

parametrize :: Density -> [Double] -> ParametrizedDensity
parametrize (Density c s f) p =
  let pValid = (and [cond par | (cond, par) <- zip c p]) && (length c == length p)
      pdf
        | not pValid = Nothing
        | otherwise  = Just $ f p
  in ParametrizedDensity s pdf

dEval :: ParametrizedDensity -> Double -> Maybe Double
dEval (ParametrizedDensity s f) x
  | not $ s x = Just 0
  | otherwise = Just $ f x

-- Exponential distribution
dExp = Density p s pdf
  where
    p          = [(>= 0)]
    s          = (>= 0)
    pdf [λ] x  = λ * exp ((-λ) * x) 

-- Normal distribution 
dNorm = Density p s pdf
  where
    p           = [(const True), (>= 0)]
    s           = const True
    pdf [μ,σ] x = (exp (-(x - μ)^2 / (2 * σ^2))) / (σ * sqrt (2 * pi))


-- Random variables in Haskell

-- data ContinuousRV = ContinuousRV (pdf: 
-- 
-- rvX :: ContinuousRV
