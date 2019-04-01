{-# LANGUAGE DuplicateRecordFields #-}

-- Library for working with distribution functions

-- Wrapper function that implements CDF
-- Should do something more sophisticated than integrating from -100
cdf :: ParametrizedDensity -> Double -> Double -> Maybe Double
cdf (ParametrizedDensity d) w x = d >>= integral [(-100),(-100+w)..x]

-- Numerical integration using midpoint rule
integral :: [Double] -> (Double -> Double) -> Maybe Double
integral []  _ = Nothing
integral [x] _ = Nothing
integral xs d  = Just $ sum areas
  where
    areas     = zipWith (*) heights widths
    widths    = map (\(x, y) -> y - x) endpoints
    heights   = map d midpoints
    midpoints = map (\(x, y) -> (y + x) / 2) endpoints
    endpoints = zip xs $ drop 1 xs


-- Density functions --

-- Density typeclass
data Density = Density { parameters :: [Double -> Bool]
                       , support    :: Double -> Bool
                       , pdf        :: [Double] -> Double -> Double
                       } deriving ()

data ParametrizedDensity = ParametrizedDensity { pdf :: Maybe (Double -> Double)
                                               } deriving ()

parametrize :: Density -> [Double] -> ParametrizedDensity
parametrize (Density c s f) p =
  let pValid = (and [cond par | (cond, par) <- zip c p]) && (length c == length p)
      g :: Double -> Double
      g x
        | not $ s x = 0
        | otherwise = f p x
      pdf :: Maybe (Double -> Double)
      pdf
        | not pValid = Nothing
        | otherwise  = Just $ g
  in ParametrizedDensity pdf

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


-- Random variables in Haskell --

-- data ContinuousRV = ContinuousRV (pdf: 
-- 
-- rvX :: ContinuousRV
