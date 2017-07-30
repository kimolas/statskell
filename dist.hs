-- Library for working with distribution functions

import Control.Monad

-- Density typeclass
-- data Density = Density { }

-- Exponential distribution
dexp :: Double -> Maybe (Double -> Double)
dexp λ
  | λ <= 0    = Nothing
  | otherwise = Just pdf
  where
    pdf x
      | x < 0     = 0
      | otherwise = λ * exp ((-λ) * x)

-- Normal distribution 
dnorm :: Double -> Double -> Maybe (Double -> Double)
dnorm μ σ
  | σ <= 0    = Nothing
  | otherwise = Just pdf
  where
    pdf x = (exp (-(x - μ)^2 / (2 * σ^2))) / (σ * sqrt (2 * pi))


-- Numerical integration using midpoint rule
integral :: Maybe (Double -> Double) -> [Double] -> Maybe Double
integral d xs = sum <$> pr vs ws
  where
    pr = liftM2 $ zipWith (*)
    cs = pw (\x y -> (x + y) / 2) xs
    vs = (liftM map d) <*> cs
    ws = pw (\x y -> y - x) xs

-- Wrapper function that returns CDF
cdf d w x = integral d [(-100),(-100+w)..x]

-- Helper function for integrator
pw :: (a -> a -> a) -> [a] -> Maybe [a]
pw _ [] = Nothing
pw _ [x] = Nothing
pw f [x,y] = Just [f x y]
pw f (x:xs) = (liftM (:) (return (f x (head xs)))) `ap` (pw f xs)
