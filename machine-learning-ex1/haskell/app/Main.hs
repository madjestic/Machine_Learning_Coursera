{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Main where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel          ( MatrixOrder(..)
                                            , matrixFromVector)
import Numeric.LinearAlgebra.HMatrix
-- import Control.Lens
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import GHC.Generics (Generic)
-- import Unsafe.Coerce

import TinyMath             as Tiny

matrixFromVector' :: VS.Storable a => MatrixOrder -> Int -> Int -> V.Vector [a] -> Matrix a
matrixFromVector' o r c = matrixFromVector o r c . VS.fromList . concat . V.toList

identity :: Int -> Matrix Double
identity = ident

pause :: IO ()
pause = do
  putStrLn "\n Program paused. Press enter to continue."
  _ <- getChar
  return ()

class Decodable a where
  decode' :: HasHeader -> BL.ByteString -> V.Vector a

instance Decodable [Double] where
  decode' :: HasHeader -> BL.ByteString -> V.Vector [Double]
  decode' h f = d
    where
      d = case decode NoHeader f :: Either String (V.Vector [Double]) of
        Left err -> V.empty
        Right xs -> xs

instance Decodable (Double, Double) where
  decode' :: HasHeader -> BL.ByteString -> V.Vector (Double, Double)
  decode' h f = d
    where
      d = case decode NoHeader f :: Either String (V.Vector (Double, Double)) of
        Left err -> V.empty
        Right xs -> xs

-- instance VS.Vector Integer where
--   konst :: Integer -> (Int, Int) -> Matrix Integer
--   konst = undefined
        
--computeCost :: Matrix a -> Matrix a -> a
computeCost :: Matrix Double -> Matrix Double -> Double 
computeCost xs theta = cost
  where
    m    = fromIntegral . fst . size $ xs   :: Double
    x    = xs ¿ [0,1]                        
    y    = xs ¿ [2]                          
    hyp  = mul x theta                      :: Matrix Double
    temp = (hyp - y)^2                      :: Matrix Double
    cost = ((2*m)**(-1) * sumElements temp ) :: Double

-- app xs (VS.fromList [1.0,1.0,1.0])
-- ((2*m)**(-1)) * (sumElements $ (xs :: Matrix Double) ¿ [1])

gradientDescent :: Matrix a -> Matrix a -> Double -> Integer -> Double
gradientDescent = undefined 

-- ===================== Main                   ====================

main :: IO ()
main = do
--  ==================== Part 1: Basic Function ====================  
  putStrLn "Running warmUpExercise ..."
  putStrLn "5x5 Identity Matrix:"
  print (identity 5)
  pause

-- ======================= Part 2: Plotting =======================
  putStrLn "Plotting Data..."
  f <- BL.readFile "./data/ex1data1.txt"
  let
    d = decode' NoHeader f
    in Tiny.plot $ V.toList d
    
  pause
  
-- =================== Part 3: Cost and Gradient descent ===================
  let
    -- | Some gradient descent settings
    iterations = 1500 :: Integer
    alpha      = 0.1  :: Double
    
    d     = decode' NoHeader f :: V.Vector [Double]
    mtx   = matrixFromVector' ColumnMajor 2 (V.length d) d
    m     = V.length d
    -- | a Matrix with a column of 1's preadded.
    -- | initialize fittig parameters
    xs    = konst 1 (m, 1) ||| tr mtx      -- | Add a column of ones to x
    theta = konst 0 (2,1)      :: Matrix Double -- | initialize fitting parameters

    -- | compute and display initial cost
    cost  = computeCost xs theta
  print cost

  let
    theta'= gradientDescent xs theta alpha iterations

    -- m = 
  return ()
