{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Numeric.LinearAlgebra.Data
import Control.Lens
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector          as V
import GHC.Generics (Generic)
import Unsafe.Coerce

import TinyMath as Tiny

-- fromVector :: V.Vector a -> SV.Vector a
-- fromVector v = SV.pack $ V.toList v

identity :: Int -> Matrix Double
identity = ident

pause :: IO ()
pause = do
  putStrLn "\n Program paused. Press enter to continue."
  _ <- getChar
  return ()

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
    d = case decode NoHeader f :: Either String (V.Vector (Double, Double)) of
      Left err -> V.empty
      Right xs -> xs

  Tiny.plot $ V.toList d
  pause
  
-- -- dv = (\(Right xs) -> xs) d

-- =================== Part 3: Cost and Gradient descent ===================

-- f <- BL.readFile "./data/ex1data1.txt"
-- d = decode NoHeader f :: Either String (V.Vector (Double, Double))
-- v = (\ (Right xs) -> xs) d

-- vl = fmap (^..each) $ V.toList $ (\ (Right xs) -> xs) d
-- transpose = getZipList . sequenceA . map ZipList $ vl

-- k = transpose $ fmap (^..each) $ V.toList $ (\ (Right xs) -> xs) d
-- add a row of 1's:
-- transpose $ (take (length $ head k) $ repeat 1) : k

-- decode NoHeader f :: Either String (V.Vector [Double])
-- d' = decode NoHeader f :: Either String (V.Vector [Double])
-- v' = (\ (Right xs) -> xs) d'
-- transpose . V.toList $ v'
-- getZipList . sequenceA . map ZipList $ V.toList v'
  
  return ()
