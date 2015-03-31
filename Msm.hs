-- Author: Giedrius Jonikas
--         giedriusj1@gmail.com

{-# LANGUAGE BangPatterns #-}
module Msm (simulateVolatility,
            simulateReturns) where


import Control.Monad.State
import System.Random
import System.IO.Unsafe

rand :: Float -> Float -> Float
rand min max = unsafePerformIO $ do
                 gen <- newStdGen
                 let (prob, _) = randomR (min,max) gen :: (Float, StdGen)
                 return prob
                        

simulateReturns :: Float -> Int -> Float -> Float -> Float -> IO [Float]
simulateReturns sigma kbar gamma_kbar b m0 = do
  volatility <- simulateVolatility sigma kbar gamma_kbar b m0
  gen <- newStdGen
  let rand = (randomRs (-1,1) gen) :: [Float]
  let ret = zipWith (*) rand volatility
  return ret


simulateVolatility :: Float -> Int -> Float -> Float -> Float -> IO [Float]
simulateVolatility sigma kbar gamma_kbar b m0 = do
  gen <- newStdGen
  let ns = take 1 $ randoms gen :: [Int]
  markovStateVector <- generateMarkovStateVector m0 kbar

  -- forM monad will keep the state of current markov state vector.
  -- Each iteration will return one point from time series simulation
  -- and update markov state vector.
  let sim = flip evalState markovStateVector $ do
              forM [0..] $ \_ -> do
                -- Get Markov state vector from state
                stateVector <- get

                -- updateMarkovStateVector already uses unsafe IO, so we might as
                -- well abuse it a bit more.
                let nextStateVector = unsafePerformIO $
                                      updateMarkovStateVector stateVector gamma_v m0 kbar
                -- Save updated state vector for next step
                put nextStateVector

                -- Calculate next step in the simulation
                let result = sigma * (sqrt  (product stateVector))
                return result

  return sim
    where gamma_v = gammaVector gamma_kbar kbar b


-- Computes gamma vector from gamma_kbar by inverting the ralationship
-- defined on page 54 of Calvet-Fisher.
gammaVector ::  Float -> Int -> Float -> [Float]
gammaVector gamma_kbar kbar b =
    [gamma1] ++ [gamma_n x | x <- [2..kbar]]
    where
      gamma1 = 1 - (1 - gamma_kbar) ** (b ** (1 - fromIntegral kbar))
      gamma_n n = 1 - (1 - gamma1) ** (b ** (fromIntegral n-1))


-- Creates new random markov state vector of size kbar
generateMarkovStateVector :: Float -> Int -> IO [Float]
generateMarkovStateVector m0 kbar = do
  gen <- newStdGen
  let rand =  take kbar $ randoms gen :: [Int]
  return [r (rand !! x) | x <- [0..kbar-1]]
    where m1 = 2 - m0
          r :: Int -> Float
          r n
              | n `mod` 2 == 0 = m0
              | otherwise = m1

-- Takes MarkovStateVector, GammaVector, m0 and kbar.
-- Returns updated MakovStateVector.
updateMarkovStateVector :: [Float] -> [Float] -> Float -> Int -> IO [Float]
updateMarkovStateVector stateVector gammaVector m0 kbar = do
  gen <- newStdGen
  let rand = take kbar (randomRs (0,1) gen) :: [Float]
  gen2 <- newStdGen
  let ns = take 1 $ randoms gen2 :: [Bool]

  let result = [update x (rand !! x) | x <- [0..kbar-1]]
  return result
      where
        update n r
            | r < (gammaVector !! n) =
                -- For the sake of simplicity, unsafe IO will be perfomed.
                -- If this fails, something terribly wrong must have happened.
                if unsafePerformIO (do
                                     gen2 <- newStdGen
                                     let (x,y) =  random gen2 :: (Bool, StdGen)
                                     return x) == True then m1
                else m0
            | otherwise = stateVector !! n
        newRandomM ns
            | ns = m1
            | otherwise = m0
        m1 = 2 - m0
