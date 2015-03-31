import qualified Msm as Msm

main = do
  simulatedReturns <-Msm.simulateReturns 0.44 10 1 1 0.2
  simulatedVolatility <-Msm.simulateReturns 0.44 10 1 1 0.2

  print $ take 20 $ simulatedReturns

  print $ take 20 $ simulatedVolatility
        
  return ()
