module Interval (getIntervalTimePrediction) where

import Ultra

data Interval = Interval
				{ performancePredictor :: Predictor
				, repetitions :: Double
				, delay :: Double
				, legDistance :: Double 
				}

getIntervalTimePrediction :: Predictor -> Integer -> Double -> Double -> Double
getIntervalTimePrediction predictor repetitions delay legDistance = 
	getTime $ Interval predictor (fromIntegral repetitions) delay legDistance

-- private parts

getTime :: Interval -> Double
getTime interval = repetitions interval * legDistance interval * intervalPace interval

intervalPace :: Interval -> Double
intervalPace interval = 
	if (trueRecoveryFraction interval >= 1) then
		minPace
	else 
		maxPace - (maxPace - minPace) * trueRecoveryFraction interval
	where 
		maxPace = legPaceAtMaxEffort interval
		minPace = distanceCovered interval / timeWithoutRecovery interval


legPaceAtMaxEffort :: Interval -> Double
legPaceAtMaxEffort interval = 
		distance / (timeByDistance predictor distance)
	where 
		predictor = performancePredictor interval
		distance = legDistance interval
	
trueRecoveryFraction :: Interval -> Double
trueRecoveryFraction interval = 
	 timeWithoutRecovery interval /  ( delay interval * repetitions interval )

timeWithoutRecovery :: Interval -> Double
timeWithoutRecovery interval = 
	timeByDistance (performancePredictor interval) (distanceCovered interval)

distanceCovered :: Interval -> Double
distanceCovered interval = 
		repetitions interval * legDistance interval