module Ultra (Predictor,getPredictor, getKourosPredictor, distanceByTime, 
                        vo2maxspeed, timeByDistance, 
              -- used in tests          
              vo2maxtime) where

data Predictor = Predictor {    distanceByTime :: Double -> Double
                           ,    timeByDistance :: Double -> Double
                           ,    vo2maxspeed :: Double
                           }
                                
data Parameters = Parameters {   pvalueMiddle :: Double
                             ,   pvalueNormal :: Double
                             ,   pvalueUltra :: Double
                             }

data PredictorType = Normal | Kouros

normalParams = Parameters 1.12 1.07 1.3
kourosParams = Parameters 1.12 1.07 1.2


-- middle-to-normal distance cross-over time
-- [s]
vo2maxtime :: Double
vo2maxtime = 12*60.0

-- normal to ultra cross-over
-- [s]
ultracotime :: Double
ultracotime = 4*60.0*60.0      

--- basic set of transformation functions

-- [s] -> [m] -> [] -> [s] -> [m]
distanceByTime' :: Double -> Double -> Double -> Double -> Double
distanceByTime' time distance pvalue targettime = 
  	distance*(targettime/time)**(1/pvalue)

timeByDistance' :: Double -> Double -> Double -> Double -> Double
timeByDistance' time distance pvalue targetdistance = 
	time * (targetdistance/distance)**pvalue

---- 

pchooser' :: Parameters -> Double -> Double
pchooser' pvalue time
  | time < vo2maxtime =  (pvalueMiddle pvalue)
  | time < ultracotime = (pvalueNormal pvalue)
  | otherwise =          (pvalueUltra  pvalue)

vo2maxdistance' :: Parameters -> Double -> Double -> Double
vo2maxdistance' pvalue time distance
  | time < ultracotime = f (pchooser' pvalue  time)
  | otherwise = distanceByTime' ultracotime 
                (ultracodistance' pvalue time distance) (pvalueNormal pvalue) vo2maxtime
  where 
    f x = distanceByTime' time distance x vo2maxtime
 
-- Calculates the distance reached at the ultra cross-over
ultracodistance' ::Parameters ->  Double -> Double -> Double
ultracodistance' pvalue time distance 
  | time > vo2maxtime = f (pchooser' pvalue time)
  | otherwise = distanceByTime' vo2maxtime (vo2maxdistance' pvalue time distance) (pvalueNormal pvalue) ultracotime
  where 
    f x = distanceByTime' time distance x ultracotime


-- thing making distance predictions
getDistancePredictor' :: Parameters -> Double -> Double -> Double -> Double
getDistancePredictor' pvalue time distance = 
  \t ->   if t > vo2maxtime then 
            distanceByTime'  ultracotime ultraCrossoverDistance (pchooser' pvalue t) t
          else 
            distanceByTime'  vo2maxtime v2d (pchooser' pvalue t) t
  where 
    ultraCrossoverDistance = ultracodistance' pvalue time distance
    v2d = vo2maxdistance' pvalue time distance 

getTimePredictor' :: Parameters -> Double -> Double -> Double -> Double
getTimePredictor' pvalue time distance =
  \d ->    if d > ultraCrossoverDistance then
             timeByDistance' ultracotime ultraCrossoverDistance (pvalueUltra pvalue) d
           else 
             if d > v2d then 
               timeByDistance' ultracotime ultraCrossoverDistance (pvalueNormal pvalue) d
             else
               timeByDistance' vo2maxtime v2d (pvalueMiddle pvalue) d
  where 
    ultraCrossoverDistance = ultracodistance' pvalue time distance
    v2d = vo2maxdistance' pvalue time distance

getPredictor :: Double -> Double -> Predictor
getPredictor time distance =
    let parameters = normalParams in
    getPredictorWithParameters parameters time distance

getTypedPredictor :: PredictorType -> Double -> Double -> Predictor
getTypedPredictor type' time distance =
  case type' of
    Normal -> getPredictorWithParameters normalParams time distance
    Kouros -> getPredictorWithParameters kourosParams time distance

getPredictorWithParameters :: Parameters -> Double -> Double -> Predictor
getPredictorWithParameters parameters time distance
  | time > 0.0 && distance > 0.0 = 
    Predictor 
    (getDistancePredictor' parameters time distance) 
    (getTimePredictor' parameters time distance)
    (vo2maxdistance' parameters time distance / vo2maxtime)
  | otherwise =  error "Nonpositive parameters"    

getKourosPredictor = getTypedPredictor Kouros