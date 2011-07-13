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
timeByDistance' time distance p targetdistance = time * (targetdistance/distance)**p

---- 

pchooser' :: Parameters -> Double -> Double
pchooser' p time
  | time < vo2maxtime =  (pvalueMiddle p)
  | time < ultracotime = (pvalueNormal p)
  | otherwise =          (pvalueUltra  p)

vo2maxdistance' :: Parameters -> Double -> Double -> Double
vo2maxdistance' p time distance
  | time < ultracotime = f (pchooser' p  time)
  | otherwise = distanceByTime' ultracotime 
                (ultracodistance' p time distance) (pvalueNormal p) vo2maxtime
  where 
    f x = distanceByTime' time distance x vo2maxtime
 
-- Calculates the distance reached at the ultra cross-over
ultracodistance' ::Parameters ->  Double -> Double -> Double
ultracodistance' p time distance 
  | time > vo2maxtime = f (pchooser' p time)
  | otherwise = distanceByTime' vo2maxtime (vo2maxdistance' p time distance) (pvalueNormal p) ultracotime
  where 
    f x = distanceByTime' time distance x ultracotime


-- thing making distance predictions
getdistancepredictor' :: Parameters -> Double -> Double -> Double -> Double
getdistancepredictor' p time distance = 
  \t ->   if t > vo2maxtime then 
            distanceByTime'  ultracotime ucd (pchooser' p t) t
          else 
            distanceByTime'  vo2maxtime v2d (pchooser' p t) t
  where 
    ucd = ultracodistance' p time distance
    v2d = vo2maxdistance' p time distance 

gettimepredictor' :: Parameters -> Double -> Double -> Double -> Double
gettimepredictor' p time distance =
  \d ->    if d > ucd then
             timeByDistance' ultracotime ucd (pvalueUltra p) d
           else 
             if d > v2d then 
               timeByDistance' ultracotime ucd (pvalueNormal p) d
             else
               timeByDistance' vo2maxtime v2d (pvalueMiddle p) d
  where 
    ucd = ultracodistance' p time distance
    v2d = vo2maxdistance' p time distance

normalParams = Parameters 1.12 1.07 1.3

getPredictor :: Double -> Double -> Predictor
getPredictor time distance =
    let parameters = normalParams in
    getPredictorWithP parameters time distance

data Type = Normal | Kouros

getTypedPredictor :: Type -> Double -> Double -> Predictor
getTypedPredictor type' time distance =
  case type' of
    Normal -> getPredictorWithP (normalParams) time distance
    Kouros -> getPredictorWithP (Parameters 1.12 1.07 1.2) time distance

getPredictorWithP :: Parameters -> Double -> Double -> Predictor
getPredictorWithP parameters time distance
  | time > 0.0 && distance > 0.0 = 
    Predictor 
    (getdistancepredictor' parameters time distance) 
    (gettimepredictor' parameters time distance)
    (vo2maxdistance' parameters time distance / vo2maxtime)
  | otherwise =  error "Nonpositive parameters"    

getKourosPredictor = getTypedPredictor Kouros