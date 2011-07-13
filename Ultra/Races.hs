module Races (predictions, predictionsExts) where

import Ultra
import Converters
import Data.Maybe
import PredictionExts
import Interval

import Text.Printf

data Race = 
  Normal    { name :: String 
            , distance :: Double } |

  TimeBased { name :: String 
            , time :: Double     } |

  Trail     { name :: String
            , distance :: Double
            , hardness :: Double } |

  Interval { name :: String
				, reps :: Integer
				, delay :: Double
				, legDistance :: Double	
				}

oneHour :: Double
oneHour = 3600.0

races :: [Race]            
races = 
   [ Normal    "800m"         800
   , TimeBased "6h rata"      (6 *  oneHour)
   , TimeBased "12h rata"     (12 * oneHour)
   , TimeBased "24h rata"     (24 * oneHour)
   , TimeBased "48h rata"     (48 * oneHour)

   , Normal    "100km"        100000
   , Normal    "100 mailia"   161000
   , Normal    "200km"        200000

   , Trail     "Vaajakosken maastoultra"     60000 66000
   , Trail     "Vaarojen ultra"              84000 105588
   , Trail     "Mongolian Sunrise to Sunset" 100000 125100
   , Trail     "Spartathlon"                 246000 280000

   , Interval "Ultraintervalli, 8x13km / 3h " 8 (3 * oneHour) 13000
   , Interval "Tonnit, 5x1km / 6min" 8 (6 * 60.0) 1000

   ]
  

getRaceTime :: Predictor -> Race -> Double
getRaceTime predictor race = case race of 
  Normal _ distance    -> timeByDistance predictor distance
  Trail _ _ hardness   -> timeByDistance predictor hardness
  TimeBased _ time -> time
  Interval _ repetitions delay legDistance -> getIntervalTimePrediction predictor repetitions delay legDistance

getRaceDistance :: Predictor -> Race -> Double
getRaceDistance predictor race = case race of
  Normal _ distance ->  distance
  Trail _ distance _ -> distance
  TimeBased _ time -> distanceByTime predictor time
  Interval _ repetitions _ distance -> (fromIntegral repetitions) * distance

-- TODO: Floats behave slightly different than in conventional langs?
formatSpeed :: Double -> Double -> String
formatSpeed time distance = 
  let timeticks = 0.001 + (fromIntegral $ round time)::Double 
      minutes = timeticks/60.0
      kilometers = distance/1000
      pace = minutes / kilometers
      paceminutes = (fromIntegral $ (floor pace))::Double
      paceseconds = ((pace - paceminutes) * 60.0)::Double 
  in
  printf "%02.f:%02.fmin/km" paceminutes paceseconds

getPrediction :: Predictor -> Extensions -> Race -> [(String, String)]
getPrediction predictor pexts race = 
  let raceTime = getRaceTime predictor 
      raceDistance = getRaceDistance predictor in
  [ ("name", name race)
  , ("time", fromJust $ fromTime  $ raceTime race)
  , ("distance", fromDistance $ raceDistance race)
  , ("speed", formatSpeed (raceTime race) (raceDistance race))
  ] 
  ++ 
  predictionExts predictor pexts (raceTime race)
   
   
predictions :: Predictor -> [[(String, String)]]
predictions predictor = map (getPrediction predictor (extensions 0.0 0.0 0.0)) races                               
predictionsExts :: Predictor -> Extensions -> [[(String, String)]]
predictionsExts predictor pexts = map (getPrediction predictor pexts) races