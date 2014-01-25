module PredictionExts(extensions, Extensions, restHr, maxHr, 
                      predictionExts, karvonen, 
                      karvonenInv, fromPower) where



import Ultra

import Text.Printf

data Extensions = Extensions { restHr::Double, maxHr::Double, weight::Double } deriving Show


-- The famous Karvonen formula and its inverse. Any source does not
-- bother to mention a true source. Correspondence to VO2max is rather
-- good, and the biggest source of the error is the heat, and its 
-- increased blood circulation. The predictions are usually on the
-- lowest possible HR average on given distance. 
-- In Mongolian S2S I had very high average HR, which might
-- have been due to the high altitude of the course. 
karvonen ::  Extensions -> Double -> Double
karvonen hr rate 
  | rate > restHr hr = (rate - restHr hr) / (maxHr hr - restHr hr)
  | otherwise = 0.0
                     
karvonenInv :: Extensions -> Double -> Double
karvonenInv hr power =
  power * (maxHr hr - restHr hr) + restHr hr
                
predictionExts :: Predictor -> Extensions -> Double -> [(String, String)]
predictionExts predictor exts time = 
  predExts predictor exts time

relativePower :: Predictor -> Double -> Double 
relativePower p t = 
  (distanceByTime p t) / t / (vo2maxspeed p) 

fromPower :: Double -> String
fromPower power  
  | power > 0.0 = printf "%02d% %" ((round (100*power))::Int)
  | otherwise = ""
                  
extensions :: Double -> Double -> Double -> Extensions
extensions r m w
-- Oh, well. After two beers. Consider refactoring.
  | m > r && r > 0 && w <= 0  = Extensions r m 0
  | m > r && r > 0 && w > 0  = Extensions r m w
  | w > 0  = Extensions 0 0 w                               
  | otherwise = Extensions 0 0 0 


fromExtensions :: Double -> String
fromExtensions rate 
  | rate > 0 = printf "%d" ((round rate)::Int)
  | otherwise = ""


-- Typical variation of O2 consumption is around 190-210ml/kg/km 
-- for the recreational athletes and 180 - 220 ml/kg/km covers already 
-- most elites and runners without any skill. 
-- kcalkgkm == 1 correspond approximately 200. Did not bother to check
-- though.
kcal :: Double -> Double -> String
kcal w d  
  | w > 0 = printf "%d" ((round (kcalkgkm * (d/1000) * w))::Int)
  | otherwise = ""
  where kcalkgkm = 1

-- Also known as respiratory exchange ratio. 
-- Assuming linear relationship, so that 50% power corresponds 100% fat and
-- And 85% (anaerobic threshold) corresponds 100% from carbs. Using 0.7 and 1.0 
-- as ratios on these ends, and the linear between. 
-- The relation is not really linear, and the biggest error is in the
-- low power regime.
co2o2ratio rp 
  | rp >= 0.85 = 1.0
  | rp < 0.5 = 0.7
  | otherwise = 0.7 + 0.3/0.35*(rp-0.5)

-- Sources for the error are the running economy (litersperminutperkg) and
-- RER (co2o2ratio). The magnitude is correct, but +/- 10% error is probably
-- true. 
co2 :: Double -> Double -> Double -> String
co2 rp d w  
  | rp > 0 && w > 0 = printf "%dg" ((round (w * (d/1000) * 
                                            litersperminuteperkg 
                                            / lpmol * co2molweight *
                                            (co2o2ratio rp)))::Int)
  | otherwise = ""
      where litersperminuteperkg = 0.2 -- oxygen consumption
            lpmol = 22.04 -- L/mol 
            co2molweight = 44.01 -- molar mass g/mol
  

predExts :: Predictor -> Extensions -> Double -> [(String, String)]
predExts predictor exts time = 
  [ ("power", fromPower rp)
  , ("hr"   , fromExtensions (karvonenInv exts rp))
  , ("kcal" , kcal w d)
  , ("co2"  , co2 rp d w)
  ]
  where rp = relativePower predictor time
        d = distanceByTime predictor time
        w = weight exts
