module Main( main ) where

import System( getArgs )

import Ultra
import Converters
import Races
import PredictionExts

import Data.Maybe



usage :: Either [Char] b
usage = Left "Usage: hh:mm:ss.ii distance or hh:mm:ss.ii distance resthr maxhr weight [kouros]"

predictOrDie :: (Double->Double->Predictor) -> [String] -> Either [Char] [[(String, String)]]
predictOrDie  preFun [timeString , distanceString] = 
  case ((toTime timeString), (readMaybePositiveDouble distanceString)) of 
    (Nothing, _) -> usage
    (_, Nothing) -> usage
    (Just t, Just d) -> Right $ predictions $ preFun t d

predictOrDie preFunc [ts, ds, rhs, mhs,weight, type'] = 
  if type' == "kouros" then
      predictOrDie getKourosPredictor [ts, ds, rhs, mhs]
    else
      usage

predictOrDie preFun [timeString , distanceString, restHrString, maxHrString,weightString] = 
  let params    = [ toTime timeString ] 
                  ++ 
                  map readMaybePositiveDouble 
                  [distanceString, restHrString, maxHrString,weightString] 
      [t, d, r, h,w] = map fromJust params in
  if (all isJust params) then  
    Right $ predictionsExts (preFun t d) (extensions r h w)
  else
    usage


predictOrDie _ _ = usage

main :: IO ()
main = do
  args <- getArgs
  case (predictOrDie getPredictor args) of  
    Left s -> putStrLn s 
    Right p -> putStrLn $ show $ p
