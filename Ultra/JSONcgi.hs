module Main where

import Data.Maybe (fromJust)
import Text.JSON
import Network.CGI

import Races
import Ultra
import PredictionExts
import Converters
import Data.Maybe

errorResponse :: [JSObject String]
errorResponse = [toJSObject [("error", "Virheellinen sy&ouml;te")]]

result ::  (Double, Double, Double, Double,Double,String) -> [JSObject String]
result (t, d, hrmin, hrmax, weight, predictorType) = 
  let exts = extensions hrmin hrmax weight in
  case  predictorType of
    "kouros" -> ptime ++ (map toJSObject $ predictionsExts 
                          (getKourosPredictor t d) exts)
    "normal" -> ptime ++ (map toJSObject $ predictionsExts 
                          (getPredictor t d) exts)
    _ -> errorResponse
  where
    ptime = [(toJSObject [("time", fromJust $ fromTime t) 
                         , ("distance", fromDistance d)
                         ])::(JSObject String)
            ]
    
    
formValues :: [String] -> [JSObject String]
formValues  [timeString , distanceString, restHrString, maxHrString, weightString, ptype] = 
    let params    = [ toTime timeString ] 
                  ++ 
                  [readMaybePositiveDouble distanceString] 
                  ++ 
                  map readEmptyOrPositive
                  [ restHrString
                  , maxHrString
                  , weightString
                  ]
        [t, d, r, h,w] = map fromJust params in
    if (all isJust params) then
      result  (t,d,r,h,w,ptype)
    else
      errorResponse

formValues _ = errorResponse

getAndValidate :: String -> [JSObject String]
getAndValidate jsonInput = 
  case ((decode jsonInput) :: Result (JSObject String)) of
    Ok jsobj -> formValues $ map snd ((fromJSObject jsobj)::[(String, String)])
    Error s -> [toJSObject [("error"
                           ,  "JSON Parse error: <JSON>" 
                              ++ 
                              jsonInput 
                              ++ 
                              "</JSON> Error message:" ++ s)
                           ]
               ] 

jsonPrediction :: String -> String
jsonPrediction jsonInput = encode (getAndValidate jsonInput)

jsonEcho :: CGI CGIResult
jsonEcho = do inputs <- getInputs
              logCGI (jsonPrediction (fst $ head inputs))
              setHeader "Content-type" "application/x-javascript"
              output $ jsonPrediction (fst $ head inputs)

main = runCGI jsonEcho
