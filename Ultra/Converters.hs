module Converters(fromTime, toTime, fromDistance, 
                  readDouble, readMaybePositiveDouble,
                  readEmptyOrPositive) where

import Text.Printf

import Monad
import Data.Maybe

import Text.ParserCombinators.Parsec

readMaybeConditionDouble :: (Double->Bool) -> String -> Maybe Double
readMaybeConditionDouble cnd s = ((fmap fst . listToMaybe . reads) s)  
                            >>= \x -> if cnd x then 
                                        return x
                                      else
                                        Nothing

readMaybePositiveDouble :: String -> Maybe Double
readMaybePositiveDouble = readMaybeConditionDouble (>0)

readEmptyOrPositive :: String -> Maybe Double
readEmptyOrPositive s = if s == "" then 
                          Just 0.0
                        else 
                          readMaybeConditionDouble (>0) s

readDouble :: String -> Double
readDouble = read

separator :: Parser Char
separator = oneOf ":"

separator2 :: Parser Char
separator2 = oneOf "."

convertHundreds :: Double -> Double
convertHundreds d = d / (head $ filter (>d) $ map (\x->10.0**x) [1..])


stringtime :: Parser Double
stringtime = do hours <- many1 digit
                _ <- separator
                minutes <- many1 digit
                seconds <- option "0" (separator >> many1 digit)
                hundreds <- option "0" (separator2 >> many1 digit)
                eof
                return $ ((readDouble hours)*3600 + 
                          (readDouble minutes)*60 
                          + readDouble seconds +
                          convertHundreds (readDouble hundreds)) 

toTime :: String -> Maybe Double
toTime input = case parse (stringtime) "time" input of
  Left _ -> Nothing
  Right val -> Just val

floor' :: Double -> Int
floor' = floor 

fromTime :: Double -> Maybe String
fromTime t' 
  | t <= 0.0 = Nothing
  | t < 60.0 * 60.0 = Just $ printf "%02d:%02d.%02d" 
                      (minutes::Int)
                      (seconds::Int)
                      ((round (100 * (t - fromIntegral (floor' t))))::Int)
  | otherwise = Just $ printf "%02d:%02d:%02d" 
                (hours::Int)
                ((minutes -  hours * 60)::Int)
                (seconds::Int)
  where
    t = (fromIntegral $ floor' (t' * 100.0 + 0.5)) / 100.0
    hours = floor' $ t / 3600.0
    minutes = floor' $ t / 60.0
    seconds = mod (floor' t) 60
    
fromDistance :: Double -> String
fromDistance x = printf "%.3fkm" (x/1000.0::Double)