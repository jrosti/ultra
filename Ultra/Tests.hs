import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck

import Monad
import Data.Maybe
import Text.Printf

import PredictionExts
import Ultra 
import Converters
import Races

main :: IO ()
main = defaultMain tests

-- tests :: [t] ????
tests = [
  testGroup "Predictor" [
     testProperty "prop_identitytime" prop_identitytime,
     testProperty "prop_monotonic" prop_monotonic,
     testProperty "prop_slowing" prop_slowing,
     testProperty "prop_inverse" prop_inverse,
     testProperty "prop_vo2max" prop_vo2max,                
     testCase "test_11h100km" test_11h100km
     ],
  testGroup "Converters" [
    testCase "test_fromTimeSmall" test_fromTimeSmall,
    testCase "test_fromTimeLarge" test_fromTimeLarge,
    testCase "test_toTime" test_toTime,
    testProperty "prop_fromTimeLen" prop_fromTimeLen, 
    testProperty "prop_toTimeShort" prop_toTimeShort,
    testProperty "prop_predictionArbitraryTS" prop_predictionArbitraryTS
    ],
  testGroup "Races" [
    testProperty "prop_simplePredictorTest" prop_simplePredictorTest
    ],
  testGroup "Prediction Extensions" [
    testProperty "prop_karvonenKarvonenInv" prop_karvonenKarvonenInv,
    testProperty "prop_cooperIs100" prop_cooperIs100
    ]
  ]

epsilon :: Double
epsilon = 0.0001



-- Arbitrary types

-- "hh:mm:02"
newtype TimeString = TS { timeString :: String} deriving Show

instance Arbitrary TimeString where
  arbitrary = do hours <- (listOf1 $ elements ['0'..'9'])
                 -- intentional restriction to light lifts
                 minutes <- elements ['0'..'5'] >>= 
                            \x -> ((liftM (\y -> (x:y:[])) (elements ['0'..'9'])))
                 return (TS (hours ++ ":" ++ minutes++ ":02"))




-- General properties valid for all predictors

-- given time and distance, prediction by distance by time must return distance
prop_identitytime :: Double -> Double -> Property
prop_identitytime time distance = abstime > epsilon && 
                                  absdistance > epsilon  ==> 
                                  abs (distanceByTime (getPredictor 
                                                       abstime absdistance) 
                                       abstime - absdistance) < epsilon
  where
    abstime = abs time
    absdistance = abs distance
        
-- any increment in time should lead to larger predicted distance
prop_monotonic :: Double -> Double -> Double -> Property
prop_monotonic t' d' t_delta' = t > epsilon && 
                               d > epsilon && 
                               t_delta > epsilon ==>
                               (distanceByTime predictor $ (t + t_delta)) > 
                               d
  where 
    t = abs t'
    d = abs d'
    t_delta = abs t_delta'
    predictor = getPredictor t d

-- speed slows down when distance increases
prop_slowing :: Double -> Double -> Property
prop_slowing time distance = abstime > epsilon && absdistance > epsilon  ==>
                             (distanceByTime predictor $ (abstime * 2)) < 
                             2 * absdistance
  where
    abstime = abs time
    absdistance = abs distance
    predictor = getPredictor abstime absdistance

-- applying timeByDistance to d and then distanceByTime to result gives d
prop_inverse :: Double -> Double -> Double -> Property
prop_inverse t' d' d_p' = 
  t > epsilon && d > epsilon && d_p > epsilon ==>
   (distanceByTime predictor $ timeByDistance predictor $ d_p) - d_p < epsilon
  where
    t = abs t'
    d = abs d' 
    d_p = abs d_p'
    predictor = getPredictor t d


-- Test cases related to old Ultralaskuri compatibility

-- HM 1:44:22.3 corresponds 11h 100km time
test_11h100km :: Assertion
test_11h100km = assertBool "11-hour-from-hm" 
                (( (-) (distanceByTime predictor $ elevenhours) 
                  100000.0) < epsilon)
  where 
    hmtime = 60 * 60 + 44 * 60 + 22 + 0.3
    predictor = getPredictor hmtime 21090
    elevenhours = 11 * 60 * 60
    
-- Other properties

prop_vo2max :: Double -> Double -> Property
prop_vo2max t d = (abs t) > 0.0 && (abs d) > 0.0 ==>
              abs ((vo2maxspeed predictor) - 
              (distanceByTime predictor $ vo2maxtime) /
              vo2maxtime) < epsilon
  where
    predictor = getPredictor (abs t) (abs d)
    
    
-- Converters

test_fromTimeSmall :: Assertion
test_fromTimeSmall = assertEqual "fromSmallTime"
                     [Just "53:20.20", Just "10:00.00", 
                      Just "01:00.01", Just "02:45.00", Nothing,
                      Just "100:00:00"]
                     (map fromTime [3200.20, 600.0, 
                                    60.01, 164.99753086419753, -10,
                                    3600*100.0])

                

test_fromTimeLarge :: Assertion
test_fromTimeLarge = assertEqual "fromLargeTime"
                     (Just [ "01:53:20", "11:53:19"])
                     (mapM fromTime [3600 + 3200.20, 3600 * 11 + 3199.6 ])


prop_fromTimeLen :: Double -> Property
prop_fromTimeLen t' = t < 3600*99 ==>
                     (length $ fromJust $ fromTime t) == 8 
  where
    t = (abs t') + 1
                 
prop_predictionArbitraryTS :: TimeString -> Double -> Double -> Property
prop_predictionArbitraryTS arbts d' t' = d > epsilon && t > epsilon ==>
                                   ((distanceByTime predictor t) > 0.0)
  where tstr = timeString arbts
        d = abs d'
        t = abs t'
        predictor = getPredictor (fromJust $ toTime tstr) d
                   
                                  

test_toTime :: Assertion
test_toTime = assertEqual "toTime"
              ([Just 1.2, Just (12*3600.0), Nothing, Nothing])
              (map toTime ["0:00:1.2", "12:00", "asdf", "12:00aa"])


toTime' :: Int -> Int -> Double
toTime' h m = fromIntegral h * 3600.0 + fromIntegral m * 60.0

prop_toTimeShort :: Int -> Int -> Property
prop_toTimeShort h' m' = (m < 60 ) && (h > 0 || m > 0) ==>
                         abs  ((toTime' h m) - 
                               (fromJust $ toTime $ printf "%d:%02d" (h::Int) (m::Int))) 
                         < epsilon
  where
    h = abs h'
    m = abs m'
                         
-- Races

-- check that predictions returns some result
prop_simplePredictorTest :: Double -> Double -> Property
prop_simplePredictorTest t' d' = t > 0.0 && d > 0 ==> 
                                 length (predictions (getPredictor t d)) > 1
  where t = abs t'
        d = abs d'
        
-- Karvonen formula is inverse of KarvonenInv
prop_karvonenKarvonenInv :: Double -> Property
prop_karvonenKarvonenInv r = rate > epsilon ==>
                             abs ((karvonenInv hr (karvonen hr rate)) - rate) 
                             < epsilon

  where rest = 10  
        rate = (abs r) + rest + 1
        hr = extensions rest (rate+rest) 0

-- for any distance, cooper test effort should be max hr and 100%
prop_cooperIs100 :: Double -> Double -> Property
prop_cooperIs100 t' d' = t > epsilon && d > epsilon ==> 
                         ((predictionExts predictor exts vo2maxtime) ==
                         [("power", "100%"), ("hr", "192"), 
                          ("kcal",""), ("co2","")])
  where
    hrmax = 192 
    t = abs t'
    d = abs d' 
    predictor = getPredictor t d
    exts = extensions 42 hrmax 0
    
    
