{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module ML.Exp.Classification (
  ClassificationCounts,
  ClassificationReport(..),
  Mode(..),
  showClassificationReport, 
  showClassificationReport',
  showConfusionMatrix,
  average,
  variance,
  stdDev,
  main
  ) where

import Data.List (foldl', nub)     --base
import Text.Printf (printf)        --base
import qualified Data.Text as T    --text
import qualified Data.Text.IO as T --text

type ClassificationCounts = (Int,Int,Int,Int)

data ClassificationReport = ClassificationReport {
  title :: T.Text,
  precision :: Double,
  recall :: Double,
  f1 :: Double,
  support :: Int
  } deriving (Eq)

instance Show ClassificationReport where
  show ClassificationReport{..} =
    "precision=" ++ (show precision) ++ ", " ++
    "recall=" ++ (show recall) ++ ", " ++
    "f1=" ++ (show f1) ++ ", " ++
    "support=" ++ (show support)

-- | 表示する際のラベル名の表示文字数
labelLength :: Int
labelLength = 10

-- | ラベルの型はBounded (Prelude)であると仮定
-- 例：data Fruit = Orange | Apple | Grape deriving (Eq,Show,Bounded)
--     [minBound..maxBound::Fruit]で[Orange,Apple,Grape]を作れる

classificationCountsFor :: (Show label, Eq label) => label -> [(label,label)] -> ClassificationCounts
classificationCountsFor pivot =
  foldl' (\(tp,fp,fn,tn) (prediction,answer) ->
                               if | (prediction == pivot) && (answer == pivot) -> (tp+1,fp,fn,tn)
                                  | prediction == pivot -> (tp,fp+1,fn,tn)
                                  | answer == pivot -> (tp,fp,fn+1,tn)
                                  | otherwise -> (tp,fp,fn,tn+1)
         ) (0,0,0,0) 

count2report :: (T.Text, ClassificationCounts) -> ClassificationReport
count2report (title, counts@(tp,_,fn,_)) =
  let precision = calc_precision counts
      recall = calc_recall counts
      f1 = calc_f1 precision recall
      support = tp + fn
  in ClassificationReport{..}

overall :: [ClassificationCounts] -> ClassificationCounts
overall = foldl' (\(tp,fp,fn,tn) (tp',fp',fn',tn') -> (tp+tp',fp+fp',fn+fn',tn+tn')) (0,0,0,0) 
--  ClassificationCounts (sum $ map tp counts) (sum $ map fp counts) (sum $ map fn counts) (sum $ map tn counts) 

micro :: [ClassificationCounts] -> ClassificationReport
micro counts =
  let title = "micro"
      overall_counts = overall counts
      precision = calc_precision overall_counts
      recall    = calc_recall overall_counts
      f1        = calc_f1 precision recall
      support    = sum $ map (\(tp,_,fn,_) -> tp + fn) counts
   in ClassificationReport{..}

macro :: (Eq label) => [label] -> [ClassificationCounts] -> ClassificationReport
macro labels counts =
  let title = "macro"
      numOfLabels = length labels
      normalizer = (1::Double)/(fromIntegral numOfLabels)  
      precision = normalizer * (sum $ map calc_precision counts)
      recall    = normalizer * (sum $ map calc_recall counts)
      f1        = normalizer * (sum $ map (\c -> calc_f1 (calc_precision c) (calc_recall c)) counts)
--      f1        = calc_f1 precision recall  
      support    = sum $ map (\(tp,_,fn,_) -> tp + fn) counts
   in ClassificationReport{..}

weighted :: [ClassificationReport] -> ClassificationReport
weighted reports =
  let denominator = sum $ map support reports in
  if denominator == 0
    then ClassificationReport "w-avg" 1 0 0 denominator
    else
      let prec = (sum $ map (\r -> (fromIntegral $ support r) * (precision r)) reports)/(fromIntegral denominator)
          recc = (sum $ map (\r -> (fromIntegral $ support r) * (recall r)) reports)/(fromIntegral denominator)
          f_1 = (sum $ map (\r -> (fromIntegral $ support r) * (f1 r)) reports)/(fromIntegral denominator)
          supp = denominator
          in ClassificationReport "w-avg" prec recc f_1 supp

{-
overall_accuracy :: (Eq label) => [(label,label)] -> Float
overall_accuracy results =
  let denominator = length results
      tp = length $ filter (\(pre,ans) -> pre == ans) results in
  if denominator == 0
    then 0 :: Float
    else (fromIntegral tp)/(fromIntegral denominator)

average_accuracy :: (Eq label, Enum label, Bounded label) => [label] -> [(label,label)] -> Float
average_accuracy labels results =
  let numOfLabels = length labels
      normalizer = (fromIntegral 1)/(fromIntegral numOfLabels)
  in 0

calc_accuracy :: ClassificationCounts -> Float
calc_accuracy ClassificationCounts{..} =
  let denominator = tp+fp+fn+tn in
  if denominator == 0
     then 0 :: Float
     else (fromIntegral (tp+tn))/(fromIntegral denominator)
-}

calc_precision :: ClassificationCounts -> Double
calc_precision (tp,fp,_,_) = 
  let denominator = tp+fp in
  if denominator == 0
     then 1 :: Double
     else (fromIntegral tp)/(fromIntegral denominator)

calc_recall :: ClassificationCounts -> Double
calc_recall (tp,_,fn,_) = 
  let denominator = tp+fn in
  if denominator == 0
     then 0 :: Double
     else (fromIntegral tp)/(fromIntegral denominator)

calc_f1 :: Double -> Double -> Double
calc_f1 precision recall =
  let denominator = precision + recall in
  if denominator == 0
     then 0 :: Double
     else (2*precision*recall)/denominator

for :: [a] -> (a -> b) -> [b]
for = flip map

pairListToList :: [(a,a)] -> [a]
pairListToList [] = []
pairListToList ((x, y):rest) = x:y:(pairListToList rest)

data Mode = TEXT | TEX deriving (Eq, Show)

showClassificationReport :: (Show label, Eq label) => Int -> [(label,label)] -> T.Text
showClassificationReport = showClassificationReport' TEXT

delimOf :: Mode -> T.Text
delimOf TEXT = "|"
delimOf TEX = ""

tabOf :: Mode -> T.Text
tabOf TEXT = "\t"
tabOf TEX = " & "

crOf :: Mode -> T.Text
crOf TEXT = " \n"
crOf TEX = "  \\\\\n"

showConfusionMatrix :: (Show label, Eq label) => Int -> [(label,label)] -> T.Text
showConfusionMatrix = showConfusionMatrix' TEXT

showConfusionMatrix' :: (Show label, Eq label) => Mode -> Int -> [(label,label)] -> T.Text
showConfusionMatrix' mode labelLength results =
  let labels = nub $ pairListToList results
  in T.concat [
    case mode of
        TEXT -> ""
        TEX -> "\\begin{tabular}{lrrrr}\n",
    delimOf mode,
    tabOf mode,
    T.concat $ for labels $ \answer -> T.concat [
       delimOf mode,
       T.pack $ take labelLength $ show $ answer,
       tabOf mode
       ],
    delimOf mode,
    crOf mode,
    case mode of
      TEXT -> ""
      TEX -> "\\hline\n",
    T.concat $ for labels $ \prediction -> T.concat [
       delimOf mode,
       T.pack $ take labelLength $ show $ prediction,
       tabOf mode,
       T.concat $ for labels $ \answer -> T.concat [
           delimOf mode,
           T.pack $ show $ length $ filter (\(p,a) -> p==prediction && a==answer) results,
           tabOf mode
           ],
       delimOf mode,
       crOf mode
       ],
    case mode of
      TEXT -> ""
      TEX -> "\\end{tabular}\n"
    ]

showClassificationReport' :: (Show label, Eq label) => Mode -> Int -> [(label,label)] -> T.Text
showClassificationReport' mode labelLength results = 
  --let dat = zip predictions answers
  let labels = nub $ pairListToList results
      counts = map (flip classificationCountsFor results) labels  -- ::[ClassificationCounts]
      reports = map count2report $ zip (map (T.pack . show) labels) counts
  in T.concat [
    "Scores: \n",
    case mode of
      TEXT -> ""
      TEX -> "\\begin{tabular}{lrrrrr}\n",
    delimOf mode,
    tabDelim,
    "Prec ",
    tabDelim,
    "Rec ",
    tabDelim,
    "F1 ",
    tabDelim,
    "Supp ",
    tabDelim,
    crOf mode,
    case mode of
      TEXT -> ""
      TEX -> "\\hline\n",
    T.concat $ for reports $ formatReport mode, 
    case mode of
      TEXT -> "\n"
      TEX -> "\\hline\n",
    formatReport mode $ micro counts,
    formatReport mode $ macro labels counts,
    formatReport mode $ weighted reports,
    case mode of
      TEXT -> ""
      TEX -> "\\end{tabular}\n",
    "\n",
    "Confusion matrix: \n",
    showConfusionMatrix' mode labelLength results
    ]
  where
    tabDelim :: T.Text
    tabDelim = T.concat [tabOf mode, delimOf mode]
    formatReport :: Mode -> ClassificationReport -> T.Text
    formatReport mode repo = T.concat [
      delimOf mode,
      T.take labelLength $ title repo,
      tabDelim,
      T.intercalate tabDelim $ map (\action -> T.pack $ printf "%3.3f" $ action repo) [precision, recall, f1],
      tabDelim,
      T.pack $ show $ support repo,
      tabDelim,
      crOf mode
      ]

average :: (Real a) => [a] -> Double
average xs = (realToFrac $ sum xs) / fromIntegral (length xs)

variance :: (Real a) => [a] -> Double
variance xs = let avgxs = average xs in
              average $ for xs $ \x -> (realToFrac x - avgxs)^(2::Integer)

stdDev :: (Real a) => [a] -> Double
stdDev xs = sqrt $ variance xs

--test code

data Animal = Cat | Fish | Hen deriving (Eq, Show)

main :: IO()
main = T.putStrLn $ showClassificationReport' TEXT 3 [
         (Cat,Cat),(Cat,Cat),(Cat,Cat),(Cat,Cat),
         (Fish,Cat),
         (Hen,Cat),
         (Cat,Fish),(Cat,Fish),(Cat,Fish),(Cat,Fish),(Cat,Fish),(Cat,Fish),
         (Fish,Fish),(Fish,Fish),
         (Hen,Fish),(Hen,Fish),
         (Cat,Hen),(Cat,Hen),(Cat,Hen),
         (Hen,Hen),(Hen,Hen),(Hen,Hen),(Hen,Hen),(Hen,Hen),(Hen,Hen)
         ]
