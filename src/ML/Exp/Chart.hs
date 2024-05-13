module ML.Exp.Chart (
  drawLearningCurve,
  drawConfusionMatrix
  ) where

--import Data.List (transpose)    --base
import Graphics.Gnuplot.Simple  --gnuplot
import Graphics.Matplotlib
import Data.List (nub)     --base


type Name = String
type Loss = Float
type LearningChart = (Name,[Loss]) --Name of an experiment and list of loss values

-- | LearningChart
-- |   Create a PNG file of learning charts
drawLearningCurve ::
  FilePath           -- ^ The filepath for the PNG file
  -> String          -- ^ The caption of the PNG file
  -> [LearningChart] -- ^ The list of data
  -> IO()
drawLearningCurve filepath title learningcharts = do
  let maxepoch   = fromIntegral $ maximum $ for learningcharts (length . snd)
      styleddata = for learningcharts formatLearningChart -- | [(PlotStyle, [(Double,Double)])]
      graphstyle = [(PNG filepath),(Title title),(XLabel "Epoch"),(YLabel "Loss"),(XRange (0,maxepoch::Double))]
  plotPathsStyle graphstyle styleddata
  where formatLearningChart (name,losses) =
          let name' = PlotStyle LinesPoints (CustomStyle [LineTitle name])
              losses' = for losses $ \loss -> (realToFrac loss)::Double
              epochs' = for [(0::Int)..] $ \i -> (fromIntegral i)::Double
          in (name', zip epochs' losses')
        for :: [a] -> (a -> b) -> [b]
        for = flip map


pairListToList :: [(a,a)] -> [a]
pairListToList [] = []
pairListToList ((x, y):rest) = x:y:(pairListToList rest)

-- | ConfusionMatrix
-- |   Create a PNG file of confusion matrix
drawConfusionMatrix :: (Show label, Eq label) => 
  FilePath           -- ^ The filepath for the PNG file
  -> Int             -- ^ The number of classes
  -> [(label,label)] -- ^ The list of data (prediction, excpected)
  -> IO()
drawConfusionMatrix filePath labelLength results = do
  let labels = nub $ pairListToList results
      confMat = for labels $ \prediction -> concat [for labels $ \answer -> length $ filter (\(p,a) -> p==prediction && a==answer) results]
      valuesText = concat $ [[text ((x+0.5) :: Double) ((y+0.5) :: Double) (show v) @@ [o2 ("ha" :: String) ("center" :: String), o2 ("va" :: String) ("center" :: String), o2 ("fontsize" :: String) (8 :: Int)] | (v,x) <- zip l [0..]] | (l,y) <- zip confMat [0..]]
      labelsText = [
          text (-0.1 :: Double) ((y+0.5) :: Double) (show labelName) @@ [o2 ("ha" :: String) ("right" :: String), o2 ("va" :: String) ("center" :: String), o2 ("fontsize" :: String) (8 :: Int)] 
          % text ((y + 0.5) :: Double) (((fromIntegral $ length labels) + 0.1) :: Double) (show labelName) @@ [o2 ("ha" :: String) ("center" :: String), o2 ("va" :: String) ("top" :: String), o2 ("fontsize" :: String) (8 :: Int), o2 ("rotation" :: String) (90 :: Int)] 
          | (labelName,y) <- zip labels [0..]]
      numRows = length labels 
      newSizeX = round $ fromIntegral numRows / 2.0 * 1.3
      newSizeY = round $ fromIntegral numRows / 2.0
      graph = pcolor confMat @@ [o2 ("edgecolors" :: String) ("k" :: String), o2 ("linewidth" :: String) (1 :: Int)]
              % foldr (%) ( foldr (%) (
              title "Confusion Matrix"
              % setSizeInches (newSizeX :: Int) (newSizeY :: Int)
              % xlabel "Actual" 
              % ylabel "Expected" 
              % colorbar 
              % mp # ("ax.invert_yaxis()" :: String)) labelsText) valuesText
  _ <- file filePath graph
  return ()
  where for :: [a] -> (a -> b) -> [b]
        for = flip map
