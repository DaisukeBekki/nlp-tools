import qualified Data.Text as T    --text
import qualified Data.Text.IO as T --text

main :: IO()
main = do
  annFile <- T.readFile "tutorial/brat/test.ann"
  mapM_ T.putStrLn $ T.lines annFile
