import System.FilePath ((</>)) --filepath
import Text.Brat (prepareBratData) --nlp-tools

main :: IO()
main = do
  let bratDataDir = "tutorial/brat"
      saveFilePath = bratDataDir </> "test.yaml"
  prepareBratData True bratDataDir saveFilePath 
