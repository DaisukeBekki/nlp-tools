module Text.Distiller (
  collectText,
  canonicalize,
  cleanse,
  degree2color
  ) where

import Control.Monad (filterM)         --base
import System.FilePath ((</>),isExtensionOf)    --filepath
import qualified System.Directory as D --directory
import qualified Data.Text as T        --text
--import qualified Data.Text.IO as T   --text
import qualified Shelly as S           --shelly
--import NLP.Similarity.VectorSim as N --chatter
--import NLP.Types as N                --chatter

collectText :: FilePath -> IO([T.Text])
collectText dataFolder = do
  _ <- D.doesDirectoryExist dataFolder
  dirs <- filterM D.doesDirectoryExist =<< map (dataFolder </>) <$> D.listDirectory dataFolder
  texts <- filter (isExtensionOf "txt") <$> concat <$> mapM (\d -> map (\o -> dataFolder </> d </> o) <$> D.listDirectory d) dirs
  mapM readFileUtf8 texts

readFileUtf8 :: FilePath -> IO(T.Text)
readFileUtf8 filepath =
  S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.concat ["cat \"", T.pack filepath, "\" | nkf -w -Lu"]

canonicalize :: T.Text -> [T.Text]
canonicalize = convert . T.lines . cleanse

cleanse :: T.Text -> T.Text
cleanse text =
  case T.uncons text of
    Nothing -> T.empty
    Just (c,t) | T.any (==c) "。．" -> case T.uncons t of
                                        Nothing -> "。"
                                        Just (c',t') | c'=='」' -> T.cons '」' $ cleanse t'
                                                     | otherwise -> T.cons '。' $ cleanse t
               | T.any (==c) "◎○●▲△▼▽■□◆◇★☆※†‡" -> cleanse t
               | T.any (==c) "(" -> T.cons '（' $ cleanse t
               | T.any (==c) ")" -> T.cons '）' $ cleanse t
               | T.any (==c) ",，" -> T.cons '、' $ cleanse t
               | otherwise -> T.cons c $ cleanse t

convert :: [T.Text] -> [T.Text]
convert texts = filter (/= T.empty) $ convertLoop texts T.empty []

convertLoop :: [T.Text] -> T.Text -> [T.Text] -> [T.Text]
convertLoop [] buffer result = result ++ (T.splitOn "。" buffer)
convertLoop (h:ts) buffer result
  | (T.isPrefixOf "1." h || T.isPrefixOf "（1）" h) = convertLoop ts T.empty result
  | isDelimiter h = convertLoop ts T.empty (result ++ (T.splitOn "。" buffer))
  | otherwise = convertLoop ts (T.concat [buffer,h]) result

isDelimiter :: T.Text -> Bool
isDelimiter t = t=="2." || t=="3." || t=="4." || t=="5." || t=="6." || t=="7." || t=="8." || t=="9." || t=="10." || t=="11." || t=="12." || t=="13." || T.isPrefixOf "（2）" t || T.isPrefixOf "（3）" t || T.isPrefixOf "（4）" t || T.isPrefixOf "（5）" t || t=="1" || t=="2" || t=="3" || t=="4" || t=="5" || t=="6" || t=="7" || t=="8" || t=="9"

degree2color :: Double -> String
degree2color d
  | d <= 0.2 = "#fff4f4"
  | d <= 0.3 = "#ffeaea"
  | d <= 0.4 = "#ffd5d5"
  | d <= 0.5 = "#ffaaaa"
  | d <= 0.6  = "#ff8080"
  | d <= 0.7 = "#ff5555"
  | d <= 0.8 = "#ff2b2b"
  | otherwise = "#ff0000"
