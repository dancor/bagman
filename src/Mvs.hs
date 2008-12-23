module Mvs where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import FUtil
import Text.ParserCombinators.Parsec hiding (oneOf)
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import qualified Data.Set as S

type Roll = (Int, Int)
type Mv = MS.MultiSet (Int, [(Int, Bool)])
-- "cube-mvs" is the struct of different optimal moves depending on the cube
type CubeMvs = M.Map Char Mv
data RollMvEry = RollMvEry {
  rmMv :: Mv,
  rmRoll :: Roll,
  rmCubeMvs :: CubeMvs
  }

mvPts :: Mv -> MS.MultiSet [Int]
mvPts = MS.map (\ (i0, iRest) -> i0:map fst iRest)

parseInt :: GenParser Char () Int
parseInt = read <$> many1 digit

parseRoll :: GenParser Char () Roll
parseRoll = do
  r1 <- parseInt
  char ' '
  r2 <- parseInt
  return (r1, r2)

parseMv :: GenParser Char () Mv
parseMv = do
  let
    singMv :: GenParser Char () ((Int, [(Int, Bool)]), Int)
    singMv = do
      i1 <- parseInt
      iRest <- many1 $ do
        char '/'
        i <- parseInt
        isHitMb <- optionMaybe $ char '*'
        let
          isHit = case isHitMb of
            Nothing -> False
            Just _ -> True
        return (i, isHit)
      count <-
        do
          char '('
          c <- parseInt
          char ')'
          return c
        <|>
        return 1
      return ((i1, iRest), count)
  mv1 <- singMv
  mvRest <- many $ try (char ' ' >> singMv)
  return . MS.fromOccurList $ mv1:mvRest

parseLine :: GenParser Char () (Maybe RollMvEry)
parseLine =
  do
    char '#'
    manyTill anyToken newline
    return Nothing
  <|>
  do
    newline
    return Nothing
  <|>
  do
    mv <- parseMv
    string "  "
    roll <- parseRoll
    char '|'
    mvMain <- parseMv
    mvsRest <- many $ do
      string ", "
      mv <- parseMv
      char ' '
      cs <- many1 $ oneOf "dgs"
      return (cs, mv)
    let
      keysSameVal ks v = M.fromList $ map (\ k -> (k, v)) ks
      mvMap = M.unions . map (uncurry keysSameVal) $
        mvsRest ++ [("cdgs", mvMain)]
    return . Just $ RollMvEry {
      rmMv = mv,
      rmRoll = roll,
      rmCubeMvs = mvMap
      }

parseFile :: GenParser Char () [RollMvEry]
parseFile = catMaybes <$> manyTill parseLine eof

showMv :: Mv -> String
showMv = intercalate " " . map showMvPart . MS.toOccurList where
  showMvPart (j, 1) = showJump j
  showMvPart (j, n) = showJump j ++ "(" ++ show n ++ ")"
  showJump (i0, iRest) = intercalate "/" $ [show i0] ++
    map (\ (i, isHit) -> show i ++ if isHit then "*" else "") iRest

showCubeMvs :: CubeMvs -> String
showCubeMvs cubeMvs = intercalate ", " $ showMv mvMain:map showMvAnn mvsRest
  where
  cubeMvsFlip = flipMap cubeMvs
  Just mvMain = M.lookup 'c' cubeMvs
  mvsRest = sortBy (compare `on` snd) . M.toList $
    M.delete mvMain cubeMvsFlip
  -- note: we make use of coincidence that "cdgs" is in order
  showMvAnn (mv, ann) = showMv mv ++ " " ++ S.toList ann

readRollMvs :: FilePath -> IO (Either ParseError [RollMvEry])
readRollMvs fileName = do
  c <- readFileStrict fileName
  return $ runParser parseFile () fileName c
