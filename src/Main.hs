module Main where

import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import FUtil
import qualified Data.Map as M
import qualified Data.MultiSet as MS

data Color = Blk | Red deriving Eq
data BdPoint = Emp | Pts Color Int

instance Show Color where
  show Blk = "O"
  show Red = "#"

type Bd = Array Int BdPoint
type Mv = MS.MultiSet [Int]

bdStart :: Bd
bdStart = listArray (1, 24) (repeat Emp) // [
  (24, Pts Blk 2), (13, Pts Blk 5), ( 8, Pts Blk 3), ( 6, Pts Blk 5),
  ( 1, Pts Red 2), (12, Pts Red 5), (17, Pts Red 3), (19, Pts Red 5)
  ]

showBd :: Bd -> String
showBd bd = intercalate "\n" . map (\ (a, b) -> a ++ " |" ++ b) $
  [bothond (intercalate " " . map (padl ' ' 2 . show . fst)) tops] ++
  [(bothond ((" " ++) . intercalate "  " . map (pip i . snd)) tops) |
    i <- [0..4]] ++
  [join (,) $ replicate (6 * 3 - 1) ' '] ++
  [(bothond ((" " ++) . intercalate "  " . map (pip i . snd)) btms) |
    i <- reverse [0..4]] ++
  [bothond (intercalate " " . map (padl ' ' 2 . show . fst)) btms]
  where
  pip 0 Emp = "."
  pip _ Emp = " "
  pip 0 (Pts c j) = if j > 5 then show j else show c
  pip i (Pts c j) = if j > i then show c else " "
  (btm, top) = first reverse . splitAt 12 $ assocs bd
  tops = splitAt 6 top
  btms = splitAt 6 btm

-- readMv "24/23 13/11" == MS.fromList [[24, 23], [13, 11]]
-- readMv "24/20(2) 13/11" == MS.fromOccurList [([24, 20], 2), ([13, 11], 1)]
-- readMv "6/2/1" == MS.fromList [[6, 2, 1]]
-- readMv "6/2/1(2)" == MS.fromOccurList [([6, 2, 1], 2)]
-- *'s (for hits) just ignored currently
-- todo: error check for moves not at least size 2?
-- todo: where check move against roll (also only-can-move-one rule etc)
readMv :: String -> Mv
readMv s = MS.fromOccurList parts where
  parts = map (first (map read . breaks (== '/')) . getRep) . breaks (== ' ') $
    filter (/= '*') s
  getRep s = case breakMb (== '(') s of
    Nothing -> (s, 1)
    Just (sing, numParen) -> (sing, read $ init numParen)

-- todo: where will we error check correct-side-moving-correct-dir
-- todo: this doesn't check intermediate hops etc either
-- use Either String instead of Maybe?
doMv :: Color -> Mv -> Bd -> Maybe Bd
doMv col mv bd =
  foldM (\ b pts -> decr (head pts) =<< incr (last pts) b) bd $ MS.toList mv
  where
  decr i bd = case bd ! i of
    Pts c n -> if c == col then Just $ bd // [(i, Pts c $ n - 1)] else Nothing
    _ -> Nothing
  incr i bd = case bd ! i of
    Pts c n -> if c == col then Just $ bd // [(i, Pts c $ n + 1)] else Nothing
    Emp -> Just $ bd // [(i, Pts col 1)]

readMvs :: String -> IO [((Int, Int), Mv)]
readMvs fileName = do
  c <- readFileStrict fileName
  let
    ls =
      filter (\ l -> not (null $ filter (not . isSpace) l) && head l /= '#') $
      lines c
  -- just takes first move currently
  return $ map (first (bothond read . fromJust . breakMb (== ' ')) .
    second (readMv . takeWhile (/= ',') . drop 1) . span (/= '|')) ls

main :: IO ()
main = do
  mvs <- readMvs "open.mem"
  --mapM_ print mvs
  mapM_ (putStrLn . showBd . fromJust . (\ mv -> doMv Blk mv bdStart) . snd)
    mvs
