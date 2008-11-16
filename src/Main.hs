module Main where

import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import FUtil
import Mvs
import System.IO
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import qualified Data.Set as S

data Color = Blk | Red deriving (Eq, Ix, Ord)
data BdPoint = Emp | Pts Color Int deriving Eq

instance Show Color where
  show Blk = "O"
  show Red = "#"

type Bd = (Array Int BdPoint, Array Color Int)

bdStart :: Bd
bdStart = (listArray (1, 24) (repeat Emp) // [
  (24, Pts Blk 2), (13, Pts Blk 5), ( 8, Pts Blk 3), ( 6, Pts Blk 5),
  ( 1, Pts Red 2), (12, Pts Red 5), (17, Pts Red 3), (19, Pts Red 5)
  ], listArray (Blk, Red) $ repeat 0)

showBd :: Bd -> String
-- show bar better?
showBd (bd, bar) = mainBd ++ "\n" ++ show (assocs bar)
  where
  mainBd = intercalate "\n" . zipWith ($)
    (repeat $ (\ (a, b) -> a ++ " |" ++ b)) $
    [bothond (intercalate " " . map (padl ' ' 2 . show . fst)) tops] ++
    [(bothond ((" " ++) . intercalate "  " . map (pip i . snd)) tops) |
      i <- [0..4]] ++
    [join (,) $ replicate (6 * 3 - 1) ' '] ++
    [(bothond ((" " ++) . intercalate "  " . map (pip i . snd)) btms) |
      i <- reverse [0..4]] ++
    [bothond (intercalate " " . map (padl ' ' 2 . show . fst)) btms]
  pip 0 Emp = "."
  pip _ Emp = " "
  pip 0 (Pts c j) = if j > 5 then show j else show c
  pip i (Pts c j) = if j > i then show c else " "
  (btm, top) = first reverse . splitAt 12 $ assocs bd
  tops = splitAt 6 top
  btms = splitAt 6 btm

-- right now, just hits performed in the move
type MvExtraInfo = S.Set Int

-- todo: where will we error check correct-side-moving-correct-dir
-- todo: this doesn't check intermediate hops
doMv :: Color -> Mv -> Bd -> Either String (Bd, MvExtraInfo)
doMv col mv bdBar =
  foldM (\ b pts -> decr (head pts) =<< incr (last pts) b) (bdBar, S.empty) .
    MS.toList $ mvPts mv
  where
  decr i ((bd, bar), extraInfo) = case bd ! i of
    Pts c n -> if c == col
      then return ((bd // [(i, if n == 1 then Emp else Pts c $ n - 1)], bar),
        extraInfo)
      else fail $ "Trying to move " ++ show col ++ " piece from point " ++
        show i ++ ", but that point has " ++ show n ++ " " ++ show c ++
        " pieces."
    Emp -> fail $ "Trying to move " ++ show col ++ " piece from point " ++
        show i ++ ", but that point has no pieces."
  incr i ((bd, bar), extraInfo) = case bd ! i of
    Pts c n -> if c == col
      then return ((bd // [(i, Pts c $ n + 1)], bar), extraInfo)
      else if n == 1
        then return ((bd // [(i, Pts col 1)], bar // [(c, bar ! c + 1)]),
          S.insert i extraInfo)
        else fail $ "Trying to move " ++ show col ++ " piece to point " ++
          show i ++ ", but that point has " ++ show n ++ " " ++ show c ++
          " pieces."
    Emp -> return ((bd // [(i, Pts col 1)], bar), extraInfo)

revMv :: Mv -> Mv
revMv = MS.fromOccurList .
  map (first (\ (i0, iRest) -> (25 - i0, map (first (25 -)) iRest))) .
  MS.toOccurList

{-
processMv :: ((Mv, Roll), Mv) -> IO ()
processMv ((mv, roll), reply) = do
  let Right (bd, _) = doMv Blk mv bdStart
  putStrLn $ showBd bd
  print roll
  guessBestRepl bd roll
  --print $ guessBestRepl bd roll
  -}

-- coming in is not implemented
-- bearing off is not implemented
allDieMvs :: Bd -> Color -> Int -> [(Int, Int)]
allDieMvs (bd, _) color r = catMaybes $ map tryStart [1..24] where
  tryStart i = case bd ! i of
    Pts c n -> if c == color && i' >= 1 && i' <= 24
      then case bd ! i' of
        Pts c n -> if c == color || n == 1
          then Just (i, i')
          else Nothing
        Emp -> Just (i, i')
      else Nothing where
      i' = if color == Blk then i - r else i + r
    Emp -> Nothing

-- Takes a state and inputs, and a way of generating different ways to use an
--    input on a state (with an input-use signature), and a way to convert an
--    input-use signature into a state-modifier.
-- Returns all the possible ways of applying all the inputs in order to the
--    initial state.  So the result is a list of lists that are each the same
--    size as the inp list.
genInpUses :: (inpUse -> st -> st) -> (st -> inp -> [inpUse]) -> st -> [inp] ->
  [[inpUse]]
genInpUses _ _ _ [] = [[]]
genInpUses doUseOnSt stInpToUses initSt (inp:inps) = concat
  [map (use:) $ genInpUses doUseOnSt stInpToUses (doUseOnSt use initSt) inps |
   use <- stInpToUses initSt inp]

-- todo: doesn't do coming in, bearing off, or if not both poss
-- could be more efficient and not gen dupes,
--    but we don't care and just nub after..
--allMvPoss :: Bd -> Color -> Roll -> [Mv]
allMvPoss bdBar color (r1, r2) = nub . map pairListToMv $
  genInpUses ((\ x -> fst . fromRight . x) . doMv color . pairListToMv . (:[]))
    (\ bdBar' r -> allDieMvs bdBar' color r) bdBar pipCounts
  where
  pairListToMv :: [(Int, Int)] -> Mv
  -- todo: make this cooler to combine re-moves
  -- note: we aren't doing hit annotations..
  pairListToMv = MS.fromList . map (\ (x, y) -> (x, [(y, False)])) where
  pipCounts :: [Int]
  pipCounts = if r1 == r2 then [r1, r1, r1, r1] else [r1, r2]

--guessBestRepl :: Bd -> Roll -> Either String Mv
--guessBestRepl :: Bd -> Roll -> [Mv]
guessBestRepl bd roll =
  flip mapM_ mvsHitOppSide $ \ mv -> do
    print mv
    printDoMv $ doMv Red mv bd
  where
  printDoMv = putStrLn . either id (showBd . fst)
  mvs = allMvPoss bd Red roll
  --mvsHit = filter (\ mv -> not . S.null . snd . fromRight $ doMv Red mv bd) mvs
  mvsHitOppSide = filter (\ mv ->
    not . S.null . S.filter (<= 12) . snd . fromRight $ doMv Red mv bd) mvs

-- testing move equality is complicated by collapsing shorthand
-- (e.g. 24/20 on double-ones), but still should be doable

showRoll :: Roll -> String
showRoll (r1, r2) = show r1 ++ " " ++ show r2

showRollMv rm = showMv (rmMv rm) ++ "  " ++ showRoll (rmRoll rm) ++ "|" ++
  showCubeMvs (rmCubeMvs rm)

main :: IO ()
main = do
  res <- readRollMvs "data/open-reply.mem"
  case res of
    Left e -> print e
    Right mvs -> putStr . unlines . concat .
      map (\ (roll, cubeMvs) -> [showRoll roll] ++ cubeMvs) .
      M.toList .
      M.map (
        map (\ (x, y) ->
          showCubeMvs x
          ++ " <- " ++
          show y
          --show (length $ S.toList y)
          --(intercalate ", " . S.toList $ S.map showMv y)
        ) .
        reverse . sortBy (compare `on` snd) .
        map (second (length . S.toList)) .
        M.toList . flipMap . M.fromList
      ) .
      M.fromListWith (++) $
      map (\ rm -> (rmRoll rm, [(rmMv rm, rmCubeMvs rm)])) mvs
