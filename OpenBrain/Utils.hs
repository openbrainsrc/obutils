module OpenBrain.Utils where

import System.Directory
import System.Cmd 
import System.IO
import System.Exit
import System.Environment
import Data.Maybe
import Data.List
import Control.Monad
import Data.Char (toLower)

import System.Process

headOr y [] = y
headOr _ (x:_) = x

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy p [] = []
splitBy p s@(c:cs) | c == p = splitBy p cs
                   | otherwise = let (hd, tl) = break (==p) s 
                                 in hd : splitBy p tl

dropAt c = tail . dropWhile (/=c)

takeUntil c = takeWhile (/=c)

onFst f (x,y) = (f x, y)
onSnd f (x,y) = (x, f y)

notNull :: (Foldable t) => t a -> Bool
notNull = not . null

uncap [] = []
uncap (c:cs) = toLower c : cs

safeRead x = case readsPrec 5 x of 
               [] -> Nothing
               (x,_):_ -> Just x

ifM mp mc ma = do p <- mp
                  if p then mc else ma

whenM mb ma = do
   b <- mb
   when b ma

dropLast n s = take (length s - n) s
takeLast n s = drop (length s - n) s

failWith s = do
  hPutStrLn stdout s
  exitWith $ ExitFailure 1
