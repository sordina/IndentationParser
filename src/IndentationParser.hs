
{-# LANGUAGE QuasiQuotes #-}

module IndentationParser where

import Data.Tree
import Control.Arrow
import Text.RawString.QQ
import Test.QuickCheck

data Line = L {indent :: Int, line :: String} deriving (Eq, Show)

instance Ord Line where (L a _) `compare` (L b _) = a `compare` b

buildForest :: [Line] -> Forest String
buildForest []     = []
buildForest (l:ls) = Node (line l) (buildForest gt) : buildForest lt
  where (gt,lt) = span (> l) ls

parse :: String -> Forest String
parse = buildForest . map ( uncurry L . (indented &&& text) ) . filter (not . blank) . lines

indented :: String -> Int
indented = sum . map value . takeWhile blankChar

text :: String -> String
text = dropWhile blankChar

blank :: String -> Bool
blank = all blankChar

blankChar :: Char -> Bool
blankChar = flip elem " \t"

prop_indented_spaces :: NonNegative Int -> String -> Bool
prop_indented_spaces (NonNegative n) s = indented (replicate n ' '  ++ "x" ++ s) ==     n

prop_indented_tabs :: NonNegative Int -> String -> Bool
prop_indented_tabs   (NonNegative n) s = indented (replicate n '\t' ++ "x" ++ s) == 4 * n

value :: Num a => Char -> a
value '\t' = 4
value ' '  = 1
value _    = 0

-- TESTING

prop_parse_test :: Bool
prop_parse_test = renderF (parse testTree) == testTree

prop_parse :: String -> Bool
prop_parse    x = renderF (parse x) == renderF (parse (renderF (parse x)))

renderF :: Forest String -> String
renderF = unlines . map (render "")
  where
  render :: String -> Tree String -> String
  render p (Node s ls) = p ++ s ++ concatMap (render ("\n " ++ drop 1 p)) ls

testTree :: String
testTree = drop 1 [r|
a
 b
  c
 d
e
 f
 g
h
 i
  j
   k
|]
