module Main where

import qualified IndentationParser as IP

import System.IO
import System.Exit
import System.Environment
import Data.Tree
import Data.Maybe
import Text.Regex
import Control.Lens
import Data.Tree.Lens

main :: IO ()
main = getArgs >>= top

usage :: IO b
usage = hPutStrLn stderr "Usage indentation_prune <regex>*" >> exitFailure

top :: [String] -> IO ()
top [] = usage
top as = interact (IP.renderF . prune (constrainAll as) . IP.parse)

constrainAll :: [String] -> String -> Bool
constrainAll as s = any (constrain s) as

constrain :: String -> String -> Bool
constrain s r = isJust (matchRegex (mkRegex r) s)

prune :: (b -> Bool) -> Forest b -> [Tree b]
prune c f = filter (c . rootLabel) (map (over branches (prune c)) f)
