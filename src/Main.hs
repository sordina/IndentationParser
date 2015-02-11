module Main where

import qualified IndentationParser as IP

main :: IO ()
main = interact (IP.renderF . IP.parse)
