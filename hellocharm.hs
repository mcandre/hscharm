module Main where

import Charm

main :: IO ()
main = do
	w <- getWidth
	putStrLn $ "Width: " ++ show w