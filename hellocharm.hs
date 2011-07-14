#!/usr/bin/env runhaskell

module HelloCharm where

import Charm

main :: IO ()
main = do
	putStrLn $ "Accessing width..."

	w <- getWidth

	putStrLn $ "Width: " ++ show w