#!/usr/bin/env runhaskell

module Main where

import HsCharm

main :: IO ()
main = do
	w <- getWidth
	putStrLn $ "Width: " ++ show w