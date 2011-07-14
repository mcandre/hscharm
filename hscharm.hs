{-# LANGUAGE ForeignFunctionInterface #-}

module Main (
	getWidth,
	main
) where

import Foreign

foreign import ccall "charm.h get_width" getWidth :: IO Int

main :: IO ()
main = do
	putStrLn $ "Accessing width..."

	w <- getWidth

	putStrLn $ "Width: " ++ show w