{-# LANGUAGE ForeignFunctionInterface #-}

module HsCharm (
	getWidth
) where

import Foreign

foreign import ccall "charm.h get_width" getWidth :: IO Int