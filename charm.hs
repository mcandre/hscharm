{-# LANGUAGE ForeignFunctionInterface #-}

module Charm (
	getWidth
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.C.Error

foreign import ccall "get_width" getWidth :: IO Int