#!/usr/bin/env runhaskell

import HsCharm
import Control.Monad (forever, when)

react :: Key -> IO ()
react KeyEscape = return ()
react KeyQ = return ()
react _ = getKey >>= react

main :: IO ()
main = do
	startCharm

	vCenterString "Hello Charm! Press Escape, q, or Control-C to quit."

	getKey >>= react

	endCharm