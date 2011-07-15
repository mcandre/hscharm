import HsCharm
import Control.Monad (forever, when)
import Text.Printf (printf)
import Random (randomRIO)

pick :: [a] -> IO a
pick xs = (randomRIO (0, length xs - 1)) >>= (return . (xs !!))

data Game = Game {
		score :: Int,
		delta :: Int
	}

drawScore :: Int -> IO ()
drawScore s = do
	moveCursor 1 3
	hCenterString $ printf "SCORE %04d" s

drawArrow :: Key -> IO ()
drawArrow a = do
	moveCursor 1 5

	hCenterString $ case a of
		KeyUp -> "^"
		KeyDown -> "v"
		KeyRight -> ">"
		KeyLeft -> "<"

drawHoot :: Bool -> IO ()
drawHoot b = do
	moveCursor 1 7

	hCenterString $ case b of
		True -> "GOOD!"
		False -> "BAD! "

loop :: Game -> IO ()
loop g = do
	drawScore $ score g

	arrow <- pick [KeyUp, KeyDown, KeyRight, KeyLeft]

	drawArrow arrow

	k <- getKey

	when (k `notElem` [KeyEscape, KeyQ])
		(do
			let match = arrow == k

			drawHoot match

			let g' = g { score = score g + case match of
					True -> 1
					False -> -1
				}

			loop g')

main :: IO ()
main = do
	startCharm

	hCenterString "DDR: How fast can you play?"

	let g = Game {
			score = 0,
			delta = 0
		}

	loop g

	endCharm