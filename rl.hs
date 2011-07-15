-- Roguelike

import HsCharm
import Control.Monad (when)

data Game = Game {
		width :: Int,
		height :: Int,
		loc :: (Int, Int)
	}

move :: Game -> Key -> Game
move g KeyUp = g { loc = (x, if y == 1 then y else (y - 1)) }
	where
		(x, y) = loc g

move g KeyDown = g { loc = (x, if y == height g then y else (y + 1)) }
	where
		(x, y) = loc g

move g KeyRight = g { loc = (if x == width g then x else (x + 1), y) }
	where
		(x, y) = loc g

move g KeyLeft = g { loc = (if x == 1 then x else (x - 1), y) }
	where
		(x, y) = loc g

loop :: Game -> IO ()
loop g = do
	clearScreen

	let (x, y) = loc g

	moveCursor x y

	blotChar '@'

	k <- getKey

	when (k `notElem` [KeyEscape, KeyQ])
		(do
			let g' = if k `elem` [KeyUp, KeyDown, KeyRight, KeyLeft] then
					move g k
				else
					g

			loop g')

main :: IO ()
main = do
	startCharm

	w <- getWidth
	h <- getHeight

	let g = Game { width = w, height = h, loc = (1, 1) }

	loop g

	endCharm