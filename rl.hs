-- Roguelike

import HsCharm
import Control.Monad (when)

data Game = Game {
		width :: Int,
		height :: Int,
		loc :: (Int, Int),
		messages :: [String]
	}

defaultGame :: Game
defaultGame = Game {
		width = 80,
		height = 24 - messageSpace,
		loc = (1, 1),
		messages = []-- ,
		-- level = [[Cell]]
	}

voicemail :: Game -> [String]
voicemail = take 2 . messages

-- data Cell
-- 	= Empty
-- 	| Wall
-- 	deriving (Eq)
-- 
-- instance Show Cell where
-- 	show Empty = ' '
-- 	show Wall = '#'

move :: Game -> Key -> Game
move g KeyUp = g {
			loc = (x, if y == 1 then y else (y - 1)),
			messages = "You moved up!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyDown = g {
			loc = (x, if y == height g then y else (y + 1)),
			messages = "You moved down!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyRight = g {
			loc = (if x == width g then x else (x + 1), y),
			messages = "You moved right!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyLeft = g {
			loc = (if x == 1 then x else (x - 1), y),
			messages = "You moved left!":(voicemail g)
		}
	where
		(x, y) = loc g

messageSpace :: Int
messageSpace = 3

blotMessages :: [String] -> Int -> IO ()
blotMessages [] _ = return ()
blotMessages (m:ms) row = do
	moveCursor 1 row
	hCenterString m
	blotMessages ms (row - 1)

loop :: Game -> IO ()
loop g = do
	clearScreen

	let (x, y) = loc g

	moveCursor x y

	c <- getCursor
	let m = "Cursor: (" ++ show (fst c) ++ ", " ++ show (snd c) ++ ")"

	blotChar '@'

	blotMessages ([m]) (height g + messageSpace)

	-- blotMessages (messages g) (height g + messageSpace)

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

	-- Reserve space for messages
	let h' = h - messageSpace

	let g = defaultGame {
			width = w,
			height = h'
		}

	loop g

	endCharm