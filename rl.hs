-- Roguelike

import HsCharm
import Control.Monad (when)
import Maybe (fromJust)

data Game = Game {
		width :: Int,
		height :: Int,
		loc :: (Int, Int),
		messages :: [String],
		level :: [[Cell]]
	}

defaultGame :: Game
defaultGame = Game {
		width = 80,
		height = 24 - messageSpace,
		loc = (1, 1),
		messages = [],
		level = replicate (24 - messageSpace) (replicate 80 Empty)
	}

voicemail :: Game -> [String]
voicemail = take 2 . messages

data Cell
	= Empty
	| Wall
	deriving (Eq)

impassible :: Cell -> Bool
impassible Empty = False
impassible Wall = True

instance Show Cell where
	show Empty = " "
	show Wall = "#"

cellAt :: Game -> (Int, Int) -> Maybe Cell
cellAt g (x, y)
	| (x < 1) || (y < 1) || (x > width g) || (y > height g) = Nothing
	| otherwise = Just $ ((level g) !! x) !! y

move :: Game -> Key -> Game
move g KeyUp
	| y == 1 = g
	| impassible $ fromJust $ cellAt g (x, y - 1) = g
	| otherwise = g {
			loc = (x, y - 1),
			messages = "You moved up!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyDown
	| y == height g = g
	| impassible $ fromJust $ cellAt g (x, y + 1) = g
	| otherwise = g {
			loc = (x, y + 1),
			messages = "You moved down!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyRight
	| x == width g = g
	| impassible $ fromJust $ cellAt g (x + 1, y) = g
	| otherwise = g {
			loc = (x + 1, y),
			messages = "You moved right!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyLeft
	| x == 1 = g
	| impassible $ fromJust $ cellAt g (x - 1, y) = g
	| otherwise = g {
			loc = (x - 1, y),
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

blotLevel :: Game -> IO ()
blotLevel g = do
	-- ...

loop :: Game -> IO ()
loop g = do
	clearScreen

	let (x, y) = loc g

	moveCursor x y
	blotChar '@'

	blotMessages (messages g) (height g + messageSpace)

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