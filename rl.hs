-- Roguelike

import HsCharm
import Control.Monad (when, replicateM)
import Maybe (fromJust)
import Data.List.Utils (join)
import Random (randomRIO)

pick :: [a] -> IO a
pick xs = (randomRIO (0, length xs - 1)) >>= (return . (xs !!))

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
		loc = (0, 0),
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
	| (x < 0) || (y < 0) || (x > (width g) - 1) || (y > (height g) - 1) = Nothing
	| otherwise = Just $ ((level g) !! y) !! x

move :: Game -> Key -> Game
move g KeyUp
	| y == 0 = g
	| impassible $ fromJust $ cellAt g (x, y - 1) = g
	| otherwise = g {
			loc = (x, y - 1),
			messages = "You moved up!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyDown
	| y == (height g) - 1 = g
	| impassible $ fromJust $ cellAt g (x, y + 1) = g
	| otherwise = g {
			loc = (x, y + 1),
			messages = "You moved down!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyRight
	| x == (width g) - 1 = g
	| impassible $ fromJust $ cellAt g (x + 1, y) = g
	| otherwise = g {
			loc = (x + 1, y),
			messages = "You moved right!":(voicemail g)
		}
	where
		(x, y) = loc g

move g KeyLeft
	| x == 0 = g
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
	moveCursor 0 row
	hCenterString m
	blotMessages ms (row - 1)

blotLevel :: [[Cell]] -> IO ()
blotLevel lev = do
	moveCursor 0 0
	(blotString . join "\n" . (map (join "" . (map show)))) lev

loop :: Game -> IO ()
loop g = do
	clearScreen

	blotLevel $ level g

	let (x, y) = loc g
	moveCursor x y
	blotChar '@'

	blotMessages (messages g) (height g + messageSpace - 1)

	k <- getKey

	when (k `notElem` [KeyEscape, KeyQ])
		(do
			let g' = if k `elem` [KeyUp, KeyDown, KeyRight, KeyLeft] then
					move g k
				else
					g

			loop g')

generateRow :: Int -> IO [Cell]
generateRow w = replicateM w (pick [Empty, Empty, Wall])

generateLevel :: Int -> Int -> IO [[Cell]]
generateLevel w h = replicateM h (generateRow w)

main :: IO ()
main = do
	startCharm

	w <- getWidth
	h <- getHeight

	-- Reserve space for messages
	let h' = h - messageSpace

	locX <- pick [0 .. (w - 1)]
	locY <- pick [0 .. (h' - 1)]

	lev <- generateLevel w h'

	let g = defaultGame {
			width = w,
			height = h',
			loc = (locX, locY),
			level = lev
		}

	loop g

	endCharm