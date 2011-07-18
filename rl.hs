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
		messages :: [String],
		level :: [[Cell]],
		rogue :: Monster
	}

defaultGame :: Game
defaultGame = Game {
		width = 80,
		height = 24 - messageSpace,
		messages = [],
		level = replicate (24 - messageSpace) (replicate 80 Empty),
		rogue = defaultRogue
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

data Monster = Monster {
		symbol :: String,
		loc :: (Int, Int),
		hp :: Int
	}

instance Show Monster where
	show = symbol

defaultRogue :: Monster
defaultRogue = Monster {
		symbol = "@",
		loc = (0, 0),
		hp = 10
	}

cellAt :: Game -> (Int, Int) -> Maybe Cell
cellAt g (x, y)
	| (x < 0) || (y < 0) || (x > (width g) - 1) || (y > (height g) - 1) = Nothing
	| otherwise = Just $ ((level g) !! y) !! x

move :: Game -> Key -> Game
move g KeyUp
	| y == 0 = g
	| impassible $ fromJust $ cellAt g (x, y - 1) = g
	| otherwise = g {
			rogue = r { loc = (x, y - 1) },
			messages = "You moved up!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r

move g KeyDown
	| y == (height g) - 1 = g
	| impassible $ fromJust $ cellAt g (x, y + 1) = g
	| otherwise = g {
			rogue = r { loc = (x, y + 1) },
			messages = "You moved down!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r

move g KeyRight
	| x == (width g) - 1 = g
	| impassible $ fromJust $ cellAt g (x + 1, y) = g
	| otherwise = g {
			rogue = r { loc = (x + 1, y) },
			messages = "You moved right!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r

move g KeyLeft
	| x == 0 = g
	| impassible $ fromJust $ cellAt g (x - 1, y) = g
	| otherwise = g {
			rogue = r { loc = (x - 1, y) },
			messages = "You moved left!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r

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
	blotLevel $ level g

	let (x, y) = (loc . rogue) g
	moveCursor x y
	blotChar '@'

	-- Clear messages
	blotMessages (replicate 3 $ join "" $ replicate (width g) " ") (height g + messageSpace - 1)

	blotMessages (reverse $ messages g) (height g + messageSpace - 1)

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
			level = lev,
			rogue = defaultRogue {
					loc = (locX, locY)
				}
		}

	loop g

	endCharm