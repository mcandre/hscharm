-- Roguelike

import HsCharm
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import Control.Monad (when, replicateM)
import Data.List (find, delete)
import Data.List.Utils (join)

data Game = Game {
		width :: Int,
		height :: Int,
		messages :: [String],
		level :: [[Monster]],
		rogue :: Monster,
		monsters :: [Monster]
	}

defaultGame :: Game
defaultGame = Game {
		width = 80,
		height = 24 - messageSpace,
		messages = [],
		level = replicate (24 - messageSpace) (replicate 80 defaultFloor),
		rogue = defaultRogue,
		monsters = []
	}

voicemail :: Game -> [String]
voicemail = take 2 . messages

data Monster = Monster {
		symbol :: String,
		loc :: (Int, Int),
		impassible :: Bool,
		hp :: Int
	} deriving (Eq)

instance Show Monster where
	show = symbol

defaultFloor :: Monster
defaultFloor = Monster {
		symbol = " ",
		loc = (0, 0),
		impassible = False,
		hp = 0
	}

defaultWall :: Monster
defaultWall = Monster {
		symbol = "#",
		loc = (0, 0),
		impassible = True,
		hp = 0
	}

defaultRogue :: Monster
defaultRogue = Monster {
		symbol = "@",
		loc = (0, 0),
		impassible = True,
		hp = 10
	}

defaultZombie :: Monster
defaultZombie = Monster {
		symbol = "z",
		loc = (0, 0),
		impassible = True,
		hp = 1
	}

cellAt :: Game -> (Int, Int) -> Monster
cellAt g (x, y) = ((level g) !! y) !! x

thingAt :: Game -> (Int, Int) -> Monster
thingAt g (x, y) = case find (\e -> (((x, y) ==) . loc) e) (monsters g) of
	Just m -> m
	_ -> cellAt g (x, y)

attack :: Game -> Monster -> IO Game
attack g m = case (symbol m) of
	"#" -> return g { messages = "Immobile wall.":(voicemail g) }
	"z" -> do
		let m' = m { hp = hp m - 1 }
		let ms = delete m (monsters g)
		return g {
				monsters = if hp m' == 0 then ms else m':ms,
				messages = "You hit a zombie.":(voicemail g)
			}
	_ -> return g

move :: Game -> Key -> IO Game
move g KeyUp
	| y == 0 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x, y - 1) },
			messages = "You moved up!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x, y - 1)

move g KeyDown
	| y == (height g) - 1 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x, y + 1) },
			messages = "You moved down!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x, y + 1)

move g KeyRight
	| x == (width g) - 1 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x + 1, y) },
			messages = "You moved right!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x + 1, y)

move g KeyLeft
	| x == 0 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x - 1, y) },
			messages = "You moved left!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x - 1, y)

messageSpace :: Int
messageSpace = 3

blotMessages :: [String] -> Int -> IO ()
blotMessages [] _ = return ()
blotMessages (m:ms) row = do
	moveCursor 0 row
	hCenterString m
	blotMessages ms (row - 1)

blotLevel :: [[Monster]] -> IO ()
blotLevel lev = do
	moveCursor 0 0
	(blotString . join "\n" . (map (join "" . (map show)))) lev

blotMonster :: Monster -> IO ()
blotMonster m = do
	let (x, y) = loc m
	moveCursor x y
	blotString $ show m

loop :: Game -> IO ()
loop g = do
	blotLevel $ level g

	mapM blotMonster (monsters g)

	blotMonster $ rogue g

	-- Clear messages
	blotMessages (replicate 3 $ join "" $ replicate (width g) " ") (height g + messageSpace - 1)

	blotMessages (reverse $ messages g) (height g + messageSpace - 1)

	k <- getKey

	when (k `notElem` [KeyEscape, KeyQ])
		(do
			g' <- if k `elem` [KeyUp, KeyDown, KeyRight, KeyLeft] then
					move g k
				else
					return g

			loop g')

generateRow :: Int -> IO [Monster]
generateRow w = (replicateM w . flip runRVar DevRandom . choice) [defaultFloor, defaultFloor, defaultWall]

generateLevel :: Int -> Int -> IO [[Monster]]
generateLevel w h = replicateM h (generateRow w)

zombies :: Game -> Int
zombies g = width g `div` 10

generateMonsters :: Game -> [Monster] -> IO Game
generateMonsters g [] = return g
generateMonsters g (m:ms) = do
	let r = (loc . rogue) g

	x <- runRVar (choice [0 .. width g - 1]) DevRandom
	y <- runRVar (choice [0 .. height g - 1]) DevRandom

	if r == (x, y) then do
		generateMonsters g (m:ms)
	else do
		let c = cellAt g (x, y)

		case symbol c of
			" " -> do
				let placedMonsters = monsters g
				let locs = map loc placedMonsters

				if (x, y) `elem` locs then do
					generateMonsters g (m:ms)
				else do
					let m' = m { loc = (x, y) }
					let g' = g { monsters = m':placedMonsters }
					generateMonsters g' ms
			_ -> generateMonsters g (m:ms)

main :: IO ()
main = do
	startCharm

	w <- getWidth
	h <- getHeight

	-- Reserve space for messages
	let h' = h - messageSpace

	locX <- runRVar (choice [0 .. w - 1]) DevRandom
	locY <- runRVar (choice [0 .. h' - 1]) DevRandom

	lev <- generateLevel w h'
	let g = defaultGame {
			width = w,
			height = h',
			level = lev,
			rogue = defaultRogue {
					loc = (locX, locY)
				}
		}

	g' <- generateMonsters g (replicate (zombies g) defaultZombie)

	loop g'

	endCharm