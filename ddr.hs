import HsCharm
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import Control.Monad (when)
import Text.Printf (printf)

data Game = Game {
  score :: Int,
  delta :: Int
  }

drawScore :: Int -> IO ()
drawScore s = do
  moveCursor 0 2
  hCenterString $ printf "SCORE %04d" s

drawArrow :: Key -> IO ()
drawArrow a = do
  moveCursor 0 4

  hCenterString $ case a of
    KeyUp -> "^"
    KeyDown -> "v"
    KeyRight -> ">"
    KeyLeft -> "<"
    _ -> ""

drawHoot :: Bool -> IO ()
drawHoot b = do
  moveCursor 0 6

  hCenterString $ if b then "GOOD!" else "BAD! "

loop :: Game -> IO ()
loop g = do
  drawScore $ score g

  arrow <- runRVar (choice [KeyUp, KeyDown, KeyRight, KeyLeft]) DevRandom

  drawArrow arrow

  k <- getKey

  when (k `notElem` [KeyEscape, KeyQ])
    (do
       let match = arrow == k

       drawHoot match

       let g' = g { score = score g + (if match then 1 else -1) }

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
