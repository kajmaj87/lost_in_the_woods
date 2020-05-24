import           System.IO                      ( stdin
                                                , hReady
                                                , hSetEcho
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           System.Random.PCG
import qualified Data.Map                      as M
import           Control.Monad                  ( when )
import           Control.Monad.ST

-- IO 

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  mainLoop $ Game (10, -10) M.empty

drawUI :: Game -> IO ()
drawUI g = do
  draw $ M.differenceWith
    (\new old -> if old == new then Nothing else Just new)
    (levelMap g)
    (lastMap g)
  drawPlayer g
  drawSummary 0 (-20) g

draw :: GameMap -> IO ()
draw g = mapM_ (drawTile . mapElementToTile) (M.toList g)

tileToMapElement :: Tile -> ((Int, Int), Char)
tileToMapElement Tile { tilePosition = (x, y), tile = c } = ((x, y), c)

mapElementToTile :: ((Int, Int), Char) -> Tile
mapElementToTile ((x, y), c) = Tile { tilePosition = (x, y), tile = c }

levelMap :: Game -> GameMap
levelMap g =
  let
    p  = playerPostion g
    px = fst p
    py = snd p
    inPlayerRange (x, y) r = (x - px) ^ 2 + 2 * (y - py) ^ 2 <= r * r
    seenLevel r =
      [ (x, y)
      | x <- [px - r .. px + r]
      , y <- [py - r .. py + r]
      , inPlayerRange (x, y) r
      ]
    seenMap = M.fromList $ map (tileToMapElement . pointToTile) (seenLevel 16)
  in
    M.insert (px, py) '@' seenMap


drawPlayer :: Game -> IO ()
drawPlayer g = drawChar '@' (playerPostion g)

drawChar :: Char -> Point -> IO ()
drawChar c p = case uncurry maybeMove p of
  Just escapeCode -> putStr $ escapeCode ++ [c]
  Nothing         -> return ()

pointToTile :: Point -> Tile
pointToTile p = Tile p (tileFromInt $ randomFromPoint 50 p)

drawTile :: Tile -> IO ()
drawTile Tile { tilePosition = (x, y), tile = c } = drawChar c (x, y)


drawSummary :: Int -> Int -> Game -> IO ()
drawSummary x y g = do
  moveCursor x y
  showPointPosition $ playerPostion g

clearCurrentLine :: () -> IO ()
clearCurrentLine _ = putStr "\ESC[2K"

clearScreen :: () -> IO ()
clearScreen _ = putStr "\ESC[2J"

moveCursor :: Int -> Int -> IO ()
moveCursor x y = case maybeMove x y of
  Just escapeCode -> putStr escapeCode
  Nothing         -> return ()

maybeMove :: Int -> Int -> Maybe String
maybeMove x y
  | 1 - y > 0
  , 1 + x > 0
  = let escapeCode = "[" ++ show (1 - y) ++ ";" ++ show (1 + x) ++ "f"
    in  Just $ "\ESC" ++ escapeCode
  | otherwise
  = Nothing

showPointPosition :: Point -> IO ()
showPointPosition p =
  putStr $ "Cursor position: " ++ show (fst p) ++ ", " ++ show (snd p)

mainLoop :: Game -> IO ()
mainLoop g = do
  drawUI g
  key       <- getKey
  lastState <- return $ g { lastMap = levelMap g }
  when (key /= "q") $ case key of
    "h" -> mainLoop (gameStep lastState (Move West))
    "j" -> mainLoop (gameStep lastState (Move South))
    "k" -> mainLoop (gameStep lastState (Move North))
    "l" -> mainLoop (gameStep lastState (Move East))
    _   -> mainLoop (gameStep lastState Wait)

getKey :: IO String
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)


-- PURE 
type Point = (Int, Int)
type GameMap = M.Map Point Char
data Tile = Tile { tilePosition :: Point, tile :: Char } deriving (Ord, Eq)
data Game = Game { playerPostion :: Point, lastMap :: GameMap}

data Dir = West | East | North | South
data Event a = Move Dir | Wait

gameStep :: Game -> Event a -> Game
gameStep g (Move dir) = case dir of
  West ->
    g { playerPostion = (fst (playerPostion g) - 1, snd $ playerPostion g) }
  East ->
    g { playerPostion = (fst (playerPostion g) + 1, snd $ playerPostion g) }
  North ->
    g { playerPostion = (fst $ playerPostion g, snd (playerPostion g) + 1) }
  South ->
    g { playerPostion = (fst $ playerPostion g, snd (playerPostion g) - 1) }
gameStep g Wait = g

tileFromInt :: Int -> Char
tileFromInt i | i < 1     = 't'
              | i < 4     = 'T'
              | otherwise = '.'

toNeutral :: Int -> Int
toNeutral n | n < 0     = (-2) * n
            | otherwise = 2 * n - 1

cantorPairing :: Point -> Int
cantorPairing (x, y) = (x + y) * (x + y + 1) `div` 2 + y

seedFromPoint :: Point -> Int
--seedFromPoint (x, y) = 89 ^ abs x * 97 ^ abs y
seedFromPoint (x, y) = cantorPairing (toNeutral x, toNeutral y)

randomFromPoint :: Int -> Point -> Int
randomFromPoint max p = randomFromSeed max (seedFromPoint p)

randomFromSeed :: Int -> Int -> Int
randomFromSeed max seed = runST $ do
  g <- initialize (fromIntegral seed) 0
  uniformR (0, max - 1) g


