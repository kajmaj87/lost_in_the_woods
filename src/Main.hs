import           System.IO                      ( stdin
                                                , hReady
                                                , hSetEcho
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           System.Random.PCG
import           Control.Monad                  ( when )
import           Control.Monad.ST

-- IO 

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  mainLoop $ Game (10, -10)

drawUI :: Game -> IO ()
drawUI g = do
  clearScreen ()
  drawLevel g
  drawPlayer g
  drawSummary 0 (-20) g

drawLevel :: Game -> IO ()
drawLevel g =
  let px = fst (playerPostion g)
      py = snd (playerPostion g)
      inPlayerRange (x, y) r = (x - px) ^ 2 + 2 * (y - py) ^ 2 <= r * r
      seenLevel r =
          [ (x, y)
          | x <- [px - r .. px + r]
          , y <- [py - r .. py + r]
          , inPlayerRange (x, y) r
          ]
  in  mapM_ drawTile (seenLevel 16)

drawPlayer :: Game -> IO ()
drawPlayer g = drawChar '@' (playerPostion g)

drawChar :: Char -> Point -> IO ()
drawChar c p = case uncurry maybeMove p of
  Just escapeCode -> putStr $ escapeCode ++ [c]
  Nothing         -> return ()


drawTile :: Point -> IO ()
drawTile (x, y) =
  let c = tileFromInt $ randomFromPoint 50 (x, y)

  --if randomFromPoint 100 (x, y) < 2 then 't' else '.'
  in  drawChar c (x, y)


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
  key <- getKey
  when (key /= "q") $ case key of
    "h" -> mainLoop (gameStep g (Move West))
    "j" -> mainLoop (gameStep g (Move South))
    "k" -> mainLoop (gameStep g (Move North))
    "l" -> mainLoop (gameStep g (Move East))
    _   -> mainLoop (gameStep g Wait)

getKey :: IO String
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)


-- PURE 
type Point = (Int, Int)
data Tile = Tile { tilePosition :: Point, tile :: Char }
data Game = Game { playerPostion :: Point}

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


