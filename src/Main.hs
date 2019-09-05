module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game
import System.Random

window :: Display
window = InWindow "Snake" (600, 400) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = rectangleSolid 10 10

main :: IO ()
main = playIO window background fps initGame renderGame handleInput updateGame 

fps :: Int
fps = 30

initGame :: Game
initGame = Game (Snake [Position (0, 0)]) (Fruit (Position (10,10))) DirectionUp

renderGame :: Game -> IO Picture
renderGame (Game (Snake sps) (Fruit fp) _) = pure $ pictures [snake, fruit]
  where
    snake :: Picture
    snake = pictures (square green <$> sps)

    fruit :: Picture
    fruit = square red fp

    square :: Color -> Position -> Picture
    square c (Position (x, y)) = color c $ translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 10 10 

handleInput :: Event -> Game -> IO Game
handleInput event g@(Game _ _ d) = pure $ g { gameDirection = updateDirection d event } 

updateGame :: Float -> Game -> IO Game
updateGame delta game@(Game snake fruit d) = do
  let snake' = moveSnake d snake 
  fruit' <- updateFruit snake' fruit
  return $ game { gameSnake = snake', gameFruit = fruit' }
  where
    updateFruit :: Snake -> Fruit -> IO Fruit
    updateFruit snake fruit = if collision then mkFruit else pure fruit
      where
        collision = head (unSnake snake) == unFruit fruit

    moveSnake :: Direction -> Snake -> Snake
    moveSnake d (Snake sps) = Snake $ updatedHead : (if collision then sps else init sps)  
      where
        updatedHead = updateSnakeHead d (head sps)
        collision = updatedHead == unFruit fruit

    updateSnakeHead :: Direction -> Position -> Position
    updateSnakeHead d (Position (x, y)) = case d of
                                     DirectionUp -> Position (x, y + blockSize)
                                     DirectionDown -> Position (x, y - blockSize)
                                     DirectionLeft -> Position (x - blockSize, y)
                                     DirectionRight -> Position (x + blockSize, y)

blockSize :: Int
blockSize = 10

mkFruit :: IO Fruit
mkFruit = do
  x <- r 
  y <- r
  return . Fruit $ Position (x * blockSize, y * blockSize)
  where
    r = getStdRandom (randomR (-20, 20))

newtype Position = Position { unPosition :: (Int, Int) } deriving (Show, Eq)
newtype Snake = Snake { unSnake :: [Position] } deriving (Show, Eq)
newtype Fruit = Fruit { unFruit :: Position } deriving (Show, Eq)

data Direction
  =  DirectionUp
  | DirectionDown
  | DirectionLeft
  | DirectionRight
  deriving (Show, Eq)

data Game = Game
  { gameSnake :: Snake
  , gameFruit :: Fruit
  , gameDirection :: Direction
  } deriving (Show, Eq)

coerceEventDirection:: SpecialKey -> Direction
coerceEventDirection k = case k of
  KeyUp -> DirectionUp
  KeyDown -> DirectionDown
  KeyLeft -> DirectionLeft
  KeyRight -> DirectionRight

inverse :: Direction -> Direction
inverse DirectionUp    = DirectionDown
inverse DirectionLeft  = DirectionRight
inverse DirectionDown  = DirectionUp
inverse DirectionRight = DirectionLeft

updateDirection :: Direction -> Event -> Direction
updateDirection d (EventKey (SpecialKey e) _ _ _) = if inverse d /= d' then d' else d
  where
    d' = coerceEventDirection e
updateDirection d _ = d
