module Main where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import GHC.IO.Handle
import System.Console.ANSI
import System.Exit (exitSuccess)
import System.IO
import System.Random

setupGame :: IO ()
setupGame = do
  clearScreen
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor

exitGame :: IO ()
exitGame = do
  clearScreen
  setCursorPosition 0 0
  hSetEcho stdin True
  showCursor

main :: IO ()
main = do
  config <- mkConfig
  print config
  let game = mkGame
  print game
  setupGame
  runStateT (runReaderT play config) game
  _ <- getChar
  exitGame

type Score = Int
type Row = Int
type Col = Int
type Point = (Row, Col)
type Size = (Row, Col)

data Direction = North | West | South | East deriving (Show)

data Game = MkGame
  { gSnake :: [Point]
  , gFood :: Point
  , gScore :: Score
  , gDirection :: Direction
  }
  deriving (Show)

data Config = MkConfig
  { cSize :: Size
  , cRatio :: Double
  }
  deriving (Show)

ratio :: Int -> Int -> Double
ratio x y = fromIntegral x / fromIntegral y

mkConfig :: IO Config
mkConfig = do
  size <- getTerminalSize
  let (rows, cols) = fromJust size
  return
    $ MkConfig
      { cSize = (rows, cols)
      , cRatio = ratio cols rows
      }

mkGame :: Game
mkGame =
  MkGame
    { gSnake = [(12, 40), (12, 41), (12, 42), (12, 43), (12, 44)]
    , gFood = (4, 100)
    , gScore = 0
    , gDirection = West
    }

type Board a = ReaderT Config (StateT Game IO) a

putCharAt :: Char -> Point -> IO ()
putCharAt ch (r, c) = do
  setCursorPosition r c
  putChar ch

printSnake :: Board ()
printSnake = do
  game <- get
  let snake = gSnake game
  liftIO $ do
    printHead snake
    printBody $ tail snake
 where
  printHead (p : _) = putCharAt 'o' p
  printBody [l] = putCharAt 'x' l
  printBody (h : t) = putCharAt 'x' h >> printBody t

printFood :: Board ()
printFood = do
  game <- get
  let food = gFood game
  liftIO $ putCharAt '@' food

printBoard :: Board ()
printBoard = do
  liftIO clearScreen
  printFood
  printSnake

randomizeFood :: Board Point
randomizeFood = do
  config <- ask
  let (maxRows, maxCols) = cSize config
  newRow <- liftIO $ randomRIO (0, maxRows)
  newCol <- liftIO $ randomRIO (0, maxCols)
  return (newRow, newCol)

overflow :: Point -> Size -> Direction -> Point
overflow (r, c) (_, maxCols) East = if c == maxCols then (r, 1) else (r, c)
overflow (r, c) (_, maxCols) West = if c == 0 then (r, maxCols - 1) else (r, c)
overflow (r, c) (maxRows, _) South = if r == maxRows then (1, c) else (r, c)
overflow (r, c) (maxRows, _) North = if r == 0 then (maxRows - 1, c) else (r, c)

moveSnake :: Board ()
moveSnake = do
  game <- get
  config <- ask
  let direction = gDirection game
      food = gFood game
      size = cSize config
      snake@(head : _) = gSnake game
      ateFood = food == head
      (r, c) = head
      head' = case direction of
        West -> (r, c - 1)
        East -> (r, c + 1)
        North -> (r - 1, c)
        South -> (r + 1, c)
      newHead = overflow head' size direction
  if ateFood
    then do
      let newScore = gScore game + 1
          newBody = snake
          newSnake = newHead : newBody
      newFood <- randomizeFood
      modify $ \g -> g{gSnake = newSnake, gScore = newScore, gFood = newFood}
    else do
      let newBody = init snake
          newSnake = newHead : newBody
      modify $ \g -> g{gSnake = newSnake}

waitFromDirection :: Board Int
waitFromDirection = do
  config <- ask
  game <- get
  let ratio = cRatio config
      direction = gDirection game
      wait = case direction of
        West -> round $ 100 / cRatio config * 2
        East -> round $ 100 / cRatio config * 2
        _ -> 100
  return wait

play :: Board ()
play = do
  printBoard
  moveSnake
  wait <- waitFromDirection
  isInput <- liftIO $ hWaitForInput stdin wait
  if isInput
    then parseInput
    else play

parseInput :: Board ()
parseInput = do
  ch <- liftIO getChar
  case ch of
    'q' -> liftIO $ exitGame >> exitSuccess
    'i' -> changeDirection North
    'j' -> changeDirection West
    'k' -> changeDirection South
    'l' -> changeDirection East
    _ -> return ()
  play

changeDirection :: Direction -> Board ()
changeDirection d = modify $ \game -> game{gDirection = d}
