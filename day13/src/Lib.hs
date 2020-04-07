module Lib
    ( IC.newProg
    , IC.runProg
    , IC.Prog (..)
    , IC.setInputProg
    , Tid (..)
    , buildCommands
    , gridDisplay
    , editDisplay
    , playGame
    , stepGame
    , Game (..)
    , newGame
) where

import qualified Intcode as IC
  ( newProg
  , runProg
  , Prog (..)
  , ProgState (..)
  , setInputProg
  , scrubOutput)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Lazy

data Tid = Empty | Wall | Block | Paddle | Ball | BadTid | Score{theScore::Int} deriving(Show, Eq)

tidFromInt :: Int -> Tid
tidFromInt 0 = Empty
tidFromInt 1 = Wall
tidFromInt 2 = Block
tidFromInt 3 = Paddle
tidFromInt 4 = Ball
tidFromInt _ = BadTid

tidToChar :: Tid -> Char
tidToChar Empty = ' '
tidToChar Wall = '|'
tidToChar Block = '#'
tidToChar Paddle = '='
tidToChar Ball = 'o'
tidToChar BadTid = 'X'
tidToChar (Score{theScore=_}) = 'Z'

type Tile = (Int, Int)

buildCommands :: [Int] -> [(Tile, Tid)]
buildCommands prog = reverse $ buildCommandsInner prog []

buildCommandsInner :: [Int] -> [(Tile, Tid)] -> [(Tile, Tid)]
buildCommandsInner progOut xs
  | length progOut < 3 = xs
  | otherwise = buildCommandsInner newOut newXs
  where
    (tile, newOut) = splitAt 3 (progOut)
    newXs = (toCommand (tile!!0) (tile!!1) (tile!!2)):xs
    toCommand x y z
      | (x, y) == (-1, 0) = ((-1,0), Score z)
      | otherwise = ((x,y), tidFromInt z)

--
-- A display is a HashMap of coordinates and a Tile.
--
type Displ = HM.HashMap Tile Tid

editDisplay :: Displ -> [(Tile, Tid)] -> Displ
editDisplay displ = foldl (\acc x-> HM.insert (fst x) (snd x) acc) displ

xBall :: Displ -> Int
xBall = fst . head . HM.keys . HM.filter (\v -> v == Ball)

xPaddle :: Displ -> Int
xPaddle = fst . head . HM.keys . HM.filter (\v -> v == Paddle)

countBlocks :: Displ -> Int
countBlocks = HM.size . HM.filter (\v -> v == Block)

score :: Displ -> Int
score displ = theScore $ displ HM.! (-1,0)

gridDisplay :: Displ -> [[Char]]
gridDisplay tiles =
  [[ tidToChar $ getTid (x,y) tiles | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    (minX, maxX) = (\k -> (minimum k, maximum k)) . fmap fst . HM.keys $ tiles
    (minY, maxY) = (\k -> (minimum k, maximum k)) . fmap snd . HM.keys $ tiles

getTid :: Tile -> Displ -> Tid
getTid tile tiles = case HM.lookup tile tiles of
   Nothing -> Empty
   Just t -> t

--
-- The state that matters
--
data Game = Game {
  gameProg :: IC.Prog,
  gameDisplay :: Displ
} deriving(Show)

newGame :: [Int] -> Game
newGame code = Game {
  gameProg = IC.newProg code [],
  gameDisplay = HM.empty
}

--
-- Functions to run game
--
data LiveData = LiveData {
  xb::Int,
  xp::Int,
  sc::Int,
  bl::Int
} deriving(Show)

stepGame' :: [Int] -> Game -> (LiveData, Game)
stepGame' inp game = (LiveData{xb=xb',xp=xp',sc=sc',bl=bl'}, game')
  where
    prog = IC.runProg (IC.setInputProg inp (gameProg game))
    displ = editDisplay (gameDisplay game) . buildCommands . reverse . IC.output $ prog
    sc' = score displ
    bl' = countBlocks displ
    xp' = xPaddle displ
    xb' = xBall displ
    game' = Game{gameProg=(IC.scrubOutput prog), gameDisplay=displ}

stepGame :: [Int] -> State Game LiveData
stepGame = state . stepGame'

joystick :: Int -> Int -> Int
joystick ballx paddlex
  | ballx > paddlex = 1
  | ballx == paddlex = 0
  | ballx < paddlex = -1
joystick _ _ = 0

playGame :: State Game Int
playGame = playGame' []

playGame' :: [Int] -> State Game Int
playGame' inp = do
  liveData <- stepGame inp
  game <- get
  case IC.progState (gameProg game) of
    IC.AwaitInput -> case bl liveData of
      0 -> return $ sc liveData
      _ -> playGame' [joystick (xb liveData) (xp liveData)]
    _ -> return $ sc liveData

