module TicTacToe.Solver (bestMove, Marker(..)) where

import Data.List
import Data.List.Index
import Data.Maybe
import Data.Tuple
import Data.Ord
import Control.Lens

data Marker = N | X | O
  deriving(Eq)

type Board = [Marker]

data GameState = WinX | WinO | Draw | InPlay
  deriving(Eq)

gameStateFor (O:O:O:_:_:_:_:_:_:[]) = WinO 
gameStateFor (_:_:_:O:O:O:_:_:_:[]) = WinO 
gameStateFor (_:_:_:_:_:_:O:O:O:[]) = WinO 
gameStateFor (O:_:_:O:_:_:O:_:_:[]) = WinO 
gameStateFor (_:O:_:_:O:_:_:O:_:[]) = WinO 
gameStateFor (_:_:O:_:_:O:_:_:O:[]) = WinO 
gameStateFor (_:_:O:_:O:_:O:_:_:[]) = WinO 
gameStateFor (O:_:_:_:O:_:_:_:O:[]) = WinO 

gameStateFor (X:X:X:_:_:_:_:_:_:[]) = WinX 
gameStateFor (_:_:_:X:X:X:_:_:_:[]) = WinX 
gameStateFor (_:_:_:_:_:_:X:X:X:[]) = WinX 
gameStateFor (X:_:_:X:_:_:X:_:_:[]) = WinX 
gameStateFor (_:X:_:_:X:_:_:X:_:[]) = WinX 
gameStateFor (_:_:X:_:_:X:_:_:X:[]) = WinX 
gameStateFor (_:_:X:_:X:_:X:_:_:[]) = WinX 
gameStateFor (X:_:_:_:X:_:_:_:X:[]) = WinX 

gameStateFor board = if noEmptySpacesRemainingOn board then Draw else InPlay

noEmptySpacesRemainingOn board = isNothing $ elemIndex N $ nub board

isPieceAt index marker board = board !! (index-1) == O

placeMarkerAt position marker board = setAt (position-1) marker board

data ScoredMove = Safe Int Board | Lose Int | Win Int  

aWinningMove (Win _) = True
aWinningMove (_) = False

aLosingMove (Lose _) = True
aLosingMove (_) = False

thePosition (Lose p) = p
thePosition (Win p) = p
thePosition (Safe p _) = p

_bestMove marker board = fmap thePosition $ if isNothing losingMove then winningMove else losingMove
  where 
    losingMove = find aLosingMove scoredMoves
    winningMove = find aWinningMove scoredMoves
    indexedMoves = zip [1,2..] board
    scoredMoves = map scoreMove indexedMoves 
    scoreMove (index, _) = let newBoard = (placeMarkerAt index O board)
                               otherPlayerBoard = (placeMarkerAt index X board)
                               winState = gameStateFor newBoard
                               winStateForOtherPlayer = gameStateFor otherPlayerBoard
                           in if winStateForOtherPlayer == WinX then Lose index else (
                             if winState == WinO then Win index
                             else Safe index newBoard
                           )

bestMove :: Marker -> Board -> Maybe Int
bestMove marker board =
  if gameStateFor board == Draw
    then Nothing
    else _bestMove marker board