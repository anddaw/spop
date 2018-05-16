module Interface where

import Data.Char
import Data.List
import Board
import System.IO
import System.Exit

initialBoard = Board
  (Wolf (0,7))
  [
    (Sheep (1,0)),
    (Sheep (3,0)),
    (Sheep (5,0)),
    (Sheep (7,0))
  ]

splitCommand :: String -> (String, String)
splitCommand cmd =
  (\(x,y) -> (x, dropWhile (==' ') y)) $ span (/=' ') cmd

readAndApplyCommand :: Board -> IO ()
readAndApplyCommand b = do
  putStr $ "\n" ++ (show b) ++ "\n> "
  hFlush stdout
  input <- getLine
  let
    (cmd, args) = splitCommand input
    in
    case cmd of
      "load" -> doLoad args b
      "save" -> doSave args b
      "move" -> doMove args b
      "help" -> displayHelp b
      "restart" -> readAndApplyCommand initialBoard
      "quit" -> exitSuccess
      _ -> wrongInput input b
  

doLoad :: [Char] -> Board -> IO ()
doLoad c b = do
  putStrLn $ "LOADING: " ++ c ++ "\n"
  -- TODO
  readAndApplyCommand b

doSave :: [Char] -> Board -> IO ()
doSave c b = do
  putStrLn "SAVING"
  -- TODO
  readAndApplyCommand b

wrongInput :: [Char] -> Board -> IO ()
wrongInput i b = do
  putStrLn $ "Wrong input: " ++ i
  readAndApplyCommand b


doMove :: [Char] -> Board -> IO ()
doMove c b = do
  if length(ws) == 2 && all isDigit (ws !! 0) && (ws !! 1) `elem` ["l", "r"]
    then doMoveSheep (dir $ ws !! 1) (read (ws !! 0) :: Int) b
    else do
    putStrLn "Wrong move"
    readAndApplyCommand b
    where
      ws = words c

doMoveSheep :: Direction -> Int -> Board -> IO ()
doMoveSheep d i b = do
  if b' /= b
    then doMoveWolf b'
    else do
    putStrLn "Move not possible"
    readAndApplyCommand b
    where
      b' = moveSheep d i b

doMoveWolf :: Board -> IO ()
doMoveWolf b = do
  -- TODO
  checkResult b
  
checkResult :: Board -> IO ()
checkResult b = do
  case res of
    Unconcluded -> readAndApplyCommand b
    SheepWon -> displayResult "SHEEP WON!"
    WolfWon -> displayResult "WOLF WON!"
    where
      res = result b
    
displayResult :: String -> IO ()
displayResult r  = do
  putStrLn r
  putStrLn "Press enter to play again"
  _ <- getLine
  readAndApplyCommand initialBoard

displayHelp :: Board -> IO ()
displayHelp b = do
  putStrLn $ "move SHEEP_NB [l|r] - move nth sheep to left/right\n" ++
    "load FILE - load game state\n" ++
    "save FILE - save game state\n" ++
    "quit - exit game\n" ++
    "restart - restart game to initial state\n" ++
    "help - display this message\n\n" ++
    "press enter to continue"
  _ <- getLine
  readAndApplyCommand b
