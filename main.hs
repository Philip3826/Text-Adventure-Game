import Prelude
import Types
import  TestStuff
import Lib

gameLoop :: World -> IO ()
gameLoop world = do
  putStrLn "What would you like to do ? \n"
  input <- getLine
  let command = parseCommand world input
  let (updatedWorld, result) = executeCommand world command  
  case result of
    End -> return ()
    GameError -> gameLoop world 
    Continue -> do
        case command of
            GoTo (EntityId command) -> do
                let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
                putStrLn renderResult
            Inventory -> do
              putStrLn "Inventory : "
              putStrLn $ displayInventory updatedWorld
            Take (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              putStrLn renderResult
        gameLoop updatedWorld

--TODO :: add a way to clear the screen

main::IO()
main = do
    putStrLn "Welcome to .........\n"
    putStrLn $ renderRoom (snd (currentRoom initial)) initial
    gameLoop initial
    