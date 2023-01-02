
import Prelude
import Types
import  TestStuff
import Lib
import Utils


fightLoop::World -> Hero -> (EntityId Person,Person) -> [String] -> IO ()
fightLoop world hero enemy history
 | getHeroStat hero myFirst <= 0 = do
    putStrLn "Game Over!"
    return()
 | getPersonStat (snd enemy) myFirst <= 0 = gameLoop updatedWorld ("Fight won by Hero!":history)
 | otherwise = do
    roll <- getSingleDiceRoll
    let fightResult = singleRoundOfCombat hero (snd enemy) roll
    print roll
    print (fst fightResult)
    print (snd fightResult)
    fightLoop world (fst fightResult) (fst enemy ,snd fightResult) history
  where updatedWorld = World (worldRooms world) (allItems world) (worldPeople world) (fst(currentRoom world),removePersonRoom (snd (currentRoom world)) (fst enemy)) hero

gameLoop :: World ->[String] -> IO ()
gameLoop world history = do
  putStrLn "What would you like to do ? \n"
  input <- getLine
  let command = parseCommand world input
  let (updatedWorld, result) = executeCommand world command   
  case result of
    End -> return ()
    GameError -> gameLoop world (input:history)
    -- InitiateFight -> do
    --     putStrLn "Initiating fight with" ++ 
    Continue -> do
        case command of
            GoTo (EntityId command) -> do
                let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
                putStrLn renderResult
            Inventory -> do
              putStrLn "Inventory : "
              putStrLn $ displayInventory updatedWorld
            Use (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              putStrLn renderResult
            History -> do
              putStrLn $ unlines history  
        gameLoop updatedWorld (input:history)

--TODO :: add a way to clear the screen

main::IO()
main = do
    putStrLn "Welcome to .........\n"
    putStrLn $ renderRoom (snd (currentRoom initial)) initial
    gameLoop initial []