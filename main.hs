
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
 | getPersonStat (snd enemy) myFirst <= 0 = do
     putStrLn $ "You have slain " ++ personName (snd enemy)
     gameLoop updatedWorld ("Fight won by Hero!":history)
 | otherwise = do
    roll <- getSingleDiceRoll
    let fightResult = singleRoundOfCombat hero (snd enemy) roll
    evaluateFight roll fightResult
    fightLoop world (fst fightResult) (fst enemy ,snd fightResult) history
  where updatedWorld = World updatedWorldRooms (allItems world) (worldPeople world) updatedRoom hero
        updatedRoom = (fst(currentRoom world),removePersonRoom (snd (currentRoom world)) (fst enemy))
        updatedWorldRooms = updateRoomInList (worldRooms world) updatedRoom

evaluateFight::(Int,Int) -> (Hero,Person) -> IO()
evaluateFight roll (hero,enemy) = do
  let rollMsg = "You rolled: " ++ show (fst roll) ++ "\nThe enemy rolled: " ++ show (snd roll) ++ "\n"
      hit = fst roll + getHeroStat hero mySecond - snd roll - getPersonStat enemy mySecond
  if hit >= 0
    then do 
      let winnerMsg = rollMsg ++ "You attack " ++ personName enemy ++ " for " 
             ++ show hit ++ " damage"
      if hit > getPersonStat enemy myThird 
        then do 
          let finalMsg = winnerMsg ++ "\nHit succesful!"
          putStrLn finalMsg
        else do
          let finalMsg = winnerMsg ++ "\nHit blocked!"
          putStrLn finalMsg
    else do
      let oppositeHit = negate hit
          loserMsg = rollMsg ++ "You were attacked by " ++ personName enemy ++ " for " ++ show oppositeHit ++ " damage"
      if oppositeHit > getHeroStat hero myThird
        then do 
          let finalMsg = loserMsg ++ "\nHit succesful!" ++ "\n You have " ++ show (getHeroStat hero myFirst) ++ "health left"
          putStrLn finalMsg
        else do
          let finalMsg = loserMsg ++ "\nHit blocked!"
          putStrLn finalMsg

 

gameLoop :: World ->[String] -> IO ()
gameLoop world history = do
  putStrLn "What would you like to do ? \n"
  input <- getLine
  let command = parseCommand world input
  let (updatedWorld, result) = executeCommand world command   
  case result of
    End -> return ()
    GameError -> gameLoop world (input:history)
    InitiateFight (EntityId command) -> do
      let enemy = (EntityId command , searchByKey (EntityId command) (worldPeople updatedWorld))
      fightLoop updatedWorld (worldhero updatedWorld) enemy history
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
              putStrLn $ displayHeroStats updatedWorld
              putStrLn renderResult
            Drop (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              putStrLn $ displayHeroStats updatedWorld
              putStrLn renderResult
            See (EntityId command) -> do
              let renderResult = seePerson updatedWorld (EntityId command)
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