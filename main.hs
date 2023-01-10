import Prelude
import Types
import Src
import Lib
import Utils
import System.Console.ANSI
import Entities
 

specialRoomLoop::World -> [String] -> Int -> IO()
specialRoomLoop world history 0 = do
  putStrLn "The time has expired!\nGame Over!"
  return ()
specialRoomLoop world history n = do
  putStrLn $ "You have " ++ show n ++ " minutes left!"
  putStrLn "What would you like to do ?"
  input <- getLine
  let command = parseSpecialCommand world input
  case command of
    Quit -> return ()
    DefaultCommand -> do 
      putStrLn  "This doesn't work!"
      specialRoomLoop world (input:history) (n - 1)
    History ->  do 
      putStrLn $ unlines history
      specialRoomLoop world (input:history) n
    Help -> do 
      putStrLn $ unlines printHelp
      specialRoomLoop world (input:history) n
    Inventory -> do
       putStrLn "Inventory : "
       putStrLn $ displayInventory world
       specialRoomLoop world (input:history) n
    Use (EntityId command) -> do
      if EntityId command `notElem` heroInventory (worldhero world)
        then do
          putStrLn "You do not have this!"
          specialRoomLoop world (input:history) (n - 1)
        else do  
          let item = searchByKey (EntityId command) (allItems world)
              result = evaluateSpecialCommand item
          putStrLn (snd result)
          if fst result == End
            then gameLoop world (input:history)
            else specialRoomLoop world (input:history) (n - 1)

fightLoop::World -> Hero -> (EntityId Person,Person) -> [String] -> IO ()
fightLoop world hero enemy history
 | getHeroStat hero myFirst <= 0 = do
    putStrLn "You have died!\nGame Over!"
    return()
 | getPersonStat (snd enemy) myFirst <= 0 = do
     putStrLn $ "You have slain " ++ personName (snd enemy)
     if fst enemy == EntityId 101
      then do
        putStrLn $ renderRoom (snd (currentRoom addedItemWorld)) updatedWorld
        gameLoop addedItemWorld ("Fight won by Hero!":history) 
      else do
        putStrLn $ renderRoom (snd (currentRoom updatedWorld)) updatedWorld
        gameLoop updatedWorld ("Fight won by Hero!":history)
 | otherwise = do
    roll <- getSingleDiceRoll
    let fightResult = singleRoundOfCombat hero (snd enemy) roll
    putStrLn $ evaluateFight roll fightResult ++ "\n"
    fightLoop world (fst fightResult) (fst enemy ,snd fightResult) history
  where addedItemWorld = World updatedWorldRooms (allItems world) (worldPeople world) newItemRoom hero
        updatedWorld = World updatedWorldRooms (allItems world) (worldPeople world) updatedRoom hero
        updatedRoom = (fst(currentRoom world),removePersonRoom (snd (currentRoom world)) (fst enemy))
        newItemRoom = (fst (currentRoom world), addItemRoom (snd updatedRoom) (EntityId 100))
        updatedWorldRooms = updateRoomInList (worldRooms world) updatedRoom
        addedItemUpdateWorldRoom = updateRoomInList (worldRooms world) newItemRoom




gameLoop :: World -> [String] -> IO ()
gameLoop world history = do
  putStrLn "What would you like to do ?"
  input <- getLine
  putStrLn "\n"
  let command = parseCommand world input
  let (updatedWorld, result) = executeCommand world command   
  case result of
    End -> do
      if fst (currentRoom updatedWorld) == EntityId 14
        then do
          putStrLn $ renderRoom (snd (currentRoom updatedWorld)) updatedWorld
          putStrLn "Game  Over!"
        else  putStrLn "Game  Over!"
      return ()
    GameError -> do
      putStrLn "You can't do that!" 
      gameLoop world (input:history)
    InitiateFight (EntityId command)  -> do
      let enemy = (EntityId command , searchByKey (EntityId command) (worldPeople updatedWorld))
      fightLoop updatedWorld (worldhero updatedWorld) enemy history
    SpecialEncounter -> do
      let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
      putStrLn  renderResult
      if world == updatedWorld
        then do
          putStrLn $ "You can't go there!\n"
          gameLoop updatedWorld (input:history)
        else do
          specialRoomLoop updatedWorld (input:history) 10
    Continue -> do
        case command of
            GoTo (EntityId command) -> do
                let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
                    roomBeforeAction = fst (currentRoom world)
                    roomAfterAction = fst (currentRoom updatedWorld)
                if roomBeforeAction == roomAfterAction
                  then do
                    putStrLn $ "You can't go there!\n" ++ renderResult
                  else
                    putStrLn  renderResult
                    
            Inventory -> do
              putStrLn "Inventory : "
              putStrLn $ displayInventory updatedWorld ++ "\n"

            Use (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              if EntityId command == defaultEntityID
                then putStrLn $ "No such item...\n" ++ renderResult
                else do 
                  putStrLn $ displayHeroStats updatedWorld ++ renderResult ++ "\n"

            Drop (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              putStrLn $ displayHeroStats updatedWorld
              putStrLn renderResult

            LookAt (EntityId command) -> do
              let renderResult = seePerson updatedWorld (EntityId command)
              putStrLn renderResult

            See (EntityId command) -> do
              let renderResult = seeItem updatedWorld (EntityId command)
              putStrLn renderResult

            History -> do
              putStrLn $ unlines history

            Help -> do
              putStrLn $ unlines printHelp

        gameLoop updatedWorld (input:history)

--TODO :: add a way to clear the screen

main::IO()
main = do
    putStrLn "Welcome to The Gladiator Tournament\n"
    putStrLn $ renderRoom (snd (currentRoom initialWorld)) initialWorld
    gameLoop initialWorld []