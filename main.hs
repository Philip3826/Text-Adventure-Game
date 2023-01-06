
import Prelude
import Types
import Src
import Lib
import Utils
import System.Console.ANSI



fightLoop::World -> Hero -> (EntityId Person,Person) -> [String] -> IO ()
fightLoop world hero enemy history
 | getHeroStat hero myFirst <= 0 = do
    putStrLn "You have died!\nGame Over!"
    return()
 | getPersonStat (snd enemy) myFirst <= 0 = do
     putStrLn $ "You have slain " ++ personName (snd enemy)
     gameLoop updatedWorld ("Fight won by Hero!":history)
 | otherwise = do
    roll <- getSingleDiceRoll
    let fightResult = singleRoundOfCombat hero (snd enemy) roll
    putStrLn $ evaluateFight roll fightResult ++ "\n"
    putStrLn $ show (fst fightResult) ++"\n" ++show (snd fightResult)
    fightLoop world (fst fightResult) (fst enemy ,snd fightResult) history
  where updatedWorld = World updatedWorldRooms (allItems world) (worldPeople world) updatedRoom hero
        updatedRoom = (fst(currentRoom world),removePersonRoom (snd (currentRoom world)) (fst enemy))
        updatedWorldRooms = updateRoomInList (worldRooms world) updatedRoom


testRoll = do
  roll1 <- getLine
  roll2 <- getLine
  let hroll = read roll1::Int
      eroll = read roll2::Int
  return (hroll,eroll)

gameLoop :: World -> [String] -> IO ()
gameLoop world history = do
  putStrLn "What would you like to do ?"
  input <- getLine
  putStrLn "\n"
  let command = parseCommand world input
  let (updatedWorld, result) = executeCommand world command   
  case result of
    End -> return ()
    GameError -> do
      putStrLn "You can't do that!" 
      gameLoop world (input:history)
    InitiateFight (EntityId command)  -> do
      let enemy = (EntityId command , searchByKey (EntityId command) (worldPeople updatedWorld))
      fightLoop updatedWorld (worldhero updatedWorld) enemy history
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
              putStrLn $ displayInventory updatedWorld
            Use (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              if EntityId command == defaultEntityID
                then putStrLn $ "No such item...\n" ++ renderResult
                else putStrLn $ displayInventory updatedWorld ++ renderResult
            Drop (EntityId command) -> do
              let renderResult = renderRoom (snd (currentRoom updatedWorld)) updatedWorld
              putStrLn $ displayHeroStats updatedWorld
              putStrLn renderResult
            See (EntityId command) -> do
              let renderResult = seePerson updatedWorld (EntityId command)
              putStrLn renderResult
            History -> do
              putStrLn $ unlines history
            Help -> do
              putStrLn $ unlines printHelp
        gameLoop updatedWorld (input:history)

--TODO :: add a way to clear the screen

main::IO()
main = do
    putStrLn "Welcome to .........\n"
    putStrLn $ renderRoom (snd (currentRoom initialWorld)) initialWorld
    gameLoop initialWorld []