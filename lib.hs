module Lib where
import Types
import Prelude
import Data.List (delete)
import Utils 
import System.Random
import Data.Char (toLower)


{-
    Takes the current state of the world and a command to be executed and 
    calls a function that executes the given command. Returns the updated version of the world and a status about 
    the result of the exection of the command.
-}
executeCommand::World -> Command -> (World,WorldUpdateResult)
executeCommand world command =
    case command of
        GoTo (EntityId 11) -> (updateCurrentRoom world (EntityId 11),SpecialEncounter)
        GoTo (EntityId 14) -> (updateCurrentRoom world (EntityId 14),End)
        GoTo (EntityId command)-> (updateCurrentRoom world (EntityId command),Continue)
        Use (EntityId command) -> (useItem world (EntityId command),Continue)
        Drop (EntityId command) -> (dropItem world (EntityId command),Continue)
        Fight (EntityId command) -> if EntityId command `notElem` roomPeople (snd (currentRoom world))
            then (world,GameError)
            else (world,InitiateFight (EntityId command))
        Help -> (world,Continue)
        Inventory -> (world,Continue)
        Quit -> (world,End)
        History -> (world,Continue)
        LookAt (EntityId command)-> (world,Continue)
        See (EntityId command) -> (world,Continue)
        DefaultCommand -> (world,GameError)

{-
    Takes the current state of the world and input string , calls a function to parse the string decided by the type
    of command that is needed and returns an object of type command.
-}
parseCommand::World -> String -> Command
parseCommand world input =
    case words loweredInput of
       ("go":"to":room) -> GoTo (parseRoom world room)
       ("go" : "in" : room) -> GoTo (parseRoom world room)
       ("pick":"up":item) -> Use (parseItem world item) 
       ("pick":item) -> Use (parseItem world item) 
       ("take":item) -> Use (parseItem world item)
       ("use" : item) -> Use (parseItem world item)
       ("equip": item) -> Use (parseItem world item)
       ("drop" : item) -> Drop (parseItem world item)
       ("unequip" : item) -> Drop (parseItem world item)
       ("see":item) -> See (parseItem world item)
       ("look":"up":item) -> See (parseItem world item)
       ("fight": person) -> Fight (parsePerson world person)
       ("hit": person) -> Fight (parsePerson world person)
       ("look":"at":person) -> LookAt (parsePerson world person)
       ["inventory"] -> Inventory
       ["history"] -> History
       ["help"] -> Help
       ["quit"] -> Quit
       ["end"] -> Quit
       _ -> DefaultCommand
    where loweredInput = lowerString input


{-
    Same logic applies here as parseCommand function. This one is used in the specialEncounterLoop.
-}
parseSpecialCommand::World -> String -> Command
parseSpecialCommand world input =
    case words loweredInput of
        ("break":"door":"with":item) -> Use (parseItem world item)
        ("break":"lock":"with":item) -> Use (parseItem world item)
        ("use":item) -> Use (parseItem world item)
        ["lockpick","door"] -> Use (parseItem world ["lockpick"])
        ["inventory"] -> Inventory
        ["history"] -> History
        ["help"] -> Help
        ["quit"] -> Quit
        ["end"] -> Quit
        _ -> DefaultCommand
    where loweredInput = lowerString input

{-
    Takes an item and returns an status message about the state of the world and output string.
    Used in the special encounter loop to decide if the item used in the command is the correct one.
-}
evaluateSpecialCommand :: Item -> (WorldUpdateResult, String)
evaluateSpecialCommand  item
 | itemName item == "Axe" = (End,"You break open the door with your axe!")
 | itemName item == "Lockpick" = (End,"You manage to open the door with the lockpick you got from the thief!")
 | otherwise = (Continue,"This doesn't work!")



{-
    Takes a world object and a list of strings. Returns the EntityId of the first item whose itemName matches the strings.
    Returns a default value otherwise.
-}
parseItem::World -> [String] -> EntityId Item
parseItem world item =
    if null filtered
        then defaultEntityID
        else head filtered
    where filtered = keyListByName (unwords item) itemName (allItems world)
          
{-
    Same logic as parseItem but for Person objects.
-}
parsePerson::World -> [String] -> EntityId Person
parsePerson world person = 
    if null filtered
        then defaultEntityID
        else head filtered
    where filtered = keyListByName (unwords person) personName (worldPeople world)
{-
    Same logic as parseItem but for Room objects.
-}
parseRoom :: World -> [String] -> EntityId Room
parseRoom world room =
    if null filtered
        then defaultEntityID
        else head filtered
    where filtered = keyListByName (unwords room) roomName (worldRooms world)
            
{-
    Receives a list of Items and returns a concatenation of their itemNames
-}
getItemsString :: [Item] -> String
getItemsString [] = ""
getItemsString (x:xs) = itemName x ++ ", " ++ getItemsString xs

{-
    Receives a list of People and returns a concatenation of their personNames
-}
getPeopleString :: [Person] -> String
getPeopleString [] = ""
getPeopleString (x:xs) = personName x ++ ", " ++ getPeopleString xs

{-
    Receives a lis of Rooms and returns a concatenation of their roomNames
-}
getRoomString :: [Room] -> String
getRoomString [] = ""
getRoomString (x:xs) = roomName x ++ ", " ++ getRoomString xs

{-
    Receives a Room object and World object. Returns an output string about
    the contents of the room which are taken from the World object.
-}
renderRoom::Room -> World -> String
renderRoom room world =
    unlines [roomName room , 
    ">---------------------------------------------<",
    roomDescription room,
    ">---------------------------------------------<",
    "Items: " ++ getItemsString items,
    "People: " ++ getPeopleString peopleInRoom,
    "You can go to: " ++ getRoomString exits]
    where items = map (\x -> searchByKey x (allItems world)) (roomItems room) 
          peopleInRoom = map (\x -> searchByKey x (worldPeople  world)) (roomPeople room)
          exits = map (\x -> searchByKey x (worldRooms world)) (roomOtherRooms room)


{-
    Receives World object and id of a Room and updates the currentRoom field of the World object.
    The function does not update the world and instead returns the object in its original form if:
        - id is the default value
        - id of the new room is not in the list of the rooms that you can go to (roomOtherRoom)
        - There is a person in the current Room roomPeople list (You have to defeat it first.)
-}
updateCurrentRoom :: World -> EntityId Room -> World
updateCurrentRoom world id 
 | id == defaultEntityID = world
 | id `notElem` roomOtherRooms currRoom = world
 | not (null (roomPeople currRoom)) = world
 | otherwise = World (worldRooms world) (allItems world) (worldPeople world) (id,newRoom) (worldhero world)
    where newRoom = searchByKey id (worldRooms world)
          currRoom = snd (currentRoom world)

{-
    Receives current state of the world and EntityId of the target item. Returns an updated version of the World in which
    the item is removed from the room(via removeItemRoom function and updateRoomInList function) , 
    its effects are applied on the hero(via the applyItemOnHero funtion).
    The function returns the original state of the world if:
        - id of the target item is default value
        - id is not in the roomItems of the currentRoom
        - id is already in the inventory of the hero
        - adding the item to the inventory would make the number of items from the same tag as the target item , more than 2
-}
useItem::World -> EntityId Item -> World
useItem world id 
 | id == defaultEntityID = world
 | id `notElem` visibleItems = world
 | id `elem` inventory = world
 | itemType item == Power && fst (itemCounters (worldhero world)) >= 2 = world
 | itemType item == Defence && snd (itemCounters (worldhero world)) >= 2 = world
 | otherwise = World (updateRoomInList (worldRooms world) updatedRoom) (allItems world) (worldPeople world)  updatedRoom updatedHero
    where item = searchByKey id (allItems world) 
          visibleItems = roomItems (snd (currentRoom world))
          inventory = heroInventory (worldhero world)
          updatedRoom = (fst (currentRoom world),removeItemRoom (snd (currentRoom world)) id)
          updatedHero = applyItemOnHero (worldhero world) item id
{-
    Receives a World object and an EntityId of an item and returns an updated 
    world in which the item is removed from the hero inventory (via removeItemHero function) 
    and added to the roomItems list of the current room(via addItemRoom function). 
    The function returns the original version of the world if:
        - id is default value
        - id is not a member of the heroInventory list
-}
dropItem::World -> EntityId Item -> World
dropItem world id 
 | id == defaultEntityID = world
 | id `notElem` inventory = world
 | otherwise = World updatedRooms (allItems world) (worldPeople world) updatedRoom updatedHero
    where item = searchByKey id (allItems world)
          inventory = heroInventory (worldhero world)
          updatedHero = removeItemHero (worldhero world) item id
          updatedRoom = (fst (currentRoom world),addItemRoom (snd (currentRoom world)) id)
          updatedRooms = updateRoomInList (worldRooms world) updatedRoom

{-
    Receives a Hero object, an Item object and an id of an item and returns an updated version of the hero where
    the item is no longer a member of its inventory and its stats are substracted from the total stats of the hero.
-}
removeItemHero :: Hero -> Item -> EntityId Item -> Hero
removeItemHero hero item id
 | itemType item == Power = Hero (heroName hero) newStats (fst (itemCounters hero) - 1 , snd (itemCounters hero)) newInventory
 | itemType item == Defence = Hero (heroName hero) newStats (fst (itemCounters hero)  , snd (itemCounters hero) - 1) newInventory
 | otherwise = Hero (heroName hero) (heroStats hero) (itemCounters hero) newInventory
    where newStats = tupleSubstract (heroStats hero) (itemStats item)
          newInventory = delete id (heroInventory hero) 
{-
    Takes a Room object and EntityId of an item and returns an updated version of the room in which the item is not a member of the roomItems list.
    Returns the original Room object if it doesn't contain the item.
-}
removeItemRoom:: Room -> EntityId Item -> Room
removeItemRoom room item =
    if item `notElem` roomItems room
        then room
        else Room (roomName room) (roomDescription room) updatedItems (roomPeople room) (roomOtherRooms room)
    where updatedItems = delete item (roomItems room) 


{-
    Adds an EntityId of an Item to the roomItems list of a Room
-}
addItemRoom::Room -> EntityId Item -> Room
addItemRoom (Room name desc items people exits) id = Room name desc (id:items) people exits

{-
    Takes a World object and returns an output string of the concatenation of the itemNames in the hero inventory
-}
displayInventory::World -> String
displayInventory world =
    getItemsString inventory
    where inventory = map  (\x -> searchByKey x (allItems  world)) (heroInventory (worldhero world)) 
        
{-
    Updates Room object in a list of (room id , room)
-}
updateRoomInList::[(EntityId Room,Room)] -> (EntityId Room,Room) -> [(EntityId Room,Room)]
updateRoomInList [] pair = []
updateRoomInList (x:xs) pair =
    if fst pair == fst x
        then pair:updateRoomInList xs pair
        else x:updateRoomInList xs pair 

{-
    Takes a Hero object , an Item object and an id of the item. If itemType is Health then updates hero stats and consumes the item.
    If its Power or Defence , updates hero stata and adds to the item counter and inventory. Otherwise it just adds to the inventory.
    Returns an updated version of the Hero object.
-}
applyItemOnHero::Hero -> Item  -> EntityId Item -> Hero
applyItemOnHero hero item id 
 | itemType item == Health = Hero (heroName hero) newStats (itemCounters hero) (heroInventory hero)
 | itemType item == Power = Hero (heroName hero) newStats (fst (itemCounters hero) + 1 , snd (itemCounters hero)) (id:heroInventory hero)
 | itemType item == Defence = Hero (heroName hero) newStats (fst (itemCounters hero) , snd (itemCounters hero) + 1) (id:heroInventory hero)
 | otherwise = Hero (heroName hero) (heroStats hero) (itemCounters hero) (id:heroInventory hero)
    where newStats = tupleAddition (heroStats hero) (itemStats item)


{-
    Takes a World object and returns an output string containing the heroStats and his inventory.
-}
displayHeroStats::World -> String
displayHeroStats world =
    unlines[heroName hero , 
             "Health: " ++ show (getHeroStat hero myFirst) ,
             "Power: " ++ show (getHeroStat hero mySecond) ,
             "Defence: " ++ show (getHeroStat hero myThird) ,  
             "Inventory: " ++ displayInventory world]
    where hero = worldhero world

{-
    Takes a World object and EntityId of Person and returns an output string containing personDescription. Returns a default message if:
        -id is default value
        -id is not a member of the roomPeople list of the currentRoom
-}
seePerson::World -> EntityId Person -> String
seePerson world id
 | id == defaultEntityID = "There is no such character"
 | id `notElem` roomPeople  (snd (currentRoom world)) = "Maybe he is in another room" 
 | otherwise = unlines [personName person,">---------------------------------------------<", 
                        personDescription person,">---------------------------------------------<"]
    where person = searchByKey id (worldPeople world)

{-
    Takes a World object and EntityId of Item and returns an output string containing itemDescription. Returns a default message if:
        -id is default value
        -id is not a member of the heroInventory list or the roomItems list of the current room
-}
seeItem::World -> EntityId Item -> String
seeItem world id 
 | id == defaultEntityID = "No such item."
 | id `notElem` heroInventory (worldhero world) && id `notElem` roomItems (snd (currentRoom world)) = "No such item."
 | otherwise = unlines [itemName item , itemDescription item]
    where item = searchByKey id (allItems world)



{-
    Returns an output string based on an bool value
-}
evaluateHit :: Bool -> String
evaluateHit expression =
  if expression
    then "\nHit succesful!"
    else "\nHit blocked!"

{-
    Takes a Int pair (dice rolls) and a (Hero,Person) pair and returns an output string based on the result of the fight.
-}
evaluateFight::(Int,Int) -> (Hero,Person) -> String
evaluateFight (heroRoll,enemyRoll) (hero,enemy) = 
  if hit >= 0 
    then rollMsg ++  "You attack " ++ personName enemy ++ " for " 
             ++ show hit ++ " damage" ++ evaluateHit (hit > getPersonStat enemy myThird)
    else rollMsg ++ "You were attacked by " ++ personName enemy ++ " for " ++ show (negate hit) 
          ++ " damage" ++ evaluateHit (negate hit> getHeroStat hero myThird)
  where rollMsg = "You rolled: " ++ show heroRoll ++ "\nThe enemy rolled: " ++ show enemyRoll ++ "\n"
        hit = heroRoll + getHeroStat hero mySecond - enemyRoll - getPersonStat enemy mySecond

{-
    Calls the randomRio function from System.Random to generate a 2 random numbers between 1 and 6.
    roll1 is the random number for the hero dice roll in combat.
    roll2 is the random number for the enemy dice roll in combat.
-}
getSingleDiceRoll::IO (Int,Int)
getSingleDiceRoll = do
    roll1 <- randomRIO (1,6)
    roll2 <- randomRIO (1,6)
    return (roll1,roll2)


{-
    Simulates a single round of the combat loop between the hero and the enemy. Receives a Hero , a Person and a dice roll (Int,Int) as arguments.
    Winner is decided by comparing heroPower + heroRoll and enemyPower + enemyRoll. The difference between the two sums is applied as damage to the loser of the battle.
    Returns a (Hero,Person) pair containing the updated objects after damage applying (via applyDmgHero or applyDmgPerson).
-}
singleRoundOfCombat :: Hero -> Person ->(Int,Int) -> (Hero,Person)
singleRoundOfCombat hero enemy roll = do
    let heroRoll = fst roll
        enemyRoll = snd roll
        result = (heroRoll + getHeroStat hero mySecond) - (enemyRoll + getPersonStat enemy mySecond)
    case result of
        result | result > 0 -> (hero , applyDmgPerson enemy result)
        0 -> (hero,enemy)
        _ -> (applyDmgHero hero (negate result),enemy)

{-
    Receives a Hero object and a integer and returns an updated Hero object with damage applied to the hero.
    If the damage is less than the defence of the hero , no damage is applied.
-}
applyDmgHero :: Hero -> Int -> Hero
applyDmgHero (Hero name (hp,power,def) (countPower,countDef) inventory) dmg =
    if dmg <= def
        then Hero name (hp,power,def) (countPower,countDef) inventory 
        else Hero name (hp - dmg , power , def) (countPower,countDef) inventory 
{-
    Same logic as previous function.
-}
applyDmgPerson :: Person -> Int -> Person
applyDmgPerson (Person name desc (hp,power,def)) dmg = 
    if dmg <= def
        then Person name desc (hp,power,def)
        else Person name desc (hp - dmg,power,def)

{-
    Receives a Room object and a EntityId of a Person and removes it from the roomPeople list
-}
removePersonRoom:: Room -> EntityId Person -> Room
removePersonRoom (Room name desc items people exits) id = Room name desc items (delete id people) exits

{-
    prints output string containing commands that the game recognises.
-}
printHelp::[String]
printHelp = 
    ["Type \"go to\" or \"go in\" + place to travel",
     "Type \"pick up\", \"pick\", \"take\", \"use\" or \"equip\" + an item to pick up and equip",
     "Type \"drop\" or \" unequip\" + an item in your inventory to remove it ",
     "Type \"fight\" or \" hit \" + person to fight a person in the room",
     "Type \"look at\" + person to gain additional info about the person",
     "Type \"see\" or \"look up\" + item to gain additional info about the item",
     "Type \"inventory\" to see your items",
     "Type \"history\" to see your previous commands",
     "Type \"quit\" or \"end\" to exit the game"]
    


