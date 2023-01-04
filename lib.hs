module Lib where
import Types
import Prelude
import Data.List (delete)
import Utils 
import System.Random


executeCommand::World -> Command -> (World,WorldUpdateResult)
executeCommand world command =
    case command of 
        GoTo (EntityId command)-> (updateCurrentRoom world (EntityId command),Continue)
        Use (EntityId command) -> (useItem world (EntityId command),Continue)
        Drop (EntityId command) -> (dropItem world (EntityId command),Continue)
        Fight (EntityId command) -> if EntityId command `notElem` roomPeople (snd (currentRoom world))
            then (world,GameError)
            else (world,InitiateFight (EntityId command))
        Inventory -> (world,Continue)
        Quit -> (world,End)
        History -> (world,Continue)
        See (EntityId command)-> (world,Continue)
        DefaultCommand -> (world,GameError)


parseCommand::World -> String -> Command
parseCommand world input =
    case words input of
       ("go":"to":room) -> GoTo (parseRoom world room)
       ("go" : "in" : room) -> GoTo (parseRoom world room)
       ("pick":"up":item) -> Use (parseItem world item) 
       ("pick":item) -> Use (parseItem world item) 
       ("take":item) -> Use (parseItem world item)
       ("use" : item) -> Use (parseItem world item)
       ("equip": item) -> Use (parseItem world item)
       ("drop" : item) -> Drop (parseItem world item)
       ("Unequip" : item) -> Drop (parseItem world item)
       ("fight": person) -> Fight (parsePerson world person)
       ("hit": person) -> Fight (parsePerson world person)
       ("see":person) -> See (parsePerson world person)
       ["inventory"] -> Inventory
       ["history"] -> History
       ["quit"] -> Quit
       ["end"] -> Quit
       _ -> DefaultCommand
       
   

parseItem::World -> [String] -> EntityId Item
parseItem world item =
    if null filtered
        then defaultEntityID
        else fst (head filtered)
    where filtered = filter (\x -> words (itemName (snd x)) ==  item) items
            where items = allItems world

parsePerson::World -> [String] -> EntityId Person
parsePerson world person = 
    if null filtered
        then defaultEntityID
        else fst (head filtered)
    where filtered = filter (\x -> words (personName (snd x)) == person) (worldPeople world)

parseRoom :: World -> [String] -> EntityId Room
parseRoom world room =
    if null filtered
        then defaultEntityID
        else fst (head filtered)
    where filtered = filter (\x -> words (roomName (snd x)) == room) (worldRooms world)
            

getItemsString :: [Item] -> String
getItemsString [] = ""
getItemsString (x:xs) = itemName x ++ ", " ++ getItemsString xs

getPeopleString :: [Person] -> String
getPeopleString [] = ""
getPeopleString (x:xs) = personName x ++ ", " ++ getPeopleString xs

getRoomString :: [Room] -> String
getRoomString [] = ""
getRoomString (x:xs) = roomName x ++ ", " ++ getRoomString xs

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



updateCurrentRoom :: World -> EntityId Room -> World
updateCurrentRoom world room 
 | room == defaultEntityID = world
 | room `notElem` roomOtherRooms ( snd (currentRoom world)) = world
 | otherwise = World (worldRooms world) (allItems world) (worldPeople world) newRoom (worldhero world)
    where newRoom = head (filter (\x -> fst x == tmp) (worldRooms world))
            where tmp = head ( filter (\x -> x == room) (roomOtherRooms (snd (currentRoom world))))


useItem::World -> EntityId Item -> World
useItem world id 
 | id == defaultEntityID = world
 | id `notElem` visibleItems = world
 | id `elem` inventory = world
 | otherwise = World (updateRoomInList (worldRooms world) updatedRoom) (allItems world) (worldPeople world)  updatedRoom updatedHero
    where item = searchByKey id (allItems world) 
          visibleItems = roomItems (snd (currentRoom world))
          inventory = heroInventory (worldhero world)
          updatedRoom = (fst (currentRoom world),removeItemRoom (snd (currentRoom world)) id)
          updatedHero = applyItemOnHero (worldhero world) item id

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


removeItemHero :: Hero -> Item -> EntityId Item -> Hero
removeItemHero hero item id 
 | itemType item == Power = Hero (heroName hero) newStats (fst (itemCounters hero) - 1 , snd (itemCounters hero)) newInventory
 | itemType item == Defence = Hero (heroName hero) newStats (fst (itemCounters hero)  , snd (itemCounters hero) - 1) newInventory
 | otherwise = hero
    where newStats = tupleSubstract (heroStats hero) (itemStats item)
          newInventory = delete id (heroInventory hero) 

removeItemRoom:: Room -> EntityId Item -> Room
removeItemRoom room item =
    if item `notElem` roomItems room
        then room
        else Room (roomName room) (roomDescription room) updatedItems (roomPeople room) (roomOtherRooms room)
    where updatedItems = delete item (roomItems room) 

addItemRoom::Room -> EntityId Item -> Room
addItemRoom (Room name desc items people exits) id = Room name desc (id:items) people exits


displayInventory::World -> String
displayInventory world =
    getItemsString inventory
    where inventory = map  (\x -> searchByKey x (allItems  world)) (heroInventory (worldhero world)) 
        

updateRoomInList::[(EntityId Room,Room)] -> (EntityId Room,Room) -> [(EntityId Room,Room)]
updateRoomInList [] pair = []
updateRoomInList (x:xs) pair =
    if fst pair == fst x
        then pair:updateRoomInList xs pair
        else x:updateRoomInList xs pair 


applyItemOnHero::Hero -> Item  -> EntityId Item -> Hero
applyItemOnHero hero item id 
 | itemType item == Health = Hero (heroName hero) (newHealth, newPower, newDefence) (itemCounters hero) (delete id (heroInventory hero))
 | itemType item == Power = if fst (itemCounters hero) >= 2 
        then hero
        else Hero (heroName hero) (newHealth, newPower, newDefence) (fst (itemCounters hero) + 1 , snd (itemCounters hero)) (id:heroInventory hero)
 | otherwise = if snd (itemCounters hero) >= 2
        then hero
        else Hero (heroName hero) (newHealth, newPower, newDefence) (fst (itemCounters hero) , snd (itemCounters hero) + 1) (id:heroInventory hero)
    where newHealth = getHeroStat hero myFirst + getItemStat item myFirst
          newPower = getHeroStat hero mySecond + getItemStat item mySecond
          newDefence = getHeroStat hero myThird + getItemStat item myThird



displayHeroStats::World -> String
displayHeroStats world =
    unlines[heroName hero , 
             "Health: " ++ show (getHeroStat hero myFirst) ,
             "Power: " ++ show (getHeroStat hero mySecond) ,
             "Defence: " ++ show (getHeroStat hero myThird) ,  
             displayInventory world]
    where hero = worldhero world

seePerson::World -> EntityId Person -> String
seePerson world id
 | id == defaultEntityID = "There is no such character"
 | id `notElem` roomPeople  (snd (currentRoom world)) = "Maybe he is in another room" 
 | otherwise = unlines [personName person, personDescription person]
    where person = searchByKey id (worldPeople world)

getSingleDiceRoll::IO (Int,Int)
getSingleDiceRoll = do
    roll1 <- randomRIO (1,6)
    roll2 <- randomRIO (1,6)
    return (roll1,roll2)



singleRoundOfCombat :: Hero -> Person ->(Int,Int) -> (Hero,Person)
singleRoundOfCombat hero enemy roll = do
    let heroRoll = fst roll
        enemyRoll = snd roll
        result = (heroRoll - getHeroStat hero mySecond) - (enemyRoll - getPersonStat enemy mySecond)
    case result of
        result | result > 0 -> (hero , applyDmgPerson enemy result)
        0 -> (hero,enemy)
        _ -> (applyDmgHero hero (negate result),enemy)

applyDmgHero :: Hero -> Int -> Hero
applyDmgHero (Hero name (hp,power,def) (countPower,countDef) inventory) dmg =
    if dmg <= def
        then Hero name (hp,power,def) (countPower,countDef) inventory 
        else Hero name (hp - dmg , power , def) (countPower,countDef) inventory 

applyDmgPerson :: Person -> Int -> Person
applyDmgPerson (Person name desc (hp,power,def)) dmg = 
    if dmg <= def
        then Person name desc (hp,power,def)
        else Person name desc (hp - dmg,power,def)


removePersonRoom:: Room -> EntityId Person -> Room
removePersonRoom (Room name desc items people exits) id = Room name desc items (delete id people) exits


