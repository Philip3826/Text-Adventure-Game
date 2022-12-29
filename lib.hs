module Lib where
import Types
import Prelude
import Data.List (delete)
import Utils 
import System.Random
import Types (World(worldPeople))
import TestStuff (hero)



executeCommand::World -> Command -> (World,WorldUpdateResult)
executeCommand world command =
    case command of 
        GoTo (EntityId command)-> (updateCurrentRoom world (EntityId command),Continue)
        Take (EntityId command) -> (takeFromRoom world (EntityId command),Continue)
        Use (EntityId command) -> (useItem world (EntityId command),Continue)
        Inventory -> (world,Continue)
        Quit -> (world,End)
        History -> (world,Continue)
        DefaultCommand -> (world,GameError)


parseCommand::World -> String -> Command
parseCommand world input =
    case words input of
       ("go":"to":room) -> GoTo (parseRoom world room)
       ("go" : "in" : room) -> GoTo (parseRoom world room)
       ("pick":"up":item) -> Take (parseItem world item) 
       ("pick":item) -> Take (parseItem world item) 
       ("take":item) -> Take (parseItem world item)
       ("use" : item) -> Use (parseItem world item)
       ("equip": item) -> Use (parseItem world item)
       ("fight": person) -> Fight (parsePerson world person)
       ("hit": person) -> Fight (parsePerson world person)
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
    where items = map snd (filter (\x -> fst x `elem` itemsIDs) (allItems world))
          itemsIDs = roomItems room
          peopleInRoom = map snd (filter (\x -> fst x `elem` peopleIDs) (worldPeople world))
          peopleIDs = roomPeople room
          exits = map snd (filter (\x -> fst x `elem` exitsIDs) (worldRooms world))
          exitsIDs = roomOtherRooms room

updateCurrentRoom :: World -> EntityId Room -> World
updateCurrentRoom world room 
 | room == defaultEntityID = world
 | room `notElem` roomOtherRooms ( snd (currentRoom world)) = world
 | otherwise = World (worldRooms world) (allItems world) (worldPeople world) newRoom (worldhero world)
    where newRoom = head (filter (\x -> fst x == tmp) (worldRooms world))
            where tmp = head ( filter (\x -> x == room) (roomOtherRooms (snd (currentRoom world))))


--TODO : fix this so the main copy of the room is being updated not the one that sits in currentRoom
-- go into the list of Rooms and update the room there
takeFromRoom :: World -> EntityId Item -> World
takeFromRoom world item 
 | item == defaultEntityID = world
 | item `notElem` roomItems (snd (currentRoom world)) = world
 | otherwise = World (updateRoomInList (worldRooms world) updatedRoom) (allItems world) (worldPeople world)  updatedRoom updatedHero
    where updatedRoom = (fst (currentRoom world),removeItem (snd (currentRoom world)) item)
          updatedHero = addItemToInventory (worldhero world) item

removeItem:: Room -> EntityId Item -> Room
removeItem room item =
    if item `notElem` roomItems room
        then room
        else Room (roomName room) (roomDescription room) updatedItems (roomPeople room) (roomOtherRooms room)
    where updatedItems = delete item (roomItems room) 

addItemToInventory:: Hero -> EntityId Item -> Hero
addItemToInventory (Hero name stats inv) item = Hero name stats (item:inv)

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

useItem::World -> EntityId Item -> World
useItem world itemId 
 | itemId == defaultEntityID = world
 | itemId `notElem` inventory = world
 | otherwise = World (worldRooms world) (allItems world) (worldPeople world) (currentRoom world) updatedHero
    where inventory = heroInventory (worldhero world)
          updatedHero = applyItemOnHero (worldhero world) item itemId
            where item = searchByKey itemId (allItems world)


applyItemOnHero::Hero -> Item  -> EntityId Item -> Hero
applyItemOnHero hero item id = Hero (heroName hero) (newHealth, newPower, newDefence) (delete id (heroInventory hero)) 
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


battleLoop::World -> Hero -> EntityId Person -> (World,WorldUpdateResult)
battleLoop world hero id 
 | getHeroStat hero myFirst <= 0 = (world,End)
 | getPersonStat person myFirst  <= 0 = (removeEnemyWorld,Continue)
 | otherwise = do 
        rawHeroRoll <- randomRIO (1,6)
        rawPersonRoll <- randomRIO (1,6)
        let heroRoll = rawHeroRoll
        let personRoll = rawPersonRoll
        let battleResult = calculateBattle hero person (heroRoll,personRoll)
        case (fst battleResult) of
            Draw -> battleLoop world hero id 
            Win ->  battleLoop dmgEnemyWorld hero id 
            Loss -> battleLoop world dmgHero id 
    where removeEnemyWorld = World (worldRooms world) (allItems world) (worldPeople world) (removePersonRoom (currentRoom world) id) hero
          dmgEnemyWorld =   

calculateBattle::Hero -> Person -> (Int,Int) -> (BattleResult,Int)
calculateBattle hero person (heroHit,enemyHit) 
 | heroDmg > enemyDmg = (Win,heroDmg - enemyDmg)
 | enemyDmg > heroDmg = (Loss,enemyDmg - heroDmg)
 | otherwise = (Draw,0)
    where heroDmg = heroHit + getHeroStat hero mySecond
          enemyDmg = enemyHit + getPersonStat person mySecond
          

applyDmgHero :: Hero -> Int -> Hero
applyDmgHero (Hero name (hp,power,def) inventory) dmg = Hero name (hp - dmg , power , def) inventory

applyDmgPerson :: Person -> Int -> Person
applyDmgPerson (Person name desc (hp,power,def)) dmg = Person name desc (hp - dmg,power,def)


removePersonRoom:: Room -> EntityId Person -> Room
removePersonRoom (Room name desc items people exits) id = Room name desc items (delete id people) exit