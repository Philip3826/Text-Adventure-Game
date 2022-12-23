module Lib where
import Types
import TestStuff
import Prelude
import Data.List (delete)

executeCommand::World -> Command -> (World,WorldUpdateResult)
executeCommand world command =
    case command of 
        GoTo (EntityId command)-> (updateCurrentRoom world (EntityId command),Continue)
        Take (EntityId command) -> (takeFromRoom world (EntityId command),Continue)
        Inventory -> (world,Continue)
        Quit -> (world,End)
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
    where filtered = filter (\x -> words (personName (snd x)) == person) (people world)

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
          peopleInRoom = map snd (filter (\x -> fst x `elem` peopleIDs) (people world))
          peopleIDs = roomPeople room
          exits = map snd (filter (\x -> fst x `elem` exitsIDs) (worldRooms world))
          exitsIDs = roomOtherRooms room

updateCurrentRoom :: World -> EntityId Room -> World
updateCurrentRoom world room 
 | room == defaultEntityID = world
 | room `notElem` roomOtherRooms ( snd (currentRoom world)) = world
 | otherwise = World (worldRooms world) (allItems world) (people world) newRoom (worldhero world)
    where newRoom = head (filter (\x -> fst x == tmp) (worldRooms world))
            where tmp = head ( filter (\x -> x == room) (roomOtherRooms (snd (currentRoom world))))


--TODO : fix this so the main copy of the room is being updated not the one that sits in currentRoom
-- go into the list of Rooms and update the room there
takeFromRoom :: World -> EntityId Item -> World
takeFromRoom world item 
 | item == defaultEntityID = world
 | item `notElem` roomItems (snd (currentRoom world)) = world
 | otherwise = World (worldRooms world) (allItems world) (people world)  updatedRoom updatedHero
    where updatedRoom = (fst (currentRoom world),removeItem (snd (currentRoom world)) item)
          updatedHero = addItemToInventory (worldhero world) item

removeItem:: Room -> EntityId Item -> Room
removeItem room item =
    if item `notElem` roomItems room
        then room
        else Room (roomName room) (roomDescription room) updatedItems (roomPeople room) (roomOtherRooms room)
    where updatedItems = delete item (roomItems room) 

addItemToInventory:: Hero -> EntityId Item -> Hero
addItemToInventory (Hero name health power def inv) item = Hero name health power def (item:inv)

--fix the filter to allow duplicates
displayInventory::World -> String
displayInventory world =
    getItemsString inventory
    where inventory = map snd (filter (\x -> fst x `elem` inventoryIDs) (allItems world))
          inventoryIDs = heroInventory (worldhero world)