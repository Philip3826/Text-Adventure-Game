module Types where
import Prelude
import Data.Char (toLower)



newtype EntityId a = EntityId Int
    deriving (Eq,Show)

defaultEntityID :: EntityId a
defaultEntityID = EntityId 0


data Room = Room
    {
        roomName :: String,
        roomDescription :: String,
        roomItems :: [EntityId Item],
        roomPeople :: [EntityId Person],
        roomOtherRooms :: [EntityId Room]
    }
    deriving (Eq,Show)

data Item = Item
    {
        itemName :: String,
        itemDescription :: String,
        itemType:: ItemType,
        itemStats :: (Int,Int,Int)
    }
    deriving (Eq,Show)

data Person = Person
    {
        personName :: String,
        personDescription :: String,
        personStats :: (Int,Int,Int)
    }
    deriving (Eq,Show)

data Hero = Hero
    {
        heroName :: String,
        heroStats :: (Int,Int,Int),
        itemCounters :: (Int,Int),
        heroInventory :: [EntityId Item]
       
    }
    deriving (Eq,Show)

data World = World
    {
        worldRooms :: [(EntityId Room,Room)],
        allItems :: [(EntityId Item,Item)],
        worldPeople :: [(EntityId Person,Person)],
        currentRoom :: (EntityId Room,Room),
        worldhero :: Hero
    }
    deriving (Eq,Show)

data Command  = GoTo (EntityId Room) | Fight (EntityId Person) | See (EntityId Person) | Drop (EntityId Item)
                | Use (EntityId Item)| History | Inventory  | DefaultCommand | Quit | Help 
    deriving (Eq,Show)

data WorldUpdateResult = Continue | End | GameError | InitiateFight (EntityId Person)
    deriving (Eq,Show)

data ItemType = Health | Power | Defence | Special
    deriving (Eq,Show)

