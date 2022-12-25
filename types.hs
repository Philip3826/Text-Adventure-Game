module Types where
import Prelude
import Data.Char (toLower)



newtype EntityId a = EntityId Int
    deriving (Eq,Show)

defaultEntityID :: EntityId a
defaultEntityID = EntityId 0

newtype Health = Health Int deriving (Eq,Show)
newtype Power = Power Int deriving (Eq,Show)
newtype Defence = Defence Int deriving (Eq,Show)

instance Num Power where
    (+) (Power x) (Power y) = Power (x + y)
    (-) (Power x) (Power y) = Power (x - y)
    negate (Power x) = Power (negate x)

instance Num Health where
    (+) (Health x) (Health y) = Health (x + y)
    (-) (Health x) (Health y) = Health (x - y)
    

instance Num Defence where
    (+) (Defence x) (Defence y) = Defence (x + y)
    (-) (Defence x) (Defence y) = Defence (x - y)



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
        itemHealth :: Health,
        itemPower :: Power,
        itemDefence :: Defence
    }
    deriving (Eq,Show)

data Person = Person
    {
        personName :: String,
        personDescription :: String,
        personHealth :: Health,
        personPower :: Power,
        personDefence :: Defence
    }
    deriving (Eq,Show)

data Hero = Hero
    {
        heroName :: String,
        heroHealth :: Health,
        heroPower :: Power,
        heroDefence:: Defence,
        heroInventory :: [EntityId Item]
        --heroEquiped :: [EntityId Item]
    }
    deriving (Eq,Show)

data World = World
    {
        worldRooms :: [(EntityId Room,Room)],
        allItems :: [(EntityId Item,Item)],
        people :: [(EntityId Person,Person)],
        currentRoom :: (EntityId Room,Room),
        worldhero :: Hero
    }
    deriving (Eq,Show)

data Command  = GoTo (EntityId Room) | Take (EntityId Item) | Fight (EntityId Person) | Talk (EntityId Person) 
                | Use (EntityId Item)| History | Inventory  | DefaultCommand | Quit 
    deriving (Eq,Show)

data WorldUpdateResult = Continue | End | GameError 
    deriving (Eq,Show)

