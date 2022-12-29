module TestStuff where
import Types
import Utils
import Lib

exItem :: Item
exItem = Item "Sword" "A big bad sword" (0,1,0)

shield::Item
shield = Item "Shield" "A rusty Shield" (0,0,1)

axe :: Item
axe = Item "Axe" "a woodcutters axe" (0,1,0)

bartender :: Person
bartender = Person "Bob" "Bartender in the inn of Bolksvile" (1,1,0)

exampleRoom :: Room
exampleRoom = Room "The Tavern" "The Tavern is full of adventurers resting after a long day" [EntityId 2 , EntityId 3] [EntityId 4] [EntityId 5]


nextRoom :: Room
nextRoom = Room "Main Street" "The night is quiet and everyone is sleeping in their homes. There is a full moon" [] [] [EntityId 1]

hero::Hero
hero = Hero "Bob" (1,1,0) [EntityId 3,EntityId 5]

initial :: World
initial = World [(EntityId 1, exampleRoom), (EntityId 5 ,nextRoom)]  [(EntityId  2,exItem),(EntityId 3,axe),(EntityId 5,shield)] [(EntityId 4,bartender)] (EntityId 1,exampleRoom) hero
    
