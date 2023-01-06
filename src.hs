module Src where
import Types 
--hero
hero::Hero
hero = Hero "Gosho" (10,0,0) (0,0) []

--World
initialWorld::World
initialWorld = World rooms [] []  (EntityId 1,startingRoom) hero
    where rooms = [(EntityId 1,startingRoom),(EntityId 2,townSquareRoom),(EntityId 3,mainStreetRoom)]

--descriptions
startingRoomDesc = " You just put down your sword and sat down at the local tavern to have a cold beer and to rest when you heard a big " ++
 "commotion outside.\n You see other visitors of the tavern going outside to see what is happening so you decide to check it out too." 
townSquareDesc = " You go outside to find a huge crowd gathered in the central square of the city. When you ask the people around you what is going on they tell you that " ++
 "the Duke is waiting to announce a big event.\n After a few minutes of waiting the Duke asks for silence. He then announces that a gladiator tournament will " ++
 "be held in the newly built arena of the town.\n The winner of the tournament will be awarded countless treasured and the honor of being the personal guard of the Duke. " ++
 "Number of participants is limited so he suggested everyone hurry to the arena to register.\n Luckily you know a shortcut throught the shady part of the town that will lead you faster to the arena than the rest."

mainStreetDesc = "The whole population of the town is rushing to the arena through the main street.\nAs you are struggle to make your way through the crowded street you worry if you will make it in" ++
 "in time. Maybe you should have taken that shortcut"

-- Rooms
--EntityId 1
startingRoom::Room
startingRoom = Room "The Tavern" startingRoomDesc [] [] [EntityId 2]


--EntityId 2
townSquareRoom::Room
townSquareRoom = Room "Town Square" townSquareDesc  [] [] [EntityId 1,EntityId 3]

--EntitId 3
mainStreetRoom::Room
mainStreetRoom = Room "Main Street" mainStreetDesc [] [] [EntityId 2]

