module Src where
import Types 
import Descriptions
import Entities

--hero
hero::Hero
hero = Hero "Gosho" (10,0,0) (0,0) []

--World
initialWorld::World
initialWorld = World rooms items []  (EntityId 1,startingRoom) hero
    where rooms = [(EntityId 1,startingRoom),(EntityId 2,townSquareRoom),(EntityId 3,mainStreetRoom),(EntityId 4,darkAlleyRoom),(EntityId 5,arenaRoom),
                   (EntityId 6,arenaEntranceRoom),(EntityId 7,firstFightRoom),(EntityId 8,barracksRoom),(EntityId 9,secondFightRoom),(EntityId 10,arenaCorridorsRoom)]
          items = [(EntityId 15,rustyPlank),(EntityId 16,axe),(EntityId 17,shield),(EntityId 18,spear),(EntityId 20,potion)]
--EntityId 1
startingRoom::Room
startingRoom = Room "The Tavern" startingRoomDesc [EntityId 20] [] [EntityId 2]


--EntityId 2
townSquareRoom::Room
townSquareRoom = Room "Town Square" townSquareDesc  [] [] [EntityId 1,EntityId 3,EntityId 4]

--EntitId 3
mainStreetRoom::Room
mainStreetRoom = Room "Main Street" mainStreetDesc [] [] [EntityId 2,EntityId 5]

--EntityId 4
darkAlleyRoom::Room
darkAlleyRoom = Room "Dark Alley" darkAlleyDesc [EntityId 15] [] [EntityId 2,EntityId 5]


--EntityId 5
arenaRoom::Room
arenaRoom = Room "Town Arena" arenaDesc [] [] [EntityId 3, EntityId 4,EntityId 6]

--EntityId 6
arenaEntranceRoom::Room
arenaEntranceRoom = Room "Arena Entrance" entranceDesc [EntityId 16,EntityId 18] [] [EntityId 5,EntityId 7]

--EntityId 7
firstFightRoom::Room
firstFightRoom = Room "Arena" firstFightRoomDesc [] [] [EntityId 8]

--EntityId 8
barracksRoom::Room
barracksRoom = Room "Barracks" barracksRoomDesc [] [] [EntityId 9]

 --EntityId 9 
secondFightRoom::Room
secondFightRoom = Room "Arena" secondFightRoomDesc [] [] [EntityId 10]

--EntityId 10
arenaCorridorsRoom::Room
arenaCorridorsRoom = Room "Corridors" arenaCorridorsRoomDesc [] [] [EntityId 11]

 --EntiytyId 11
escapeRoom::Room
escapeRoom = Room "Safe Room" escapeRoomDesc [] [] []
-- --EntityId 12
-- finalBossRoom::Room

-- --EntityId 13
-- ambushRoom::Room

-- --EntityId 14
-- emptyArenaRoom::Room
