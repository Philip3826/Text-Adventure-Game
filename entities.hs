module Entities where
import Types 
import Descriptions

--Items
--EntityId 15
rustyPlank::Item
rustyPlank = Item "Plank" "A rusty old Plank " Power (0,1,0)

--EntityId 16
axe::Item
axe = Item "Axe" "Sturdy Axe perfect for cutting... things" Power (0,2,0)

--EntityId 17
shield::Item
shield = Item "Shield" "Old and dented shield but still usable" Defence (0,0,1)

--EntityId 18
spear::Item
spear = Item "Spear" "Perfect for poking and keeping enemies away" Power (0,1,1)

--EntityId 19
trident::Item
trident = Item "Trident" "Very effective for poking three people at once" Power (0,3,1)

--EntityId 20
potion::Item
potion = Item "Potion" "Like a quick breakfast" Health (3,0,0)

--EntityId 21
greatPotion::Item
greatPotion = Item "Great Potion" "I bet you can't drink this in one go" Health (6,0,0)

--EntityId 22
greatSword::Item
greatSword = Item "Great Sword" "It's taller than you" Power (0,4,0)

--EntityId 23
greatShield::Item
greatShield = Item "Great Shield" "Great for blocking the outside world" Defence (0,1,2)

--special EntityId 100
lockpick::Item
lockpick = Item "Lockpick" "Very useful for thiefs... You are not a thief are you?" Special (0,0,0)



--People

--
--special EntityId 101
thief::Person
thief = Person "Thief" thiefDesc (6,0,0)

--EntityId 23
firstFighter::Person
firstFighter = Person "Enemy" firstFighterDesc (10,1,1)

--EntityId 24
secondFighter :: Person
secondFighter = Person "Burly Fighter" secondFighterDesc (12,5,2)

--EntityId 25
assassin::Person
assassin = Person "Assassin" assassinDesc  (8,5,0)

--EntityId 26
bolvar::Person
bolvar = Person "Bolvar" bolvarDesc (15,5,3)