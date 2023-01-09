module Entities where
import Types 
--Items
--EntityId 15
rustyPlank::Item
rustyPlank = Item "Plank" "A rusty old Plank " Power (0,1,0)

--EntityId 16
axe::Item
axe = Item "Axe" "Sturdy Axe perfect for cutting... things" Power (0,3,0)

--EntityId 17
shield::Item
shield = Item "Shield" "Old and dented shield but still usable" Defence (0,0,1)

--EntityId 18
spear::Item
spear = Item "Spear" "Perfect for poking and keeping enemies away" Power (0,2,1)

--EntityId 19
trident::Item
trident = Item "Trident" "Very effective for poking three people at once" Power (0,3,1)

--test
potion::Item
potion = Item "potion" "Like a quick breakfast" Health (2,0,0)