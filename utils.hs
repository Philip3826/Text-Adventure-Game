module Utils where
import Types 

searchByKey :: Eq k => k -> [(k, v)] ->  v
searchByKey _ [] = error "key not found"
searchByKey key ((x, y) : xys)
  | key == x   =  y
  | otherwise  = searchByKey key xys

myFirst (x,_,_) = x
mySecond (_,x,_) = x
myThird(_,_,x) = x


getItemStat item func = func (itemStats item)
getPersonStat person func = func (personStats person)

getHeroStat hero func = func (heroStats hero)