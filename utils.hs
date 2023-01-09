module Utils where
import Types 
import Data.Char (toLower)

searchByKey :: Eq k => k -> [(k, v)] ->  v
searchByKey _ [] = error "key not found"
searchByKey key ((x, y) : xys)
  | key == x   =  y
  | otherwise  = searchByKey key xys

keyListByName :: String -> (b -> String) -> [(a, b)] -> [a]
keyListByName _ nameGet [] = []
keyListByName name nameGet (x:xs) =
  if parsedName == parsedX
    then fst x : keyListByName name nameGet xs
    else keyListByName name nameGet xs
  where parsedName = words (lowerString name)
        parsedX = words (lowerString (nameGet (snd x)))

myFirst (x,_,_) = x
mySecond (_,x,_) = x
myThird(_,_,x) = x


getItemStat item func = func (itemStats item)
getPersonStat person func = func (personStats person)

getHeroStat hero func = func (heroStats hero)


tupleSubstract (a,b,c) (x,y,z) = (a - x , b - y , c - z)
tupleAddition (a,b,c) (x,y,z) = (a + x , b + y , c + z)

lowerString::String -> String
lowerString string = [toLower c | c <- string]