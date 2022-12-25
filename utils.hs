module Utils where
import Types (EntityId)

searchByKey :: Eq k => k -> [(k, v)] ->  v
searchByKey _ [] = error "key not found"
searchByKey key ((x, y) : xys)
  | key == x   =  y
  | otherwise  = searchByKey key xys


