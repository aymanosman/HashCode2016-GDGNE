
data Cmd 
 = L { warehouse :: Int, product :: Int, quantity :: Int }
 | D { order_num :: Int, product :: Int, quantity :: Int }  
 | U { warehouse :: Int, product :: Int, quantity :: Int }
 | W { turns :: Int }
   deriving (Eq, Ord, Show)

data DroneState
 = DroneState { product :: Int, quantity :: Int }
   deriving (Eq, Ord, Show)




