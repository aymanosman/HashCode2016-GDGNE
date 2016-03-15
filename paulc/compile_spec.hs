import Data.List(sort)

main = do ls <- readFile "mother_of_all_warehouses.in"
          let cs = config_ns (lines ls)
          writeFile "mother.hs" (unlines $ prelude ++ cs)

prelude 
  = [ "module Test where"
    , "rows :: Int"
    , "cols :: Int"
    , "drones :: Int"
    , "turns :: Int"
    , "maxload :: Int"
    , "prods :: Int "
    , "pw :: Int -> Int"
    , "warehouses :: Int"
    , "w_pos :: Int -> (Int, Int)"
    , "w_stk :: Int -> [Int]"
    , "orders :: Int "
    , "o_pos :: Int -> (Int,Int)"
    , "o_stk :: Int -> [Int]" ]
 
config_ns (l:ls) 
 = zipWith (\v n -> n ++ " = " ++ v) (words l) (words "rows cols drones turns maxload")
   ++ config_ps ls

config_ps (np:pws:ls) 
 =   ("prods = " ++ np)
   : zipWith (\i v -> unwords ["pw", show i, "=", v]) [0..] (words pws)
   ++ config_ws ls

config_ws (nw:ls) 
 = [ "warehouses = " ++ nw ]
   ++ sort (concat $ zipWith (\i f -> f i) [0..] (take n_ws $ warehouses ls))
   ++ config_os (drop (2 * n_ws) ls)
   where
     n_ws = read nw

-- maybe convert to FM later
warehouses (a:b:cs) 
 = (\i -> [ unwords ["w_pos", show i, "=", show (read x :: Int, read y :: Int) ]
          , unwords ["w_stk", show i, "=", show (map read $ words b :: [Int]) ] ]) 
   : warehouses cs
   where
     x:y:_ = words a 

config_os (no:ls)
 = [ "orders = " ++ no ]
   ++ sort (concat $ zipWith (\i f -> f i) [0..] (take (read no) $ orders ls))


orders :: [String] -> [Int -> [String]]
orders (a:b:c:ds) 
 = (\i -> [ unwords ["o_pos", show i, "=", show ((read x, read y) :: (Int,Int)) ]
          , unwords ["o_stk", show i, "=", show (map read $ words c :: [Int]) ] 
          ]) 
   : orders ds
   where
     x:y:_ = words a 
   
