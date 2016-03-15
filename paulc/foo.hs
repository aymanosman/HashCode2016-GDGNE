
import Data.List
import Mother

x |> f = f x

{- 
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
-}

----------------------------------------

-- one warehouse, lots of orders
-- drones start at W0

----------------------------------------

-- would like to precalc some of these 

all_orders :: [Int]
all_orders = [0 .. orders-1] 
all_products :: [Int]
all_products = [0 .. prods-1] 


-- exploration
p_capacity n = maxload `div` pw n 

o_size o = sum $ map pw $ o_stk o 

p_demand q = length $ orders_for q
orders_for q = [ o | o <- all_orders, p <- o_stk o, p == q ]

p_info = sort [ (p_demand p, p_capacity p, p) | p <- all_products ]



-- distance warehouse to order
w_to_o w o   = dist (w_pos w)  (o_pos o)
o_to_o o1 o2 = dist (o_pos o1) (o_pos o2)

-- rounding error could be significant... 
dist :: (Int,Int) -> (Int,Int) -> Int
dist (ax,ay) (bx,by) 
 = ceiling $ sqrt $ fromIntegral $ (ax - bx)^2 + (ay - by)^2


order_by_dist = [ (w_to_o 0 o, o) | o <- all_orders ] 
              |> sortBy (\x y -> fst x `compare` fst y)
              |> groupBy (\x y -> fst x == fst y)
              |> map (\ods -> ( fst $ head ods
                              , [ (o, o_pos o, o_stk o) | (_,o) <- ods]))


dist_view n = putStr $ unlines $ map show $ take n order_by_dist

tst xs = xs |> map show |> unlines |> putStr



-- assuming adjacent o's for multiples, which we squash out later
plan_for p
 = sort [ o_pos o | o <- os ] 
   where os = orders_for p


