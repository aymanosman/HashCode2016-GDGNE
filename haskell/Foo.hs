import Data.Array
import Model
import Parse

main :: IO ()
main = parse file2 >>= print

main2 =
  do (params, prods, wares, orders) <- parse file1
     putStrLn $ unlines $ map show $ commands orders
  where
    commands :: [Order] -> [Command]
    commands ords = concatMap (fulfill (0,0) []) $ take 2 ords

file1, file2 :: String
file1 =  "../mother_of_all_warehouses.in"
file2 = "../busy_day.in"


-- Processing
{-
Order
 { ordCoords = (340,371)
 , ordNumItems = 8, ordProdTypes = [226,183,6,220,299,280,12,42]}
-}

fulfill
  ::(Int, Int) -- drone pos
  -> [Warehouse]
  -> Order
  -> [Command]
fulfill (x, y) wares ord =
  -- do goTo ware1
  --    loadItems [a, b, c]
  --    goTo ord1
  --    unloadItems [a, b, c]
  loads ++ delivers ++ []
  where
    drone = 0
    Order ordId _ordPos _ ordProds = ord
    loads =
      map
      (\prodT -> Load drone 0 prodT 1)
      ordProds
    delivers =
      map
      (\prodT -> Deliver drone ordId prodT 1)
      ordProds

dist (a, b) (x, y) =
  round . sqrt
  $ abs (a - x) + abs (x - y)


-- Output

renderCommand c =
  let ff s did wid pt n =
        unwords [show did, s, show wid, show pt, show n]
  in
  case c of
    Load did wid pt n ->
      ff "L" did wid pt n

    UnLoad did wid pt n ->
      ff "U" did wid pt n

    Deliver did wid pt n ->
      ff "D" did wid pt n

    Wait did turns ->
      unwords [show did, "W", show turns]


ex_commands =
  [ Load 0 0 0 1
  , Load 0 0 1 1
  , Deliver 0 0 0 1
  ]



-- lb, ub :: Int
-- lb = 0
-- ub = 9

-- grid :: Array Int Cell
-- grid =
--   listArray (lb, ub) (repeat emptyCell)

-- data Cell = Cell
--   { cellWare :: Maybe Warehouse
--   } deriving (Show)

-- emptyCell :: Cell
-- emptyCell = Cell Nothing
