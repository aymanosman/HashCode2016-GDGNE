import Data.List.Split
import Data.Array
import qualified Data.Map as Map

main :: IO ()
main = run file2 >>= print

file1, file2 :: String
file1 =  "../mother_of_all_warehouses.in"
file2 = "../busy_day.in"

-- run :: IO (Params, Int, [Int])
run filename =
  do f <- lines <$> readFile filename
     let
         (params':numprodtypes':weights':numWares':f1) = f
         [a,b,c,d,e] = read <$> words params' :: [Int]
         -- params = Params a b c d e
         -- numProdTypes = read numprodtypes' :: Int -- assert numProdTypes == Map.size prods
         weights = read <$> words weights' :: [Int] -- assert lenght w == numP
         numWares = read numWares' :: Int
         (wares', f2) = splitAt (2*numWares) f1
         wares = map parseWare $ chunksOf 2 wares' :: [Warehouse]
         (numOrders':f3) = f2
         numOrders = read numOrders' :: Int
         (orders', f4) = splitAt (3*numOrders) f3
         orders = map parseOrder $ chunksOf 3 orders' :: [Order]
         prods = Map.fromAscList $ zip [0..] weights

         paramsExtra =
           ParamsExtra a b c d e (length wares) (length orders) (Map.size prods)

         -- max' (x, y) (p, q) = (max x p, max y q)
         -- largest (x, y) = foldr1 max' $ map (snd . ordCoords) ords
         -- (395, 577) in file2 and (237,395) in file1

     return (paramsExtra, prods, wares, orders)


data Product = Product
  { prodType :: Int
  , prodWeight :: Int
  }

parseOrder :: [String] -> Order
parseOrder [coord', numItems', ptypes] =
  let [x, y] = read <$> words coord' :: [Int]
      numItems = read numItems' :: Int
      stock = read <$> words ptypes :: [Int]
  in
    Order (x, y) numItems stock
parseOrder _ = error "Bork! Expected thre length list"

parseWare :: [String] -> Warehouse
parseWare [coord', prodFoo'] =
  let [x, y] = read <$> words coord' :: [Int]
      prodFoo = read <$> words prodFoo' :: [Int]
      stock = Map.fromAscList $ zip [0..] prodFoo
  in
    Warehouse 99 (x, y) stock
parseWare _ = error "Bork! Expected two length list"

data Order = Order
  { ordCoords :: (Int, Int)
  , ordNumItems :: Int
  , ordProdTypes :: [Int]
  } deriving (Show)

data Params = ParamsExtra
  { numRows :: Int -- 1 to 10,000
  , numCols :: Int -- 1 to 10,000
  , numDrones :: Int -- 1 to 1000
  , deadline :: Int -- 1 to 1,000,000
  , maxLoad :: Int -- 1 to 10,000
  , numWares :: Int
  , numOrders :: Int
  , numProds :: Int
  } deriving (Show)

data Warehouse = Warehouse
 { wareId :: Int
 , wareCoord :: (Int, Int)
 , wareProdStock :: Map.Map Int Int -- prod stuff
 }

instance Show Warehouse where
  show (Warehouse id coord prodStock) =
    unwords
    ["Warehouse"
    , show id
    , ": at"
    , show coord
    ]


-- Processing
{-
Order
 { ordCoords = (340,371)
 , ordNumItems = 8, ordProdTypes = [226,183,6,220,299,280,12,42]}
-}

fulfill
  :: Order
  -> (Int, Int) -- drone pos
  -> [Warehouse]
  -> [Command]
fulfill ord (x, y) wares =
  -- do goTo ware1
  --    loadItems [a, b, c]
  --    goTo ord1
  --    unloadItems [a, b, c]
  loads ++ []
  where
    Order _ordPos _ ordProds = ord
    loads =
      map
      (\prodT -> Load 0 0 prodT 1)
      ordProds
    

dist (a, b) (x, y) =
  round . sqrt
  $ abs (a - x) + abs (x - y)


-- Output

type DroneId = Int
type WareId = Int
type ProdType = Int
type OrderId = Int

data Command
  = Load DroneId WareId ProdType Int
  | UnLoad DroneId WareId ProdType Int
  | Deliver DroneId OrderId ProdType Int
  | Wait DroneId Int -- num of turns to wait
  deriving (Show)

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
