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
         params = Params a b c d e
         numProdTypes = read numprodtypes' :: Int
         weights = read <$> words weights' :: [Int] -- assert lenght w == numP
         numWares = read numWares' :: Int
         (wares', f2) = splitAt (2*numWares) f1
         wares = map parseWare $ chunksOf 2 wares' :: [Warehouse]
         (numOrders':f3) = f2
         numOrders = read numOrders' :: Int
         (orders', f4) = splitAt (3*numOrders) f3
         orders = map parseOrder $ chunksOf 3 orders' :: [Order]

         -- max' (x, y) (p, q) = (max x p, max y q)
         -- largest (x, y) = foldr1 max' $ map (snd . ordCoords) ords
         -- (395, 577) in file2 and (237,395) in file1

     return (params, numProdTypes, weights, wares, orders)

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

data Params = Params
  { numRows :: Int -- 1 to 10,000
  , numCols :: Int -- 1 to 10,000
  , numDrones :: Int -- 1 to 1000
  , deadline :: Int -- 1 to 1,000,000
  , maxLoad :: Int -- 1 to 10,000
  } deriving (Show)

data Warehouse = Warehouse
 { wareId :: Int
 , wareCoord :: (Int, Int)
 , wareProdStock :: Map.Map Int Int -- prod stuff
 } deriving (Show)

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
