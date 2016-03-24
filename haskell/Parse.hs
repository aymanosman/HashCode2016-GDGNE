module Parse where

import Data.List.Split
import qualified Data.Map as Map
import Model

-- parse :: IO (Params, Int, [Int])
parse filename =
  parse' <$> readFile filename

-- parse' :: String -> (Params, Map.Map Int Int, [Warehouse], [Order])
parse' file =
  (paramsExtra, prods, wares, orders)
  where
    (params':_numprodtypes':weights':numWares':f1) = lines file
    [a,b,c,d,e] = read <$> words params' :: [Int]
    -- params = Params a b c d e
    -- numProdTypes = read numprodtypes' :: Int -- assert numProdTypes == Map.size prods
    (wares', f2) = splitAt (2*read numWares') f1
    wares = zipWith parseWare [0..] $ chunksOf 2 wares' :: [Warehouse]

    (numOrders':f3) = f2
    (orders', _f4) = splitAt (3*read numOrders') f3
    orders = zipWith parseOrder [0..] $ chunksOf 3 orders' :: [Order]

    weights = read <$> words weights' :: [Int] -- assert lenght w == numP
    prods = Map.fromAscList $ zip [0..] weights

    paramsExtra =
      ParamsExtra a b c d e (read numWares') (length orders) (Map.size prods)


parseOrder :: Int -> [String] -> Order
parseOrder oid [coord', numItems', ptypes] =
  let [x, y] = read <$> words coord' :: [Int]
      stock = read <$> words ptypes :: [Int]
  in
    Order oid (x, y) (read numItems') stock
parseOrder _ _ = error "Bork! Expected three length list"

parseWare :: Int -> [String] -> Warehouse
parseWare wid [coord', prodFoo'] =
  let [x, y] = read <$> words coord' :: [Int]
      prodFoo = read <$> words prodFoo' :: [Int]
      stock = Map.fromAscList $ zip [0..] prodFoo
  in
    Warehouse wid (x, y) stock
parseWare _ _ = error "Bork! Expected two length list"
