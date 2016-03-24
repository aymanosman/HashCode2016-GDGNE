module Parse where

import Data.List.Split
import qualified Data.Map as Map
import Model

-- parse :: IO (Params, Int, [Int])
parse filename =
  do f <- lines <$> readFile filename
     let
         (params':numprodtypes':weights':numWares':f1) = f
         [a,b,c,d,e] = read <$> words params' :: [Int]
         -- params = Params a b c d e
         -- numProdTypes = read numprodtypes' :: Int -- assert numProdTypes == Map.size prods
         weights = read <$> words weights' :: [Int] -- assert lenght w == numP
         numWares = read numWares' :: Int
         (wares', f2) = splitAt (2*numWares) f1
         wares = zipWith parseWare [0..] $ chunksOf 2 wares' :: [Warehouse]
         (numOrders':f3) = f2
         numOrders = read numOrders' :: Int
         (orders', f4) = splitAt (3*numOrders) f3
         orders = map parseOrder $ zip [0..] $ chunksOf 3 orders' :: [Order]
         prods = Map.fromAscList $ zip [0..] weights

         paramsExtra =
           ParamsExtra a b c d e (length wares) (length orders) (Map.size prods)

         -- max' (x, y) (p, q) = (max x p, max y q)
         -- largest (x, y) = foldr1 max' $ map (snd . ordCoords) ords
         -- (395, 577) in file2 and (237,395) in file1

     return (paramsExtra, prods, wares, orders)


parseOrder :: (Int, [String]) -> Order
parseOrder (ordId, [coord', numItems', ptypes]) =
  let [x, y] = read <$> words coord' :: [Int]
      numItems = read numItems' :: Int
      stock = read <$> words ptypes :: [Int]
  in
    Order ordId (x, y) numItems stock
parseOrder _ = error "Bork! Expected three length list"

parseWare :: Int -> [String] -> Warehouse
parseWare wareId [coord', prodFoo'] =
  let [x, y] = read <$> words coord' :: [Int]
      prodFoo = read <$> words prodFoo' :: [Int]
      stock = Map.fromAscList $ zip [0..] prodFoo
  in
    Warehouse wareId (x, y) stock
parseWare _ _ = error "Bork! Expected two length list"

