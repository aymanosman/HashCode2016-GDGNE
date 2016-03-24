module Model where

import qualified Data.Map as Map

data Product = Product
  { prodType :: Int
  , prodWeight :: Int
  }

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

data Order = Order
  { ordId :: Int
  , ordCoords :: (Int, Int)
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
