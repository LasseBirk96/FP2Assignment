{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import qualified Data.Text.Lazy as L

someFunc :: IO ()
someFunc = do -- IO Monad
  putStrLn "Startin Server at 4711 ..."
  scotty 4711 $ do -- ScottyM Monad

    get "/wedidit" $ do  -- ActionM
      text "Vi lavede opgaven"
    get "/hello/:name" $ do
      name <- param "name"
      text $ L.pack ("Hello " ++ name ++ "!")
    get "/address" $ json b60
    get "/greeter" $ json p1
    post "/address" $ do
      vejenHvorDuBor <- jsonData :: ActionM Address
      json vejenHvorDuBor
    post "/saveGreeter" $ do
      greeter <- jsonData :: ActionM Person
      text $ L.pack ("Greeter saved was: " ++ (name greeter) ++ "!")

b60 :: Address
b60 = Address "Herlevvej" "Herlev"

data Address = Address
  { street :: String
  , city :: String
  } deriving (Show, Generic)

instance ToJSON Address
instance FromJSON Address

p1 :: Person
p1 = Person "Lasse" "Hej med dig" 21 b60

data Person = Person
  { name :: String
  , greeting :: String
  , age :: Int
  , address :: Address
  } deriving (Show, Generic)

instance ToJSON Person
instance FromJSON Person
