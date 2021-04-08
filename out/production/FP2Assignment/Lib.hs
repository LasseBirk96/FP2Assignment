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

    get "/hello" $ do  -- ActionM
      text "Hello World!"
    get "/hello/:name" $ do
      name <- param "name"
      text $ L.pack ("Hello " ++ name ++ "!")
    get "/address" $ json a4
    get "/greeter" $ json g1
    post "/address" $ do
      vejenHvorDuBor <- jsonData :: ActionM Address
      json vejenHvorDuBor
    post "/saveGreeter" $ do
      greeter <- jsonData :: ActionM Greeter
      text $ L.pack ("Greeter saved was: " ++ (name greeter) ++ "!")


a4 :: Address
a4 = Address "Byvej 4" "Roskilde"

data Address = Address
  { street :: String
  , city :: String
  } deriving (Show, Generic)

instance ToJSON Address
instance FromJSON Address

g1 :: Greeter
g1 = Greeter "Kurt" "Hejsa" 34 a4

data Greeter = Greeter
  { name :: String
  , greeting :: String
  , age :: Int
  , address :: Address
  } deriving (Show, Generic)

instance ToJSON Greeter
instance FromJSON Greeter
