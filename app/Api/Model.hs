{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} 

module Api.Model where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Database.PostgreSQL.Simple.FromRow

data Heroi = Heroi
  { idHeroi :: Int,
    nome :: Text,
    classe :: Text,
    nivel :: Int
  }
  deriving (Show, Generic)

instance FromJSON Heroi
instance ToJSON Heroi

instance FromRow Heroi where
  fromRow = Heroi <$> field <*> field <*> field <*> field

data Herois = Herois
  { herois :: [Heroi]
  }
  deriving (Show, Generic)

instance FromJSON Herois
instance ToJSON Herois

data NovoHeroi = NovoHeroi
  { nome :: Text,
    classe :: Text,
    nivel :: Int
  }
  deriving (Show, Generic)

instance FromJSON NovoHeroi
instance ToJSON NovoHeroi

data Resultado = Resultado
  { resultado :: Int
  }
  deriving (Show, Generic)

instance FromJSON Resultado
instance ToJSON Resultado
