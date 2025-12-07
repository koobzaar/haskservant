{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Routes where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, execute, query, query_, Only(..))
import Servant

import Api.Model

-- Definição da API
type HeroisAPI =
       "herois" :> Get '[JSON] Herois
  :<|> "heroi" :> ReqBody '[JSON] NovoHeroi :> Post '[JSON] Resultado
  :<|> "heroi" :> Capture "id" Int :> Get '[JSON] Heroi
  :<|> "heroi" :> Capture "id" Int :> ReqBody '[JSON] Heroi :> Put '[JSON] ()
  :<|> "heroi" :> Capture "id" Int :> Delete '[JSON] ()

-- Configuração do Handler com conexão ao banco
type AppM = ReaderT Connection Handler

server :: ServerT HeroisAPI AppM
server = 
       getHerois
  :<|> postHeroi
  :<|> getHeroiById
  :<|> putHeroi
  :<|> deleteHeroi

-- Implementação das rotas

getHerois :: AppM Herois
getHerois = do
  conn <- asks id
  rows <- liftIO $ query_ conn "SELECT id, nome, classe, nivel FROM Heroi"
  return $ Herois rows

postHeroi :: NovoHeroi -> AppM Resultado
postHeroi (NovoHeroi n c v) = do
  conn <- asks id
  [Only heroiId] <- liftIO $ query conn "INSERT INTO Heroi (nome, classe, nivel) VALUES (?, ?, ?) RETURNING id" (n, c, v)
  return $ Resultado heroiId

getHeroiById :: Int -> AppM Heroi
getHeroiById heroiId = do
  conn <- asks id
  [heroi] <- liftIO $ query conn "SELECT id, nome, classe, nivel FROM Heroi WHERE id = ?" (Only heroiId)
  return heroi

putHeroi :: Int -> Heroi -> AppM ()
putHeroi heroiId (Heroi _ n c v) = do
  conn <- asks id
  _ <- liftIO $ execute conn "UPDATE Heroi SET nome = ?, classe = ?, nivel = ? WHERE id = ?" (n, c, v, heroiId)
  return ()

deleteHeroi :: Int -> AppM ()
deleteHeroi heroiId = do
  conn <- asks id
  _ <- liftIO $ execute conn "DELETE FROM Heroi WHERE id = ?" (Only heroiId)
  return ()
