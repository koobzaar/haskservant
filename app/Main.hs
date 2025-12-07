{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Servant
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Control.Monad.Reader (runReaderT)

-- Importando seus módulos
import Server.Routes (HeroisAPI, server, AppM)

-- Proxy da API
heroisAPI :: Proxy HeroisAPI
heroisAPI = Proxy

-- Função Principal
main :: IO ()
main = do
    putStrLn "Conectando ao banco de dados..."
    -- Graças ao OverloadedStrings, este texto vira ByteString automaticamente
    conn <- connectPostgreSQL "host=localhost dbname=testdb user=testuser password=testpassword"

    putStrLn "Iniciando servidor na porta 8080..."
    run 8080 (myCors $ app conn)

-- Configuração do CORS
myCors :: Application -> Application
myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"] 
      , corsRequestHeaders = ["Content-Type"]
      }

-- Aplicação Servant
app :: Connection -> Application
app conn = serve heroisAPI $ hoistServer heroisAPI (nt conn) server
  where
    nt :: Connection -> AppM a -> Handler a
    nt c action = runReaderT action c
