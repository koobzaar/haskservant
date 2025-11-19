{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Routes where 

import Api.Model
import Data.Proxy
import Network.Wai.Middleware.Cors
import Network.Wai
import Servant.API.Sub
import Servant.API
import Servant.Server
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

type API = 
         "hello" :> Get '[PlainText] String 
    :<|> "soma" :> ReqBody '[JSON] Calculadora :> Post '[JSON] ResultadoResponse
    :<|> "soma"  :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "cliente" :> ReqBody '[JSON] Cliente :> Post '[JSON] ResultadoResponse 
    :<|> "cliente"  :> Verb 'OPTIONS 200 '[JSON] ()

handlerCliente :: Connection -> Cliente -> Handler ResultadoResponse
handlerCliente conn cli = do 
    res <- liftIO $ query conn "INSERT INTO Cliente (nome,cpf) VALUES (?,?) RETURNING id" (nome cli, cpf cli)
    case res of 
        [Only novoId] -> pure (ResultadoResponse $ novoId)
        _ -> throwError err500

handlerSoma :: Calculadora -> Handler ResultadoResponse
handlerSoma (Calculadora x y) = pure (ResultadoResponse $ x + y)

options :: Handler ()
options = pure ()

-- Handler eh uma Monada que tem IO dentro
handlerHello :: Handler String 
handlerHello = pure "Ola, mundo!"

server :: Connection -> Server API 
server conn = handlerHello :<|> handlerSoma :<|> options :<|> handlerCliente conn :<|> options

addCorsHeader :: Middleware
addCorsHeader app req respond =
  app req $ \res ->
    respond $ mapResponseHeaders
      ( \hs ->
          [ ("Access-Control-Allow-Origin", "*")
          , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
          , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
          ] ++ hs
      )
      res

app :: Connection -> Application 
app conn = addCorsHeader (serve (Proxy @API) (server conn))