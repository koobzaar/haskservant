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

type API = 
         "hello" :> Get '[PlainText] String 
    :<|> "soma" :> ReqBody '[JSON] Calculadora :> Post '[JSON] ResultadoResponse
    :<|> "soma"  :> Verb 'OPTIONS 200 '[JSON] ()

handlerSoma :: Calculadora -> Handler ResultadoResponse
handlerSoma (Calculadora x y) = pure (ResultadoResponse $ x + y)

optionsSoma :: Handler ()
optionsSoma = pure ()

-- Handler eh uma Monada que tem IO dentro
handlerHello :: Handler String 
handlerHello = pure "Ola, mundo!"

server :: Connection -> Server API 
server conn = handlerHello :<|> handlerSoma :<|> optionsSoma

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