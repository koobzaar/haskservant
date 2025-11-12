{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Server.Routes
import Network.Wai.Handler.Warp
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import qualified Data.ByteString.Char8 as BS

runMigration :: Connection -> FilePath -> IO ()
runMigration conn fp = do
  sql <- readFile fp
  void $ execute_ conn (Query $ BS.pack sql)

main :: IO ()
main = do 
    putStrLn "Servidor rodando na porta 8080"

    conn <- connectPostgreSQL "host=localhost dbname=hask user=postgres password=root"

    runMigration conn "migration.sql"

    run 8080 (app conn)
