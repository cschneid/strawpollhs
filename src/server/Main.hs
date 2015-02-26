{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import StrawPoll.Types

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans (liftIO)
import Control.Monad (void)
import Data.IORef
import Data.Proxy
import Data.Time
import Data.Aeson

import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles
import Servant.IP
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Socket

import Database.Persist as DB
import Database.Persist.Sqlite as DB

import GHC.Generics

getPool :: IO DB.ConnectionPool
getPool = do
  let s = "ghci.db"
  let n = 1
  runStdoutLoggingT (DB.createSqlitePool s n)

runDB :: DB.ConnectionPool -> DB.SqlPersistT IO a -> IO a
runDB pool query = liftIO $ DB.runSqlPool query pool

type MyAPI =      "counter" :> Get Counter
             :<|> "counter" :> Post Counter
             :<|> "polls"   :> Get [Poll]
             :<|> Raw


main = do
  counter <- newIORef (Counter 0)
  pool <- getPool
  runDB pool (DB.runMigration migrateAll)

  run 7000 $ applyMiddleware $ serve myAPI (server counter pool)
  where myAPI :: Proxy MyAPI
        myAPI = Proxy

applyMiddleware :: Application -> Application
applyMiddleware = logStdoutDev . (gzip def)

server :: IORef Counter -> DB.ConnectionPool -> Server MyAPI
server ref pool = getCounter :<|> postCounter :<|> getPolls :<|> serveRoot
  where
    getCounter = liftIO $ readIORef ref

    postCounter = liftIO $ atomicModifyIORef ref
                           (\c -> let c' = incrementCounter c in (c', c'))

    getPolls = liftIO $ do
      polls <- runDB pool (selectList [] [])
      return (map entityVal polls)

    serveRoot   = serveDirectory "static"
