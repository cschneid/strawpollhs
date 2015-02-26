{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.IP where

import Servant.API
import Servant.Server
import Network.Wai
import Network.Socket (SockAddr)
import Data.Proxy

data IP

instance (HasServer sublayout) => HasServer (IP :> sublayout) where
  type Server (IP :> sublayout) = SockAddr -> Server sublayout

  route Proxy subserver request respond = do
    let ip = remoteHost request
    route (Proxy :: Proxy sublayout) (subserver ip) request respond



