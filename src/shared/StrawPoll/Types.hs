{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE DeriveGeneric #-}

module StrawPoll.Types where

import Data.Time
import GHC.Generics
import Data.Aeson
import Data.Text

import Database.Persist as DB
import Database.Persist.TH

share [mkPersist sqlSettings { mpsGenerateLenses = True, mpsPrefixFields = False }, mkMigrate "migrateAll" ] [persistLowerCase|
Poll json
    question Text
    createdAt UTCTime Maybe default=CURRENT_TIME
    deriving Show
    deriving Eq
    deriving Generic
PollOption json
    pid PollId Maybe
    option Text
    deriving Show
    deriving Eq
    deriving Generic
PollAnswer json
    questionId PollOptionId
    answeredAt UTCTime default=CURRENT_TIME
    ip Text Maybe
    deriving Show
    deriving Eq
    deriving Generic
|]

emptyPoll = Poll "" Nothing
emptyOption = PollOption Nothing ""
