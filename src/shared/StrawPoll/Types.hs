{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import Database.Persist as DB
import Database.Persist.TH

data Counter = Counter { count :: Int } deriving (Show, Generic)

instance ToJSON Counter
instance FromJSON Counter

incrementCounter :: Counter -> Counter
incrementCounter (Counter i) = Counter (i + 1)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Poll
    question String
    createdAt UTCTime default=CURRENT_TIME
    deriving Show
    deriving Generic
PollOption
    pollId PollId
    option String
    deriving Show
    deriving Generic
PollAnswer
    questionId PollOptionId
    answeredAt UTCTime default=CURRENT_TIME
    ip String Maybe
    deriving Show
    deriving Generic
|]

instance FromJSON Poll
instance FromJSON PollOption
instance FromJSON PollAnswer
instance ToJSON Poll
instance ToJSON PollOption
instance ToJSON PollAnswer
