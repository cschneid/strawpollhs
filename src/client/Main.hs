module Main where

import StrawPoll.Types

import Prelude hiding (div)
import Blaze.React
import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Event.Keycode             as Keycode
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import qualified Blaze.React.Run.ReactJS          as ReactJS

import qualified Data.Text       as T
import Data.Time
import Data.Aeson
import Control.Monad.State
import Control.Monad.Writer

import NewPoll as NewPoll

main :: IO ()
main = ReactJS.runApp $ ignoreWindowActions NewPoll.app


