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


-----------------------------------------------
-- Data Definitions
-----------------------------------------------

data PollState = PollState {currentCount :: Int} deriving Show
data Actions = IncrementA
             | NullActionA -- Do nothing
               deriving Show

-----------------------------------------------
-- Action Handling
-----------------------------------------------

applyAction :: Actions -> Transition PollState Actions
applyAction action =
  runTransitionM $ case action of
    IncrementA  -> applyIncrementA
    NullActionA -> return ()

applyIncrementA = do
  tell $ consoleLog "Incrementing!"
  i <- gets currentCount
  put (PollState (i + 1))

consoleLog :: T.Text -> [IO Actions]
consoleLog msg = [ print msg >> return NullActionA ]

-----------------------------------------------
-- Views
-----------------------------------------------

renderState :: PollState -> WindowState Actions
renderState state = WindowState
    { _wsBody = renderBody state
    , _wsPath = ""
    }

renderBody :: PollState -> H.Html Actions
renderBody (PollState i) = do
  H.div $ do
    H.toHtml ("Clicked " :: T.Text)
    H.toHtml i
    H.toHtml (" Times" :: T.Text)

    H.button H.! E.onClick' IncrementA
             $ H.toHtml ("+1" :: T.Text)


-----------------------------------------------
-- Entry Point
-----------------------------------------------

main :: IO ()
main = do
  t <- getCurrentTime
  print $ Poll "Foo" t
  print $ encode $ Poll "Bar" t
  ReactJS.runApp $ ignoreWindowActions app

app :: App PollState Actions
app = App
    { appInitialState    = initialState
    , appInitialRequests = []
    , appApplyAction     = applyAction
    , appRender          = renderState
    }

initialState :: PollState
initialState = PollState 0

