module NewPoll where

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
import Data.Foldable   (foldMap)
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens

-----------------------------------------------
-- Data Definitions
-----------------------------------------------

data PollState = PollState { question :: T.Text
                           , answers  :: [T.Text]
                           } deriving Show

data Actions = UpdateQuestionTextA T.Text
             | UpdateAnswerTextA Int T.Text
             | NullActionA -- Do nothing
               deriving Show

-----------------------------------------------
-- Action Handling
-----------------------------------------------

applyAction :: Actions -> Transition PollState Actions
applyAction action =
  runTransitionM $ case action of
    UpdateQuestionTextA questionText -> do
      (PollState _ answers) <- get
      put $ PollState questionText answers

    UpdateAnswerTextA index newAnswerText  -> do
      (PollState question answers) <- get
      let newAnswers = answers & element index .~ newAnswerText
      let shortened = shortenAnswers newAnswers
      let lengthed = lengthenAnswers shortened
      put $ PollState question lengthed

    NullActionA -> return ()

-- Add an empty item to the end of the list if we're editing the last item
lengthenAnswers :: [T.Text] -> [T.Text]
lengthenAnswers a = if length a < 4
                    then lengthenAnswers (a ++ [""])
                    else a

dropTrailing :: (Eq a) => a -> [a] -> [a]
dropTrailing item = reverse . (dropWhile (== item)) . reverse

shortenAnswers :: [T.Text] -> [T.Text]
shortenAnswers answers = (dropTrailing "" answers) ++ [""]

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
renderBody (PollState question answers) =
  H.div $ do
    H.form $ do
      H.h1 $ H.toHtml question
      H.input H.! A.id "question"
              H.! A.class_ "form-control"
              H.! A.placeholder "Question:"
              H.! A.value (H.toValue question)
              H.! E.onValueChange UpdateQuestionTextA
      H.ul $ foldMap renderAnswer (zip [0..] answers)
      H.a H.! A.class_ "btn btn-default"
          H.! A.href "http://google.com"
          $ "Create Poll"

renderAnswer (index, answer) =
  H.div H.! A.class_ "answer" $ do
    H.toHtml index
    H.input H.! A.placeholder "Enter Poll Option:"
            H.! A.class_ "form-control"
            H.! A.value (H.toValue answer)
            H.! E.onValueChange (UpdateAnswerTextA index)
    H.toHtml answer

-----------------------------------------------
-- Entry Point
-----------------------------------------------

app :: App PollState Actions
app = App
    { appInitialState    = initialState
    , appInitialRequests = []
    , appApplyAction     = applyAction
    , appRender          = renderState
    }

initialState :: PollState
initialState = PollState "" ["", "", "", ""]

