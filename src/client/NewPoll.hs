module NewPoll where

import StrawPoll.Types
import StrawPoll.ListHelpers

import Bootstrap

import Prelude hiding (div)
import Blaze.React
import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Event.Keycode             as Keycode
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import qualified Blaze.React.Run.ReactJS          as ReactJS

import qualified Data.Text       as T
import Data.Time
import Data.Foldable   (foldMap)
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens

-----------------------------------------------
-- Data Definitions
-----------------------------------------------

data PollState = PollState { _question           :: T.Text
                           , _answers            :: [T.Text]
                           , _multipleChoices    :: Bool
                           , _permissiveChecking :: Bool
                           } deriving Show
makeLenses ''PollState

data Actions = UpdateQuestionTextA T.Text
             | UpdateAnswerTextA Int T.Text
             | ToggleMultipleChoicesA Bool
             | TogglePermissiveCheckingA Bool
             | NullActionA -- Do nothing
               deriving Show

-----------------------------------------------
-- Action Handling
-----------------------------------------------

applyAction :: Actions -> Transition PollState Actions
applyAction action =
  runTransitionM $ case action of
    UpdateQuestionTextA questionText -> do
      (PollState _ answers mult perm) <- get
      put $ PollState questionText answers mult perm

    UpdateAnswerTextA index newAnswerText  -> do
      a <- use answers
      let newAnswers = a & element index .~ newAnswerText
      let fixedUp = ((padToMinLength 4 "") . (++ [""]) . (dropTrailing "")) newAnswers
      answers .= fixedUp

    ToggleMultipleChoicesA t -> multipleChoices .= t

    TogglePermissiveCheckingA t -> permissiveChecking .= t

    NullActionA -> return ()

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
renderBody (PollState question answers multipleChoices permissiveChecking) =
  H.div $
    bootstrapRow $ do
      bootstrapColumn 4 $ H.toHtml ("left stuff" :: T.Text)
      bootstrapColumn 8 $
        bootstrapForm FormHorizontal $ do
          H.h1 $ H.toHtml question
          bootstrapInput H.! A.id "question"
                         H.! A.placeholder "Question:"
                         H.! A.value (H.toValue question)
                         H.! E.onValueChange UpdateQuestionTextA
          H.ul $ foldMap renderAnswer (zip [0..] answers)
          bootstrapFormGroup $ do
            H.input H.! A.type_ "checkbox"
                    H.! A.id "multiple_choices"
                    H.! A.checked multipleChoices
                    H.! E.onCheckedChange ToggleMultipleChoicesA
            H.label H.! A.for "multiple_choices"
                    $ "Allow Multiple Poll Choices?"
          bootstrapFormGroup $ do
            H.input H.! A.type_ "checkbox"
                    H.! A.id "permissive_checking"
                    H.! A.checked permissiveChecking
                    H.! E.onCheckedChange TogglePermissiveCheckingA
            H.label H.! A.for "permissive_checking"
                    $ "Permissive vote duplication checking?"
          bootstrapButton BtnSuccess
                          H.! A.href "http://google.com"
                          $ "Create Poll"


renderAnswer (index, answer) =
  bootstrapFormGroup $ do
      bootstrapColumn 2 $ H.label $ H.toHtml index
      bootstrapColumn 10 $
        bootstrapInput H.! A.placeholder "Enter Poll Option:"
                       H.! A.value (H.toValue answer)
                       H.! E.onValueChange (UpdateAnswerTextA index)

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
initialState = PollState "" ["", "", "", ""] True False

