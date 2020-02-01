module DynamicHtml.AddingEventHandling where

import Prelude

-- Imports for lesson
import CSS (backgroundColor, green, yellow, red)
import Control.Monad.State (get, put)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE

-- Imports for scaffolding
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

-- | Our state type. The button cycles through every stoplight color.
data State = Green
           | Yellow
           | Red

-- | Our action type. It indicates the button's state should be advanced.
data Action = NextColor

-- | Shows how to add event handling.
toggleButton :: StateAndActionRenderer State Action
toggleButton buttonState =
  let
    toggleLabel = case buttonState of
      Green -> "GREEN"
      Yellow -> "YELLOW"
      Red -> "RED"
    lightColor = case buttonState of
      Green -> green
      Yellow -> yellow
      Red -> red
  in
    HH.button
      [ HE.onClick \_ -> Just NextColor
      , CSS.style $ backgroundColor lightColor ]
      [ HH.text $ "The light is " <> toggleLabel ]

-- | Shows how to use actions to update the component's state
handleAction :: HandleSimpleAction State Action
handleAction = case _ of
  NextColor -> do
    oldState <- get
    let
      newState = case oldState of
        Green -> Yellow
        Yellow -> Red
        Red -> Green
    put newState

-- Now we can run the code

main :: Effect Unit
main =
  runStateAndActionComponent
    { initialState: Green
    , render: toggleButton
    , handleAction: handleAction
    }

-- Scaffolded Code --

-- | Renders HTML that can respond to events by translating them
-- | into a value of the `action` that one uses to handle the event.
type DynamicHtml action = ComponentHTML action () Aff

-- | A function that uses the `state` type's value to render HTML
-- | with simple event-handling via the `action` type.
type StateAndActionRenderer state action = (state -> DynamicHtml action)

-- | When an `action` type's value is received, this function
-- | determines how to update the component (e.g. state updates).
type HandleSimpleAction state action =
  (action -> H.HalogenM state action () Void Aff Unit)

-- | Combines all the code we need to define a simple componenet that supports
-- | state and simple event handling
type SimpleChildComponent state action =
  { initialState :: state
  , render :: StateAndActionRenderer state action
  , handleAction :: HandleSimpleAction state action
  }

-- | Uses the `state` type's value to render dynamic HTML
-- | with event handling via the `action` type.
runStateAndActionComponent :: forall state action.
                               SimpleChildComponent state action
                            -> Effect Unit
runStateAndActionComponent childSpec = do
  launchAff_ do
    body <- awaitBody
    runUI (stateAndActionCompontent childSpec) unit body

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
stateAndActionCompontent :: forall state action.
                            SimpleChildComponent state action
                         -> H.Component HH.HTML (Const Unit) Unit Void Aff
stateAndActionCompontent spec =
  H.mkComponent
    { initialState: const spec.initialState
    , render: spec.render
    , eval: H.mkEval $ H.defaultEval { handleAction = spec.handleAction }
    }
