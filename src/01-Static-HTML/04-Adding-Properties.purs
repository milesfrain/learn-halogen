module StaticHTML.AddingProperties where

import Prelude

-- Imports for lesson
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP

-- Imports for scaffolding
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

-- | Shows how to use Halogen VDOM DSL to render static HTML
-- | that also includes properties
staticHtmlWithProps :: StaticHTML
staticHtmlWithProps =
  HH.div
    [ HP.id_ "top-div" ]
    [ HH.div
      [ HP.class_ $ ClassName "special-div" ]
      [ HH.span
        [ HP.classes [ ClassName "class1", ClassName "class2", ClassName "class3" ] ]
        [ HH.text "This is text in a span!" ]
      ]
    , HH.button
      [ HP.type_ ButtonButton
      , HP.classes [ ClassName "special-button", ClassName "it-clicks" ]
      ]
      [ HH.text "You can click me, but I don't do anything." ]
    ]

--- Scaffolded code below ---

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  runUI (staticComponent staticHtmlWithProps) unit body

-- | HTML written in Purescript via Halogen's HTML DSL
-- | that is always rendered the same and does not include any event handling.
type StaticHTML = H.ComponentHTML Unit () Aff

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
staticComponent :: StaticHTML
                -> H.Component HH.HTML (Const Unit) Unit Void Aff
staticComponent renderHtml =
  H.mkComponent
    { initialState: const unit
    , render: \_ -> renderHtml
    , eval: H.mkEval H.defaultEval
    }
