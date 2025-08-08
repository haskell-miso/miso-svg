-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad.State
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso hiding (update, text_)
import           Miso.String (ms)
import           Miso.Lens (this, (.=))
import           Miso.Svg hiding (height_, id_, style_, width_)
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp app
  { events = M.singleton "pointermove" False
  }
-----------------------------------------------------------------------------
-- | Component definition (uses 'component' smart constructor)
app :: App Model Action
app = component emptyModel updateModel viewModel
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = (0, 0)
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel (HandlePointer pointer) = this .= client pointer
-----------------------------------------------------------------------------
data Action = HandlePointer PointerEvent
-----------------------------------------------------------------------------
type Model = (Double, Double)
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel (x, y) =
  div_
    []
    [ svg_
      [ CSS.style_
        [ CSS.borderStyle "solid"
        , CSS.height "700px"
        , CSS.width "100%"
        ]
      , onPointerMove HandlePointer
      ]
      [ g_
        []
        [ ellipse_
          [ cx_ (ms x)
          , cy_ (ms y)
          , CSS.style_
            [ CSS.fill "yellow"
            , CSS.stroke "purple"
            , CSS.strokeWidth "2"
            ]
          , rx_ "100"
          , ry_ "100"
          ]
          []
        ]
      , text_
        [ x_ (ms x)
        , y_ (ms y)
        ]
        [ text $ ms $ show (round x, round y)
        ]
      ]
    ]
-----------------------------------------------------------------------------
