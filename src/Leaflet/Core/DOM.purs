module Leaflet.Core.DOM
  ( testProp
  , setStyle
  , any3d
  , setPosition
  , setTransform
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn3, Fn2, runFn3, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Leaflet.Core.Types (Point)
import Leaflet.Util ((×))
import Web.HTML (HTMLElement)

foreign import any3d_
  ∷ Effect Boolean

foreign import testProp_
  ∷ ∀ a. Fn3 a (a → Maybe a) (Array String) (Effect (Maybe String))

foreign import setStyle_
  ∷ Fn3 String String HTMLElement (Effect Unit)

foreign import setPosition_
  ∷ Fn2 HTMLElement (Array Int) (Effect Unit)

foreign import setTransform_
  ∷ Fn3 HTMLElement (Array Int) Number (Effect Unit)

testProp
  ∷ ∀ m f
  . MonadEffect m
  ⇒ Foldable f
  ⇒ f String
  → m (Maybe String)
testProp a =
  liftEffect $ runFn3 testProp_ Nothing Just $ A.fromFoldable a

setStyle
  ∷ ∀ m
  . MonadEffect m
  ⇒ String
  → String
  → HTMLElement
  → m Unit
setStyle k v n = liftEffect $ runFn3 setStyle_ k v n

any3d
  ∷ ∀ m
  . MonadEffect m
  ⇒ m Boolean
any3d = liftEffect any3d_

setPosition
  ∷ ∀ m
  . MonadEffect m
  ⇒ HTMLElement
  → Point
  → m Unit
setPosition n (a × b) =
  liftEffect $ runFn2 setPosition_ n [a, b]

setTransform
  ∷ ∀ m
  . MonadEffect m
  ⇒ HTMLElement
  → Point
  → Number
  → m Unit
setTransform n (a × b) scale =
  liftEffect $ runFn3 setTransform_ n [a, b] scale
