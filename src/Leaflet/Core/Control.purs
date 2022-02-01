module Leaflet.Core.Control
  ( layers
  , addTo
  , remove
  ) where

import Prelude

import Prim.Row (class Union)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Foreign.Object as Foreign
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)

import Leaflet.Core.Types as T

foreign import layers_
  ∷ ∀ r. Fn3 (Foreign.Object T.Layer) (Foreign.Object T.LayerGroup) r (Effect T.Control)

foreign import addTo_
  ∷ Fn2 T.Leaflet T.Control (Effect T.Control)

foreign import remove_
  ∷ T.Control → Effect Unit

layers
  ∷ ∀ m r1 r2
  . MonadEffect m
  ⇒ Union r1 r2 (T.LayerControlConf ())
  ⇒ Foreign.Object T.Layer
  → Foreign.Object T.LayerGroup
  → Record r1
  → m T.Control
layers bs os r =
  liftEffect $ runFn3 layers_ bs os r

addTo
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → T.Control
  → m T.Control
addTo leaf control =
  liftEffect $ runFn2 addTo_ leaf control

remove
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Control
  → m Unit
remove control =
  liftEffect $ remove_ control
