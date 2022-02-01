module Leaflet.Core.Plugin
  ( onAddRemove
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, new, write, read)

import Data.Function.Uncurried (Fn2, Fn4, runFn4, mkFn2)
import Data.Maybe (Maybe(..))

import Leaflet.Core.Types as T

foreign import onAddRemove_
  ∷ Fn4
      (Fn2 T.Layer T.Leaflet (Effect Unit))
      (Fn2 T.Layer T.Leaflet (Effect Unit))
      T.Layer
      T.Leaflet
      (Effect Unit)

onAddRemove
  ∷ ∀ a m
  . MonadEffect m
  ⇒ (T.Layer → T.Leaflet → Effect a)
  → (T.Layer → T.Leaflet → Maybe a → Effect Unit)
  → T.Layer
  → T.Leaflet
  → m (Ref (Maybe a))
onAddRemove init finish lay leaf = liftEffect do
  ref ← new Nothing
  runFn4 onAddRemove_
    (mkFn2 \l lf → do
        res ← init l lf
        write (Just res) ref)
    (mkFn2 \l lf → do
        mbv ← read ref
        finish l lf mbv)
    lay
    leaf
  pure ref
