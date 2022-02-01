module Leaflet.Core.Event
  ( eventCenter
  , eventZoom
  , eventContainerPoint
  , eventLatLng
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Leaflet.Util (type (×))
import Leaflet.Core.Types (Point, Zoom, Event, LatLng)


foreign import eventZoom_
  ∷ ∀ a. Fn3 (Maybe a) (a → Maybe a) Event (Effect (Maybe Zoom))

foreign import eventCenter_
  ∷ ∀ a. Fn4 (Maybe a) (a → Maybe a) (a → a → a × a) Event (Effect (Maybe LatLng))

foreign import eventContainerPoint_
  ∷ ∀ a. Fn4 (Maybe a) (a → Maybe a) (a → a → a × a) Event (Effect (Maybe Point))

foreign import eventLatLng_
  ∷ ∀ a. Fn3 (Maybe a) (a → Maybe a) Event (Effect (Maybe LatLng))

eventCenter
  ∷ ∀ m
  . MonadEffect m
  ⇒ Event
  → m (Maybe LatLng)
eventCenter e =
  liftEffect $ runFn4 eventCenter_ Nothing Just Tuple e

eventZoom
  ∷ ∀ m
  . MonadEffect m
  ⇒ Event
  → m (Maybe Zoom)
eventZoom e =
  liftEffect $ runFn3 eventZoom_ Nothing Just e

eventContainerPoint
  ∷ ∀ m
  . MonadEffect m
  ⇒ Event
  → m (Maybe Point)
eventContainerPoint e =
  liftEffect $ runFn4 eventContainerPoint_ Nothing Just Tuple e


eventLatLng
  ∷ ∀ m
  . MonadEffect m
  ⇒ Event
  → m (Maybe LatLng)
eventLatLng e =
  liftEffect $ runFn3 eventLatLng_ Nothing Just e
