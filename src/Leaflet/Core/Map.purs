module Leaflet.Core.Map
  ( containerPointToLayerPoint
  , getCenterOffset
  , getMapPanePos
  , getMaxZoom
  , getPanes
  , getSize
  , getZoom
  , getZoomScale
  , invalidateSize
  , latLngToContainerPoint
  , leaflet
  , setView
  , setZoom
  , zoomAnimation
  )
  where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object as Foreign
import Leaflet.Core.Converter (converter)
import Leaflet.Core.Types as T
import Leaflet.Util ((∘), (×))
import Web.HTML.HTMLElement (HTMLElement)

foreign import map_
  ∷ HTMLElement → Effect T.Leaflet

foreign import setView_
  ∷ Fn2 (Array T.Degrees) T.Leaflet (Effect T.Leaflet)

foreign import setZoom_
  ∷ Fn2 T.Zoom T.Leaflet (Effect T.Leaflet)

foreign import getSize_
  ∷ Fn2 (Int → Int → T.Point) T.Leaflet (Effect T.Point)

foreign import zoomAnimation_
  ∷ T.Leaflet → Effect Boolean

foreign import getPanes_
  ∷ T.Leaflet → Effect (Foreign.Object HTMLElement)

foreign import containerPointToLayerPoint_
  ∷ Fn3 (Int → Int → T.Point) (Array Int) T.Leaflet (Effect T.Point)

foreign import latLngToContainerPoint_
  ∷ Fn3 (Int → Int → T.Point) (Array T.Degrees) T.Leaflet (Effect T.Point)

foreign import getZoomScale_
  ∷ Fn2 T.Zoom T.Leaflet (Effect Number)

foreign import getMapPanePos_
  ∷ Fn2 (Int → Int → T.Point) T.Leaflet (Effect T.Point)

foreign import getCenterOffset_
  ∷ Fn3 (Int → Int → T.Point) T.LatLng T.Leaflet (Effect T.Point)

foreign import getMaxZoom_
  ∷ T.Leaflet → Effect T.Zoom

foreign import getZoom_
  ∷ T.Leaflet → Effect T.Zoom

foreign import invalidateSize_
  ∷ Fn2 Boolean T.Leaflet (Effect T.Leaflet)


leaflet
  ∷ ∀ m
  . MonadEffect m
  ⇒ HTMLElement
  → m T.Leaflet
leaflet = liftEffect ∘ map_

setView
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.LatLng
  → T.Leaflet
  → m T.Leaflet
setView latLng l =
  liftEffect $ runFn2 setView_ (converter.convertLatLng latLng) l

setZoom
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Zoom
  → T.Leaflet
  → m T.Leaflet
setZoom i l =
  liftEffect $ runFn2 setZoom_ i l

getSize
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → m T.Point
getSize l =
  liftEffect $ runFn2 getSize_ Tuple l

zoomAnimation
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → m Boolean
zoomAnimation = liftEffect ∘ zoomAnimation_

getPanes
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → m (Foreign.Object HTMLElement)
getPanes = liftEffect ∘ getPanes_


containerPointToLayerPoint
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Point
  → T.Leaflet
  → m T.Point
containerPointToLayerPoint (a × b) l =
  liftEffect $ runFn3 containerPointToLayerPoint_ Tuple [a, b] l

latLngToContainerPoint
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.LatLng
  → T.Leaflet
  → m T.Point
latLngToContainerPoint {lat, lng} l =
  liftEffect $ runFn3 latLngToContainerPoint_ Tuple [lat, lng] l

getZoomScale
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Zoom
  → T.Leaflet
  → m Number
getZoomScale zoom l =
  liftEffect $ runFn2 getZoomScale_ zoom l

getMapPanePos
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → m T.Point
getMapPanePos l =
  liftEffect $ runFn2 getMapPanePos_ Tuple l

getCenterOffset
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.LatLng
  → T.Leaflet
  → m T.Point
getCenterOffset a b =
  liftEffect $ runFn3 getCenterOffset_ Tuple a b

getMaxZoom
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → m T.Zoom
getMaxZoom = liftEffect ∘ getMaxZoom_

getZoom
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → m T.Zoom
getZoom = liftEffect ∘ getZoom_

invalidateSize
  ∷ ∀ m
  . MonadEffect m
  ⇒ Boolean
  → T.Leaflet
  → m T.Leaflet
invalidateSize b l =
  liftEffect $ runFn2 invalidateSize_ b l
