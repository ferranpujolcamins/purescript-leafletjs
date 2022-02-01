module Leaflet.Core.Layer
  ( layer
  , tileLayer
  , marker
  , icon
  , popup
  , setURI
  , setContent
  , setLatLng
  , openOn
  , bindPopup
  , openPopup
  , circleMarker
  , circle
  , polyline
  , polygon
  , rectangle
  , on
  , once
  , setIcon
  , addLayer
  , removeLayer
  , layerGroup
  , off
  ) where

import Prelude

import Prim.Row (class Union)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Array as A
import Data.Foldable (class Foldable, foldMap)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Leaflet.Core.Types (LeafURIRef)

import Leaflet.Core.Converter (ConvertDict, converter)
import Leaflet.Core.Types as T
import Leaflet.Util ((∘))

foreign import layer_
  ∷ Effect T.Layer

foreign import tileLayer_
  ∷ String → Effect T.TileLayer

foreign import marker_
  ∷ Array T.Degrees → Effect T.Marker

foreign import icon_
  ∷ ∀ r. Fn2 ConvertDict r (Effect T.Icon)

foreign import popup_
  ∷ ∀ r. Fn2 ConvertDict r (Effect T.Popup)

foreign import setURI_
  ∷ Fn2 String T.TileLayer (Effect T.TileLayer)

foreign import setLatLng_
  ∷ Fn2 (Array T.Degrees) T.Popup (Effect T.Popup)

foreign import setContent_
  ∷ Fn2 String T.Popup (Effect T.Popup)

foreign import openOn_
  ∷ Fn2 T.Leaflet T.Popup (Effect T.Popup)

foreign import bindPopup_
  ∷ Fn2 String T.Layer (Effect T.Layer)

foreign import openPopup_
  ∷ Fn3 Boolean (Array T.Degrees) T.Layer (Effect T.Layer)

foreign import circleMarker_
  ∷ ∀ r. Fn3 (Array T.Degrees) ConvertDict r (Effect T.CircleMarker)

foreign import circle_
  ∷ ∀ r. Fn3 (Array T.Degrees) ConvertDict r (Effect T.Circle)

foreign import polyline_
  ∷ ∀ r. Fn3 (Array (Array T.Degrees)) ConvertDict r (Effect T.Polyline)

foreign import polygon_
  ∷ ∀ r. Fn3 (Array (Array T.Degrees)) ConvertDict r (Effect T.Polygon)

foreign import rectangle_
  ∷ ∀ r. Fn3 (Array (Array T.Degrees)) ConvertDict r (Effect T.Rectangle)

foreign import on_
  ∷ Fn3 String (T.Event → Effect Unit) T.Evented (Effect Unit)

foreign import off_
  ∷ Fn2 String T.Evented (Effect Unit)

foreign import once_
  ∷ Fn3 String (T.Event → Effect Unit) T.Evented (Effect Unit)

foreign import setIcon_
  ∷ Fn2 T.Icon T.Marker (Effect T.Marker)

foreign import addLayer_
  ∷ Fn2 T.Layer T.Leaflet (Effect T.Leaflet)

foreign import removeLayer_
  ∷ Fn2 T.Layer T.Leaflet (Effect T.Leaflet)

foreign import layerGroup_
  ∷ Array T.Layer → Effect T.LayerGroup


layerGroup
  ∷ ∀ m f
  . Foldable f
  ⇒ MonadEffect m
  ⇒ f T.Layer
  → m T.LayerGroup
layerGroup =
  liftEffect ∘ layerGroup_ ∘ A.fromFoldable

layer
  ∷ ∀ m
  . MonadEffect m
  ⇒ m T.Layer
layer = liftEffect layer_

tileLayer
  ∷ ∀ m
  . MonadEffect m
  ⇒ LeafURIRef
  → m T.TileLayer
tileLayer =
  liftEffect ∘ tileLayer_ ∘ converter.printURI

marker
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.LatLng
  → m T.Marker
marker latLng =
  liftEffect $ marker_ $ converter.convertLatLng latLng

icon
  ∷ ∀ r1 r2 m
  . Union r1 r2 T.IconConf
  ⇒ MonadEffect m
  ⇒ Record r1
  → m T.Icon
icon r =
  liftEffect $ runFn2 icon_ converter r

popup
  ∷ ∀ r1 r2 m
  . MonadEffect m
  ⇒ Union r1 r2 T.PopupConf
  ⇒ Record r1
  → m T.Popup
popup r =
  liftEffect $ runFn2 popup_ converter r

setURI
  ∷ ∀ m
  . MonadEffect m
  ⇒ LeafURIRef
  → T.TileLayer
  → m T.TileLayer
setURI uri tl =
  liftEffect $ runFn2 setURI_ (converter.printURI uri) tl

setLatLng
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.LatLng
  → T.Popup
  → m T.Popup
setLatLng latLng p =
  liftEffect $ runFn2 setLatLng_ (converter.convertLatLng latLng) p

setContent
  ∷ ∀ m
  . MonadEffect m
  ⇒ String
  → T.Popup
  → m T.Popup
setContent c p =
  liftEffect $ runFn2 setContent_ c p

openOn
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Leaflet
  → T.Popup
  → m T.Popup
openOn m p =
  liftEffect $ runFn2 openOn_ m p

circleMarker
  ∷ ∀ r1 r2 m
  . MonadEffect m
  ⇒ Union r1 r2 T.CircleConf
  ⇒ T.LatLng
  → Record r1
  → m T.CircleMarker
circleMarker ll r =
  liftEffect $ runFn3 circleMarker_ (converter.convertLatLng ll) converter r

circle
  ∷ ∀ r1 r2 m
  . MonadEffect m
  ⇒ Union r1 r2 T.CircleConf
  ⇒ T.LatLng
  → Record r1
  → m T.Circle
circle ll r =
  liftEffect $ runFn3 circle_ (converter.convertLatLng ll) converter r

polyline
  ∷ ∀ r1 r2 m f
  . MonadEffect m
  ⇒ Union r1 r2 (T.PolylineConf ())
  ⇒ Foldable f
  ⇒ f T.LatLng
  → Record r1
  → m T.Polyline
polyline lls r =
  liftEffect $ runFn3 polyline_ (foldMap (A.singleton ∘ converter.convertLatLng) lls) converter r

polygon
  ∷ ∀ r1 r2 m f
  . MonadEffect m
  ⇒ Union r1 r2 (T.PolylineConf ())
  ⇒ Foldable f
  ⇒ f T.LatLng
  → Record r1
  → m T.Polygon
polygon lls r =
  liftEffect $ runFn3 polygon_ (foldMap (A.singleton ∘ converter.convertLatLng) lls) converter r

rectangle
  ∷ ∀ r1 r2 m
  . MonadEffect m
  ⇒ Union r1 r2 (T.PolylineConf ())
  ⇒ T.LatLng
  → T.LatLng
  → Record r1
  → m T.Rectangle
rectangle a b r =
  liftEffect
  $ runFn3
      rectangle_
      [ converter.convertLatLng a, converter.convertLatLng b ]
      converter
      r
on
  ∷ ∀ m
  . MonadEffect m
  ⇒ String
  → (T.Event → Effect Unit)
  → T.Evented
  → m Unit
on e fn l =
  liftEffect $ runFn3 on_ e fn l

once
  ∷ ∀ m
  . MonadEffect m
  ⇒ String
  → (T.Event → Effect Unit)
  → T.Evented
  → m Unit
once e fn l =
  liftEffect $ runFn3 once_ e fn l

off
  ∷ ∀ m
  . MonadEffect m
  ⇒ String
  → T.Evented
  → m Unit
off e l =
  liftEffect $ runFn2 off_ e l

setIcon
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Icon
  → T.Marker
  → m T.Marker
setIcon i m =
  liftEffect $ runFn2 setIcon_ i m

bindPopup
  ∷ ∀ m
  . MonadEffect m
  ⇒ String
  → T.Layer
  → m T.Layer
bindPopup s l =
  liftEffect $ runFn2 bindPopup_ s l

openPopup
  ∷ ∀ m
  . MonadEffect m
  ⇒ Maybe T.LatLng
  → T.Layer
  → m T.Layer
openPopup mbLL l = liftEffect $ case mbLL of
  Nothing → runFn3 openPopup_ false [ ] l
  Just ll → runFn3 openPopup_ true (converter.convertLatLng ll) l

addLayer
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Layer
  → T.Leaflet
  → m T.Leaflet
addLayer lay leaf =
  liftEffect $ runFn2 addLayer_ lay leaf

removeLayer
  ∷ ∀ m
  . MonadEffect m
  ⇒ T.Layer
  → T.Leaflet
  → m T.Leaflet
removeLayer lay leaf =
  liftEffect $ runFn2 removeLayer_ lay leaf
