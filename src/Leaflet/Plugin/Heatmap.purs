module Leaflet.Plugin.Heatmap
  ( mkHeatmap
  , module C
  )
  where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as A
import Data.Foldable (class Foldable, for_, intercalate)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Foreign.Object as Foreign
import Leaflet.Core as LC
import Leaflet.Plugin.Heatmap.Internal.Canvas as C
import Leaflet.Util ((∘), (×), type (×))
import Math as Math
import Web.DOM.Element (setAttribute)
import Web.DOM.Node (appendChild, removeChild)
import Web.HTML.HTMLElement (toElement, toNode)

mkHeatmap
  ∷ ∀ m f
  . MonadEffect m
  ⇒ Foldable f
  ⇒ C.HeatmapOptions
  → f { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
  → LC.Layer
  → LC.Leaflet
  → m (Ref (Maybe C.CanvasElement))
mkHeatmap opts items lay leaf =
  LC.onAddRemove (onAdd opts items) onRemove lay leaf

onRemove ∷ LC.Layer → LC.Leaflet → Maybe C.CanvasElement → Effect Unit
onRemove _ leaf mbCanvas = do
  panes ← LC.getPanes leaf
  for_ mbCanvas \canvas →
    for_ (toNode <$> (Foreign.lookup "overlayPane" panes)) $ removeChild $ toNode $ C.canvasToElement canvas
  pure unit

onAdd
  ∷ ∀ f
  . Foldable f
  ⇒ C.HeatmapOptions
  → f { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
  → LC.Layer
  → LC.Leaflet
  → Effect C.CanvasElement
onAdd opts items lay leaf = do
  originProp ←
    LC.testProp [ "transformOrigin", "WebkitTransformOrigin", "msTransformOrigin" ]

  canvas ← C.createCanvas
  let
    canvasEl = C.canvasToElement canvas

  for_ originProp \p →
    LC.setStyle p "50% 50%" canvasEl

  x × y ← LC.getSize leaf
  _ ← C.setCanvasWidth canvas (Int.toNumber x)
  _ ← C.setCanvasHeight canvas (Int.toNumber y)

  threeD ← LC.any3d
  isZoom ← LC.zoomAnimation leaf
  let
    animClass
      | threeD && isZoom = "leaflet-zoom-animated"
      | otherwise = "leaflet-zoom-hide"

  setAttribute "class"
    (intercalate " " [ "leaflet-ps-heatmap-layer", "leaflet-layer", animClass ])
    (toElement canvasEl)

  panes ← LC.getPanes leaf
  for_ (toNode <$> (Foreign.lookup "overlayPane" panes)) $ appendChild $ toNode canvasEl

  let
    reset _ = do
      topLeft ← LC.containerPointToLayerPoint (0 × 0) leaf
      mapSize ← LC.getSize leaf
      _ ← C.setCanvasWidth canvas (Int.toNumber x)
      _ ← C.setCanvasHeight canvas (Int.toNumber y)
      LC.setPosition canvasEl topLeft
      redraw canvas items opts leaf

    zoomAnim e = void $ runMaybeT do
      zoom ← MaybeT $ LC.eventZoom e
      center ← MaybeT $ LC.eventCenter e
      scale ← LC.getZoomScale zoom leaf
      offset ← LC.getCenterOffset center leaf
      panePos ← LC.getMapPanePos leaf
      let coord = offset `LC.scalePoint` (-scale) # flip LC.subtractPoint panePos
      LC.setTransform canvasEl coord scale

  when (threeD && isZoom) $ LC.mapToEvented leaf # LC.on "zoomanim" zoomAnim
  LC.mapToEvented leaf # LC.on "moveend" reset

  redraw canvas items opts leaf

  pure canvas

redraw
  ∷ ∀ f
  . Foldable f
  ⇒ C.CanvasElement
  → f { lng ∷ LC.Degrees, lat ∷ LC.Degrees, i ∷ Number }
  → C.HeatmapOptions
  → LC.Leaflet
  → Effect Unit
redraw el items opts leaf = do
  size ← LC.getSize leaf
  maxZoom ← map LC.zoomToNumber $ LC.getMaxZoom leaf
  zoom ← map LC.zoomToNumber $ LC.getZoom leaf
  panePos ← LC.getMapPanePos leaf

  let
    radius ∷ Int
    radius = Int.floor opts.radius

    bounds ∷ LC.Bounds
    bounds = ((-radius) × (-radius)) × (LC.addPoint (radius × radius) size)

    intensityMultiplier ∷ Number
    intensityMultiplier =
      1.0 / (Math.pow 2.0 $ Math.max 0.0 $ Math.min 12.0 $ maxZoom - zoom)

    cellSize ∷ Int
    cellSize = radius / 2

    offsetX ∷ Int
    offsetX = (fst panePos) `mod` cellSize

    offsetY ∷ Int
    offsetY = (snd panePos) `mod` cellSize

    alterFn r Nothing = Just r
    alterFn rr (Just r) =
      let newI = rr.i + r.i
          newX = (r.x * r.i + rr.x * rr.i) / newI
          newY = (r.y * r.i + rr.y * rr.i) / newI
      in Just { x: newX, y: newY, i: newI }

    foldFn
      ∷ Map.Map (Int × Int) { x ∷ Number, y ∷ Number, i ∷ Number }
      → { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
      → Effect (Map.Map (Int × Int) { x ∷ Number, y ∷ Number, i ∷ Number })
    foldFn acc { lat, lng, i} = do
      p@(px × py) ← LC.latLngToContainerPoint {lat, lng} leaf
      if not $ LC.contains bounds p
        then pure acc
        else do
        let
          gx = (px - offsetX) / cellSize + 2
          gy = (py - offsetY) / cellSize + 2
          item = { x: Int.toNumber px, y: Int.toNumber py, i: i * intensityMultiplier }
        pure $ Map.alter (alterFn item) (gx × gy) acc

    groupPoints
      ∷ Array { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
      → Effect (Array { x ∷ Number, y ∷ Number, i ∷ Number })
    groupPoints is =
      map (A.fromFoldable ∘ Map.values) $ A.foldRecM foldFn Map.empty is

    adjustPoints
      ∷ { x ∷ Number, y ∷ Number, i ∷ Number }
      → { x ∷ Number, y ∷ Number, i ∷ Number }
    adjustPoints {x, y, i} =
      { x: x - opts.radius / 2.0
      , y: y - opts.radius / 2.0
      , i: Math.min i opts.maxIntensity
      }

  grouppedPoints ← groupPoints $ A.fromFoldable items

  liftEffect
    $ C.draw el opts
    $ map adjustPoints
    $ grouppedPoints
