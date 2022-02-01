module Leaflet.Core.Types where

import Color (Color)
import Data.Tuple (Tuple)
import URI (URIRef)
import URI.URIRef (URIRefOptions)
import Unsafe.Coerce (unsafeCoerce)
--------------------------------------------------------------------------------
-- Abstract imported types
--------------------------------------------------------------------------------
data Leaflet
data Layer
data TileLayer
data Path
data UILayer
data Marker
data Icon
data Popup
data CircleMarker
data Circle
data Polyline
data Polygon
data Rectangle
data Renderer
data Evented
data Event
data Zoom
data Degrees
data LayerGroup
data Control
data LeafURIRef

type LeafURIRefR userInfo hosts path hierPath relPath query fragment =
  { uri :: URIRef userInfo hosts path hierPath relPath query fragment
  , opts :: Record (URIRefOptions userInfo hosts path hierPath relPath query fragment)
  }

mkLeafURIRef
  :: forall userInfo hosts path hierPath relPath query fragment
   . LeafURIRefR userInfo hosts path hierPath relPath query fragment
  -> LeafURIRef
mkLeafURIRef = unsafeCoerce

runLeafURIRef
  :: forall r
  . (forall userInfo hosts path hierPath relPath query fragment
    . LeafURIRefR userInfo hosts path hierPath relPath query fragment
    -> r)
  -> LeafURIRef
  -> r
runLeafURIRef = unsafeCoerce

-- layer converters
tileToLayer ∷ TileLayer → Layer
tileToLayer = unsafeCoerce

pathToLayer ∷ Path → Layer
pathToLayer = unsafeCoerce

markerToLayer ∷ Marker → Layer
markerToLayer = unsafeCoerce

polylineToLayer ∷ Polyline → Layer
polylineToLayer = unsafeCoerce

polygonToLayer ∷ Polygon → Layer
polygonToLayer = unsafeCoerce

circleMarkerToLayer ∷ CircleMarker → Layer
circleMarkerToLayer = unsafeCoerce

circleToLayer ∷ Circle → Layer
circleToLayer = unsafeCoerce

rectangleToLayer ∷ Rectangle → Layer
rectangleToLayer = unsafeCoerce

groupToLayer ∷ LayerGroup → Layer
groupToLayer = unsafeCoerce

mapToEvented ∷ Leaflet → Evented
mapToEvented = unsafeCoerce

layerToEvented ∷ Layer → Evented
layerToEvented = unsafeCoerce

zoomToNumber ∷ Zoom → Number
zoomToNumber = unsafeCoerce

degreesToNumber ∷ Degrees → Number
degreesToNumber = unsafeCoerce

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------
data LineCap
  = ButtLC
  | RoundLC
  | SquareLC

data LineJoin
  = MiterLJ
  | RoundLJ
  | BevelLJ

data PercentOrPixel
  = Percent Number
  | Pixel Int

data FillRule
  = NonZero
  | EvenOdd
  | Inherit

type Point = Tuple Int Int
type LatLng = { lat ∷ Degrees, lng ∷ Degrees }
type Bounds = Tuple Point Point

--------------------------------------------------------------------------------
-- Constructor options
--------------------------------------------------------------------------------
type LayerConf r =
  ( pane ∷ String
  , attribution ∷ String
  | r)

type InteractiveLayerConf r =
  LayerConf
  ( interactive ∷ Boolean
  | r)

type DivOverlayConf r =
  LayerConf
  ( offset ∷ Point
  | r)

type PopupConf =
  DivOverlayConf
  ( maxWidth ∷ Int
  , minWidth ∷ Int
  , maxHeight ∷ Int
  , minHeight ∷ Int
  , autoPan ∷ Boolean
  , autoPanPaddingTopLeft ∷ Point
  , autoPanPaddingBottomRight ∷ Point
  , autoPanPadding ∷ Point
  , keepInView ∷ Boolean
  , closeButton ∷ Boolean
  , closeOnClick ∷ Boolean
  , autoClose ∷ Boolean
  , className ∷ String
  )

type IconConf =
  InteractiveLayerConf
  ( iconUrl ∷ LeafURIRef
  , iconRetinaUrl ∷ LeafURIRef
  , iconSize ∷ Point
  , iconAnchor ∷ Point
  , popupAnchor ∷ Point
  , shadowUrl ∷ LeafURIRef
  , shadowRetinaUrl ∷ LeafURIRef
  , shadowSize ∷ Point
  , shadowAnchor ∷ Point
  , className ∷ String
  )

type PathConf r =
  InteractiveLayerConf
  ( stroke ∷ Boolean
  , color ∷ Color
  , weight ∷ Int
  , opacity ∷ Number
  , lineCap ∷ LineCap
  , lineJoin ∷ LineJoin
  , dashArray ∷ Array Int
  , dashOffset ∷ PercentOrPixel
  , fill ∷ Boolean
  , fillColor ∷ Color
  , fillOpacity ∷ Number
  , fillRule ∷ FillRule
  , renderer ∷ Renderer
  , className ∷ String
  |r )

type PolylineConf r =
  PathConf
  ( smoothFactor ∷ Number
  , noClip ∷ Boolean
  |r )


-- Note: `radius` is in _meters_ for `Circle` and in _pixels_ for `CircleMarker`
type CircleConf =
  PathConf
  ( radius ∷ Number )

--------------------------------------------------------------------------------
-- Control options
--------------------------------------------------------------------------------

type ControlConf r =
  ( position ∷ String
  | r)

type LayerControlConf r =
  ControlConf
  ( collapsed ∷ Boolean
  , autoZIndex ∷ Boolean
  , hideSingleBase ∷ Boolean
  , sortLayers ∷ Boolean
  | r )


