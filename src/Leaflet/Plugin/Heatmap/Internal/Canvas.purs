module Leaflet.Plugin.Heatmap.Internal.Canvas
  ( draw
  , createCanvas
  , canvasToElement
  , defaultOptions
  , HeatmapOptions
  , HeatmapPoint
  , module G
  ) where

import Prelude

import Color (Color)
import Color as Color

import Effect (Effect, foreachE)

import Data.Array as A
import Data.ArrayBuffer.Types (Uint8ClampedArray)
import Data.Function.Uncurried (Fn2, Fn3, runFn3, runFn2, Fn4, runFn4)

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLElement (HTMLElement)
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element)

import Graphics.Canvas as G

import Math as Math

import Unsafe.Coerce (unsafeCoerce)

import Leaflet.Util ((∘))

foreign import modifyImageData ∷ Fn2 G.ImageData (Uint8ClampedArray → Uint8ClampedArray) G.ImageData
foreign import unsafeSet ∷ ∀ a. Fn3 Int a (Array a) (Array a)
foreign import unsafeGet ∷ ∀ a. Fn2 Int (Array a) a
foreign import unsafeFor ∷ ∀ a. Fn4 Int Int (Array a) (Int → Array a) (Array a)

asIntArray ∷ Uint8ClampedArray → Array Int
asIntArray = unsafeCoerce

fromIntArray ∷ Array Int → Uint8ClampedArray
fromIntArray = unsafeCoerce

createCanvas ∷ Effect G.CanvasElement
createCanvas = do
  w ← window
  doc ← document w
  map elementToCanvas
    $ createElement "canvas"
    $ htmlDocumentToDocument doc
  where
  htmlDocumentToDocument ∷ HTMLDocument → Document
  htmlDocumentToDocument = unsafeCoerce

elementToCanvas ∷ Element → G.CanvasElement
elementToCanvas = unsafeCoerce

canvasToElement ∷ G.CanvasElement → HTMLElement
canvasToElement = unsafeCoerce

type HeatmapPoint = { x ∷ Number, y ∷ Number, i ∷ Number }

gradientData
  ∷ Array {color ∷ Color, stop ∷ Number }
  → Effect (Array Int)
gradientData stops = do
  canvas ← createCanvas
  _ ← G.setCanvasDimensions canvas {width: 1.0, height: 256.0}
  ctx ← G.getContext2D canvas
  gradient ← G.createLinearGradient ctx {x0: 0.0, y0: 0.0, x1: 0.0, y1: 256.0}
  foreachE stops \{color, stop} → do
    void $ G.addColorStop gradient stop (Color.cssStringRGBA color)
  _ ← G.setGradientFillStyle ctx gradient
  _ ← G.fillRect ctx { x: 0.0, y: 0.0, height: 256.0, width: 1.0 }
  imgData ← G.getImageData ctx 0.0 0.0 1.0 256.0
  effectPure $ asIntArray $ G.imageDataBuffer imgData

radius
  ∷ Number
  → Number
  → Effect G.CanvasImageSource
radius r blur = do
  canvas ← createCanvas
  _ ← G.setCanvasDimensions canvas { width: dia, height: dia }

  ctx ← G.getContext2D canvas
  _ ← G.setShadowOffsetX ctx dia
  _ ← G.setShadowOffsetY ctx dia
  _ ← G.setShadowBlur ctx blur
  _ ← G.setShadowColor ctx "black"
  _ ← G.beginPath ctx
  _ ← G.arc ctx {x: -1.0 * rad, y: -1.0 * rad, radius: r, start: 0.0, end: Math.pi * 2.0 }
  _ ← G.closePath ctx
  _ ← G.fill ctx

  effectPure $ G.canvasElementToImageSource canvas
  where
  rad = r + blur
  dia = rad + rad

draw
  ∷ G.CanvasElement
  → HeatmapOptions
  → Array HeatmapPoint
  → Effect Unit
draw canvas opts points = do
  gr ← gradientData opts.colorStops
  circle ← radius opts.radius opts.blur
  height ← G.getCanvasHeight canvas
  width ← G.getCanvasWidth canvas
  ctx ← G.getContext2D canvas
  _ ← G.clearRect ctx { x: 0.0, y: 0.0, height, width }
  foreachE points \{x, y, i} → do
    _ ← G.setGlobalAlpha ctx $ Math.max (i / opts.maxIntensity) opts.minOpacity
    _ ← G.drawImage ctx circle (x - opts.radius) (y - opts.radius)
    effectUnit
  imgData ← G.getImageData ctx 0.0 0.0 width height
  let
    newImageData =
      runFn2 modifyImageData imgData
      $ fromIntArray
      ∘ colorize gr
      ∘ asIntArray
  void $ G.putImageData ctx newImageData 0.0 0.0

type HeatmapOptions =
  { minOpacity ∷ Number
  , maxIntensity ∷ Number
  , radius ∷ Number
  , blur ∷ Number
  , colorStops ∷ Array { stop ∷ Number, color ∷ Color }
  }

defaultOptions ∷ HeatmapOptions
defaultOptions =
  { minOpacity: 0.05
  , maxIntensity: 1.0
  , radius: 25.0
  , blur: 15.0
  , colorStops:
      [ { stop: 0.4, color: Color.rgba 0 0 255 1.0 }
      , { stop: 0.6, color: Color.rgba 0 255 255 1.0 }
      , { stop: 0.7, color: Color.rgba 0 255 0 1.0 }
      , { stop: 0.8, color: Color.rgba 255 255 0 1.0 }
      , { stop: 1.0, color: Color.rgba 255 0 0 1.0 }
      ]
  }

colorize ∷ Array Int → Array Int → Array Int
colorize grs circle =
  -- Using `circle` is totally unsafe after `colorize`
  runFn4 unsafeFor 0 (len - 1) circle \i →
    if i `mod` 4 /= 0
    then circle
    else
    let j = runFn2 unsafeGet (i + 3) circle
    in runFn3 unsafeSet i (runFn2 unsafeGet (j * 4) grs)
       $ runFn3 unsafeSet (i + 1) (runFn2 unsafeGet (j * 4 + 1) grs)
       $ runFn3 unsafeSet (i + 2) (runFn2 unsafeGet (j * 4 + 2) grs)
       $ circle
  where
  len ∷ Int
  len = A.length circle

effectUnit ∷ Effect Unit
effectUnit = effectPure unit

effectPure ∷ ∀ a. a → Effect a
effectPure = pure
