module GameObject where

import Prelude

import Data.Lens
import Data.Array
import Data.Maybe
import Math
import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Control.Monad.Aff
import Graphics.Canvas as C
import Signal as S
import Signal.DOM as S

import Utils
import Collisions
import Input as I


-----------
-- Model
-----------

type GameObject =
  { pos :: Point
  , size  :: Point
  , speed :: Number
  , collision :: Maybe Point
  , image :: C.CanvasImageSource
  }

rect1 :: Aff _ GameObject
rect1 = do
  image <- loadImageData "http://www.animatedimages.org/data/media/293/animated-pig-image-0131.gif"
  pure $
    { pos:  { x: width / 2.0 - 115.0, y: height / 2.0 - 15.0 }
    , size: { x: 40.0, y: 40.0 }
    , speed: 6.0
    , collision: Nothing
    , image: image
    }

rect2 :: Aff _ GameObject
rect2 = do
  image <- loadImageData "http://www.picgifs.com/graphics/a/apples/graphics-apples-474290.gif"
  pure $
    { pos:  { x : width / 2.0 - 15.0, y : height / 2.0 - 15.0 }
    , size: { x: 50.0, y: 50.0 }
    , speed: 0.0
    , collision: Nothing
    , image: image
    }

rect3 :: Aff _ GameObject
rect3 = do
  image <- loadImageData "http://www.picgifs.com/graphics/a/apples/graphics-apples-474290.gif"
  pure $
    { pos:  { x : width / 2.0 - 15.0, y : height / 2.0 + 110.0 }
    , size: { x: 50.0, y: 50.0 }
    , speed: 0.0
    , collision: Nothing
    , image: image
    }



------------
-- Update
------------

moveObj :: Point -> GameObject -> GameObject
moveObj direction rect =
  case rect.collision of
    Nothing ->
      over (pos <<< y) (_ + (direction.y * rect.speed)) <<< over (pos <<< x) (_ + (direction.x * rect.speed)) $ rect
    Just dir ->
      let direction = { x: -dir.x, y: -dir.y }
      in
        over (pos <<< y) (_ + (direction.y * rect.speed)) <<< over (pos <<< x) (_ + (direction.x * rect.speed)) $ rect

undoCollision :: GameObject -> GameObject
undoCollision rect =
  case rect.collision of
    Nothing ->
      rect
    Just dir ->
      let direction = { x: -dir.x, y: -dir.y }
      in
        over (pos <<< y) (_ + (direction.y * rect.speed)) <<< over (pos <<< x) (_ + (direction.x * rect.speed)) $ rect



------------
-- Render
------------

renderObj :: C.Context2D -> GameObject -> Eff ( canvas :: C.CANVAS | _) Unit
renderObj ctx state = do
  let obj = calculateSizeAndPosFromBB state
  C.drawImageScale ctx state.image obj.x obj.y obj.width obj.height
  pure unit

showCol :: Maybe Point -> String
showCol Nothing  = ""
showCol (Just a) = "(" <> show a.x <> "," <> show a.y <> ")"

showX :: Number -> String
showX 0.0 = "Center"
showX 1.0 = "Right"
showX (-1.0) = "Left"
showX _ = "What?"

showY :: Number -> String
showY 0.0 = "Center"
showY 1.0 = "Down"
showY (-1.0) = "Up"
showY _ = "What?"

calculateSizeAndPosFromBB :: { pos :: Point, size :: Point | _ }
                          -> { x :: Number, y :: Number, width :: Number, height :: Number }
calculateSizeAndPosFromBB obj =
  { x: obj.pos.x - (obj.size.x / 6.0)
  , y: obj.pos.y - (obj.size.y / 6.0)
  , width:  obj.size.x + (obj.size.x / 3.0)
  , height: obj.size.y + (obj.size.y / 3.0)
  }

centerBottom :: GameObject -> Point
centerBottom obj =
  let x = obj.pos.x + (obj.size.x / 2.0)
      y = obj.pos.y +  obj.size.y
  in {x: x, y: y}
