module Main where

import Prelude
import Data.Lens
import Data.Array
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Control.Monad.Aff
import Utils
import GameObject
import Collisions
import Graphics.Canvas as C
import Input as I
import Signal as S
import Signal.DOM as S
import Control.Monad.Eff.Console (log)


main = do
  c <- C.getCanvasElementById "canvas"
  case c of
    Nothing ->
      launchAff $ liftEff' $ log "Could not load canvas"
    Just canvas -> do
      context <- C.getContext2D canvas
      input <- I.input
      launchAff $ do
        initState <- initialState
        let game = S.foldp update initState input
        liftEff' $ S.runSignal (render context <$> game)

-----------
-- Model
-----------

type State
  = { objs1 :: Array GameObject
    , objs2 :: Array GameObject
    }

initialState :: Aff _ State
initialState = do
  obj1 <- rect1
  obj2 <- rect2
  obj3 <- rect3
  pure $
    { objs1: [obj1]
    , objs2: [obj2,obj3]
    }

------------
-- Update
------------

update :: I.Input -> State -> State
update input state =
  undoCollisions $
  collisionLayers $
  (\state -> { objs1: map (moveObj input.direction) state.objs1
  , objs2: map (moveObj input.direction) state.objs2
  }) $ updateGravity $ collisionLayers state

collisionLayers :: State -> State
collisionLayers state =
  { objs1: state.objs1 `testCollisionWith` state.objs2
  , objs2: state.objs2 `testCollisionWith` state.objs1
  }

undoCollisions :: State -> State
undoCollisions state =
  { objs1: map undoCollision state.objs1
  , objs2: map undoCollision state.objs2
  }

updateGravity :: State -> State
updateGravity state =
  { objs1: map (gravity groundFunc) state.objs1
  , objs2: map (gravity groundFunc) state.objs2
  }

gravity :: (Point -> Point) -> GameObject -> GameObject
gravity groundFunc obj =
  let objPos = centerBottom obj
      ground = groundFunc objPos
      margin = min (ground.y - objPos.y) 0.8
  in moveObj {x: 0.0, y: margin} obj

groundFunc :: Point -> Point
groundFunc p =
  if p.y > 600.0 then p else p {y = p.y + 0.3}

------------
-- Render
------------

render :: C.Context2D -> State -> Eff ( canvas :: C.CANVAS | _) Unit
render context state = do
  clearCanvas context
  traverse (renderObj context) state.objs1
  traverse (renderObj context) state.objs2
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

