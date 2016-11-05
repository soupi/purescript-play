module GameState where

import Graphics.Canvas as C
import Input as I
import StateStack as ST
import Collisions (testCollisionWith)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Either (Either)
import Data.Exists (mkExists)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import GameObject (rect3, rect2, rect1, GameObject, renderObj, moveObj, centerBottom, undoCollision)
import Graphics.Canvas (Context2D, CANVAS)
import Prelude (Unit, bind, unit, pure, min, map, (+), (>), (-), ($))
import Utils (Point)

-----------
-- State --
-----------

initState :: State -> ST.State
initState s = mkExists $ ST.StateF { state : s, update : update, render : render }

tryLoadState :: forall e. Aff (canvas :: CANVAS | e) (Either String State)
tryLoadState = do
  mObj1 <- rect1
  mObj2 <- rect2
  mObj3 <- rect3
  pure do
    obj1 <- mObj1
    obj2 <- mObj2
    obj3 <- mObj3
    pure $
      { objs1: [obj1]
      , objs2: [obj2,obj3]
      }

-----------
-- Model
-----------

type State
  = { objs1 :: Array GameObject
    , objs2 :: Array GameObject
    }

------------
-- Update
------------

update :: I.Input -> State -> Tuple ST.Command State
update input state = Tuple (if input.esc then ST.Done else ST.None) $
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
gravity groundFunc' obj =
  let objPos = centerBottom obj
      ground = groundFunc' objPos
      margin = min (ground.y - objPos.y) 0.8
  in moveObj {x: 0.0, y: margin} obj

groundFunc :: Point -> Point
groundFunc p =
  if p.y > 600.0 then p else p {y = p.y + 0.3}

------------
-- Render
------------

render :: forall e. C.Context2D -> State -> Eff ( canvas :: C.CANVAS | e) Unit
render context state = do
  clearCanvas context
  traverse (renderObj context) state.objs1
  traverse (renderObj context) state.objs2
  pure unit

clearCanvas :: forall e. Context2D -> Eff ( canvas :: CANVAS | e ) Context2D
clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

