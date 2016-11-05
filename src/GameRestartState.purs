module GameRestartState where

import GameState as GS
import Graphics.Canvas as C
import Input as I
import StateStack as ST
import Control.Monad.Eff (Eff)
import Data.Exists (mkExists)
import Data.Tuple (Tuple)
import Prelude (Unit, unit, pure, ($))
import Utils ((><))

-----------
-- State --
-----------

initState :: State -> ST.State
initState s = mkExists $ ST.StateF { state : s, update : update, render : render }

-----------
-- Model
-----------

type State = GS.State

------------
-- Update
------------

update :: I.Input -> State -> Tuple ST.Command State
update _ state = ST.Push (GS.initState state) >< state

------------
-- Render
------------

render :: forall e. C.Context2D -> State -> Eff ( canvas :: C.CANVAS | e) Unit
render _ _ = pure unit

