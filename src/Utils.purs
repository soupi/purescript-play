module Utils where

import Control.Monad.Aff
import Graphics.Canvas as C
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple))
import Graphics.Canvas (CANVAS)

width :: Number
width = 1024.0
height :: Number
height = 800.0

type Point =
  { x :: Number, y :: Number }

makePoint :: Number -> Number -> Point
makePoint _x _y = { x: _x, y: _y }

loadImageData :: forall e. String -> Aff (canvas :: CANVAS | e) (Maybe C.CanvasImageSource)
loadImageData src = makeAff (\error success -> C.tryLoadImage src success)

------------
-- Lenses
------------

x :: Lens' Point Number
x = lens _.x (_ { x = _ })
y :: Lens' Point Number
y = lens _.y (_ { y = _ })

infixl 0 Tuple as ><
