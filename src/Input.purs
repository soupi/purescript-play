module Input where

import Utils
import Signal.DOM as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Prelude (pure, bind, (<*>), (-), (<$>), ($))
import Signal (Signal)

type Input =
  { direction :: Point
  , action1 :: Boolean
  , action2 :: Boolean
  }

input :: forall e. Eff (dom :: DOM, timer :: TIMER | e) (Signal Input)               
input = do
  frames <- S.animationFrame
  arrowsInputs <- arrows
  aBtn <- S.keyPressed zKeyCode
  bBtn <- S.keyPressed xKeyCode
  pure $ (\_ arr act1 act2 -> { direction: arr, action1: act1, action2: act2 })
    <$> frames
    <*> arrowsInputs
    <*> aBtn
    <*> bBtn

arrows :: forall e. Eff (dom :: DOM | e) (Signal { x :: Number, y :: Number })               
arrows = do
  leftInput  <- S.keyPressed leftKeyCode
  rightInput <- S.keyPressed rightKeyCode
  upInput    <- S.keyPressed upKeyCode
  downInput  <- S.keyPressed downKeyCode
  let asNum b = if b then 1.0 else 0.0
  pure $  (\l r u d -> { x: asNum r - asNum l, y: asNum d - asNum u } )
      <$> leftInput
      <*> rightInput
      <*> upInput
      <*> downInput

leftKeyCode :: Int
leftKeyCode = 37
upKeyCode :: Int
upKeyCode = 38
rightKeyCode :: Int
rightKeyCode = 39
downKeyCode :: Int
downKeyCode = 40

xKeyCode :: Int
xKeyCode = 88
zKeyCode :: Int
zKeyCode = 90
