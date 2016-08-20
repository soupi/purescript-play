module Input where

import Prelude

import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Signal as S
import Signal.DOM as S

import Utils

type Input =
  { direction :: Point
  , action1 :: Boolean
  , action2 :: Boolean
  }

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

leftKeyCode = 37
rightKeyCode = 39
upKeyCode = 38
downKeyCode = 40

zKeyCode = 90
xKeyCode = 88
