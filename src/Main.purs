module Main where

import Graphics.Canvas as C
import Input as I
import StateStack as ST
import GameState as GS
import GameRestartState as GRS
import Control.Monad.Aff (Canceler, Aff, liftEff', launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS)
import Prelude (pure, bind, ($), (<$>), (<<<))
import Signal (runSignal, foldp) as S


main :: forall e.
        Eff (canvas :: CANVAS, exception :: EXCEPTION, console :: CONSOLE, dom :: DOM, timer :: TIMER | e )
            (Canceler (console :: CONSOLE, canvas :: CANVAS, dom :: DOM, timer :: TIMER | e ) )
main = do
  c <- C.getCanvasElementById "canvas"
  let f = ST.pop <<< ST.init
  case c of
    Nothing ->
      launchAff $ liftEff' $ log "Could not load canvas"
    Just canvas -> do
      context <- C.getContext2D canvas
      input <- I.input
      launchAff $ do
        mStateStack <- initStateStack
        case mStateStack of
          Left err -> liftEff' $ log err
          Right states -> do
            let game = S.foldp ST.update states input
            liftEff' $ S.runSignal (ST.render context <$> game)

initStateStack :: forall e. Aff (canvas :: CANVAS | e) (Either String ST.StateStack)
initStateStack = do
  s <- GS.tryLoadState
  pure (ST.init <<< GRS.initState <$> s)

