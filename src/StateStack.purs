module StateStack where

import Data.List.NonEmpty as L
import Stack as Stack
import Control.Monad.Eff (Eff)
import Data.Exists (mkExists, runExists, Exists)
import Data.List.NonEmpty (fromList)
import Data.Maybe (Maybe, fromMaybe')
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple)
import Graphics.Canvas (CANVAS, Context2D)
import Input (Input)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (id, (>>>), Unit, ($))
import Stack (Stack)
import Utils ((><))

-- import Debug.Trace (traceShow)

type State = Exists StateF

newtype StateF s = StateF
  { state  :: s
  , update :: Input -> s -> Tuple Command s
  , render :: forall e. Context2D -> s -> Eff (canvas :: CANVAS | e) Unit
  }

data Command
  = Done
  | None
  | Push State

type StateStack = Stack State

push :: forall a. a -> Stack a -> Stack a
push = Stack.push

init :: forall a. a -> Stack a
init = Stack.init

pop :: forall a. Stack a -> Tuple a (Maybe (Stack a))
pop = Stack.pop

update :: Input -> StateStack -> StateStack
update i states =
  case updateState i x.head of
    Done >< s -> fromMaybe' (\_ -> unsafeCrashWith "Unexpected empty stack of states") (fromList x.tail)
    None >< s -> L.NonEmptyList (s :| x.tail)
    Push newS >< s -> newS `push` L.NonEmptyList (s :| x.tail)
  where x = L.uncons states

updateState :: Input -> State -> Tuple Command State
updateState i = runExists \(StateF s) ->
  case s.update i s.state of
    ms >< newS -> ms >< (mkExists $ StateF s { state = newS })

render :: forall e. Context2D -> StateStack -> Eff (canvas :: CANVAS | e) Unit
render ctx = debug >>> L.head >>> runExists \(StateF s) -> s.render ctx s.state
  where
    debug = id
--  debug x = traceShow (L.length x) (\_ -> x)
