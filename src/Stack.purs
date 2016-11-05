module Stack where

import Data.List.NonEmpty as L
import Data.List.Types as LT
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple)
import Utils ((><))

type Stack a = L.NonEmptyList a

init :: forall a. a -> Stack a
init = L.singleton

push :: forall a. a -> Stack a -> Stack a
push x s = LT.NonEmptyList (x :| toList s)

pop :: forall a. Stack a -> Tuple a (Maybe (Stack a))
pop s = L.head s >< L.fromList (L.tail s)
