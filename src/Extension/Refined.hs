module Extension.Refined 
  ( pattern Refined
  , rerefine
  , parseSomeException

  -- re-exports
  , Refined
  , Predicate(..)
  , RefineException(..)
  , FromTo(..)
  , type (&&)
  , refine
  , unrefine
  , success
  , throwRefineSomeException
  ) where

import Refined (Refined, Predicate(..), RefineException(..), FromTo(..), type (&&), refine, unrefine, success, throwRefineSomeException)
import Data.These (These(..))
import Control.Exception.Base (SomeException)
import Control.Applicative ((<|>))

pattern Refined :: x -> Refined p x
pattern Refined x <- (unrefine -> x)

rerefine :: forall p x. Predicate p x => (x -> x) -> Refined p x -> Either RefineException (Refined p x)
rerefine f = refine . f . unrefine 

parseSomeException :: RefineException -> Maybe SomeException
parseSomeException = \case
  RefineSomeException _ e -> Just e

  RefineAndException _ and' -> case and' of
    This l     -> parseSomeException l
    That r     -> parseSomeException r
    These l r  -> parseSomeException l <|> parseSomeException r

  RefineOrException _ l r ->
    parseSomeException l <|> parseSomeException r

  RefineXorException _ xor -> case xor of
    Just (l, r) -> parseSomeException l <|> parseSomeException r
    Nothing     -> Nothing

  RefineNotException _ -> Nothing

  RefineOtherException _ _ -> Nothing
