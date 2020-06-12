--
-- Prog Monad, for any program that might find itself in these states.
--
{-# LANGUAGE DeriveFunctor #-}
module Program
  ( Prog (..)
  , crash
  ) where

data Prog a = Running a | AwaitInput a | End a | Crashed String deriving(Show, Eq, Functor)

instance Applicative Prog where
  pure = Running
  _ <*> (Crashed e) = Crashed e
  (End f) <*> prog = fmap f prog
  (Running f) <*> prog = fmap f prog
  (AwaitInput f) <*> prog = fmap f prog

instance Monad Prog where
  return = Running
  (Crashed e) >>= _ = (Crashed e)
  (End prog) >>= k = k prog
  (Running prog) >>= k = k prog
  (AwaitInput prog) >>= k = k prog

instance MonadFail Prog where
  fail = Crashed

crash :: Show a => String -> a -> Prog a
crash reason a = fail $ reason ++ ": " ++ show a

