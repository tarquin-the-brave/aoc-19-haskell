--
-- Prog Monad, for any program that might find itself in these states.
--
module Program
  ( Prog (..)
  , crash
  , running
  , awaitInput
  , end
  ) where

data Prog a = Running a | AwaitInput a | End a | Crashed String deriving(Show, Eq)

instance Functor Prog where
  fmap _ (Crashed e) = Crashed e
  fmap f (End a) =  End (f a)
  fmap f (Running a) = Running (f a)
  fmap f (AwaitInput a) = AwaitInput (f a)

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

crash :: Show a => String -> Prog a -> Prog a
crash reason (Running a) = fail (reason ++ ": " ++ (show a))
crash reason (AwaitInput a) = fail (reason ++ ": " ++ (show a))
crash reason (End a) = fail (reason ++ ": " ++ (show a))
crash reason (Crashed e) = Crashed (reason ++ "\n" ++ e)

running :: Show a => Prog a -> Prog a
running (Running a) = Running a
running (AwaitInput a) = Running a
running other = crash "Cannot run an ended program" other

awaitInput :: Show a => Prog a -> Prog a
awaitInput (Running a) = AwaitInput a
awaitInput (AwaitInput a) = AwaitInput a
awaitInput other = crash "Cannot set ended program to await input" other

end :: Show a => Prog a -> Prog a
end (Running a) = AwaitInput a
end (AwaitInput a) = AwaitInput a
end other = crash "Program already ended" other


