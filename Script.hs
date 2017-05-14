{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Script where

import Control.Monad

import Types

data ScriptEff o r i s a where
  Modify :: (s -> s) -> ScriptEff o r i s s
  Await :: ScriptEff o r i s i
  Tell :: o -> ScriptEff o r i s ()
  Ask :: ScriptEff o r i s r
  Delay :: Delta -> ScriptEff o r i s ()
  Exit :: ScriptEff o r i s a

type Script o r i s a = FFree (ScriptEff o r i s) a

data Status o r i s =
  forall a . Runnable (Script o r i s a) |
  forall a . Sleeping Delta (Script o r i s a) |
  forall a . Awaiting (i -> Script o r i s a) |
  Ended

runScript :: r -> s -> Script o r i s a -> ([o], s, Status o r i s)
runScript r s0 scr = go scr [] s0 where
  go scr outs s = case scr of
    FPure _ -> (reverse outs, s, Ended)
    FImpure eff k -> case eff of
      Modify f -> let s' = f s in go (k s') outs s'
      Await -> (reverse outs, s, Awaiting k)
      Tell o -> go (k ()) (o:outs) s
      Ask -> go (k r) outs s
      Delay dt -> (reverse outs, s, Sleeping dt (k ()))
      Exit -> (reverse outs, s, Ended)

ask :: Script o r i s r
ask = etaF Ask

modify :: (s -> s) -> Script o r i s s
modify f = etaF (Modify f)

await :: Script o r i s i
await = etaF Await

tell :: o -> Script o r i s ()
tell = etaF . Tell

delay :: Delta -> Script o r i s ()
delay = etaF . Delay

exit :: Script o r i s a
exit = etaF Exit

-- freer monad
data Lan g a where
  Lan :: g x -> (x -> a) -> Lan g a

instance Functor (Lan g) where
  fmap f (Lan gx h) = Lan gx (f . h)

data FFree g a where
  FPure :: a -> FFree g a
  FImpure :: g x -> (x -> FFree g a) -> FFree g a

instance Functor (FFree f) where
  fmap f (FPure x) = FPure (f x)
  fmap f (FImpure u q) = FImpure u (fmap f . q)

instance Applicative (FFree f) where
  pure = return
  (<*>) = ap
  
instance Monad (FFree f) where
  return = FPure
  FPure x >>= k = k x
  FImpure u k' >>= k = FImpure u ((>>= k) . k')

etaF :: g a -> FFree g a
etaF ga = FImpure ga FPure

