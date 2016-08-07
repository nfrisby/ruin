module Data.Ruin.Eval (
  Eval(..),
  runEval,
  ) where

-- | An evaluation box. It is crucially not a newtype nor strict in
-- its contents.
--
-- This data type is a simplification of
-- 'Control.Parallel.Strategies.Eval'.
data Eval a = Done a

runEval :: Eval a -> a
{-# INLINE runEval #-}
runEval (Done a) = a

instance Functor Eval where
  {-# INLINE fmap #-}
  fmap f (Done a) = Done (f a)

instance Applicative Eval where
  {-# INLINE pure #-}
  pure = Done
  {-# INLINE (<*>) #-}
  Done f <*> Done a = Done (f a)

instance Monad Eval where
  {-# INLINE return #-}
  return = Done
  {-# INLINE (>>=) #-}
  Done a >>= k = k a
