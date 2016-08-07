{-# Language LambdaCase #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

-- | This is a small wrapper around the generation capability of the
-- @smallcheck@ library.
--
-- The key idea is that everywhere smallcheck would create a
-- non-strict data constructor, it should also include the possibility
-- that that position is an exception. In any given generated value,
-- each such exception is unique. This means that if two functions
-- applies to that same generated value always produce either the same
-- value or the same exception, then those functions have the exact
-- same strictness.

module StrictCheck (
  SSerial(..),
  sameStrictness,
  sdecDepth,
  (<~>),
  ) where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad (guard)
import Control.Monad.State (State,evalState,get,modify)
import Control.Monad.Trans (lift)
import Test.SmallCheck.Series

import Data.Ruin ((:@),dub)
import Data.Ruin.Ancillaries (Pair(..),Tup1(..),mkLabel)

data Landmine = MkLandmine {unLandmine :: Int} deriving (Eq,Show)

instance Exception Landmine

type SSeries = Series (State Int)

-- | Use instead of 'decDepth'.
sdecDepth :: SSeries a -> SSeries a
sdecDepth m = do
  do d <- getDepth
     guard $ d > 0
  localDepth (subtract 1) $ do
    n <- lift get
    lift $ modify (+1)
    pure (throw (MkLandmine n)) <|> m

-- | Use instead of 'Serial'.
class SSerial a where sseries :: SSeries a

instance SSerial () where sseries = sdecDepth $ pure ()

instance SSerial a => SSerial (Tup1 a) where sseries = sdecDepth $ MkTup1 <$> sseries

instance (SSerial a,SSerial b) => SSerial (a,b) where
  sseries = sdecDepth $ (,) <$> sseries <~> sseries

instance (SSerial a,SSerial b,SSerial c) => SSerial (a,b,c) where
  sseries = sdecDepth $ (,,) <$> sseries <~> sseries <~> sseries

instance (SSerial a,SSerial b,SSerial c,SSerial d) => SSerial (a,b,c,d) where
  sseries = sdecDepth $ (,,,) <$> sseries <~> sseries <~> sseries <~> sseries

instance SSerial a => SSerial (s :@ a) where
  sseries = dub mkLabel <$> sseries   -- newtypes don't have depth

instance (SSerial a,SSerial b) => SSerial (Pair a b) where
  sseries = sdecDepth $ MkPair <$> sseries <~> sseries

andM :: Monad m => [m Bool] -> m Bool
andM = \case
  [] -> return True
  m:ms -> m >>= \case
    True -> andM ms
    False -> return False

-- | Returns 'True' iff @f@ and @g@ force the same parts of @a@ for all
-- possible inputs up to the given depth. Although, if @f@ or @g@
-- throws some exception other than the 'Landmine's this library
-- creates, this function returns 'False'.
sameStrictness :: SSerial a => (a -> b) -> (a -> c) -> Int -> IO Bool
sameStrictness f g depth = andM $ map test xs
  where
  xs = flip evalState 0 $ listM depth sseries
  test x = do
    fx <- try $ evaluate $ f x
    gx <- try $ evaluate $ g x
    case (fx,gx) of
      (Right _,Right _) -> return True
      (,)
        (Left (fromException -> Just l))
        (Left (fromException -> Just r)) -> return $ unLandmine l == unLandmine r
      _ -> return False
