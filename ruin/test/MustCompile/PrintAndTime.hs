{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

-- | This is the example from the Haddock on 'MapF'.

module MustCompile.PrintAndTime () where

import qualified System.CPUTime

import Data.Ruin

data PrintAndTime = MkPrintAndTime

instance (Show a,f ~ (a -> IO (a,Integer))) => FPure PrintAndTime s f where
  fpure = \_ x -> do
    print x
    (,) x <$> System.CPUTime.getCPUTime

_printAndTime :: (Show x, Show y) => ("x" :@ x,"y" :@ y) -> IO ("x" :@ (x,Integer),"y" :@ (y,Integer))
_printAndTime = rmapA MkPrintAndTime
