{-# Language TemplateHaskell #-}

-- | The core subset of the libary interface. "Data.Ruin" offers more.
--
-- The basic idea of this module is that anonymous records are only
-- used to emulate named function arguments. Thus, this module
-- provides very little in the way of creating records; it expects
-- that you'll declare and build your record types as usual.
--
-- The most solid use case for this module is a data type for parsing
-- the command-line.
--
-- [Step 1] Define a record type for each command.
--
-- [Step 2] Define a sum type where each constructor contains only the
-- corresponding command record type.
--
-- [Step 3] Define a command-line parser for each record type using
-- 'rtoA'.
--
-- [Step 4] Combine those parser using 'rupA'.

module Data.Ruin.Core (
  -- * Records
  (:@),
  dub,
  undub,

  -- * Accessing parts of records
  rpat,

  -- * Building records
  Build,
  rna,
  rnaA,

  -- * Pure Combinators
  (<@),
  rfrom,
  rsym,
  rup,

  -- * 'Applicative' Combinators
  rtoA,
  rupA,

  -- * Conveniences

  -- ** Avoid unused selectors
  NoWarnUnusedTopBind(..),

  -- ** Splice
  makeRecords,
  ) where

import Data.Ruin.All
import Data.Ruin.Internal
import Data.Ruin.QQ hiding (rna,rnaA)
import Data.Ruin.QQ.Parser (QQ(..),pQQ)
import Data.Ruin.TH

import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Text.Parsec (parse)

-- | 'rna' is like 'rpat', but it also works for expressions. All of
-- the sugar is supported in the dual way.
rna :: QuasiQuoter
rna = QuasiQuoter (pars' expQQ) (pars patQQ) nope nope
  where
  nope = fail "The `rna' quasiquoter only creates expressions or patterns."

-- | 'rnaA' is like 'rna', but it only works for expressions and it
-- only works inside an 'Applicative'.
rnaA :: QuasiQuoter
rnaA = QuasiQuoter (pars' expQQA) nope nope nope
  where
  nope = fail "The `rnaA' quasiquoter only creates expressions."

pars' :: (QQ -> TH.Q a) -> String -> TH.Q a
pars' k s = either (fail . show) k' $ parse pQQ "rna quasiquote" s
  where
  k' qq@(MkQQ Just{} _) = k qq
  k' _ = fail "The Data.Ruin.Core quasiquoters require a typename when used as expressions."
