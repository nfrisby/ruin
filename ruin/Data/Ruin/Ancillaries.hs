{-# Language ExplicitNamespaces #-}

-- | How the sausage is made.
--
-- These definitions are typically not revealed to the user, unless
-- you're doing something cheeky. So they are hidden behind this extra
-- import.

module Data.Ruin.Ancillaries (
  -- * Type-level basics
  type (++),
  Difference,
  DifferenceByFst,
  Elem,
  Fst,
  Head,
  Intersection,
  MapFst,
  Snd,
  Tail,
  -- * Disjointedness
  DisjointFields,
  MustBeDisjoint,
  MustHaveNoExtras,
  -- * Search
  Find,
  FindViaFields,
  Loc(..),
  MergeLoc,
  MightHave,
  Pair(..),
  SearchBoth,
  unPair,
  -- * Generics
  GArgsHas,
  GBox,
  GFieldType,
  GFields,
  GFind,
  GenericBuildConArgs,
  IsABox,
  -- ** @GHC.Generics@ defaults
  GenericBuild,
  GenericFieldType,
  GenericFields,
  GenericHas,
  GenericShape,
  genericExtricate1,
  genericBuild,
  genericBuildNonStrict,

  -- * Proxied
  --
  -- Template Haskell doesn't yet support type applications, so these
  -- can be handy.
  phoid,
  prfrom,
  prto,
  -- * Miscellancy
  (:@)(..),
  FieldNames,
  IsSubtypeOf,
  IsSymmetricRecordOf,
  Gives(..),
  GiveAllItHas(..),
  GivesThese,
  GivesThis,
  Hoid,
  Label(..),
  Lemma_AppendGivesThese,
  SymmetricRecordsA,
  Tup1(..),
  mkLabel,
  rupEval,
  rupNonStrict,
  ) where

import Data.Ruin.All
import Data.Ruin.Hoid
import Data.Ruin.Internal
