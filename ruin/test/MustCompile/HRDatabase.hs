{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MustCompile.HRDatabase () where

import GHC.Generics (Generic)

import Data.Ruin

data Job = Baker | Carpenter | Coder | Artist
  deriving Eq

data Person = MkPerson {name :: String,age :: Int,job :: Job} deriving Generic
data Opening = MkOpening {job :: Job,company :: String} deriving Generic
data Match = MkMatch {job :: Job,company :: String,name :: String,age :: Int} deriving Generic

$(makeRecords [''Person,''Opening,''Match])

jobJoin :: [Person] -> [Opening] -> [Match]
jobJoin ps os =
  [ rto @Match (p <@ o)
  | p <- ps, o <- os, j p == j o
  ]
  where
  j (rup -> [rna|x@job|]) = x

people :: [Person]
people = [rnaA| Person (...I) name age job |]
  where
  nameI = ["Vin","Elend","Sazed"]
  ageI = (+20) <$> [1..5]
  jobI = [Baker,Carpenter]
