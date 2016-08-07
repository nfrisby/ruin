{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

module MustCompile.CLO (
    -- * All commands
    Command(..), parseCommand,
    getCommandLine,

    -- * Each command
    Diff, parseDiff,
    List, parseList,
    Sync, parseSync,
  ) where

import Data.Ruin.Core

import GHC.Generics (Generic)
import Options.Applicative

----- Full command-line parser.

getCommandLine :: IO Command
getCommandLine = execParser opts
  where
  opts = info (helper <*> parseCommand) $ (
       fullDesc
    <>
       progDesc "An empty command-line interface that  demonstrates Data.Ruin.Core."
    )

data Command =
    Diff Diff
  |
    List List
  |
    Sync Sync
  deriving Show

parseCommand :: Parser Command
parseCommand =
      (Diff <$> subcommand "diff" "Compare two directories." parseDiff)
  <|>
      (List <$> subcommand "ls" "List a directory's contents." parseList)
  <|>
      (Sync <$> subcommand "sync" "Synchronize two directories." parseSync)

----- Commands and their parsers.

-- | List differences between two directories: files only in one of
-- the two and any shared files that differ.
data Diff = MkDiff {
    dir1 :: Dir
  ,
    dir2 :: Dir
  ,
    filter :: Filter
  }
  deriving (Generic,Show)

parseDiff :: Parser Diff
parseDiff = [rnaA| Diff (p...) filter dir1 dir2 |]
  where
  pfilter = parseFilter
  pdir1 = strArgument $ metavar "DIR1" <> help "A directory to compare."
  pdir2 = parseDir "DIR2" "The other directory to compare."

-- | List the files in a directory.
data List = MkList {
    dir :: Dir
  ,
    filter :: Filter
  }
  deriving (Generic,Show)

parseList :: Parser List
parseList = [rnaA| List (p...) filter dir |]
  where
  pfilter = parseFilter
  pdir = parseDir "DIR" "The directory to list."

-- | Synchronize two directories: add any files missing in one to the
-- other.
data Sync = MkSync {
    dir1 :: Dir
  ,
    dir2 :: Dir
  ,
    filter :: Filter
  }
  deriving (Generic,Show)

parseSync :: Parser Sync
parseSync = [rnaA| Sync (p...) filter dir1 dir2 |]
  where
  pfilter = parseFilter
  pdir1 = strArgument $ metavar "DIR1" <> help "A directory to synchronize."
  pdir2 = parseDir "DIR2" "The other directory to synchronize."

----- Ancillary parsers.

-- | A set of regexs that limits consideration to certain files. The
-- set is considered a disjunction (like @grep@'s @-e@). The 'Bool' is
-- 'True' if the regexes should be used as is, and it's 'False' if
-- their disjunction should be inverted (like @grep@'s @-v@).
type Filter = ([String],Bool)

parseFilter :: Parser Filter
parseFilter = flip (,) <$> fmap not v <*> many e
  where
  e = strOption $ short 'e' <> metavar "REGEX" <> help "Consider only names matching any of these regexes; can occur more than once."
  v = switch $ short 'v' <> help "Consider only names that don't match any of the regexes."

-- | A directory, defaulting to @.@.
type Dir = FilePath

parseDir :: String -> String -> Parser Dir
parseDir meta h = strArgument $ metavar meta <> value "." <> showDefaultWith id <> help h

----- A useful optparse-applicative combinator.

subcommand :: String -> String -> Parser a -> Parser a
subcommand n h p = subparser $
     metavar (n ++ " ...")
  <> command n (info (helper <*> p) (fullDesc <> progDesc h))

----- TH splices.

$(makeRecords [
    ''Diff
  ,
    ''List
  ,
    ''Sync
  ])
