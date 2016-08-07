{-# Language ApplicativeDo #-}
{-# Language LambdaCase #-}
{-# Language MagicHash #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

{-# OPTIONS_HADDOCK hide,not-home #-}

module Data.Ruin.QQ (
  expQQ,
  expQQA,
  pars,
  patQQ,
  rna,
  rnaA,
  rpat,
  ) where

import           Data.Maybe (catMaybes)
import           GHC.Prim (Proxy#,proxy#)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Text.Parsec (parse)

import Data.Ruin.All
import Data.Ruin.Hoid
import Data.Ruin.Internal
import Data.Ruin.QQ.Parser

pars :: (QQ -> TH.Q a) -> String -> TH.Q a
pars k s = either (fail . show) k $ parse pQQ "rna quasiquote" s

expQQ :: QQ -> TH.ExpQ
expQQ (MkQQ typename binders) = case typename of
  Nothing -> e
  Just s -> [e| prto (proxy# :: Proxy# $(TH.conT (TH.mkName s))) $e |]
  where
  e = foldr TH.appE val (catMaybes seqs)
  (seqs,vals) = unzip $ map mk binders
  val = tupE vals

  mk (strictness,var,field) =
    ( if strictness then Just [e| seq $v |] else Nothing
    , [e| dub (mkLabel :: Label $(TH.litT (TH.strTyLit field))) $v |]
    )
    where
    v = TH.varE (TH.mkName var)

expQQA :: QQ -> TH.ExpQ
expQQA (MkQQ typename binders) =
  foldl app [e| pure $fun |] binders
  where
  app f (_,var,_) = [e| $f <*> $(TH.varE (TH.mkName var)) |]

  fun = do
    (seqs,pats,vals) <- unzip3 <$> mapM mk binders
    let e = tupE vals
    let result = case typename of
          Nothing -> e
          Just s -> [e| prto (proxy# :: Proxy# $(TH.conT (TH.mkName s))) $e |]
    TH.lamE pats $ foldr TH.appE result (catMaybes seqs)

  mk (strictness,var,field) = do
    n <- TH.newName (if "_" == var then "x" else var)
    let v = TH.varE n
    return (
        if strictness then Just [e| seq $v |] else Nothing
      ,
        TH.varP n
      ,
        [e| dub (mkLabel :: Label $(TH.litT (TH.strTyLit field))) $v |]
      )

patQQ :: QQ -> TH.PatQ
patQQ (MkQQ typename binders) = case typename of
  Nothing -> tp
  Just s -> [p| (rup . phoid (proxy# :: Proxy# $(TH.conT (TH.mkName s))) -> $tp) |]
  where
  tp = tupP $ map mk binders

  mk (strictness,var,field) = bang p
    where
    bang = if strictness then TH.bangP else id
    p = [p| (undub (mkLabel :: Label $(TH.litT (TH.strTyLit field))) -> $v) |]
    v = if "_" == var then TH.wildP else TH.varP (TH.mkName var)

tupE :: [TH.ExpQ] -> TH.ExpQ
tupE [e] = [e| MkTup1 $e |]
tupE es = TH.tupE es

tupP :: [TH.PatQ] -> TH.PatQ
tupP [p] = [p| MkTup1 $p |]
tupP ps = TH.tupP ps

-----

-- | Named arguments for functions.
--
-- @
--   (\\['rna'|x] -> x) :: 'Tup1' ("x" ':@' a) -> a
--
--   (\\['rna'|x y] -> (x,y)) :: ("x" ':@' a,"y" ':@' b) -> (a,b)
--
--   (\\['rna'|y x] -> (x,y)) :: ("y" ':@' b,"x" ':@' a) -> (a,b)
-- @
--
-- And so on. The 'Has' and 'Build' classes support such tuples up to
-- 8 components.
--
-- There are four pieces of special syntax, none of which can be
-- escaped.
--
-- * A @\@@ allows a different variable name than the field name.
--
--     @
--       (\\f ['rna'|l\@x|] ['rna'|r\@x|] -> f l r)
--         :: (a -> b -> c) -> 'Tup1' ("x" ':@' a) -> 'Tup1' ("x" ':@' b) -> c
--     @
--
-- * A name of @_@ is a wildcard pattern.
--
-- * A leading word of the form @(\<prefix>...\<suffix>)@ adds a
-- prefix and/or suffix to all of the variable names. This affects
-- even the names given with @\@@ syntax. It does not affect
-- wildcards.
--
--     @
--       (\\['rna'|(...L) x y|] ['rna'|(r_...') x y|] -> xL == r_x' && yL == r_y')
--         :: (Eq a,Eq b) => ("x" ':@' a,"y" ':@' b) -> ("x" ':@' a,"y" ':@' b) -> Bool
--     @
--
-- * A @!@ at the beginning of the pattern makes it strict.
--
--     @
--       -- strict
--       (\\['rna'|!x|] -> Just x) :: 'Tup1' ("x" ':@' a) -> Maybe a
--
--       -- strict
--       (\\['rna'|!x\@foo|] -> Just x) :: 'Tup1' ("foo" ':@' a) -> Maybe a
--
--       -- strict
--       (\\['rna'|!x\@!|] -> Just x) :: 'Tup1' ("!" ':@' a) -> Maybe a
--
--       -- not strict
--       (\\['rna'|x\@!|] -> Just x) :: 'Tup1' ("!" ':@' a) -> Maybe a
--     @
--
--     Note a @~@ pattern for binding would be redundant, since the
--     bindings are ultimately variable bindings. Though it may be
--     useful to apply a tilde pattern to the entire quasiquote.
--
-- * A leading word that is capitalized is interpreted as the name of
-- a record type and is ascribed via 'rfrom'.
--
--    @
--      data XY x y = MkXY {x :: x,y :: y}
--
--      (\\['rna'| XY x y|] -> (x,y)) :: XY t t1 -> (t, t1)
--    @
--
--    When both are present, the type name must precede the prefix
--    and/or suffix.
--
-- 'rna' also works as an expression. All of the sugar except
-- wildcards is supported in the dual way.
rna :: QuasiQuoter
rna = QuasiQuoter (pars expQQ) (pars patQQ) nope nope
  where
  nope = fail "The `rna' quasiquoter only creates expressions or patterns."

-- | 'rnaA' is like 'rna', but:
--
-- * it only works for expressions,
--
-- * it only works inside an 'Applicative'.
rnaA :: QuasiQuoter
rnaA = QuasiQuoter (pars expQQA) nope nope nope
  where
  nope = fail "The `rnaA' quasiquoter only creates expressions."

-----

-- | 'rpat' is like 'rna', but it only works for patterns.
rpat :: QuasiQuoter
rpat = QuasiQuoter nope (pars patQQ) nope nope
  where
  nope = fail "The `rpat' quasiquoter only creates patterns."
