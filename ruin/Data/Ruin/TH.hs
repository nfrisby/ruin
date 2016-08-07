{-# Language LambdaCase #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

{-# OPTIONS_HADDOCK hide,not-home #-}

module Data.Ruin.TH (makeRecords) where

import Data.List (find)
import Language.Haskell.TH

import Data.Ruin.All
import Data.Ruin.ClosedHas
import Data.Ruin.Hoid (Hoid)
import Data.Ruin.Internal

-- | Declare the straight-forward 'Has' and 'Build' instances for a
-- record type. A data type is a /record type/ if it has exactly one
-- constructor and that constructor is declared using record syntax.
--
-- An instance of a data family can be a record type; refer to that
-- type by the name of the instance's constructor.
--
-- The generated code relies on the "GHC.Generics" defaults in the
-- same way a user would; it merely relieves you from enumerating the
-- per-field instances.
--
-- Also, the splice will declare the instances in the style of
-- "Data.Ruin.ClosedHas".
makeRecords :: [Name] -> Q [Dec]
makeRecords = fmap concat . mapM interpretName

interpretName :: Name -> Q [Dec]
interpretName n0 = start
  where
  abort :: Q a
  abort = fail $ unwords [
      "`makeRecords' cannot handle `" ++ show n0 ++ "' because the declared data type"
    ,
      "doesn't have exactly one constructor"
    ,
      "or it doesn't use record syntax."
    ]

  start :: Q [Dec]
  start = do
    (dn,t,fnames,mshape) <- reifyDataDecl n0 >>= maybe abort interpretDataDecl
    fmap concat $ sequence $
        [d| instance NoWarnUnusedTopBind $t where
              noWarnUnusedTopBind $(recP dn [ (,) fname <$> wildP | fname <- fnames ]) = ()
          |]
      : maybe id addShape mshape
        [d| instance Build $t where
              {-# INLINE build #-}
              build = genericBuild
              {-# INLINE buildNonStrict #-}
              buildNonStrict = genericBuildNonStrict
          |]
      : [d| instance ClosedHas s $t => Has s $t where
              {-# INLINE extricate1 #-}
              extricate1 = closedExtricate1
          |]
      : [ [d| instance HasCase $s $t |]
        | s <- map (litT . strTyLit . nameBase) fnames
        ]

  addShape sh q = q >>= \case
    [InstanceD mo c ihead@(AppT _ t) decs] -> do
      o <- newName "o"
      s <- sh o
      let inst = TySynInstD ''Shape (TySynEqn [t,VarT o] s)
      return [InstanceD mo c ihead (decs ++ [inst])]
    _ -> fail "impossible! Quote of instance wasn't InstanceD"

  -- | Map a record type declaration to its ctor name, its fully
  -- applied type, its field names, and its shape.
  interpretDataDecl :: Dec -> Q (Name,TypeQ,[Name],Maybe (Name -> TypeQ))
  interpretDataDecl = \case
    DataD _ n args _ [interpretCtor -> Just (dn,fnames)] _ -> return (dn,app n (map tvb args) Nothing,fnames,Nothing)
    NewtypeD _ n args _ (interpretCtor -> Just (dn,fnames)) _ -> return (dn,app n (map tvb args) Nothing,fnames,Nothing)
    DataInstD _ n args mk [interpretCtor -> Just (dn,fnames)] _ -> return (dn,app n args mk,fnames,Just $ dfShape n args)
    NewtypeInstD _ n args mk (interpretCtor -> Just (dn,fnames)) _ -> return (dn,app n args mk,fnames,Just $ dfShape n args)
    _ -> abort
    where
    tvb = \case
      PlainTV n -> VarT n
      KindedTV n _ -> VarT n

    app :: Name -> [Type] -> Maybe Kind -> TypeQ
    app n args mk =
        return
      $ maybe id (flip SigT) mk
      $ foldl AppT (ConT n) args

  -- | Map a constructor to its ctor name and field names.
  interpretCtor :: Con -> Maybe (Name,[Name])
  interpretCtor = \case
    RecC dn vbts -> Just (dn,[ n | (n,_,_t) <- vbts ])
    ForallC _ _ ctor -> interpretCtor ctor
    RecGadtC (dn:_) vbts _ -> Just (dn,[ n | (n,_,_t) <- vbts ])
    _ -> Nothing

-- | If the name refers to a data type or a data constructor, return
-- the declaration of the data type.
--
-- Only fails monadically if 'reify' fails.
reifyDataDecl :: Name -> Q (Maybe Dec)
reifyDataDecl n0 = reify n0 >>= \case
  TyConI d -> return $ Just d
  -- indirect through a constructor name to its parent's name
  DataConI _ _ parent -> reify parent >>= \case
    TyConI d -> return $ Just d
    FamilyI DataFamilyD{} is -> return $ find sameCtorName is
    _ -> return Nothing
  _ -> return Nothing
  where
  sameCtorName :: Dec -> Bool
  sameCtorName = \case
    DataInstD _ _ _ _ [ctor] _ -> n0 == ctorName ctor
    NewtypeInstD _ _ _ _ ctor _ -> n0 == ctorName ctor
    _ -> False
    where
    ctorName :: Con -> Name
    ctorName = \case
      NormalC n _ -> n
      RecC n _ -> n
      InfixC _ n _ -> n
      ForallC _ _ ctor -> ctorName ctor
      GadtC (head -> n) _ _ -> n
      RecGadtC (head -> n) _ _ -> n

-- | Map a data family name and instance arguments to its 'Shape'.
dfShape :: Name -> [Type] -> Name -> TypeQ
dfShape dfname args (varT -> o) = reify dfname >>= \case
  FamilyI (DataFamilyD _ (length -> nidx) _) _ -> do
    let indices = take nidx args
    let t = return $ foldl AppT (ConT dfname) indices
    [t| Hoid $t $o |]
    where
  _ -> fail "impossible: DataInstD or NewtypeInstD does not name a DataFamilyD"
