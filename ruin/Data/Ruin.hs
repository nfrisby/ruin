{- | Description: -- This is usually the only import.

#TH#

[<#TH TH splice>]

    The 'makeRecords' splice reifies any data type declared with
    record syntax into instances of this library's classes.

    @
      data XY y = MkXY {x :: Int, y :: y} deriving (Generic,Show)
      data YX x = MkYX {y :: Bool, x :: x} deriving (Generic,Show)

      $('makeRecords' [''XY,''YX])
      -- or $('makeRecords' ['MkXY,'MkYX]) would also work
    @

    Either @$('makeRecords' [''XY])@ or @$('makeRecords' ['MkXY])@
    splices in instances that reify the record syntax declaration of
    @MkXY@ into instances of the @ruin@ package's classes. Naming the
    constructor lets you reify a data family instance. The generated
    declarations defer to the "GHC.Generics" defaults as much as possible.

    The field names are used exactly, so use @-XDuplicateRecordFields@
    so that <#conversion the automatic conversions> work.

#singletons#

[<#singletons Singleton records>]

    The @ruin@ library also supports anonymous record types.

    The ':@' newtype is the singleton record type.

    @
      *> :t 'dub'
      'Label' s -> a -> s ':@' a

      *> :t 'dub' \#x   -- This uses -XOverloadedLabels.
      a -> "x" ':@' a

      *> :t 'undub' \#z
      "z" ':@' a -> a
    @

    And a tuple of record types is also a record type if the component
    record types do not have any fields with the same name. Currently it
    supports up to 8 tuple components. Note that you can nest them if you
    need more!

#projection#

[<#projection Projection>]

    The 'Has' class provides the 'extricate' projection, which allows
    <#careful-strictness careful control of strictness>.

    @
      *> :t 'Data.Ruin.Eval.runEval' . 'extricate' \#x
      'Has' "x" t => t -> 'FieldType' "x" t
    @

    'extricate' can navigate nested records with intuitive syntax.

    @
      *> :t 'Data.Ruin.Eval.runEval' . 'extricate' (\#x . \#y)
      ('Has' "y" ('FieldType' "x" t), 'Has' "x" t) =>
       t -> 'FieldType' "y" ('FieldType' "x" t)
    @

#conversion#

[<#conversion Conversion>]

    The 'Build' and 'IsSubtypeOf' constraints provide the 'rup' upcast
    with respect to record types' /width subtyping relationship/.
    'IsSubtypeOf' and 'rup' essentially delegate to 'Has' and 'extricate'
    for each necessary field.

    @
      *> let (y,z) = 'rup' ('dub' \#z (), MkXY {x=undefined,y="ash"})
      *> ('undub' \#y y,'undub' \#z z)
      ("ash",())
    @

    The 'rsym' isomorphism is 'rup' with a specialized type
    requiring that the two types be subtypes of one another.

    @
      *> let (y,z,x) = 'rsym' ('dub' \#z (), MkXY {x=1,y="ash"})
      *> ('undub' \#x x,'undub' \#y y,'undub' \#z z)
      (1,"ash",())
    @

#ascription#

[<#ascription Ascription>]

    The 'hoid' function is a family of identity functions, indexed by
    types of any order. It let's you ascribe types without having to fully
    apply them, which is often useful for polymorphic record types.

    @
      *> :t 'hoid' \@XY
      XY t -> XY t

      *> :t 'hoid' \@(->)
      (t -> t1) -> t -> t1
    @

    Record types have a notion of /shape/; see 'Shape' for
    details. The 'UnifyShape' constraint and the 'asShapeOf' ascription can
    both be used to drive type inference. There are some combinators whose
    types are very unweildy until the involved record types' shapes are
    fixed.

    The \"complement\" of a record type's shape is roughly the types
    of the record type's fields. The `UnifyFieldTypes` constraint and the
    `asFieldTypesOf` combinator support ascribing just that.

    @
      *> :t \\x y rc -> ('dub' \#x x,'dub' \#y y) \``asFieldTypesOf`\` rc
      'FieldType' "x" rc
      -> 'FieldType' "y" rc
      -> proxy rc
      -> ("x" ':@' 'FieldType' "x" rc, "y" ':@' 'FieldType' "y" rc)
    @

    Note that the second argument must have at least the fields of the
    first argument, but may have a different shape, which in particular
    means it may have \"extra\" fields.

    You'll generally use the 'hoidProxy' and 'proxyOf' combinators to
    create the second argument of 'asFieldTypesOf'.

    @
      *> :t 'hoidProxy' \@XY
      Data.Proxy.Proxy (XY t)
      *> :t 'proxyOf'
      a -> Data.Proxy.Proxy a
    @

#to-fro#

[<#to-fro Directed conversion>]

    The 'rfrom' and 'rto' combinators are 'rsym' but additionally
    require an explicit type argument (like 'hoid') so that they read
    well.

    @
      *> :t (\\(x,y) -> ('undub' \#x x,'undub' \#y y)) . 'rfrom' \@XY
      XY t -> (Int,t)

      *> 'rto' \@XY ('dub' \#x 1,'dub' \#y False)
      XY {x = 1, y = False}
    @

#qq#

[<#qq Quasiquoter>]

    The 'rna' quasiquoter enables named arguments for functions.

    @
      *> :t \\['rna'|x y|] -> x * x + 3 x * y - 2 * y * y
      Num a => ("x" ':@' a,"y" ':@' a) -> a
    @

    It can also create anonymous records.

    @
      *> :t \\x y -> ['rna'|x y|]
      a -> a1 -> ("x" ':@' a, "y" ':@' a1)
    @

    There are some usefuls syntactic sugars; see 'rna' for details.

    @
      *> :t ['rna'| id\@x show\@y |]
      Show a1 => ("x" ':@' (a -> a), "y" ':@' (a1 -> String))

      *> :t \\_x' _y' -> ['rna'| XY (_...') x y |]
      Int -> y -> XY y
    @

#suppression#

[<#suppression Suppressing fields>]

    The lopsided combinator '<@' allows for left-biased field overlap.

    @
      *> let xy = ('dub' \#x 1, 'dub' \#y False)
      *> let yz = ('dub' \#y 4, 'dub' \#z undefined)
      *> let f ['rna'|x y|] = x + y
      *> f $ 'rsym' $ yz '<@' xy
      5
    @

    The 'hide' combinator hides some fields, without having to replace
    to them.

    @
      *> :t 'extricate' \#x $ 'hide' \#x $ 'dub' \#x True
      \<interactive>:1:1: error:
          * ruin: The field \`x\' is hidden in the type
                "x" ':@' Bool
          * ...
      *> 'Data.Ruin.runEval' $ 'extricate' \#x $ 'hide' \#y $ 'dub' \#x True
      True
    @

    You can hide multiple fields at once:

    @
      *> :t 'hide' (\#x . \#y)
      rc -> 'Hide' '["x", "y"] rc
      True
    @

    Note that that hides the @x@ field and the @y@ field --- it
    doesn't hide a nested field @x.y@.

#partitioning#

[<#partitioning Partitioning records>]

    Sometimes suppressing a field isn't enough, and you need to
    actually remove it. In that case, use the partitioning combinators.

    The 'rdrop' combinator is a stronger version of 'hide'; given a
    list of labels and a record, it creates an anonymous record with the
    fields of the given record other than the given labels.

    @
      *> 'rdrop' (\#x . \#y) ('dub' \#x \'x\','dub' \#y \'y\','dub' \#z \'z\')
      'MkTup1' ('dub' \#z \'z\')
    @

    Instead of listing those labels explicitly, you can use
    'fieldLabelsOf' to take them from another known record type.

    Note that the 'rsym' combinator can split a record type into two
    other record types that fully partition the full origial.

    @
      data AB a b = {a::a,b::b} deriving (Generic,Show)
      data CD c d = {c::c,d::d} deriving (Generic,Show)
      $(makeRecords [''AB,''CD])

      *> 'hoid' \@AB *** 'hoid' \@CD $ 'rsym' ['rna'|mempty\@a mempty\@b mempty\@c mempty\@d|]
      (Monoid a3, Monoid a2, Monoid a1, Monoid a) =>
      t -> (AB a2 a3, CD a a1)
    @

    The 'rtake' combinator is similar, except it completely infers the
    type of the second component; specifically, the second component is an
    anonymous record type whose fields are those that are \"leftover\"
    from creating the first component. Otherwise, it's just like 'rsym'.

#custom-errors#

[<#custom-errors Custom errors>]

    Most of the error messages are easy to read.

    @
      *> (\\['rna'|x z|] -> x + z) $ 'rsym' ('dub' \#x 1, 'dub' \#y 2)
      \<interactive>:3:1: error:
        * ruin: Could not find a field \`z\' in the type
              "x" ':@' t
            or in the type
              "y" ':@' a
        * ...
    @

#fieldwise#

[<#fieldwise Fieldwise combinators>]

    Record types support an interface very similar to Applicative
    functors, based on fieldwise operations.

    The 'rpure', 'rmonopure', 'rmap', and 'rsplat' combinators are
    designed to mimic the familiar 'pure', '<$>', and '<*>'
    combinators.

    @
      *> let isZero x = 0 == x
      *> ['rna'|show succ pred isZero|] \``rsplat`\` 'rmonopure' (4 :: Int)
      ('dub' \#show "4",'dub' \#succ 5,'dub' \#pred 3,'dub' \#isZero False)
    @

    Others: 'rmempty', 'rmappend', and 'rlabel'. See
    <#fieldwise-more this section> for more information.

#applicative-variants#

[<#applicative-variants Applicative variants>]

    Many combinators have variants that work in an Applicative
    functor. In particular, the 'rnaA' quasiquoter only works for
    expressions, and it builds records in an Applicative functor, with the
    effects of each field ordered as in the quasiquoter text.

    @
      *> let x = [1,2]
      *> let y = ["y1","y2"]
      *> mapM_ print ['rnaA'|XY x y|]
      MkXY {x = 1,y = "y1"}
      MkXY {x = 1,y = "y2"}
      MkXY {x = 2,y = "y1"}
      MkXY {x = 2,y = "y2"}
      *> mapM_ print ['rnaA'|XY y x|]
      MkXY {x = 1,y = "y1"}
      MkXY {x = 2,y = "y1"}
      MkXY {x = 1,y = "y2"}
      MkXY {x = 2,y = "y2"}
    @

    Others: 'rfromA', 'rsymA', 'rtoA', 'rupA', 'rmapA', and 'rsplatA'.
-}
module Data.Ruin (
  -- * Singleton records
  (:@),
  dub,
  undub,

  -- * Accessing parts of records
  Has(..),
  extricate,
  rna,
  rnaA,

  -- * Hiding fields
  Hide,
  hide,

  -- * Record types' /width subtyping/

  -- ** Lowest-level combinators
  Build(..),

  -- ** Pure combinators
  (<@),
  rdrop,
  rfrom,
  rsym,
  rtake,
  rto,
  rup,

  -- ** Applicative combinators
  rfromA,
  rsymA,
  rtoA,
  rupA,

  -- * Fieldwise combinators
  --
  -- | #fieldwise-more#
  --
  -- The types of these combinators are not useful in the
  -- abstract. However, once the 'Shape' of any argument record type
  -- or result record type is fixed, the types reduce to something
  -- plain.
  --
  -- @
  --   *> :t 'rsplat' ['rna'|show\@x id\@y|]
  --   Show t => ("x" ':@' t, "y" ':@' t1) -> ("x" ':@' String, "y" ':@' t1)
  -- @
  --
  --
  -- A basic, self-contained example:
  --
  -- @
  --   data PrintAndTime = MkPrintAndTime
  --
  --   instance (Show a,f ~ (a -> IO (a,Integer))) => 'FPure' PrintAndTime s f where
  --     'fpure' _ x = do
  --       print x
  --       (,) x \<$> System.CPUTime.getCPUTime
  --
  --   *> 'rmapA' MkPrintAndTime ('dub' \#x \"OK", 'dub' \#y ())
  --   \"OK"
  --   ()
  --   ('dub' \#x (\"OK",43062500000000),'dub' \#y ((),43062500000000))
  -- @
  FPure(..),
  rlabel,
  rmempty,
  rmap,
  rmapA,
  rmappend,
  rmonopure,
  rpolypure,
  rpure,
  rsappend,
  rsplat,
  rsplatA,

  -- * Ascription
  UnifyFieldTypes,
  UnifyShape,
  asFieldTypesOf,
  asShapeOf,
  hoid,
  hoidProxy,

  -- * Conveniences
  Label,
  Labels,
  NoWarnUnusedTopBind(..),
  fieldLabelsOf,
  makeRecords,
  mkLabel,
  mkLabels,
  proxyOf,
  ) where

import Data.Ruin.All
import Data.Ruin.Deep (Labels,extricate)
import Data.Ruin.Fieldwise
import Data.Ruin.Hide
import Data.Ruin.Hoid (hoid,hoidProxy)
import Data.Ruin.Internal
import Data.Ruin.QQ (rna,rnaA)
import Data.Ruin.TH (makeRecords)
