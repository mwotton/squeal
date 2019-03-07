{-|
Module: Squeal.PostgreSQL.Manipulation
Description: Squeal data manipulation language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data manipulation language.
-}

{-# LANGUAGE
    DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternSynonyms
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation (..)
  , Manipulation_
  , queryStatement
  , QueryClause (..)
  , pattern Values_
  , ReturningClause (..)
  , pattern Returning_
  , ConflictClause (..)
  , ConflictTarget (..)
  , ConflictAction (..)
  , UsingClause (..)
    -- * Insert
  , insertInto
  , insertInto_
    -- * Update
  , update
  , update_
    -- * Delete
  , deleteFrom
  , deleteFrom_
  , also
  ) where

import Control.DeepSeq
import Data.ByteString hiding (foldr)
import Data.Kind (Type)
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

{- |
A `Manipulation` is a statement which may modify data in the database,
but does not alter its schemas. Examples are inserts, updates and deletes.
A `Query` is also considered a `Manipulation` even though it does not modify data.

simple insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint4, "col2" ::: 'Def :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '[]
  manipulation =
    insertInto_ #tab (Values_ (2 `as` #col1))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1") VALUES (2)

parameterized insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] '[]
  manipulation =
    insertInto_ #tab (Values_ (param @1 `as` #col1 :* param @2 `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (($1 :: int4), ($2 :: int4))

returning insert:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '["fromOnly" ::: 'NotNull 'PGint4]
  manipulation =
    insertInto #tab (Values_ (2 `as` #col1 :* 3 `as` #col2))
      OnConflictDoRaise (Returning (#col1 `as` #fromOnly))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, 3) RETURNING "col1" AS "fromOnly"

upsert:

>>> type CustomersColumns = '["name" ::: 'NoDef :=> 'NotNull 'PGtext, "email" ::: 'NoDef :=> 'NotNull 'PGtext]
>>> type CustomersConstraints = '["uq" ::: 'Unique '["name"]]
>>> type CustomersSchema = '["customers" ::: 'Table (CustomersConstraints :=> CustomersColumns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public CustomersSchema) '[] '[]
  manipulation =
    insertInto #customers
      (Values_ ("John Smith" `as` #name :* "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (((#excluded ! #email) <> "; " <> (#customers ! #email)) `as` #email) []))
      (Returning_ Nil)
in printSQL manipulation
:}
INSERT INTO "customers" ("name", "email") VALUES (E'John Smith', E'john@smith.com') ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || (E'; ' || "customers"."email"))

query insert:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '[]
  manipulation = insertInto_ #tab (Subquery (select Star (from (table #tab))))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") SELECT * FROM "tab" AS "tab"

update:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '[]
  manipulation = update_ #tab (2 `as` #col1) (#col1 ./= #col2)
in printSQL manipulation
:}
UPDATE "tab" SET "col1" = 2 WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[]
    '[ "col1" ::: 'NotNull 'PGint4
     , "col2" ::: 'NotNull 'PGint4 ]
  manipulation = deleteFrom #tab NoUsing (#col1 .== #col2) (Returning Star)
in printSQL manipulation
:}
DELETE FROM "tab" WHERE ("col1" = "col2") RETURNING *

delete and using clause:

>>> :{
type Schema3 =
  '[ "tab" ::: 'Table ('[] :=> Columns)
  , "other_tab" ::: 'Table ('[] :=> Columns)
  , "third_tab" ::: 'Table ('[] :=> Columns) ]
:}

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema3) '[] '[]
  manipulation =
    deleteFrom #tab (Using (table #other_tab & also (table #third_tab)))
    ( (#tab ! #col2 .== #other_tab ! #col2)
    .&& (#tab ! #col2 .== #third_tab ! #col2) )
    (Returning_ Nil)
in printSQL manipulation
:}
DELETE FROM "tab" USING "other_tab" AS "other_tab", "third_tab" AS "third_tab" WHERE (("tab"."col2" = "other_tab"."col2") AND ("tab"."col2" = "third_tab"."col2"))

with manipulation:

>>> type ProductsColumns = '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]
>>> type ProductsSchema = '["products" ::: 'Table ('[] :=> ProductsColumns), "products_deleted" ::: 'Table ('[] :=> ProductsColumns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public ProductsSchema) '[ 'NotNull 'PGdate] '[]
  manipulation = with
    (deleteFrom #products NoUsing (#date .< param @1) (Returning Star) `as` #del)
    (insertInto_ #products_deleted (Subquery (select Star (from (common #del)))))
in printSQL manipulation
:}
WITH "del" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" ("product", "date") SELECT * FROM "del" AS "del"
-}
newtype Manipulation
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Manipulation commons schemas params columns) where
  renderSQL = renderManipulation
instance With Manipulation where
  with Done manip = manip
  with ctes manip = UnsafeManipulation $
    "WITH" <+> renderSQL ctes <+> renderSQL manip

type family Manipulation_ (schemas :: SchemasType) (params :: Type) (row :: Type) where
  Manipulation_ schemas params row = Manipulation '[] schemas (TuplePG params) (RowPG row)

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query '[] commons schemas params columns
  -> Manipulation commons schemas params columns
queryStatement q = UnsafeManipulation $ renderSQL q

{-----------------------------------------
INSERT statements
-----------------------------------------}

{- |
When a table is created, it contains no data. The first thing to do
before a database can be of much use is to insert data. Data is
conceptually inserted one row at a time. Of course you can also insert
more than one row, but there is no way to insert less than one row.
Even if you know only some column values, a complete row must be created.
-}
insertInto
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , InsertRow sch tab inserts columns
     , row0 ~ TableToRow table
     , SOP.SListI columns
     , SOP.SListI row1 )
  => QualifiedAlias sch tab
  -> QueryClause commons schemas params inserts
  -> ConflictClause sch tab commons schemas params table
  -> ReturningClause commons schemas params '[tab ::: row0] row1
  -> Manipulation commons schemas params row1
insertInto tab qry conflict ret = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderSQL tab
  <+> renderSQL qry
  <> renderSQL conflict
  <> renderSQL ret

insertInto_
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , InsertRow sch tab inserts columns
     , row ~ TableToRow table
     , SOP.SListI columns )
  => QualifiedAlias sch tab
  -> QueryClause commons schemas params inserts
  -> Manipulation commons schemas params '[]
insertInto_ tab qry =
  insertInto tab qry OnConflictDoRaise (Returning_ Nil)

data QueryClause commons schemas params row where
  Values
    :: SOP.SListI row
    => NP (Aliased (Expression '[] 'Ungrouped commons schemas params '[])) row
    -> [NP (Aliased (Expression '[] 'Ungrouped commons schemas params '[])) row]
    -> QueryClause commons schemas params row
  Subquery
    :: (SOP.SListI row, SOP.All RenderCol row)
    => Query '[] commons schemas params row
    -> QueryClause commons schemas params row

instance RenderSQL (QueryClause commons schemas params row) where
  renderSQL = \case
    Values row0 rows ->
      parenthesized (renderCommaSeparated renderAliasPart row0)
      <+> "VALUES"
      <+> commaSeparated
            ( parenthesized
            . renderCommaSeparated renderValuePart <$> row0 : rows )
    Subquery qry ->
      parenthesized (commaSeparated (renderCols (SOP.hpure SOP.Proxy :: SOP.NP SOP.Proxy row)))
      <+>
      renderQuery qry
    where
      renderAliasPart, renderValuePart
        :: Aliased (Expression '[] 'Ungrouped commons schemas params '[]) col
        -> ByteString
      renderAliasPart (_ `As` name) = renderSQL name
      renderValuePart (value `As` _) = renderSQL value

class RenderCol column where
  renderCol :: SOP.Proxy column -> ByteString
instance (KnownSymbol col, column ~ (col ::: ty))
  => RenderCol column where
    renderCol _ = renderSQL (Alias @col)

renderCols
  :: forall xs. (SOP.SListI (xs :: RowType), SOP.All RenderCol xs)
  => NP SOP.Proxy xs -> [ByteString]
renderCols = case SOP.sList @xs of
  SOP.SNil -> \ SOP.Nil -> []
  SOP.SCons -> \ (p SOP.:* ps) -> renderCol p : renderCols ps

-- renderCol
--   :: forall col ty column.
--   ( KnownSymbol col, column ~ (col ::: ty) )
--   => SOP.Proxy column -> ByteString
-- renderCol _ = renderSQL (Alias @col)

pattern Values_
  :: SOP.SListI row
  => NP (Aliased (Expression '[] 'Ungrouped commons schemas params '[])) row
  -> QueryClause commons schemas params row
pattern Values_ vals = Values vals []

-- | A `ReturningClause` computes and return value(s) based
-- on each row actually inserted, updated or deleted. This is primarily
-- useful for obtaining values that were supplied by defaults, such as a
-- serial sequence number. However, any expression using the table's columns
-- is allowed. Only rows that were successfully inserted or updated or
-- deleted will be returned. For example, if a row was locked
-- but not updated because an `OnConflict` `DoUpdate` condition was not satisfied,
-- the row will not be returned. `Returning` `Star` will return all columns
-- in the row. Use @Returning Nil@ in the common case where no return
-- values are desired.
newtype ReturningClause commons schemas params from row =
  Returning (Selection '[] 'Ungrouped commons schemas params from row)

instance RenderSQL (ReturningClause commons schemas params from row) where
  renderSQL = \case
    Returning (List Nil) -> ""
    Returning selection -> " RETURNING" <+> renderSQL selection

pattern Returning_
  :: SOP.SListI row
  => NP (Aliased (Expression '[] 'Ungrouped commons schemas params from)) row
  -> ReturningClause commons schemas params from row
pattern Returning_ list = Returning (List list)

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflict` `DoNothing` simply avoids inserting a row.
-- `OnConflict` `DoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause sch tab commons schemas params table where
  OnConflictDoRaise :: ConflictClause sch tab commons schemas params table
  OnConflict
    :: ConflictTarget constraints
    -> ConflictAction sch tab commons schemas params columns
    -> ConflictClause sch tab commons schemas params (constraints :=> columns)

-- | Render a `ConflictClause`.
instance SOP.SListI (TableToColumns table)
  => RenderSQL (ConflictClause sch tab commons schemas params table) where
    renderSQL = \case
      OnConflictDoRaise -> ""
      OnConflict target action -> " ON CONFLICT"
        <+> renderSQL target <+> renderSQL action

data ConflictAction sch tab commons schemas params columns where
  DoNothing :: ConflictAction sch tab commons schemas params columns
  DoUpdate
    :: ( row ~ ColumnsToRow columns
       , SOP.SListI columns
       , columns ~ (col0 ': cols)
       , UpdateRow sch tab updates columns
       , SOP.SListI updates )
    => NP (Aliased (Expression '[] 'Ungrouped commons schemas params '[tab ::: row, "excluded" ::: row])) updates
    -> [Condition '[] 'Ungrouped commons schemas params '[tab ::: row, "excluded" ::: row]]
    -> ConflictAction sch tab commons schemas params columns

instance RenderSQL (ConflictAction sch tab commons schemas params columns) where
  renderSQL = \case
    DoNothing -> "DO NOTHING"
    DoUpdate updates whs'
      -> "DO UPDATE SET"
        <+> renderCommaSeparated renderUpdate updates
        <> case whs' of
          [] -> ""
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)

-- | A `ConflictTarget` specifies the constraint violation that triggers a
-- `ConflictAction`.
data ConflictTarget constraints where
  OnConstraint
    :: Has con constraints constraint
    => Alias con
    -> ConflictTarget constraints

-- | Render a `ConflictTarget`
instance RenderSQL (ConflictTarget constraints) where
  renderSQL (OnConstraint con) =
    "ON" <+> "CONSTRAINT" <+> renderSQL con

{-----------------------------------------
UPDATE statements
-----------------------------------------}

-- | An `update` command changes the values of the specified columns
-- in all rows that satisfy the condition.
update
  :: ( SOP.SListI columns
     , SOP.SListI row1
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row0 ~ TableToRow table
     , columns ~ TableToColumns table
     , UpdateRow sch tab updates columns
     , SOP.SListI updates )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (Expression '[] 'Ungrouped commons schemas params '[tab ::: row0])) updates
  -- ^ modified values to replace old values
  -> Condition '[] 'Ungrouped commons schemas params '[tab ::: row0]
  -- ^ condition under which to perform update on a row
  -> ReturningClause commons schemas params '[tab ::: row0] row1 -- ^ results to return
  -> Manipulation commons schemas params row1
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderSQL tab
  <+> "SET"
  <+> renderCommaSeparated renderUpdate columns
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

renderUpdate
  :: Aliased (Expression '[] 'Ungrouped commons schemas params from) col
  -> ByteString
renderUpdate (expression `As` col) =
  renderSQL col <+> "=" <+> renderSQL expression

-- | Update a row returning `Nil`.
update_
  :: ( SOP.SListI columns
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table
     , UpdateRow sch tab updates columns
     , SOP.SListI updates )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (Expression '[] 'Ungrouped commons schemas params '[tab ::: row])) updates
  -- ^ modified values to replace old values
  -> Condition '[] 'Ungrouped commons schemas params '[tab ::: row]
  -- ^ condition under which to perform update on a row
  -> Manipulation commons schemas params '[]
update_ tab columns wh = update tab columns wh (Returning_ Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

data UsingClause commons schemas params from where
  NoUsing :: UsingClause commons schemas params '[]
  Using
    :: FromClause '[] commons schemas params from
    -> UsingClause commons schemas params from

-- | Delete rows from a table.
deleteFrom
  :: ( SOP.SListI row1
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row0 ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> UsingClause commons schemas params from
  -> Condition '[] 'Ungrouped commons schemas params (tab ::: row0 ': from)
  -- ^ condition under which to delete a row
  -> ReturningClause commons schemas params '[tab ::: row0] row1 -- ^ results to return
  -> Manipulation commons schemas params row1
deleteFrom tab using wh returning = UnsafeManipulation $
  "DELETE FROM"
  <+> renderSQL tab
  <> case using of
    NoUsing -> ""
    Using tables -> " USING" <+> renderSQL tables
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> Condition '[] 'Ungrouped commons schemas params '[tab ::: row]
  -- ^ condition under which to delete a row
  -> Manipulation commons schemas params '[]
deleteFrom_ tab wh = deleteFrom tab NoUsing wh (Returning_ Nil)

-- | This has the behaviour of a cartesian product, taking all
-- possible combinations between @left@ and @right@ - exactly like a
-- `crossJoin`. Used when no `crossJoin` syntax is required but simply
-- a comma separated list of tables. Typical case is the `UsingClause`
-- of a `deleteFrom` query.
also
  :: FromClause outer commons schemas params right
  -- ^ right
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params (Join left right)
also right left = UnsafeFromClause $
  renderSQL left <> "," <+> renderSQL right
