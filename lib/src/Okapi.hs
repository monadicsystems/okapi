{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Text qualified as Text
{-
import Network.HTTP.Types (Headers, Path, Query, StdMethod)

data GET

data POST

data Request m p q h b

instance Functor (Contract pre post i) where
      fmap = Fmap

instance Applicative (Contract pre pre i) where
      pure = Pure
      (<*>) = Apply

--------------------------------------------------------------------------------
-- Expr data family: protocol-specific primitives
--------------------------------------------------------------------------------

{- | Expr is a kind-polymorphic data family. Each instance picks its own kind
  for the index parameters and declares the available primitives for that kind.
-}

--------------------------------------------------------------------------------
-- Sub-DSL: PathContract
--
-- A simplified path contract used to refine the path slot. In a real library
-- this would itself be a full Contract over a PathExpr data family; here it
-- is kept minimal to test composition without dragging in the full framework.
--------------------------------------------------------------------------------

{- | A bidirectional path contract that extracts (on parse) and consumes (on print)
  a value of type p. "Normalized" means input type equals output type, which
  signals the sub-DSL is ready to be plugged into the outer Contract.
-}
data PathContract i o where
      PathLit :: String -> PathContract () ()

-- More constructors (variable segments, sequencing, etc.) would go here.
-- Kept minimal for the prototype.

--------------------------------------------------------------------------------
-- Expr instance for Request-indexed primitives
--------------------------------------------------------------------------------

data instance Expr (pre :: Request) (post :: Request) i o where
      -- | Fix the method slot to GET. Precondition: method is ANY. Postcondition: method is GET.
      --   Using Method twice with different methods fails because the second call's
      --   precondition 'ANY does not unify with the already-set method.
      Method :: Expr ('Request 'ANY p q h b) ('Request m p q h b) () ()
      -- | Fix the path slot to whatever type the inner PathContract produces.
      --   Precondition: path slot is the unrefined AnyPath. Postcondition: path slot is p.
      Path ::
            PathContract p p ->
            Expr ('Request m AnyPath q h b) ('Request m p q h b) p p

--------------------------------------------------------------------------------
-- Smart constructors for the primitives
--------------------------------------------------------------------------------

-- | Specify the HTTP method.
method :: Contract ('Request 'ANY p q h b) ('Request m p q h b) () ()
method = Lift Method

-- | Specify the path via a PathContract.
path ::
      PathContract p p ->
      Contract ('Request m AnyPath q h b) ('Request m p q h b) p p
path pc = Lift (Path pc)

--------------------------------------------------------------------------------
-- Example contracts (these are the actual tests; uncomment to verify behavior)
--------------------------------------------------------------------------------

-- This should typecheck: method alone, starting from the initial state.
example1 :: Contract InitialRequest ('Request m AnyPath AnyQuery AnyHeaders AnyBody) () ()
example1 = method

-- This should typecheck: a path alone, starting from the initial state.
example2 :: Contract InitialRequest ('Request 'ANY () AnyQuery AnyHeaders AnyBody) () ()
example2 = path (PathLit "users")

-- To test composition properly we need indexed Apply, not the plain Applicative.
-- That's the next step. For now, this file should compile as-is, which validates
-- the kind-polymorphic Contract, the Expr data family instance, the refinement
-- indexing, and the smart constructors.

--------------------------------------------------------------------------------
-- What to try after this compiles:
--
--   1. Add a second method variant and verify `method *> method` fails to typecheck
--      (requires implementing an indexed *> first).
--   2. Add Response as a second kind and verify that Request-indexed and
--      Response-indexed contracts cannot mix.
--   3. Add a Function constructor that combines a request contract and a response
--      contract, with a Complete constraint asserting all slots are refined.
--   4. Wire up QualifiedDo + IxApplicative for ergonomic construction syntax.
--------------------------------------------------------------------------------
-}
