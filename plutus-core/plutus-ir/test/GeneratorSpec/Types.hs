{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GeneratorSpec.Types where

import PlutusCore.Generators.PIR


import Data.Map qualified as Map




import Test.QuickCheck

-- | Check that the types we generate are kind-correct
-- See Note [Debugging generators that don't generate well-typed/kinded terms/types]
-- and see the utility tests below when this property fails.
prop_genKindCorrect :: Property
prop_genKindCorrect =
  -- Context minimality doesn't help readability, so no shrinking here
  forAllDoc "ctx" genCtx (const []) $ \ ctx ->
  -- Note, no shrinking here because shrinking relies on well-kindedness.
  forAllDoc "k,ty" genKindAndType (const []) $ \ (k, ty) ->
  checkKind ctx ty k

-- | Check that shrinking types maintains kinds
prop_shrinkTypeSound :: Property
prop_shrinkTypeSound =
  forAllDoc "ctx" genCtx (const []) $ \ ctx ->
  forAllDoc "k,ty" (genKindAndTypeWithCtx ctx) (shrinkKindAndType ctx) $ \ (k, ty) ->
  -- See discussion about the same trick in `prop_shrinkTermSound`
  checkKind ctx ty k ==>
  assertNoCounterexamples [ (k, ty) | (k, ty) <- shrinkKindAndType ctx (k, ty)
                                   , not $ checkKind ctx ty k ]

-- Utility tests for debugging

-- | Test that shrinking types results in smaller types. Useful for debugging shrinking.
prop_shrinkTypeSmallerKind :: Property
prop_shrinkTypeSmallerKind =
  forAllDoc "k,ty" genKindAndType (shrinkKindAndType Map.empty) $ \ (k, ty) ->
  assertNoCounterexamples [ (k', ty')
                          | (k', ty') <- shrinkKindAndType Map.empty (k, ty)
                          , not $ leKind k' k ]

-- | Test that shrinking kinds generates smaller kinds
prop_shrinkKindSmaller :: Property
prop_shrinkKindSmaller =
  forAllDoc "k" arbitrary shrink $ \ k ->
  assertNoCounterexamples [ k' | k' <- shrink k, not $ leKind k' k ]

-- | Test that fixKind actually gives you something of the right kind
prop_fixKind :: Property
prop_fixKind =
  forAllDoc "ctx" genCtx (const []) $ \ ctx ->
  forAllDoc "k,ty" genKindAndType (shrinkKindAndType ctx) $ \ (k, ty) ->
  -- Note, fixKind only works on smaller kinds, so we use shrink to get a definitely smaller kind
  assertNoCounterexamples [ (ty', k') | k' <- shrink k
                                     , let ty' = fixKind ctx ty k'
                                     , not $ checkKind ctx ty' k' ]
