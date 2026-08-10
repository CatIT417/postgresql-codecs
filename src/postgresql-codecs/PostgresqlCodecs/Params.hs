{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Params where

import PostgresqlCodecs.Core (Format (..), Params (..), Scalar (..))
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.IsScalar
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar (declaredOid, elementOid)
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Build a single-parameter encoder from @a@'s 'IsScalar' instance, a dimensionality, and a
-- nullability.
param :: (IsScalar a) => Dimensionality a b -> Nullability b c -> Params c
param = paramWith scalar

-- | Build a single-parameter encoder from an explicit scalar, dimensionality, and nullability.
--
-- Reach for this only when @a@ has no fixed static representation to hang an 'IsScalar'
-- instance on - e.g. a codec resolved at runtime from schema introspection. Never use it to
-- give a type that already has (or should have) an 'IsScalar' instance a different codec at
-- some call site; that reintroduces the coupling 'IsScalar' exists to prevent. A type needing
-- more than one representation should be a distinct newtype with its own instance instead.
paramWith :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
paramWith scalar dimensionality nullability =
  let Nullability {encoder = enc} = nullability
   in Params
        { count = 1,
          unresolvedTypes = scalar.unresolvedTypes,
          oids = \resolver -> [declaredOid scalar dimensionality.dims resolver],
          formats = [if scalar.supportsBinary then Binary else Textual],
          binaryEncoder = \resolver c ->
            let binEnc = dimensionality.binaryEncoder (elementOid scalar resolver) (scalar.binaryEncoder resolver)
             in fmap (: []) (enc (Right Nothing) (\a -> fmap Just (binEnc a)) c),
          textualEncoder = \resolver c ->
            let txtEnc = dimensionality.textualEncoder (scalar.textualEncoder resolver)
             in fmap (: []) (enc (Right Nothing) (\a -> fmap Just (txtEnc a)) c)
        }
