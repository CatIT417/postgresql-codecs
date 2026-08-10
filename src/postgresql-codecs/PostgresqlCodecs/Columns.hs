{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Columns where

import PostgresqlCodecs.Core (Columns (..), ConsumesColumns (..), Scalar (..))
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.IsScalar
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar (declaredOid, elementOid)

-- | Build a single-column decoder from @a@'s 'IsScalar' instance, a dimensionality, and a
-- nullability.
column :: (IsScalar a) => Dimensionality a b -> Nullability b c -> Columns c
column = columnWith scalar

-- | Build a single-column decoder from an explicit scalar, dimensionality, and nullability.
--
-- Reach for this only when @a@ has no fixed static representation to hang an 'IsScalar'
-- instance on - e.g. a codec resolved at runtime from schema introspection. Never use it to
-- give a type that already has (or should have) an 'IsScalar' instance a different codec at
-- some call site; that reintroduces the coupling 'IsScalar' exists to prevent. A type needing
-- more than one representation should be a distinct newtype with its own instance instead.
columnWith :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
columnWith scalar dimensionality nullability =
  Columns
    { count = 1,
      unresolvedTypes = scalar.unresolvedTypes,
      oids = \resolver -> [declaredOid scalar dimensionality.dims resolver],
      decoder = \resolver ->
        let binaryPeeker = dimensionality.binaryDecoder (elementOid scalar resolver) (scalar.binaryDecoder resolver)
            textualParser = dimensionality.textualDecoder (scalar.textualDecoder resolver)
         in consumeColumn
              nullability.onNull
              (fmap (fmap nullability.onNonNull) binaryPeeker)
              (fmap nullability.onNonNull textualParser)
    }
