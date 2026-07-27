{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Core.Scalar where

import qualified Data.Attoparsec.Text as Atto
import Data.Set (Set)
import PostgresqlCodecs.Core.Prelude
import PostgresqlCodecs.Core.Report (Report (..))
import PostgresqlCodecs.Core.Resolver
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Full codec specification for a single PostgreSQL scalar type.
-- Carries the schema\/type name, optional statically-known OIDs, a binary-support
-- flag, and binary\/textual encoder\/decoder pairs.
data Scalar a = Scalar
  { -- | Schema name. If empty, the default schema will be used.
    schemaName :: Text,
    -- | Type name.
    typeName :: Text,
    -- | Statically known OID for the type.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    baseOid :: Maybe Word32,
    -- | Statically known OID for the array-type with this type as the element.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
    arrayOid :: Maybe Word32,
    -- | Whether this scalar type supports binary encoding/decoding.
    supportsBinary :: Bool,
    -- | Transitive set of @(schema, name)@ pairs of referenced types whose base
    -- or array OID is not statically known and therefore must be resolved at runtime.
    unresolvedTypes :: Set (Text, Text),
    binaryEncoder :: Resolver -> a -> Either Report Write.Write,
    binaryDecoder :: Resolver -> PtrPeeker.Variable (Either Report a),
    -- | Represent in Postgres textual format.
    textualEncoder :: Resolver -> a -> Either Report TextBuilder.TextBuilder,
    -- | Decode from Postgres textual format.
    textualDecoder :: Resolver -> Atto.Parser a
  }

instance Invariant Scalar where
  invmap f g Scalar {..} =
    Scalar
      { binaryEncoder = \r -> binaryEncoder r . g,
        binaryDecoder = \r -> fmap (fmap f) (binaryDecoder r),
        textualEncoder = \r -> textualEncoder r . g,
        textualDecoder = \r -> fmap f (textualDecoder r),
        ..
      }
