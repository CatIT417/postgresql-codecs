{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Params where

import Data.Set (Set)
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (Report)
import PostgresqlCodecs.Resolver
import PostgresqlCodecs.Scalar
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Parameterized query parameters encoder.
data Params a = Params
  { -- | Number of parameters.
    count :: Int32,
    -- | Transitive set of @(schema, name)@ pairs of referenced types that have
    -- no statically known base OID and therefore must be resolved at runtime.
    unresolvedTypes :: Set (Text, Text),
    -- | OID of each parameter. A value of 0 means the server should infer the type.
    oids :: Resolver -> [Word32],
    -- | Binary encoder producing one 'Maybe Write.Write' per parameter.
    -- 'Nothing' represents a NULL parameter.
    binaryEncoder :: Resolver -> a -> Either Report [Maybe Write.Write],
    -- | Textual encoder producing one 'Maybe TextBuilder' per parameter.
    -- 'Nothing' represents a NULL parameter.
    textualEncoder :: Resolver -> a -> Either Report [Maybe TextBuilder.TextBuilder]
  }

instance Contravariant Params where
  contramap f p =
    Params
      { count = p.count,
        unresolvedTypes = p.unresolvedTypes,
        oids = p.oids,
        binaryEncoder = \r -> p.binaryEncoder r . f,
        textualEncoder = \r -> p.textualEncoder r . f
      }

instance Semigroup (Params a) where
  p1 <> p2 =
    Params
      { count = p1.count + p2.count,
        unresolvedTypes = p1.unresolvedTypes <> p2.unresolvedTypes,
        oids = \r -> p1.oids r <> p2.oids r,
        binaryEncoder = \r a -> liftA2 (<>) (p1.binaryEncoder r a) (p2.binaryEncoder r a),
        textualEncoder = \r a -> liftA2 (<>) (p1.textualEncoder r a) (p2.textualEncoder r a)
      }

instance Monoid (Params a) where
  mempty =
    Params
      { count = 0,
        unresolvedTypes = mempty,
        oids = const [],
        binaryEncoder = \_ _ -> Right [],
        textualEncoder = \_ _ -> Right []
      }

-- | Build a single-parameter encoder from a scalar, dimensionality, and nullability.
param :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
param scalar dimensionality nullability =
  let baseOid = fromMaybe 0 scalar.baseOid
      theParamOid = fromMaybe 0 (if dimensionality.dims > 0 then scalar.arrayOid else scalar.baseOid)
      Nullability {encoder = enc} = nullability
   in Params
        { count = 1,
          unresolvedTypes = scalar.unresolvedTypes,
          oids = \_resolver -> [theParamOid],
          binaryEncoder = \resolver c ->
            let binEnc = dimensionality.binaryEncoder baseOid (scalar.binaryEncoder resolver)
             in fmap (: []) (enc (Right Nothing) (\a -> fmap Just (binEnc a)) c),
          textualEncoder = \resolver c ->
            let txtEnc = dimensionality.textualEncoder (scalar.textualEncoder resolver)
             in fmap (: []) (enc (Right Nothing) (\a -> fmap Just (txtEnc a)) c)
        }
