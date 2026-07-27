{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Params where

import Data.Set (Set)
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.IsScalar
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (Report)
import PostgresqlCodecs.Resolver
import PostgresqlCodecs.Scalar
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Wire format for a single parameter.
data Format
  = -- | The parameter's binary encoder produces the value; the resolver
    -- matters here, since binary payloads embed OIDs (e.g. in array
    -- headers).
    Binary
  | -- | The parameter's textual encoder produces the value. The wire format
    -- embeds no OIDs, so the textual encoder ignores its 'Resolver' — but the
    -- parameter is still declared to the server by OID, so @oids@ needs a
    -- resolver either way.
    Textual
  deriving stock (Eq, Show)

-- | Parameterized query parameters encoder.
data Params a = Params
  { -- | Number of parameters.
    count :: Int32,
    -- | Transitive set of @(schema, name)@ pairs of referenced types whose base
    -- or array OID is not statically known and therefore must be resolved at runtime.
    unresolvedTypes :: Set (Text, Text),
    -- | OID of each parameter. A value of 0 means the server should infer the type.
    oids :: Resolver -> [Word32],
    -- | Wire format each parameter declares it can be encoded in. A driver may
    -- downgrade a 'Binary' parameter to 'Textual', but must never send a
    -- 'Textual' parameter in the binary format.
    formats :: [Format],
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
        formats = p.formats,
        binaryEncoder = \r -> p.binaryEncoder r . f,
        textualEncoder = \r -> p.textualEncoder r . f
      }

instance Semigroup (Params a) where
  p1 <> p2 =
    Params
      { count = p1.count + p2.count,
        unresolvedTypes = p1.unresolvedTypes <> p2.unresolvedTypes,
        oids = \r -> p1.oids r <> p2.oids r,
        formats = p1.formats <> p2.formats,
        binaryEncoder = \r a -> liftA2 (<>) (p1.binaryEncoder r a) (p2.binaryEncoder r a),
        textualEncoder = \r a -> liftA2 (<>) (p1.textualEncoder r a) (p2.textualEncoder r a)
      }

instance Monoid (Params a) where
  mempty =
    Params
      { count = 0,
        unresolvedTypes = mempty,
        oids = const [],
        formats = [],
        binaryEncoder = \_ _ -> Right [],
        textualEncoder = \_ _ -> Right []
      }

-- | Build a single-parameter encoder from @a@'s 'IsScalar' instance, a dimensionality, and a
-- nullability.
param :: (IsScalar a) => Dimensionality a b -> Nullability b c -> Params c
param = paramWith scalar

-- | Build a single-parameter encoder from an explicit scalar, dimensionality, and nullability.
--
-- Reach for this only when @a@ has no fixed static representation to hang an 'IsScalar'
-- instance on — e.g. a codec resolved at runtime from schema introspection. Never use it to
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
