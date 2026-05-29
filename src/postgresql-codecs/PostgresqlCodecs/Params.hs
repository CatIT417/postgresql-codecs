{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Params where

import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Parameterized query parameters encoder.
data Params a = Params
  { -- | Number of parameters.
    count :: Int32,
    -- | OID of each parameter. A value of 0 means the server should infer the type.
    oids :: [Word32],
    -- | Binary encoder producing one 'Write.Write' per parameter.
    binaryEncoder :: a -> Either EncodingError [Write.Write],
    -- | Textual encoder producing one 'TextBuilder' per parameter.
    textualEncoder :: a -> Either EncodingError [TextBuilder.TextBuilder]
  }

instance Contravariant Params where
  contramap f p =
    Params
      { count = p.count,
        oids = p.oids,
        binaryEncoder = p.binaryEncoder . f,
        textualEncoder = p.textualEncoder . f
      }

instance Semigroup (Params a) where
  p1 <> p2 =
    Params
      { count = p1.count + p2.count,
        oids = p1.oids <> p2.oids,
        binaryEncoder = \a -> liftA2 (<>) (p1.binaryEncoder a) (p2.binaryEncoder a),
        textualEncoder = \a -> liftA2 (<>) (p1.textualEncoder a) (p2.textualEncoder a)
      }

instance Monoid (Params a) where
  mempty =
    Params
      { count = 0,
        oids = [],
        binaryEncoder = const (Right []),
        textualEncoder = const (Right [])
      }

param :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
param scalar dimensionality nullability =
  let baseOid = fromMaybe 0 scalar.baseOid
      binEnc =
        nullability.binaryEncoderWrapper
          (dimensionality.binaryEncoder baseOid scalar.binaryEncoder)
      txtEnc =
        nullability.textualEncoderWrapper
          (dimensionality.textualEncoder scalar.textualEncoder)
   in Params
        { count = 1,
          oids = [baseOid],
          binaryEncoder = \c -> fmap (:[]) (binEnc c),
          textualEncoder = \c -> fmap (:[]) (txtEnc c)
        }
