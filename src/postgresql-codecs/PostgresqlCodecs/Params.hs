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
  { count :: Int32,
    binaryEncoder :: [a -> Either EncodingError (Maybe Write.Write)],
    textualEncoder :: [a -> TextBuilder.TextBuilder]
  }

instance Contravariant Params where
  contramap f params =
    Params
      { count = params.count,
        binaryEncoder = map (. f) params.binaryEncoder,
        textualEncoder = map (. f) params.textualEncoder
      }

instance Semigroup (Params a) where
  l <> r =
    Params
      { count = l.count + r.count,
        binaryEncoder = l.binaryEncoder <> r.binaryEncoder,
        textualEncoder = l.textualEncoder <> r.textualEncoder
      }

instance Monoid (Params a) where
  mempty =
    Params
      { count = 0,
        binaryEncoder = [],
        textualEncoder = []
      }

param :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
param scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
      encode = dimensionality.binaryEncoder baseOid scalar.binaryEncoder
   in Params
        { count = 1,
          binaryEncoder = [nullability.binaryParamEncoder encode],
          textualEncoder =
            [ \c ->
                case nullability.textualEncoderWrapper
                       (dimensionality.textualEncoder scalar.textualEncoder)
                       c of
                  Left _ -> mempty
                  Right tb -> tb
            ]
        }
