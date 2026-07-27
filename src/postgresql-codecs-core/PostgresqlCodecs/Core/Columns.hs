{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Core.Columns where

import qualified Data.Attoparsec.Text as Atto
import Data.Set (Set)
import PostgresqlCodecs.Core.Prelude
import PostgresqlCodecs.Core.Report (Report)
import PostgresqlCodecs.Core.Resolver
import qualified PtrPeeker

-- * Ports

-- | Typeclass for driver-specific column consumption.
-- Implement this for each PostgreSQL driver to wire up the library.
class (Applicative f) => ConsumesColumns f where
  -- | Consume the next column value in the current row of the result set.
  consumeColumn ::
    -- | How to handle NULL value.
    Either Report a ->
    -- | Binary decoder for the column.
    PtrPeeker.Variable (Either Report a) ->
    -- | Textual decoder for the column.
    Atto.Parser a ->
    f a

-- * Abstraction

-- | Result set columns decoder.
data Columns a = Columns
  { -- | Expected number of columns in the result set.
    count :: Int32,
    -- | Transitive set of @(schema, name)@ pairs of referenced types whose base
    -- or array OID is not statically known and therefore must be resolved at runtime.
    unresolvedTypes :: Set (Text, Text),
    -- | Expected type OID of each result column, in column order — for
    -- drivers that want to validate the shape of a result set against this
    -- decoder before decoding it. A value of 0 means the OID is not known,
    -- i.e. the type was not resolved.
    oids :: Resolver -> [Word32],
    -- | Wire-format decoder that supports both binary and text formats.
    decoder :: Resolver -> (forall f. (ConsumesColumns f) => f a)
  }

instance Functor Columns where
  fmap f Columns {count, unresolvedTypes, oids, decoder} =
    Columns
      { count,
        unresolvedTypes,
        oids,
        decoder = \r -> fmap f (decoder r)
      }

instance Applicative Columns where
  pure x =
    Columns
      { count = 0,
        unresolvedTypes = mempty,
        oids = const [],
        decoder = \_ -> pure x
      }
  Columns {count = cf, unresolvedTypes = uf, oids = of', decoder = df}
    <*> Columns {count = cx, unresolvedTypes = ux, oids = ox, decoder = dx} =
      Columns
        { count = cf + cx,
          unresolvedTypes = uf <> ux,
          oids = \r -> of' r <> ox r,
          decoder = \r -> df r <*> dx r
        }
