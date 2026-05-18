module PostgresqlMapping.MapsToScalar where

import qualified PostgresqlCodecs as Codec
import qualified PostgresqlTypes as Standard

class MapsToScalar a where
  scalarOf :: Codec.Scalar a

instance MapsToScalar Standard.Int8 where
  scalarOf = Codec.standard

instance MapsToScalar Standard.Text where
  scalarOf = Codec.standard

instance MapsToScalar Standard.Timestamptz where
  scalarOf = Codec.standard

field :: forall a b c. (MapsToScalar a) => Codec.Dimensionality a b -> Codec.Nullability b c -> Codec.Fields c c
field =
  Codec.field (scalarOf @a)

param :: forall a b c. (MapsToScalar a) => Codec.Dimensionality a b -> Codec.Nullability b c -> Codec.Params c
param =
  Codec.param (scalarOf @a)

column :: forall a b c. (MapsToScalar a) => Codec.Dimensionality a b -> Codec.Nullability b c -> Codec.Columns c
column =
  Codec.column (scalarOf @a)
