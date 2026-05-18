module PostgresqlMapping.ParameterizesStatement where

import qualified PostgresqlCodecs as Codec
import PostgresqlMapping.Prelude

class ParameterizesStatement a where
  type ResultOf a
  statementOf :: StatementOf a

data StatementOf a = StatementOf
  { sql :: Text,
    params :: Codec.Params a,
    result :: Codec.Result (ResultOf a)
  }
