module PostgresqlMapping.Mapping.ParameterizesStatement where

import qualified PostgresqlMapping.Core as Codec
import PostgresqlMapping.Core.Prelude

class ParameterizesStatement a where
  type ResultOf a
  statementOf :: StatementOf a

data StatementOf a = StatementOf
  { sql :: Text,
    params :: Codec.Params a,
    result :: Codec.Result (ResultOf a)
  }
