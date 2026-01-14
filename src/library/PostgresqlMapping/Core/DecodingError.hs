module PostgresqlMapping.Core.DecodingError where

import PostgresqlMapping.Core.Prelude

data DecodingError = DecodingError
  { location :: [Text],
    reason :: DecodingErrorReason
  }
  deriving stock (Show, Eq)

data DecodingErrorReason
  = ParsingDecodingErrorReason
      -- | Details.
      Text
      -- | Input.
      ByteString
  | UnexpectedValueDecodingErrorReason
      -- | Expected.
      Text
      -- | Actual.
      Text
  deriving stock (Show, Eq)
