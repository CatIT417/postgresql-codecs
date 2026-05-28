module PostgresqlCodecs.EncodingError where

import PostgresqlCodecs.Prelude

data EncodingError = EncodingError
  { -- | Explanation of the error.
    message :: Text,
    details :: [(Text, Text)],
    param :: Int,
    -- | Path within the input contexts.
    inputLocation :: [Text],
    -- | Offset within input.
    inputOffset :: Int,
    -- | Value within the input that fails.
    failingInput :: Text,
    -- | List of expected values. E.g., in case of enum.
    expectedInputs :: Maybe [Text]
  }
  deriving stock (Show, Eq)

instance IsString EncodingError where
  fromString string =
    EncodingError
      { message = fromString string,
        details = [],
        param = 0,
        inputLocation = [],
        inputOffset = 0,
        failingInput = "",
        expectedInputs = Nothing
      }
