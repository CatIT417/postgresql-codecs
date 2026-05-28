module PostgresqlCodecs.DecodingError where

import PostgresqlCodecs.Prelude

data DecodingError = DecodingError
  { -- | Explanation of the error.
    message :: Text,
    details :: Text,
    -- | Row number within the result set, if applicable.
    row :: Maybe Int,
    -- | Column number within the result set, if applicable.
    column :: Maybe Int,
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

instance IsString DecodingError where
  fromString string =
    DecodingError
      { message = fromString string,
        details = "",
        row = Nothing,
        column = Nothing,
        inputLocation = [],
        inputOffset = 0,
        failingInput = "",
        expectedInputs = Nothing
      }
