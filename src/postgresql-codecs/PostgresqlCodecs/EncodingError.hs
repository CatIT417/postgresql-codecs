module PostgresqlCodecs.EncodingError where

import qualified Data.Aeson as Aeson
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (IsReport (..))

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

instance IsReport EncodingError where
  taggedStaticMessage = Tagged "Encoding failed"
  taggedStaticSuggestion =
    Tagged "Check that the value is valid for the target PostgreSQL type."
  toReportDetails EncodingError {..} =
    [ ("message", Aeson.toJSON message),
      ("details", Aeson.toJSON details),
      ("param", Aeson.toJSON param),
      ("inputLocation", Aeson.toJSON inputLocation),
      ("inputOffset", Aeson.toJSON inputOffset),
      ("failingInput", Aeson.toJSON failingInput),
      ("expectedInputs", Aeson.toJSON expectedInputs)
    ]
