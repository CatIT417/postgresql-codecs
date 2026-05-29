module PostgresqlCodecs.DecodingError where

import qualified Data.Aeson as Aeson
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (IsReport (..))

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

instance IsReport DecodingError where
  taggedStaticMessage = Tagged "Decoding failed"
  taggedStaticSuggestion =
    Tagged "Check that the data matches the expected PostgreSQL format."
  toReportDetails DecodingError {..} =
    [ ("message", Aeson.toJSON message),
      ("details", Aeson.toJSON details),
      ("row", Aeson.toJSON row),
      ("column", Aeson.toJSON column),
      ("inputLocation", Aeson.toJSON inputLocation),
      ("inputOffset", Aeson.toJSON inputOffset),
      ("failingInput", Aeson.toJSON failingInput),
      ("expectedInputs", Aeson.toJSON expectedInputs)
    ]
