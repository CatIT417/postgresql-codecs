module PostgresqlCodecs.EncodingError where

import qualified Data.Text as Text
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
    baseDetails <> optionalDetails
    where
      baseDetails =
        [ ("message", message),
          ("details", renderKeyValues details),
          ("param", renderInt param),
          ("inputLocation", renderTextList inputLocation),
          ("inputOffset", renderInt inputOffset),
          ("failingInput", failingInput)
        ]
      optionalDetails =
        maybe [] (\value -> [("expectedInputs", renderTextList value)]) expectedInputs
      renderInt =
        Text.pack . show
      renderTextList values =
        "[" <> Text.intercalate ", " values <> "]"
      renderKeyValues pairs =
        case pairs of
          [] -> "[]"
          _ ->
            "[" <> Text.intercalate ", " (map renderPair pairs) <> "]"
      renderPair (key, value) =
        key <> ": " <> value
