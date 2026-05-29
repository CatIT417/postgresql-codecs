module PostgresqlCodecs.Report
  ( -- * Report
    Report (..),

    -- ** Destructors
    toText,

    -- ** Constructors
    fromModel,

    -- ** Mappers
    nest,
    suggest,
    addDetail,
    addDetailWithKey,

    -- * Ports
    IsReport (..),
    IsDetail (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import PostgresqlCodecs.Prelude
import qualified TextBuilder

-- | Generic error type that is not intended for pattern matching, but only for rendering error messages.
data Report = Report
  { message :: Text,
    suggestion :: Text,
    path :: [Text],
    details :: [(Text, Aeson.Value)]
  }
  deriving stock (Show, Eq)

instance IsString Report where
  fromString string =
    Report
      { message = fromString string,
        suggestion = "",
        path = [],
        details = []
      }

instance Exception Report where
  displayException =
    Text.unpack . toText

-- | Render a 'Report' as human-readable text.
toText :: Report -> Text
toText report =
  TextBuilder.toText (toTextBuilder report)

-- | Render a 'Report' as human-readable text.
toTextBuilder :: Report -> TextBuilder.TextBuilder
toTextBuilder report =
  TextBuilder.text report.message
    <> buildSuggestion report.suggestion
    <> buildPath report.path
    <> buildDetails report.details
  where
    buildSuggestion "" =
      mempty
    buildSuggestion s =
      TextBuilder.text "\nSuggestion: " <> TextBuilder.text s
    buildPath [] =
      mempty
    buildPath p =
      TextBuilder.text "\nPath: " <> TextBuilder.text (Text.intercalate " > " p)
    buildDetails [] =
      mempty
    buildDetails ds =
      TextBuilder.text "\nDetails:" <> foldMap buildDetail ds
    buildDetail (key, value) =
      TextBuilder.text "\n  "
        <> TextBuilder.text key
        <> TextBuilder.text ": "
        <> TextBuilder.text (renderValue value)
    renderValue =
      Text.Lazy.toStrict . Aeson.Text.encodeToLazyText

-- | Structured model of a report.
class IsReport a where
  -- | Static message identifying the kind of report.
  taggedStaticMessage :: Tagged a Text

  -- | Static suggestion for resolving the issue.
  taggedStaticSuggestion :: Tagged a Text

  -- | Dynamic details specific to the value.
  toReportDetails :: a -> [(Text, Aeson.Value)]

instance IsReport SomeException where
  taggedStaticMessage = Tagged "SomeException"
  taggedStaticSuggestion = Tagged ""
  toReportDetails exception =
    [ ("type", Aeson.toJSON (show (typeOf exception))),
      ("message", Aeson.toJSON (displayException exception))
    ]

-- | Generalize a structured report model to 'Report'.
fromModel :: (IsReport a) => a -> Report
fromModel report =
  Report
    { message = witness taggedStaticMessage report,
      suggestion = witness taggedStaticSuggestion report,
      path = [],
      details = toReportDetails report
    }

-- | Prepend a path segment to the report's context path.
nest :: Text -> Report -> Report
nest pathPrefix report =
  report
    { path = pathPrefix : report.path
    }

-- | Attach a suggestion to a report.
suggest :: Text -> Report -> Report
suggest suggestion report =
  report {suggestion = suggestion}

-- | Structured detail value that can be attached to a 'Report'.
class IsDetail a where
  taggedDetailKey :: Tagged a Text
  toReportDetailValue :: a -> Aeson.Value

instance IsDetail Text where
  taggedDetailKey = Tagged "text"
  toReportDetailValue = Aeson.toJSON

instance IsDetail Int where
  taggedDetailKey = Tagged "int"
  toReportDetailValue = Aeson.toJSON

instance IsDetail Integer where
  taggedDetailKey = Tagged "integer"
  toReportDetailValue = Aeson.toJSON

instance IsDetail Double where
  taggedDetailKey = Tagged "double"
  toReportDetailValue = Aeson.toJSON

instance IsDetail Bool where
  taggedDetailKey = Tagged "bool"
  toReportDetailValue = Aeson.toJSON

instance IsDetail Aeson.Value where
  taggedDetailKey = Tagged "json"
  toReportDetailValue = id

-- | Add a detail to a report using the detail type's default key.
addDetail :: (IsDetail a) => a -> Report -> Report
addDetail detail report =
  Report
    { message = report.message,
      suggestion = report.suggestion,
      path = report.path,
      details = (witness taggedDetailKey detail, toReportDetailValue detail) : report.details
    }

-- | Add a detail to a report with a custom key.
addDetailWithKey :: (IsDetail a) => Text -> a -> Report -> Report
addDetailWithKey key value report =
  Report
    { message = report.message,
      suggestion = report.suggestion,
      path = report.path,
      details = (key, toReportDetailValue value) : report.details
    }
