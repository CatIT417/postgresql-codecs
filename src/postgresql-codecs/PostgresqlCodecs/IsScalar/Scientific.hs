-- |
-- Conversions underpinning the 'Data.Scientific.Scientific' instances of
-- 'PostgresqlCodecs.IsScalar.IsScalar', which map onto PostgreSQL's @numeric@.
module PostgresqlCodecs.IsScalar.Scientific
  ( refineFromNumeric,
  )
where

import Data.Scientific (Scientific)
import PostgresqlCodecs.Core (Report (..))
import PostgresqlCodecs.Prelude
import qualified PostgresqlTypes as Types
import qualified PostgresqlTypes.Numeric

-- | Narrow a @numeric@ to 'Scientific', rejecting the three values PostgreSQL supports and
-- 'Scientific' does not.
--
-- Distinguishes which of the three was received so the failure report names it, rather than
-- leaving the caller to guess why an otherwise ordinary numeric column failed to decode.
refineFromNumeric :: Types.Numeric precision scale -> Either Report Scientific
refineFromNumeric numeric =
  case PostgresqlTypes.Numeric.refineToScientific numeric of
    Just scientific -> Right scientific
    Nothing ->
      Left
        Report
          { message = "Numeric value unrepresentable as Scientific",
            suggestion = "Decode the column as PostgresqlTypes.Numeric, which models the full numeric domain",
            details = [("failingInput", failingInput)]
          }
      where
        failingInput
          | PostgresqlTypes.Numeric.isNaN numeric = "NaN"
          | PostgresqlTypes.Numeric.isPosInfinity numeric = "Infinity"
          | PostgresqlTypes.Numeric.isNegInfinity numeric = "-Infinity"
          | otherwise = "<unrepresentable>"
