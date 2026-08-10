module IntegrationTest.ScalarSpec (spec) where

import qualified Data.ByteString as BS
import Data.Foldable (for_)
import qualified Data.Pool as Pool
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Bytea
import qualified PostgresqlTypes.Float4
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int2
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import qualified PostgresqlTypes.Multirange
import qualified PostgresqlTypes.Numeric
import qualified PostgresqlTypes.Range
import qualified PostgresqlTypes.Uuid
import Test.Hspec
import Prelude

-- ---------------------------------------------------------------------------
-- Enum type for testing

data Genre = Comedy | Action | Drama
  deriving (Eq, Ord, Show)

genreScalar :: PostgresqlCodecs.Scalar Genre
genreScalar =
  PostgresqlCodecs.enum
    "public"
    "movie_genre"
    [("Comedy", Comedy), ("Action", Action), ("Drama", Drama)]

spec :: SpecHook.HookedSpec
spec = do
  describe "Scalar" $ do
    describe "binaryPrimitive roundtrips" $ do
      it "int2" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Int2)
            (PostgresqlTypes.Int2.fromInt16 42)
            "int2"
        result `shouldBe` Right (PostgresqlTypes.Int2.fromInt16 42)

      it "int4" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Int4)
            (PostgresqlTypes.Int4.fromInt32 42)
            "int4"
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

      it "int8" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Int8)
            (PostgresqlTypes.Int8.fromInt64 9876543210)
            "int8"
        result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)

      it "float4" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Float4)
            (PostgresqlTypes.Float4.fromFloat 3.14)
            "float4"
        result `shouldBe` Right (PostgresqlTypes.Float4.fromFloat 3.14)

      it "float8" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Float8)
            (PostgresqlTypes.Float8.fromDouble 3.14)
            "float8"
        result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)

      it "text" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Text)
            ("hello" :: PostgresqlTypes.Text)
            "text"
        result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)

      it "bool" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Bool)
            (PostgresqlTypes.Bool.fromBool True)
            "bool"
        result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)

      it "bytea" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Bytea)
            (PostgresqlTypes.Bytea.fromByteString "hello")
            "bytea"
        result `shouldBe` Right (PostgresqlTypes.Bytea.fromByteString "hello")

      it "uuid" $ \env -> do
        let val = PostgresqlTypes.Uuid.fromUUID (read "550e8400-e29b-41d4-a716-446655440000")
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @PostgresqlTypes.Uuid)
            val
            "uuid"
        result `shouldBe` Right val

    describe "enum" $ do
      it "roundtrip" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          execQuery
            conn
            "SELECT $1::movie_genre"
            (PostgresqlCodecs.paramWith genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.columnWith genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            env.binary
            Comedy
        result `shouldBe` Right Comedy

    describe "Scientific" $ do
      it "roundtrips an ordinary value" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @Scientific)
            123.456
            "numeric"
        result `shouldBe` Right 123.456

      it "roundtrips a range" $ \env -> do
        let val = PostgresqlTypes.Range.normalizeBounded (Just 1) (Just 10) :: PostgresqlTypes.Range Scientific
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @(PostgresqlTypes.Range Scientific))
            val
            "numrange"
        fmap asNumeric result `shouldBe` Right (asNumeric val)

      it "roundtrips a multirange" $ \env -> do
        let val =
              PostgresqlTypes.Multirange.normalizeFromRangeList
                [ PostgresqlTypes.Range.normalizeBounded (Just 1) (Just 10),
                  PostgresqlTypes.Range.normalizeBounded (Just 20) (Just 30)
                ] ::
                PostgresqlTypes.Multirange Scientific
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @(PostgresqlTypes.Multirange Scientific))
            val
            "nummultirange"
        fmap asNumeric result `shouldBe` Right (asNumeric val)

      -- Encoding stays total: a value beyond the numeric limits is clamped rather than rejected,
      -- per the convenience-layer contract on 'PostgresqlCodecs.IsScalar'.
      it "encodes an out-of-limits value without failing" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            (PostgresqlCodecs.scalar @Scientific)
            (read "1e100000")
            "numeric"
        case result of
          Right _ -> pure ()
          Left report -> expectationFailure ("Expected encoding to succeed, got: " <> show report)

      -- Decoding is the one place the convenience layer rejects: Scientific cannot represent
      -- these three, and substituting anything for them would corrupt the result silently.
      for_ [("NaN", "NaN"), ("Infinity", "Infinity"), ("-Infinity", "-Infinity")] $ \(label, literal) ->
        it ("fails to decode " <> Text.unpack label) $ \env -> do
          result <- Pool.withResource env.pool $ \conn ->
            decodeExpression
              conn
              env.binary
              (PostgresqlCodecs.scalar @Scientific)
              "SELECT $1::numeric"
              literal
          expectFailingInput label result

      it "fails to decode a range with a NaN bound" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          decodeExpression
            conn
            env.binary
            (PostgresqlCodecs.scalar @(PostgresqlTypes.Range Scientific))
            "SELECT numrange(1::numeric, $1::numeric)"
            "NaN"
        expectFailingInput "NaN" result

      it "fails to decode a multirange with a NaN bound" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          decodeExpression
            conn
            env.binary
            (PostgresqlCodecs.scalar @(PostgresqlTypes.Multirange Scientific))
            "SELECT nummultirange(numrange(1::numeric, $1::numeric))"
            "NaN"
        expectFailingInput "NaN" result

-- ---------------------------------------------------------------------------
-- Helper

scalarRoundtrip :: PQ.Connection -> Bool -> PostgresqlCodecs.Scalar a -> a -> BS.ByteString -> IO (Either PostgresqlCodecs.Report a)
scalarRoundtrip conn isBinary scalar val pgType =
  execQuery
    conn
    ("SELECT $1::" <> pgType)
    (PostgresqlCodecs.paramWith scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    (PostgresqlCodecs.oneRow (PostgresqlCodecs.columnWith scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
    isBinary
    val

-- | Decode the result of an expression the server evaluates itself, feeding the input in as
-- @text@ for the server to cast.
--
-- Needed wherever the value under test is one the codec being exercised cannot encode - the
-- rejected @numeric@ specials have no 'Scientific' to send them as, so they have to originate
-- server-side.
decodeExpression ::
  PQ.Connection ->
  Bool ->
  PostgresqlCodecs.Scalar a ->
  BS.ByteString ->
  PostgresqlTypes.Text ->
  IO (Either PostgresqlCodecs.Report a)
decodeExpression conn isBinary scalar sql input =
  execQuery
    conn
    sql
    (PostgresqlCodecs.paramWith (PostgresqlCodecs.scalar @PostgresqlTypes.Text) PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    (PostgresqlCodecs.oneRow (PostgresqlCodecs.columnWith scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
    isBinary
    input

-- | Assert that decoding failed and that the report names the offending value, rather than
-- merely that something went wrong somewhere.
expectFailingInput :: Text.Text -> Either PostgresqlCodecs.Report a -> Expectation
expectFailingInput expected = \case
  Left report -> report.details `shouldContain` [("failingInput", expected)]
  Right _ -> expectationFailure "Expected a decoding failure, got a decoded value"

-- | Restate a container of 'Scientific' over the wrapper type, for assertion purposes only.
--
-- 'PostgresqlTypes.Range' and 'PostgresqlTypes.Multirange' derive 'Show' through their
-- @IsPrimitive@ instance, which exists only for range elements - and 'Scientific' deliberately is
-- not one. Mapping back to 'PostgresqlTypes.Numeric' recovers a printable value. Lossless here
-- because the bounds under test are well inside the numeric limits.
asNumeric :: (Functor f) => f Scientific -> f (PostgresqlTypes.Numeric 0 0)
asNumeric = fmap PostgresqlTypes.Numeric.normalizeFromScientific
