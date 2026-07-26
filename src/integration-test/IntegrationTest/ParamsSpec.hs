module IntegrationTest.ParamsSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant
import qualified Data.Pool as Pool
import qualified Data.Vector as Vector
import Data.Word (Word32)
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import qualified PtrPoker.Write as Write
import Test.Hspec
import Prelude

-- ---------------------------------------------------------------------------
-- Enum type for testing the resolved parameter OID

data Color = Red | Green deriving (Eq, Ord, Show)

colorScalar :: PostgresqlCodecs.Scalar Color
colorScalar = PostgresqlCodecs.enum "" "param_color" [("red", Red), ("green", Green)]

-- ---------------------------------------------------------------------------
-- Scalar that declares it cannot be encoded in binary

-- | An @int4@ scalar that declares @supportsBinary = False@ and whose binary
-- encoder deliberately emits 8 bytes instead of 4. Sending it in binary would
-- make the server reject the bind parameter, so any test that passes with it
-- proves the parameter was actually sent in the textual format.
textOnlyInt4 :: PostgresqlCodecs.Scalar PostgresqlTypes.Int4
textOnlyInt4 =
  let base = PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Int4
   in PostgresqlCodecs.custom
        base.schemaName
        base.typeName
        base.baseOid
        base.arrayOid
        False
        (\_resolver _value -> Right (Write.bWord64 0))
        base.binaryDecoder
        base.textualEncoder
        base.textualDecoder

-- | Extract the element-OID slot from an encoded binary array.
--
-- The binary array header is
-- @[ndim :: int4][flags :: int4][element oid :: int4][dim size :: int4][lower bound :: int4]…@,
-- so the element OID occupies bytes 8-11, big-endian.
arrayHeaderElementOid :: BS.ByteString -> Word32
arrayHeaderElementOid bytes =
  fromIntegral (BS.index bytes 8) * 16777216
    + fromIntegral (BS.index bytes 9) * 65536
    + fromIntegral (BS.index bytes 10) * 256
    + fromIntegral (BS.index bytes 11)

-- | Fetch a single-row, single-column @oid@ value as text and parse it.
fetchOid :: PQ.Connection -> BS8.ByteString -> IO Word32
fetchOid conn sql = do
  mRes <- PQ.exec conn sql
  mBytes <- case mRes of
    Just res -> PQ.getvalue' res (PQ.toRow (0 :: Int)) (PQ.toColumn (0 :: Int))
    Nothing -> pure Nothing
  pure (maybe 0 (maybe 0 (fromIntegral . fst) . BS8.readInt) mBytes)

spec :: SpecHook.HookedSpec
spec = do
  describe "Params" $ do
    it "resolved enum parameter OID matches pg_type.oid" $ \env ->
      Pool.withResource env.pool $ \conn -> do
        -- real OID straight from the catalog
        realOid <- fetchOid conn "SELECT to_regtype('param_color')::oid::text"
        let ps = PostgresqlCodecs.param colorScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
            resolver s n =
              if (s, n) == ("", "param_color")
                then PostgresqlCodecs.TypeOids realOid 0
                else PostgresqlCodecs.TypeOids 0 0
        realOid `shouldNotBe` 0
        ps.oids resolver `shouldBe` [realOid]
        ps.oids (\_ _ -> PostgresqlCodecs.TypeOids 0 0) `shouldBe` [0]

    it "binary array header carries the resolved element OID" $ \_env -> do
      let ps =
            PostgresqlCodecs.param
              colorScalar
              (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector)
              PostgresqlCodecs.nonNullable
          sentinelBaseOid = 987654 :: Word32
          sentinelArrayOid = 987655 :: Word32
          resolver s n =
            if (s, n) == ("", "param_color")
              then PostgresqlCodecs.TypeOids sentinelBaseOid sentinelArrayOid
              else PostgresqlCodecs.TypeOids 0 0
      case ps.binaryEncoder resolver (Vector.fromList [Red, Green]) of
        Left report -> expectationFailure ("Encoding failed: " <> show report)
        Right [Just write] ->
          arrayHeaderElementOid (Write.toByteString write) `shouldBe` sentinelBaseOid
        Right other ->
          expectationFailure ("Unexpected encoder output: " <> show (length other) <> " value(s)")

    it "single int4 param" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          (PostgresqlTypes.Int4.fromInt32 42)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "single int8 param" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int8"
          (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          (PostgresqlTypes.Int8.fromInt64 9876543210)
      result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)

    it "single text param" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::text"
          (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ("hello" :: PostgresqlTypes.Text)
      result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)

    it "single bool param" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::bool"
          (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          (PostgresqlTypes.Bool.fromBool True)
      result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)

    it "single float8 param" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::float8"
          (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          (PostgresqlTypes.Float8.fromDouble 3.14)
      result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)

    it "multiple params" $ \env -> do
      let ps =
            contramap fst (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
              <> contramap snd (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          cols =
            (,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4, $2::int4"
          ps
          (PostgresqlCodecs.oneRow cols)
          env.binary
          (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)

    it "formats are populated from supportsBinary and concatenated" $ \_env -> do
      let ps :: PostgresqlCodecs.Params (PostgresqlTypes.Int4, PostgresqlTypes.Int4)
          ps =
            contramap fst (PostgresqlCodecs.param textOnlyInt4 PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
              <> contramap snd (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          emptyPs :: PostgresqlCodecs.Params (PostgresqlTypes.Int4, PostgresqlTypes.Int4)
          emptyPs = mempty
      ps.formats `shouldBe` [PostgresqlCodecs.Textual, PostgresqlCodecs.Binary]
      emptyPs.formats `shouldBe` []

    it "supportsBinary=False parameter stays textual alongside a binary parameter" $ \env -> do
      let ps =
            contramap fst (PostgresqlCodecs.param textOnlyInt4 PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
              <> contramap snd (PostgresqlCodecs.param PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          cols =
            (,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4, $2::int4"
          ps
          (PostgresqlCodecs.oneRow cols)
          env.binary
          (PostgresqlTypes.Int4.fromInt32 7, PostgresqlTypes.Int4.fromInt32 9)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 7, PostgresqlTypes.Int4.fromInt32 9)
