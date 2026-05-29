module IntegrationTest.SpecHook where

import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Prelude (setupContainer)
import Test.Hspec
import qualified TestContainers.Hspec as TestContainers

type HookedSpec = SpecWith PQ.Connection

hook :: HookedSpec -> Spec
hook = aroundAll (TestContainers.withContainers setupContainer)
