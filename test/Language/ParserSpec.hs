module Language.ParserSpec
  ( main
  , spec
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Fixtures.Examples
import Language.Parser
import Language.Types
import Test.Hspec

main :: IO ()
main = hspec spec

{- HLint ignore spec "Redundant do" -}
spec :: Spec
spec = do
  describe "parse" $ do
    coreExamples <- runIO readCoreExamples
    forM_ coreExamples $ \example -> do
      let name = getExampleName example
          code = getCode example
          parsed = getParsed example
      describe ("with " ++ name) $ do
        it "A result of parsing should match with a predefined parsed" $ do
          parse code `shouldBe` parsed
