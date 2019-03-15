module Fixtures.Examples
  ( CoreExample
  , CoreExampleData
  , CoreExampleName
  , CoreExampleCode

  , readCoreExamples
  , getExampleName
  , getCode
  , getParsed
  )
where

import Control.Arrow
import Control.Monad
import Data.List
import Language.Types
import System.Directory
import System.FilePath
import System.IO.Unsafe

import Paths_core_lang_haskell

type CoreExample = (CoreExampleName, CoreExampleData)
type CoreExampleData = (CoreExampleCode, CoreProgram)
type CoreExampleName = String
type CoreExampleCode = String

getExampleName :: CoreExample -> CoreExampleName
getExampleName (name, _) = name

getCode :: CoreExample -> CoreExampleCode
getCode (_, (code, _)) = code

getParsed :: CoreExample -> CoreProgram
getParsed (_, (_, parsed)) = parsed

readCoreExamples :: IO [CoreExample]
readCoreExamples = do
  dir <- readCoreExampleDirectory
  fileNames <- listDirectory dir
  let names = sort . map dropExtension . filter isExampleFile $ fileNames
      paths = makeExamplePaths dir names
  forM (zip names paths) readCoreExample
  where
    readCoreExample = runKleisli . second $ Kleisli readFile *** readFileAsProgramKleisli
    readFileAsProgramKleisli = Kleisli readFile >>> arr read
    makeExamplePaths = map . makeExamplePath
    makeExamplePath dir = ((`addExtension` ".core") &&& (`addExtension` ".parse")) . combine dir

readCoreExampleDirectory :: IO FilePath
readCoreExampleDirectory = (</> "examples") <$> getDataDir

isExampleFile :: CoreExampleName -> Bool
isExampleFile name = ".core" `isExtensionOf` name
