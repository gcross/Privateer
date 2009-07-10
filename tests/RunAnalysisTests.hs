-- @+leo-ver=4-thin
-- @+node:gcross.20090523222635.23:@thin RunAnalysisTests.hs
-- @@language haskell

-- @<< Language extensions >>
-- @+node:gcross.20090523222635.27:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20090523222635.27:<< Language extensions >>
-- @nl

module RunAnalysisTests(tests) where

-- @<< Imports >>
-- @+node:gcross.20090523222635.24:<< Imports >>
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as S
import Data.Either.Unwrap
import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess
import System
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.PrettyPrint
import Text.XML.Expat.Tree

import Algorithm.GlobalVariablePrivatization.SizeAnalysis

import CommonTestUtils
-- @-node:gcross.20090523222635.24:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090709200011.5:makeTestFromSource
makeTestFromSource :: String -> Assertion
makeTestFromSource source = do
    temporary_directory <- getTemporaryDirectory
    (source_filepath, source_handle) <- openTempFile temporary_directory "test.c"
    (analysis_source_filepath, analysis_source_handle) <- openTempFile temporary_directory "test-analysis.c"
    (analysis_executable_filepath, analysis_executable_handle) <-  openTempFile temporary_directory "test-analysis"
    finally (do
        hClose analysis_executable_handle
        hPutStr source_handle source
        hClose source_handle
        let gcc = newGCC "gcc"
        preprocessor_result <- runPreprocessor gcc (CppArgs [] [] Nothing source_filepath Nothing)
        whenLeft preprocessor_result $ \code -> assertFailure ("Preprocessing failed with exit code " ++ show code)
        hPutStr analysis_source_handle
            .
            render
            .
            processStream
            .
            fromRight
            $
            preprocessor_result
        hClose analysis_source_handle
        (rawSystem "cc" [analysis_source_filepath,"-o",analysis_executable_filepath]) >>=
            assertEqual "Were we able to compute the analysis file?" ExitSuccess
        (_, Just process_output, _, _) <-
            createProcess (proc analysis_executable_filepath []) { std_out = CreatePipe }
        output_tree <- S.hGetContents process_output >>= return . parseTree' Nothing --'
        whenLeft output_tree $ \error -> assertFailure ("Parsing XML output failed with error " ++ show error)
        evaluate . xmlToAnalyzedModule . fromRight $ output_tree 
        return ()
      ) (deleteTemporaries
            [   source_filepath
            ,   analysis_source_filepath
            ,   analysis_executable_filepath
            ]
      )
-- @-node:gcross.20090709200011.5:makeTestFromSource
-- @+node:gcross.20090709200011.7:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090709200011.6:test 1
    [testCase "1" $ makeTestFromSource . unlines $
        ["int i;"
        ,"static int j;"
        ,"extern int k;"
        ]
    -- @-node:gcross.20090709200011.6:test 1
    -- @-others
    ]
-- @nonl
-- @-node:gcross.20090709200011.7:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090523222635.23:@thin RunAnalysisTests.hs
-- @-leo
