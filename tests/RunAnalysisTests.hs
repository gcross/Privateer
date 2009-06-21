-- @+leo-ver=4-thin
-- @+node:gcross.20090523222635.23:@thin RunAnalysisTests.hs
-- @@language haskell

-- @<< Language extensions >>
-- @+node:gcross.20090523222635.27:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20090523222635.27:<< Language extensions >>
-- @nl

module RunAnalysisTests(makeTests) where

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
import System.Process
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.PrettyPrint
import Text.XML.Expat.Tree

import Algorithm.GlobalVariablePrivatization.SizeAnalysis
-- @nonl
-- @-node:gcross.20090523222635.24:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090523222635.25:makeTests
makeTests = do
    let gcc = newGCC "gcc"
    current_directory <- getCurrentDirectory
    let directories_to_search_for_sources =
            [   current_directory </> "sources"
            ,   current_directory </> "tests" </> "sources"
            ]
    source_directories_found <- filterM doesDirectoryExist directories_to_search_for_sources
    let sourcepath =
            case source_directories_found of
                x:_ -> x
                _ -> error $ "Unable to find sources for running analysis tests!  Tried '"
                                    ++ show directories_to_search_for_sources

        makeTest :: String -> String -> Test.Framework.Test
        makeTest testname filename = testCase testname $
            let source_filepath = sourcepath </> filename <.> "c"
                analysis_filepath = source_filepath ++ "-analyze" <.> "c"
                analysis_executable_filepath = source_filepath ++ "-analyze"
            in do
                doesFileExist source_filepath >>= (assertBool $ "Does the source file exist?")
                preprocessor_result <- runPreprocessor gcc (CppArgs [] [] Nothing source_filepath Nothing)
                whenLeft preprocessor_result $ \code -> assertFailure ("Preprocessing failed with exit code " ++ show code)
                writeFile analysis_filepath
                    .
                    render
                    .
                    processStream
                    .
                    fromRight
                    $
                    preprocessor_result
                (rawSystem "cc" [analysis_filepath,"-o",analysis_executable_filepath]) >>=
                    assertEqual "Were we able to compute the analysis file?" ExitSuccess
                (_, Just process_output, _, _) <-
                    createProcess (proc analysis_executable_filepath []) { std_out = CreatePipe }
                output_tree <- S.hGetContents process_output >>= return . parseTree' Nothing --'
                whenLeft output_tree $ \error -> assertFailure ("Parsing XML output failed with error " ++ show error)
                evaluate . xmlToAnalyzedModule . fromRight $ output_tree 
                return ()

    return
        [   makeTest "simple" "source1"
        ]
-- @-node:gcross.20090523222635.25:makeTests
-- @-others

main = makeTests >>= defaultMain
-- @-node:gcross.20090523222635.23:@thin RunAnalysisTests.hs
-- @-leo
