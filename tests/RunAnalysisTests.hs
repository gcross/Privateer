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
import qualified Data.ByteString.Char8 as S8
import Data.Either.Unwrap
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Trie as Trie

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
import Algorithm.GlobalVariablePrivatization.Privatization

import Algorithm.VariableLayout

import CommonTestUtils
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
                privatized_source_filepath = source_filepath ++ "-privatized" <.> "c"
                privatized_executable_filepath = source_filepath ++ "-privatized"
            in do
                doesFileExist source_filepath >>= (assertBool $ "Does the source file exist?")
                preprocessor_result <- runPreprocessor gcc (CppArgs [] [] Nothing source_filepath Nothing)
                transl_unit <- case preprocessor_result of
                    Left code -> assertFailure ("Preprocessing failed with exit code " ++ show code) >> undefined
                    Right input ->
                        case execParser_ translUnitP input nopos of
                            Left error -> assertFailure ("Parsing failed with error " ++ show error) >> undefined
                            Right transl_unit -> return transl_unit
                writeFile analysis_filepath
                    .
                    render
                    .
                    Algorithm.GlobalVariablePrivatization.SizeAnalysis.processTranslUnit
                    $
                    transl_unit
                (rawSystem "cc" [analysis_filepath,"-o",analysis_executable_filepath]) >>=
                    assertEqual "Were we able to compute the analysis file?" ExitSuccess
                (_, Just process_output, _, _) <-
                    createProcess (proc analysis_executable_filepath []) { std_out = CreatePipe }
                output_tree <- S.hGetContents process_output >>= return . parseTree' Nothing --'
                whenLeft output_tree $ \error -> assertFailure ("Parsing XML output failed with error " ++ show error)
                let AnalyzedModule exported_variables hidden_variables functions_with_statics =
                        allocationListToAnalyzedModule
                        .
                        snd
                        .
                        fromJust
                        .
                        allocateNamedBlocks initialBlockList
                        .
                        xmlToRequestList
                        .
                        fromRight
                        $
                        output_tree
                    module_data_accessor_name = "getPtr"
                    global_variables_as_trie = exported_variables `Trie.unionR` hidden_variables
                    global_variables_as_set = Set.fromList . map S8.unpack . Trie.keys $ global_variables_as_trie
                    getGlobalVariableOffset = makeTrieLookupFunction global_variables_as_trie
                    getFunctionStaticVariableOffset =
                        fmap makeTrieLookupFunction
                        .
                        flip Trie.lookup functions_with_statics
                        .
                        S8.pack

                writeFile privatized_source_filepath
                    .
                    render
                    .
                    pretty
                    .
                    Algorithm.GlobalVariablePrivatization.Privatization.processTranslUnit
                        module_data_accessor_name
                        global_variables_as_set
                        getGlobalVariableOffset
                        getFunctionStaticVariableOffset
                    $
                    transl_unit
                return ()

    return
        [   makeTest "simple" "source1"
        ]
  where
    makeTrieLookupFunction map = toInteger . fromJust . flip Trie.lookup map . S8.pack
-- @-node:gcross.20090523222635.25:makeTests
-- @-others

main = makeTests >>= defaultMain
-- @-node:gcross.20090523222635.23:@thin RunAnalysisTests.hs
-- @-leo
