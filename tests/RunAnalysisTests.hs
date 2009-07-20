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
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Either.Unwrap
import Data.List
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
import System.IO
import System.Process

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.PrettyPrint
import Text.Printf
import Text.XML.Expat.Tree

import Algorithm.GlobalVariablePrivatization.SizeAnalysis
import Algorithm.GlobalVariablePrivatization.Privatization

import Algorithm.VariableLayout

import CommonTestUtils

import Debug.Trace
-- @-node:gcross.20090523222635.24:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090718130736.11:makeGlobalVariableAccessor
makeGlobalVariableAccessor =
    fromRight
    .
    flip (execParser_ extDeclP) nopos
    .
    inputStreamFromString
    .
    printf "void *getPtr() { static char data[%i]; return data; }"
-- @-node:gcross.20090718130736.11:makeGlobalVariableAccessor
-- @+node:gcross.20090523222635.25:makeTests
makeTests :: IO [Test.Framework.Test]
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
        includepath = sourcepath </> "include"
        buildpath = replaceFileName sourcepath "build"
    createDirectoryIfMissing True buildpath

    let makeTest :: String -> Test.Framework.Test
        makeTest filename = testCase filename $
            -- @            << File paths >>
            -- @+node:gcross.20090718130736.17:<< File paths >>
            let original_source_filepath = sourcepath </> filename <.> "c"
                modified_executable_filepath = buildpath </> filename
                modified_source_filepath = modified_executable_filepath <.> "c"
                modified_log_filepath = modified_executable_filepath <.> "log"
                analysis_executable_filepath = modified_executable_filepath ++ "-analyze"
                analysis_source_filepath = analysis_executable_filepath <.> "c"
                privatized_executable_filepath = modified_executable_filepath ++ "-privatized"
                privatized_source_filepath = privatized_executable_filepath <.> "c"
                privatized_log_filepath = privatized_executable_filepath <.> "log"
            -- @-node:gcross.20090718130736.17:<< File paths >>
            -- @nl
            in do
                -- @                << Pre-process and parse source file >>
                -- @+node:gcross.20090718130736.12:<< Pre-process and parse source file >>
                doesFileExist original_source_filepath >>= (assertBool $ "Does the source file exist?")
                preprocessor_result <- runPreprocessor gcc (CppArgs [IncludeDir includepath] [] Nothing original_source_filepath Nothing)
                transl_unit <- case preprocessor_result of
                    Left code -> assertFailure ("Preprocessing failed with exit code " ++ show code) >> undefined
                    Right input ->
                        case execParser_ translUnitP input nopos of
                            Left error -> assertFailure ("Parsing failed with error " ++ show error) >> undefined
                            Right transl_unit -> return transl_unit
                -- @-node:gcross.20090718130736.12:<< Pre-process and parse source file >>
                -- @nl
                -- @                << Perform size analysis >>
                -- @+node:gcross.20090718130736.13:<< Perform size analysis >>
                writeFile analysis_source_filepath
                    .
                    render
                    .
                    Algorithm.GlobalVariablePrivatization.SizeAnalysis.processTranslUnit
                    $
                    transl_unit
                (rawSystem "cc" [analysis_source_filepath,"-o",analysis_executable_filepath]) >>=
                    assertEqual "Were we able to compute the analysis file?" ExitSuccess
                (_, Just process_output, _, _) <-
                    createProcess (proc analysis_executable_filepath []) { std_out = CreatePipe }
                output_tree <- S.hGetContents process_output >>= return . parseTree' Nothing --'
                whenLeft output_tree $ \error -> assertFailure ("Parsing XML output failed with error " ++ show error)
                -- @-node:gcross.20090718130736.13:<< Perform size analysis >>
                -- @nl
                -- @                << Perform allocation of space for global variables >>
                -- @+node:gcross.20090718130736.14:<< Perform allocation of space for global variables >>
                let request_list =
                        xmlToRequestList
                        .
                        fromRight
                        $
                        output_tree
                    allocation_list = 
                        snd
                        .
                        fromMaybe (error "failed to allocate memory blocks!")
                        .
                        allocateNamedBlocks initialBlockList
                        $
                        request_list
                    allocation_size = totalSpaceRequired request_list allocation_list
                    (AnalyzedModule exported_variables hidden_variables functions_with_statics) =
                        allocationListToAnalyzedModule allocation_list
                    global_variables_as_trie = exported_variables `Trie.unionR` hidden_variables
                    global_variables_as_set = Set.fromList . map S8.unpack . Trie.keys $ global_variables_as_trie
                -- @nonl
                -- @-node:gcross.20090718130736.14:<< Perform allocation of space for global variables >>
                -- @nl
                -- @                << Perform privatization >>
                -- @+node:gcross.20090718130736.15:<< Perform privatization >>
                let module_data_accessor_name = "getPtr"
                    getGlobalVariableOffset = makeTrieLookupFunction global_variables_as_trie
                    getFunctionStaticVariableOffset =
                        fmap makeTrieLookupFunction
                        .
                        flip Trie.lookup functions_with_statics
                        .
                        S8.pack
                    CTranslUnit processed_declarations _ = 
                        Algorithm.GlobalVariablePrivatization.Privatization.processTranslUnit
                            module_data_accessor_name
                            global_variables_as_set
                            getGlobalVariableOffset
                            getFunctionStaticVariableOffset
                            transl_unit
                    global_variables_to_initialize = Set.elems global_variables_as_set
                    functions_with_statics_to_initialize = map S8.unpack . Trie.keys $ functions_with_statics
                    final_declarations =
                        [   makeGlobalVariableAccessor allocation_size
                        ,   CDeclExt $ makeInitializerForwardDeclaration
                                global_variables_to_initialize
                                functions_with_statics_to_initialize
                        ,   CFDefExt $ makeModuleInitializer "__initialize__"
                                global_variables_to_initialize
                                functions_with_statics_to_initialize
                        ] ++ processed_declarations
                writeFile privatized_source_filepath
                    .
                    render
                    .
                    pretty
                    .
                    flip CTranslUnit internalNode
                    $
                    final_declarations
                -- @-node:gcross.20090718130736.15:<< Perform privatization >>
                -- @nl
                -- @                << Compare outputs >>
                -- @+node:gcross.20090718130736.16:<< Compare outputs >>
                L.readFile original_source_filepath
                    >>= L.writeFile modified_source_filepath . L.append (L8.pack "void __initialize__() { }\n\n")

                openFile modified_log_filepath WriteMode
                    >>=
                    (\handle -> createProcess (shell . intercalate " " $
                        ["cc","-I",includepath,modified_source_filepath,"-o",modified_executable_filepath])
                        { std_out = CreatePipe, std_err = UseHandle handle })
                    >>=
                    (\(_,_,_,handle) -> return handle)
                    >>=
                    waitForProcess
                    >>=
                    assertEqual "Were we able to compile the modified source file?" ExitSuccess

                openFile privatized_log_filepath WriteMode
                    >>=
                    (\handle -> createProcess (shell . intercalate " " $
                        ["cc","-O3",privatized_source_filepath,"-o",privatized_executable_filepath])
                        { std_out = CreatePipe, std_err = UseHandle handle })
                    >>=
                    (\(_,_,_,handle) -> return handle)
                    >>=
                    waitForProcess
                    >>=
                    assertEqual "Were we able to compile the privatized source file?" ExitSuccess

                original_output <-
                    createProcess (proc modified_executable_filepath []) { std_out = CreatePipe }
                    >>=
                    (\(_,Just x,_,_) -> return x)
                    >>=
                    L.hGetContents


                privatized_output <-
                    createProcess (proc privatized_executable_filepath []) { std_out = CreatePipe }
                    >>=
                    (\(_,Just x,_,_) -> return x)
                    >>=
                    L.hGetContents

                assertEqual "Is the program output the same after privatization?" original_output privatized_output
                -- @-node:gcross.20090718130736.16:<< Compare outputs >>
                -- @nl
                return ()

    getDirectoryContents sourcepath >>= return . map (makeTest . dropExtension . takeFileName) . filter ((== ".c") . takeExtension)
  where
    makeTrieLookupFunction map name =
        toInteger
        .
        fromMaybe (error $ "unable to find variable named " ++ show name)
        .
        flip Trie.lookup map
        .
        S8.pack
        $
        name
-- @-node:gcross.20090523222635.25:makeTests
-- @-others

main = makeTests >>= defaultMain
-- @-node:gcross.20090523222635.23:@thin RunAnalysisTests.hs
-- @-leo
