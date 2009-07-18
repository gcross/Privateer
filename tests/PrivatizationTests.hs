-- @+leo-ver=4-thin
-- @+node:gcross.20090709200011.8:@thin PrivatizationTests.hs
-- @@language haskell

-- @<< Language extensions >>
-- @+node:gcross.20090709200011.9:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20090709200011.9:<< Language extensions >>
-- @nl

module PrivatizationTests(tests) where

-- @<< Imports >>
-- @+node:gcross.20090709200011.10:<< Imports >>
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.RWS

import qualified Data.ByteString as S
import Data.Either.Unwrap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

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

import Algorithm.GlobalVariablePrivatization.Privatization

import CommonTestUtils
-- @-node:gcross.20090709200011.10:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090715105401.29:Helpers
-- @+node:gcross.20090715105401.27:makeFunctionFromMap
makeFunctionFromMap map = toInteger . fromJust . flip Map.lookup map
-- @-node:gcross.20090715105401.27:makeFunctionFromMap
-- @+node:gcross.20090715105401.28:makeFunctionFromLocalStaticMap
makeFunctionFromLocalStaticMap map = fmap makeFunctionFromMap . flip Map.lookup map
-- @-node:gcross.20090715105401.28:makeFunctionFromLocalStaticMap
-- @-node:gcross.20090715105401.29:Helpers
-- @+node:gcross.20090709200011.36:Test makers
-- @+node:gcross.20090709200011.11:makeTestFromSource
makeTestFromSource :: String -> Assertion
makeTestFromSource source = do
    temporary_directory <- getTemporaryDirectory
    (source_filepath, source_handle) <- openTempFile temporary_directory "test.c"
    (executable_filepath, executable_handle) <-  openTempFile temporary_directory "test"
    finally (do
        hClose executable_handle
        hPutStr source_handle source
        hClose source_handle
        (rawSystem "cc" [source_filepath,"-o",executable_filepath]) >>=
            assertEqual "Were we able to compile the source code?" ExitSuccess
        (_, Just process_output, _, process_handle) <-
            createProcess (proc executable_filepath []) { std_out = CreatePipe }
        exit_code <- waitForProcess process_handle
        when (exit_code /= ExitSuccess) $ hGetContents process_output >>= assertFailure
      ) (deleteTemporaries
            [   source_filepath
            ,   executable_filepath
            ]
      )
-- @-node:gcross.20090709200011.11:makeTestFromSource
-- @+node:gcross.20090711085032.33:makeTestFromPrivatizedSource
makeTestFromPrivatizedSource :: String -> Set String -> Map String Int -> Map String (Map String Int) -> Bool -> String -> String -> Assertion
makeTestFromPrivatizedSource
    module_data_accessor_name
    global_variables
    global_variable_index_map
    local_static_variable_index_map
    print_source
    prelude
    =
    makeTestFromSource
    .
    (if print_source then echo id else id)
    .
    (prelude ++)
    .
    render
    .
    pretty
    .
    processTranslUnit module_data_accessor_name global_variables (makeFunctionFromMap global_variable_index_map) (makeFunctionFromLocalStaticMap local_static_variable_index_map)
    .
    parseTranslUnit
-- @nonl
-- @-node:gcross.20090711085032.33:makeTestFromPrivatizedSource
-- @+node:gcross.20090709200011.35:makePrivatizeExprTest
makePrivatizeExprTest :: Set String -> Set String -> String -> String -> Assertion
makePrivatizeExprTest global_variables local_static_variables original_code privatized_code =
    assertEqual "is the privatized code correct?" privatized_code
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (\(x,_,_) -> x)
    .
    (\expr -> runRWS (privatizeExpr expr) undefined (global_variables,local_static_variables))
    .
    parseExpression
    $
    original_code
-- @-node:gcross.20090709200011.35:makePrivatizeExprTest
-- @+node:gcross.20090709200011.41:makePrivatizeStmtTest
makePrivatizeStmtTest :: String -> Map String Int -> Set String -> Set String -> String -> String -> Assertion
makePrivatizeStmtTest
    module_data_accessor_name
    local_static_variable_index_map
    global_variables local_static_variables
    original_code privatized_code
    =
    assertEqual "is the privatized code correct?" (unwords . words . render . pretty . parseStatement $ privatized_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (\(x,_,_) -> x)
    .
    (\stat -> runRWS (privatizeStmt stat)
                    (FunctionProcessingEnvironment module_data_accessor_name (makeFunctionFromMap local_static_variable_index_map))
                    (global_variables,local_static_variables)
    )
    .
    parseStatement
    $
    original_code
-- @-node:gcross.20090709200011.41:makePrivatizeStmtTest
-- @+node:gcross.20090710174219.12:makePrivatizeFunctionTest
makePrivatizeFunctionTest :: String -> Map String Int -> Set String -> String -> String -> Assertion
makePrivatizeFunctionTest
    module_data_accessor_name
    local_static_variable_index_map
    global_variables
    original_code privatized_code
    =
    assertEqual "is the privatized code correct?" (unwords . words . render . pretty . parseDeclaration $ privatized_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    privatizeFunction
        module_data_accessor_name
        global_variables
        (makeFunctionFromMap local_static_variable_index_map)
    .
    (\(CFDefExt x) -> x)
    .
    parseDeclaration
    $
    original_code
-- @-node:gcross.20090710174219.12:makePrivatizeFunctionTest
-- @+node:gcross.20090711085032.9:makeProcessStmtTest
makeProcessStmtTest :: String -> Map String Int -> String -> String -> Assertion
makeProcessStmtTest
    module_data_accessor_name
    local_static_variable_index_map
    original_code processed_code
    =
    assertEqual "is the processed code correct?" (unwords . words . render . pretty . parseStatement $ processed_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (\stat -> runReader (processStmt stat) (FunctionProcessingEnvironment module_data_accessor_name (makeFunctionFromMap local_static_variable_index_map)))
    .
    parseStatement
    $
    original_code
-- @-node:gcross.20090711085032.9:makeProcessStmtTest
-- @+node:gcross.20090711085032.22:makeProcessFunctionTest
makeProcessFunctionTest :: String -> Map String Int -> String -> String -> Assertion
makeProcessFunctionTest
    module_data_accessor_name
    local_static_variable_index_map
    original_code processed_code
    =
    assertEqual "is the processed code correct?" (unwords . words . render . pretty . parseDeclaration $ processed_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    CFDefExt
    .
    processFunction module_data_accessor_name (makeFunctionFromMap local_static_variable_index_map)
    .
    (\(CFDefExt x) -> x)
    .
    parseDeclaration
    $
    original_code
-- @-node:gcross.20090711085032.22:makeProcessFunctionTest
-- @+node:gcross.20090711085032.28:makeProcessToplevelDeclarationTest
makeProcessToplevelDeclarationTest :: String -> Set String -> Map String Int -> Map String (Map String Int) -> String -> String -> Assertion
makeProcessToplevelDeclarationTest
    module_data_accessor_name
    global_variables
    global_variable_index_map
    local_static_variable_index_map
    original_code processed_code
    =
    assertEqual "is the processed code correct?" (unwords . words . render . pretty . parseTranslUnit $ processed_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (flip CTranslUnit internalNode)
    .
    processToplevelDeclaration module_data_accessor_name global_variables (makeFunctionFromMap global_variable_index_map) (makeFunctionFromLocalStaticMap local_static_variable_index_map) 
    .
    parseDeclaration
    $
    original_code
-- @-node:gcross.20090711085032.28:makeProcessToplevelDeclarationTest
-- @-node:gcross.20090709200011.36:Test makers
-- @+node:gcross.20090709200011.12:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090709200011.13:test
    [testCase "test" $ makeTestFromSource . unlines $
        ["#include <stdio.h>"
        ,"int main() { printf(\"Nothing to see here;  move along.\"); return 0; }"
        ]
    -- @-node:gcross.20090709200011.13:test
    -- @+node:gcross.20090709200011.15:makeAccessor
    ,testCase "makeAccessor" $
        let accessor_source = render . pretty $
                let size_const = CConst (CIntConst (cInteger.toInteger $ 6) internalNode)
                    indirections = [CArrDeclr [] (CArrSize False size_const) internalNode]
                in makeAccessor "getGlobals" False "global_variable" (makeTypedefSpecifier "global_variable_type") indirections 6
            source = unlines
                [""
                ,"#include <stdio.h>"
                ,"#include <stdlib.h>"
                ,""
                ,"char* global_string = \"wrong\\0right\";"
                ,"void* getGlobals() { return global_string; }"
                ,""
                ,"typedef char global_variable_type;"
                ,""
                ]
                ++ accessor_source ++ unlines
                [""
                ,"int main() {"
                ,"  char (*string)[6] = __access__global_variable();"
                ,"  printf(\"Expected 'right' but got '%s'\",string);"
                ,"  return strcmp(string,\"right\");"
                ,"}"
                ]
        in makeTestFromSource source
    -- @-node:gcross.20090709200011.15:makeAccessor
    -- @+node:gcross.20090711085032.7:Privatization
    ,testGroup "Privatization"
        -- @    @+others
        -- @+node:gcross.20090709200011.33:privatizeExpr
        [testGroup "privatizeExpr"
            -- @    @+others
            -- @+node:gcross.20090709200011.34:global variable
            [testCase "global variable" $
                makePrivatizeExprTest (Set.singleton "var") Set.empty "1 + var * 2" "1 + *__access__var() * 2"
            -- @-node:gcross.20090709200011.34:global variable
            -- @+node:gcross.20090711085032.37:nested global variable
            ,testCase "nested global variable" $
                makePrivatizeExprTest (Set.singleton "c") Set.empty "strcpy(c, d)" "strcpy(*__access__c(), d)"
            -- @-node:gcross.20090711085032.37:nested global variable
            -- @+node:gcross.20090709200011.37:local static variable
            ,testCase "local static variable" $
                makePrivatizeExprTest Set.empty (Set.singleton "var") "var * 2 == 5 ? i : j" "*var * 2 == 5 ? i : j"
            -- @-node:gcross.20090709200011.37:local static variable
            -- @+node:gcross.20090709200011.38:both
            ,testCase "both" $
                makePrivatizeExprTest (Set.singleton "var") (Set.singleton "var") "(int) sizeof(var++)" "(int) sizeof((*var)++)"
            -- @-node:gcross.20090709200011.38:both
            -- @+node:gcross.20090709200011.39:neither
            ,testCase "neither" $
                makePrivatizeExprTest (Set.singleton "var") (Set.singleton "var") "++nonvar" "++nonvar"
            -- @-node:gcross.20090709200011.39:neither
            -- @-others
            ]
        -- @-node:gcross.20090709200011.33:privatizeExpr
        -- @+node:gcross.20090709200011.42:privatizeStmt
        ,testGroup "privatizeStmt"
            -- @    @+others
            -- @+node:gcross.20090709200011.43:global variable
            [testCase "global variable" $
                makePrivatizeStmtTest undefined undefined (Set.singleton "var") Set.empty
                    "if (var == i) { return var; } else { return j; }"
                    "if (*__access__var() == i) { return *__access__var(); } else { return j; }"
            -- @-node:gcross.20090709200011.43:global variable
            -- @+node:gcross.20090709200011.44:local static variable
            ,testCase "local static variable" $
                makePrivatizeStmtTest undefined undefined Set.empty (Set.singleton "var")
                "switch (var) { case 1: var = var + 1; case 2: ++var; }"
                "switch (*var) { case 1: *var = *var + 1; case 2: ++ (*var); }"
            -- @-node:gcross.20090709200011.44:local static variable
            -- @+node:gcross.20090709200011.45:both
            ,testCase "both" $
                makePrivatizeStmtTest undefined undefined (Set.singleton "var") (Set.singleton "var")
                "for (var = 0; var < 5; var++) { return var; }"
                "for (*var = 0; *var < 5; (*var)++) { return *var; }"
            -- @-node:gcross.20090709200011.45:both
            -- @+node:gcross.20090709200011.46:neither
            ,testCase "neither" $
                makePrivatizeStmtTest undefined undefined (Set.singleton "var") (Set.singleton "var")
                "if (nonvar) return 4;"
                "if (nonvar) return 4;"
            -- @-node:gcross.20090709200011.46:neither
            -- @-others
            ]
        -- @-node:gcross.20090709200011.42:privatizeStmt
        -- @+node:gcross.20090710174219.5:privatizeBlockItem
        ,testGroup "privatizeBlockItem"
            -- @    @+others
            -- @+node:gcross.20090710174219.6:shadowing
            [testCase "shadowing" $
                makePrivatizeStmtTest undefined undefined (Set.singleton "var") (Set.singleton "var")
                "{ var++; int var; var++; }"
                "{ (*var)++; int var; var++; }"
            -- @-node:gcross.20090710174219.6:shadowing
            -- @+node:gcross.20090710174219.7:static declaration
            ,testCase "static declaration" $
                makePrivatizeStmtTest "getPtr" (Map.fromList [("var1",42),("var2",12)]) Set.empty Set.empty
                "{ ++var; static int var1, *var2; ++var1; var2 = &var1; }"
                "{ ++var; typedef int __type_of__var1; __type_of__var1 *var1 = (__type_of__var1*) (getPtr() + 42), **var2 = (__type_of__var1 **) (getPtr() + 12); ++ (*var1); *var2 = &(*var1); }"
            -- @-node:gcross.20090710174219.7:static declaration
            -- @+node:gcross.20090710174219.8:external global variable
            ,testCase "external global variable" $
                makePrivatizeStmtTest undefined undefined (Set.singleton "var") Set.empty
                "{ var; extern int var; var; }"
                "{ *__access__var(); extern int *__access__var(); *__access__var(); }"
            -- @-node:gcross.20090710174219.8:external global variable
            -- @+node:gcross.20090710174219.9:emerging from the shadow
            ,testCase "emerging from the shadow" $
                makePrivatizeStmtTest undefined undefined Set.empty (Set.singleton "var")
                "{ if(1) { int var; var++; }; var++; }"
                "{ if(1) { int var; var++; }; (*var)++; }"
            -- @-node:gcross.20090710174219.9:emerging from the shadow
            -- @-others
            ]
        -- @-node:gcross.20090710174219.5:privatizeBlockItem
        -- @+node:gcross.20090710174219.13:privatizeFunction
        ,testGroup "privatizeFunction"
            -- @    @+others
            -- @+node:gcross.20090711085032.36:simple
            [testCase "function" $
                makePrivatizeFunctionTest undefined Map.empty (Set.singleton "c")
                    "static char* f() { strcpy(c,d); }"
                    "static char* f() { strcpy(*__access__c(),d); }"
            -- @-node:gcross.20090711085032.36:simple
            -- @+node:gcross.20090710174219.14:shadowing
            ,testCase "shadowing" $
                makePrivatizeFunctionTest "getPtr" (Map.fromList [("svar",1)]) (Set.fromList ["gvar","var"]) 
                "void f(int var) { static int svar = 0; ++svar; return var + gvar + svar; }"
                "void f(int var) { typedef int __type_of__svar; __type_of__svar *svar = (__type_of__svar*)(getPtr()+1); ++(*svar); return var + *__access__gvar() + *svar; }"
            -- @-node:gcross.20090710174219.14:shadowing
            -- @-others
            ]
        -- @-node:gcross.20090710174219.13:privatizeFunction
        -- @-others
        ]
    -- @-node:gcross.20090711085032.7:Privatization
    -- @+node:gcross.20090711085032.8:Initializer Construction
    ,testGroup "Initializer Construction"
        -- @    @+others
        -- @+node:gcross.20090709200011.24:makeInitializer
        [testGroup "makeInitializer"
            -- @    @+others
            -- @+node:gcross.20090709200011.28:with initializer
            [testCase "with initializer" $
                let initializer_source = render . pretty $
                        let init = CInitList [([],CInitExpr expr internalNode) | expr <-
                                        [   makeIntegerExpr 42
                                        ,   makeFloatExpr 3.14
                                        ,   makeStringExpr "right"
                                        ] ] internalNode
                        in makeInitializer "global_variable" (makeTypedefSpecifier "global_variable_type") [] (Just init)
                    source = unlines
                        [""
                        ,"#include <stdio.h>"
                        ,"#include <stdlib.h>"
                        ,"#include <string.h>"
                        ,""
                        ,"typedef struct {"
                        ,"  int a;"
                        ,"  float b;"
                        ,"  char *c;"
                        ,"} global_variable_type;"
                        ,""
                        ,"global_variable_type global_variable = {0,0,\"wrong\"};"
                        ,"global_variable_type* __access__global_variable() { return &global_variable; }"
                        ,""
                        ]
                        ++ initializer_source ++ unlines
                        [""
                        ,"int main() {"
                        ,"  __initialize__global_variable();"
                        ,"  printf(\"Expected {42,3.14,'right'} but got {%i,%f,'%s',%i}\","
                        ,"         global_variable.a,global_variable.b,global_variable.c);"
                        ,"  if(     (global_variable.a != 42)"
                        ,"      ||  (global_variable.b != 3.14f)"
                        ,"      ||  (strcmp(global_variable.c,\"right\") != 0)"
                        ,"    )"
                        ,"      return -1;"
                        ,"  else"
                        ,"      return 0;"
                        ,"}"
                        ]
                in makeTestFromSource source
            -- @-node:gcross.20090709200011.28:with initializer
            -- @+node:gcross.20090709200011.29:without initializer
            ,testCase "without initializer" $
                let initializer_source = render . pretty $ makeInitializer "global_variable" undefined [] Nothing
                    source = unlines
                        [""
                        ,"#include <stdio.h>"
                        ,""
                        ,"typedef struct {"
                        ,"  int a;"
                        ,"  float b;"
                        ,"  char *c;"
                        ,"} global_variable_type;"
                        ,""
                        ]
                        ++ initializer_source ++ unlines
                        [""
                        ,"int main() {"
                        ,"  __initialize__global_variable();"
                        ,"  return 0;"
                        ,"}"
                        ]
                in makeTestFromSource source
            -- @-node:gcross.20090709200011.29:without initializer
            -- @-others
            ]
        -- @-node:gcross.20090709200011.24:makeInitializer
        -- @+node:gcross.20090711085032.10:processStmt
        ,testGroup "processStmt"
            -- @    @+others
            -- @+node:gcross.20090711085032.13:single statement
            [testCase "single statement" $
                makeProcessStmtTest undefined undefined
                    "return i;"
                    "{ }"
            -- @-node:gcross.20090711085032.13:single statement
            -- @+node:gcross.20090711085032.16:multiple statements
            ,testCase "multiple statements" $
                makeProcessStmtTest undefined undefined
                    "{ i = 1;  ++i; }"
                    "{ }"
            -- @-node:gcross.20090711085032.16:multiple statements
            -- @+node:gcross.20090711085032.17:non-static declarations
            ,testCase "non-static declarations" $
                makeProcessStmtTest undefined undefined
                    "{ int i = 1;  ++i;  int j = i; }"
                    "{ int i = 1; int j = i; }"
            -- @-node:gcross.20090711085032.17:non-static declarations
            -- @+node:gcross.20090711085032.18:static declarations
            ,testCase "static declarations" $
                makeProcessStmtTest "getPtr" (Map.singleton "svar" 13)
                    "{ static int svar = 1;  ++svar;  int j = svar; }"
                    "{ static int svar = 1; memcpy(getPtr()+13,&svar,sizeof(svar)); int j = svar; }"
            -- @-node:gcross.20090711085032.18:static declarations
            -- @+node:gcross.20090711085032.19:nesting
            ,testCase "nesting" $
                makeProcessStmtTest "getPtr" (Map.singleton "svar" 13)
                    "{ if (i == 1) {static int svar = 1;  ++svar;} {for (i = 0; i < 2; ++i) { return 42; }} }"
                    "{ {static int svar = 1; memcpy(getPtr()+13,&svar,sizeof(svar)); } }"
            -- @-node:gcross.20090711085032.19:nesting
            -- @-others
            ]
        -- @-node:gcross.20090711085032.10:processStmt
        -- @+node:gcross.20090711085032.23:processFunction
        ,testGroup "processFunction"
            -- @    @+others
            -- @+node:gcross.20090711085032.24:single statement
            [testCase "single statement" $
                makeProcessFunctionTest undefined undefined
                    "const int f() { return i; }"
                    "static inline void __initialize_statics_in__f() { }"
            -- @-node:gcross.20090711085032.24:single statement
            -- @+node:gcross.20090711085032.25:arguments
            ,testCase "arguments" $
                makeProcessFunctionTest undefined undefined
                    "const int f(int a, restrict char* b, const short* c) { if (a == b) { return c; } else { ++(*b); }; }"
                    "static inline void __initialize_statics_in__f() { int a; restrict char* b; const short* c; }"
            -- @-node:gcross.20090711085032.25:arguments
            -- @+node:gcross.20090711085032.26:arguments and statics
            ,testCase "arguments and statics" $
                makeProcessFunctionTest "getPtr" (Map.singleton "meaning_of_life" 24)
                    "static char* f(void ** data) { static int meaning_of_life = 42; }"
                    "static inline void __initialize_statics_in__f() { void ** data; static int meaning_of_life = 42; memcpy(getPtr()+24,&meaning_of_life,sizeof(meaning_of_life)); }"
            -- @-node:gcross.20090711085032.26:arguments and statics
            -- @-others
            ]
        -- @-node:gcross.20090711085032.23:processFunction
        -- @+node:gcross.20090711085032.29:processToplevelDeclaration
        ,testGroup "processToplevelDeclaration"
            -- @    @+others
            -- @+node:gcross.20090711085032.30:external declaration
            [testCase "external declaration" $
                makeProcessToplevelDeclarationTest undefined (Set.singleton "ext_var") undefined undefined
                    "extern char var, ext_var;"
                    "extern char var, *__access__ext_var();"
            -- @-node:gcross.20090711085032.30:external declaration
            -- @+node:gcross.20090711085032.35:function
            ,testCase "function" $
                makeProcessToplevelDeclarationTest undefined (Set.singleton "c") undefined Map.empty
                    "static char* f() { strcpy(c,d); }"
                    "static char* f() { strcpy(*__access__c(),d); }"
            -- @-node:gcross.20090711085032.35:function
            -- @+node:gcross.20090711085032.34:static variable
            ,testCase "static variable" $
                makeTestFromPrivatizedSource "getPtr" (Set.singleton "c") (Map.singleton "c" 3) (Map.singleton "main" Map.empty) False
                (unlines
                    ["#include <stdlib.h>"
                    ,"#include <stdio.h>"
                    ,"#include <string.h>"
                    ,""
                    ,"char global_array[20];"
                    ,"void* getPtr() { return global_array; }"
                    ])
                (unlines
                    ["char c[7] = \"hello!\";"
                    ,""
                    ,"int main() {"
                    ,"  memset(global_array,0,sizeof(global_array));"
                    ,"  __initialize__c();"
                    ,"  if(strcmp(c,\"hello!\") != 0) {"
                    ,"    printf(\"expected c='hello!', but saw '%s'\",c);"
                    ,"    return -1;"
                    ,"  }"
                    ,"  if(strcmp(global_array+3,\"hello!\") != 0) {"
                    ,"    printf(\"expected global_array+3='hello!', but saw '%s'\",c);"
                    ,"    return -1;"
                    ,"  }"
                    ,"}"
                    ])
            -- @-node:gcross.20090711085032.34:static variable
            -- @-others
            ]
        -- @-node:gcross.20090711085032.29:processToplevelDeclaration
        -- @-others
        ]
    -- @-node:gcross.20090711085032.8:Initializer Construction
    -- @-others
    ]
-- @-node:gcross.20090709200011.12:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090709200011.8:@thin PrivatizationTests.hs
-- @-leo
