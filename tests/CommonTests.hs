-- @+leo-ver=4-thin
-- @+node:gcross.20090520163423.2:@thin CommonTests.hs
-- @@language haskell

module CommonTests(tests) where

-- @<< Imports >>
-- @+node:gcross.20090520163423.3:<< Imports >>
import Language.C
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.PrettyPrint

import Algorithm.GlobalVariablePrivatization.Common
import CommonTestUtils
-- @-node:gcross.20090520163423.3:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090520163423.4:Tests
tests = [   testGroup "convertToInternal"
            [   testCase "simple" test_convertToInternal_simple
            ,   testCase "indirections" test_convertToInternal_indirections
            ]
        ,   testGroup "extractStorage"
            [   testCase "empty" test_extractStorage_empty
            ,   testCase "complicated" test_extractStorage_complicated
            ,   testCase "complicated, no storage" test_extractStorage_complicated_nostorage
            ]
        ]
-- @+node:gcross.20090520163423.5:extractStorage
-- @+node:gcross.20090520163423.6:empty
test_extractStorage_empty =
    let result = extractStorage []
    in assertDataEqual "make sure empty results in Nothing" Nothing result
-- @-node:gcross.20090520163423.6:empty
-- @+node:gcross.20090520163423.7:complicated
test_extractStorage_complicated =
    let code = "int static const x;"
        (CDeclExt (CDecl specs _ _)) = parseDeclaration code
        maybe_storage = extractStorage specs
    in do
        case maybe_storage of
            Nothing -> assertFailure "storage not found!"
            Just storage ->
                assertEqual "storage is static" "static" ((render.pretty) storage)
-- @-node:gcross.20090520163423.7:complicated
-- @+node:gcross.20090520163423.8:complicated_nostorage
test_extractStorage_complicated_nostorage =
    let code = "int const long volatile x;"
        (CDeclExt (CDecl specs _ _)) = parseDeclaration code
        maybe_storage = extractStorage specs
    in do
        case maybe_storage of
            Nothing -> return ()
            Just _ ->
                assertFailure "found non-existent storage!"
-- @-node:gcross.20090520163423.8:complicated_nostorage
-- @-node:gcross.20090520163423.5:extractStorage
-- @+node:gcross.20090520163423.9:convertToInternal
-- @+node:gcross.20090520163423.10:simple
test_convertToInternal_simple =
    let code = "int x;"
        Right raw_declaration = execParser_ extDeclP (inputStreamFromString code) nopos
        declaration = convertToInternal raw_declaration
    in do
        assertDataEqual "declaration has expected structure" (CDeclExt (CDecl [CTypeSpec (CIntType internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode)) declaration
-- @nonl
-- @-node:gcross.20090520163423.10:simple
-- @+node:gcross.20090520163423.11:indirections
test_convertToInternal_indirections =
    let code = "int (*const x)();"
        Right raw_declaration = execParser_ extDeclP (inputStreamFromString code) nopos
        declaration = convertToInternal raw_declaration
    in do
        assertDataEqual "declaration has expected structure" (CDeclExt (CDecl [CTypeSpec (CIntType internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [CPtrDeclr [CConstQual internalNode] internalNode, CFunDeclr (Right ([],False)) [] internalNode] Nothing [] internalNode),Nothing,Nothing)] internalNode)) declaration

-- @-node:gcross.20090520163423.11:indirections
-- @-node:gcross.20090520163423.9:convertToInternal
-- @-node:gcross.20090520163423.4:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090520163423.2:@thin CommonTests.hs
-- @-leo
