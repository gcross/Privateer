-- @+leo-ver=4-thin
-- @+node:gcross.20090209172438.8:@thin CommonTestUtils.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

module CommonTestUtils where

-- @<< Imports >>
-- @+node:gcross.20090209172438.9:<< Imports >>
import Prelude hiding (catch)

import Control.Exception (catch,evaluate,SomeException)
import Control.Monad
import Control.Monad.Reader
import Data.Generics
import qualified Data.List as List
import Data.IORef

import Language.C

import System.Directory
import System.IO.Unsafe

import Test.HUnit

import Algorithm.GlobalVariablePrivatization.Common
-- @-node:gcross.20090209172438.9:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090710174219.15:Parsers
-- @+node:gcross.20090413014846.20:parseTranslUnit
parseTranlUnit code =
    case execParser_ translUnitP (inputStreamFromString code) nopos of
         Right ast -> ast
         Left err -> (error.show) err

-- @-node:gcross.20090413014846.20:parseTranslUnit
-- @+node:gcross.20090201173206.31:parseDeclaration
parseDeclaration code =
    case execParser_ extDeclP (inputStreamFromString code) nopos of
         Right ast -> ast
         Left err -> (error.show) err

-- @-node:gcross.20090201173206.31:parseDeclaration
-- @+node:gcross.20090520220305.2:parseStatement
parseStatement code =
    case execParser_ statementP (inputStreamFromString code) nopos of
         Right ast -> ast
         Left err -> (error.show) err
-- @-node:gcross.20090520220305.2:parseStatement
-- @+node:gcross.20090709200011.32:parseExpression
parseExpression code =
    case execParser_ expressionP (inputStreamFromString code) nopos of
         Right ast -> ast
         Left err -> (error.show) err
-- @-node:gcross.20090709200011.32:parseExpression
-- @-node:gcross.20090710174219.15:Parsers
-- @+node:gcross.20090201173206.26:Utilities
-- @+node:gcross.20090209172438.10:assertDataEqual
assertDataEqual :: (Data a) => String -> a -> a -> Assertion
assertDataEqual preface expected actual =
  unless (actual `geq` expected) (assertFailure msg)
 where msg = (if List.null preface then "" else preface ++ "\n") ++
             "expected: " ++ (gshow expected) ++ "\n but got: " ++ (gshow actual)
-- @-node:gcross.20090209172438.10:assertDataEqual
-- @+node:gcross.20090201173206.19:convertToInternal
convertToInternal :: GenericT
convertToInternal = everywhere ((mkT convertToInternalNode) `extT` convertToInternalIdent)
    where
        convertToInternalNode :: NodeInfo -> NodeInfo
        convertToInternalNode _ = internalNode

        convertToInternalIdent :: Ident -> Ident
        convertToInternalIdent ident = internalIdent (identToString ident)
-- @-node:gcross.20090201173206.19:convertToInternal
-- @+node:gcross.20090203162448.10:parseDeclarationWithTypedefs
parseDeclarationWithTypedefs :: [String] -> String -> CExtDecl
parseDeclarationWithTypedefs identifiers code =
    case execParser extDeclP (inputStreamFromString code) nopos (map internalIdent identifiers) newNameSupply of
         Right (ast,_) -> ast
         Left err -> (error.show) err

-- @-node:gcross.20090203162448.10:parseDeclarationWithTypedefs
-- @+node:gcross.20090201173206.46:parseSpecsInDeclaration
parseSpecsInDeclaration code = 
    let (CDeclExt (CDecl specs _ _)) = (convertToInternal.parseDeclaration) code
    in specs
-- @-node:gcross.20090201173206.46:parseSpecsInDeclaration
-- @+node:gcross.20090201173206.27:prettify
prettyPrint :: Pretty a => P a -> String -> IO ()
prettyPrint parser s =
    let Right ast = execParser_ parser (inputStreamFromString s) nopos
    in (print . pretty) ast

prettyPrintDeclaration = prettyPrint extDeclP
-- @-node:gcross.20090201173206.27:prettify
-- @+node:gcross.20090204105634.26:parseBlock
parseBlock :: String -> [CBlockItem]
parseBlock code =
    case execParser_ statementP (inputStreamFromString code) nopos of
        Right (CCompound _ block _) -> block
        Left err -> (error.show) err
-- @-node:gcross.20090204105634.26:parseBlock
-- @+node:gcross.20090209172438.16:canBeDeeplyEvaluatedWithoutError
doesDeepEvaluationThrowError result = (evaluate $ everything (||) (\x -> x `seq` False) result) `catch` (\e -> let _ :: SomeException = e in return True)
-- @-node:gcross.20090209172438.16:canBeDeeplyEvaluatedWithoutError
-- @+node:gcross.20090709200011.14:deleteTemporaries
deleteTemporaries :: [FilePath] -> IO ()
deleteTemporaries = mapM_ (\filepath -> doesFileExist filepath >>= (flip when (removeFile filepath)))
-- @-node:gcross.20090709200011.14:deleteTemporaries
-- @-node:gcross.20090201173206.26:Utilities
-- @-others
-- @-node:gcross.20090209172438.8:@thin CommonTestUtils.hs
-- @-leo
