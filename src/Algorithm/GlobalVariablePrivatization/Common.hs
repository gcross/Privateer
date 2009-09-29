-- @+leo-ver=4-thin
-- @+node:gcross.20090520163423.12:@thin Common.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090520220305.37:<< Language Extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
-- @-node:gcross.20090520220305.37:<< Language Extensions >>
-- @nl

module Algorithm.GlobalVariablePrivatization.Common where

-- @<< Imports >>
-- @+node:gcross.20090520163423.13:<< Imports >>
import Control.Exception
import Data.Data
import Data.Generics
import Data.List
import Data.Maybe
import Data.Word
import Language.C
-- @-node:gcross.20090520163423.13:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090520220305.36:<< Types >>
type Offset = Word
type Size = Word
data Allocation = Allocation { allocationSize :: Size, allocationOffset :: Offset } deriving (Eq, Show)
-- @-node:gcross.20090520220305.36:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090506115644.15:Utilities
-- @+node:gcross.20090506115644.13:extractNestedBlocksFromStatement
extractNestedBlocksFromStatement :: CStat -> [[CBlockItem]]
extractNestedBlocksFromStatement stat =
    case stat of
        CLabel _ s _ _ -> recurse s
        CCase _ s _ -> recurse s
        CCases _ _ s _ -> recurse s
        CDefault s _ -> recurse s
        CExpr _ _ -> []
        CCompound _ items _ -> [items]
        CIf _ s Nothing _ -> recurse s
        CIf _ s1 (Just s2) _ -> concat . map recurse $ [s1,s2]
        CSwitch _ s _ -> recurse s
        CWhile _ s _ _ -> recurse s
        CFor _ _ _ s _ -> recurse s
        CGoto _ _ -> []
        CGotoPtr _ _ -> []
        CCont _ -> []
        CBreak _ -> []
        CReturn _ _ -> []
        CAsm _ _ -> []
  where
    recurse = extractNestedBlocksFromStatement
-- @-node:gcross.20090506115644.13:extractNestedBlocksFromStatement
-- @+node:gcross.20090517181648.12:extractBlockFromStatement
extractBlockFromStatement :: CStat -> Maybe [CBlockItem]
extractBlockFromStatement (CCompound _ items _) = Just items
extractBlockFromStatement _ = Nothing
-- @-node:gcross.20090517181648.12:extractBlockFromStatement
-- @+node:gcross.20090506115644.16:extractStorage
extractStorage :: [CDeclSpec] -> Maybe CStorageSpec
extractStorage decl_specs =
    find isStorageSpec decl_specs >>= (\(CStorageSpec storage_spec) -> Just storage_spec)
  where
    isStorageSpec (CStorageSpec _) = True
    isStorageSpec _ = False
-- @-node:gcross.20090506115644.16:extractStorage
-- @+node:gcross.20090709200011.61:extractNamesFromDeclarators
extractNamesFromDeclarators :: [(Maybe CDeclr,Maybe CInit,Maybe CExpr)] -> [String]
extractNamesFromDeclarators = map extractName
  where
    extractName (Just (CDeclr (Just ident) _ _ _ _),_,_) = identToString ident
-- @-node:gcross.20090709200011.61:extractNamesFromDeclarators
-- @+node:gcross.20090710174219.11:extractNamesFromDeclarations
extractNamesFromDeclarations :: [CDecl] -> [String]
extractNamesFromDeclarations declarations = declarations >>= extractNamesFromDeclarators . (\(CDecl _ declarators _) -> declarators)
-- @-node:gcross.20090710174219.11:extractNamesFromDeclarations
-- @+node:gcross.20090711085032.15:makeCompoundStmt
makeCompoundStmt :: [CBlockItem] -> CStat
makeCompoundStmt items = CCompound [] items internalNode
-- @-node:gcross.20090711085032.15:makeCompoundStmt
-- @+node:gcross.20090718130736.32:isStorageSpecifier
isStorageSpecifier (CStorageSpec _) = True
isStorageSpecifier _ = False
-- @nonl
-- @-node:gcross.20090718130736.32:isStorageSpecifier
-- @+node:gcross.20090718130736.33:removeStorageSpecifiers
removeStorageSpecifiersFrom = filter (not . isStorageSpecifier)
-- @-node:gcross.20090718130736.33:removeStorageSpecifiers
-- @-node:gcross.20090506115644.15:Utilities
-- @+node:gcross.20090523222635.16:Exceptions
-- @+node:gcross.20090523222635.17:ParseException
data ParseException = ParseException ParseError
     deriving (Show, Typeable)
instance Exception ParseException
-- @-node:gcross.20090523222635.17:ParseException
-- @-node:gcross.20090523222635.16:Exceptions
-- @-others
-- @-node:gcross.20090520163423.12:@thin Common.hs
-- @-leo
