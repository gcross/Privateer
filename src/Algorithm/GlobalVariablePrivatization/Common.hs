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
import Language.C
-- @-node:gcross.20090520163423.13:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090520220305.36:<< Types >>
-- @-node:gcross.20090520220305.36:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090506115644.15:Utilities
-- @+node:gcross.20090506115644.13:extractNestedBlocks
extractNestedBlocksFromStatement :: CStat -> [[CBlockItem]]
extractNestedBlocksFromStatement stat =
    catMaybes . map extractBlockFromStatement $
        case stat of
            CLabel _ s _ _ -> [s]
            CCase _ s _ -> [s]
            CCases _ _ s _ -> [s]
            CDefault s _ -> [s]
            CExpr _ _ -> []
            CCompound _ _ _ -> [stat]
            CIf _ s Nothing _ -> [s]
            CIf _ s1 (Just s2) _ -> [s1,s2]
            CSwitch _ s _ -> [s]
            CWhile _ s _ _ -> [s]
            CFor _ _ _ s _ -> [s]
            CGoto _ _ -> []
            CGotoPtr _ _ -> []
            CCont _ -> []
            CBreak _ -> []
            CReturn _ _ -> []
            CAsm _ _ -> []
-- @-node:gcross.20090506115644.13:extractNestedBlocks
-- @+node:gcross.20090506115644.16:extractStorage
extractStorage :: [CDeclSpec] -> Maybe CStorageSpec
extractStorage decl_specs =
    find isStorageSpec decl_specs >>= (\(CStorageSpec storage_spec) -> Just storage_spec)
  where
    isStorageSpec (CStorageSpec _) = True
    isStorageSpec _ = False
-- @-node:gcross.20090506115644.16:extractStorage
-- @+node:gcross.20090517181648.12:extractBlockFromStatement
extractBlockFromStatement :: CStat -> Maybe [CBlockItem]
extractBlockFromStatement (CCompound _ items _) = Just items
extractBlockFromStatement _ = Nothing
-- @-node:gcross.20090517181648.12:extractBlockFromStatement
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
