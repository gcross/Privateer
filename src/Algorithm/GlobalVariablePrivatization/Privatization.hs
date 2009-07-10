-- @+leo-ver=4-thin
-- @+node:gcross.20090708193517.2:@thin Privatization.hs
-- @@language Haskell

module Algorithm.GlobalVariablePrivatization.Privatization where

-- @<< Imports >>
-- @+node:gcross.20090709200011.2:<< Imports >>
import Data.Generics
import Data.Set (Set)
import qualified Data.Set as Set
import Language.C
-- @-node:gcross.20090709200011.2:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090709200011.22:Constants
-- @+node:gcross.20090709200011.18:static_specifier
static_specifier = CStorageSpec (CStatic internalNode)
-- @-node:gcross.20090709200011.18:static_specifier
-- @+node:gcross.20090709200011.23:void_specifier
void_specifier = CTypeSpec (CVoidType internalNode)
-- @-node:gcross.20090709200011.23:void_specifier
-- @-node:gcross.20090709200011.22:Constants
-- @+node:gcross.20090709200011.17:Utilities
-- @+node:gcross.20090709200011.19:makeTypedefSpecifier
makeTypedefSpecifier variable_type_name = CTypeSpec (CTypeDef (internalIdent variable_type_name) internalNode)
-- @-node:gcross.20090709200011.19:makeTypedefSpecifier
-- @+node:gcross.20090709200011.21:makeVariableExpr
makeVariableExpr variable_name = CVar (internalIdent variable_name) internalNode
-- @-node:gcross.20090709200011.21:makeVariableExpr
-- @+node:gcross.20090709200011.25:makeIntegerExpr
makeIntegerExpr value = CConst (CIntConst (cInteger.toInteger $ value) internalNode)
-- @-node:gcross.20090709200011.25:makeIntegerExpr
-- @+node:gcross.20090709200011.26:makeFloatExpr
makeFloatExpr value = CConst (CFloatConst (cFloat value) internalNode)
-- @-node:gcross.20090709200011.26:makeFloatExpr
-- @+node:gcross.20090709200011.27:makeStringExpr
makeStringExpr value = CConst (CStrConst (cString value) internalNode)
-- @-node:gcross.20090709200011.27:makeStringExpr
-- @-node:gcross.20090709200011.17:Utilities
-- @+node:gcross.20090709200011.20:Processing
-- @+node:gcross.20090708193517.3:makeAccessor
makeAccessor :: String -> Bool -> String -> String -> [CDerivedDeclr] -> Int -> CFunDef
makeAccessor module_data_access_function_name is_static variable_name variable_type_name variable_indirections module_data_offset =
    let typedef_spec = makeTypedefSpecifier variable_type_name
        specification =   typedef_spec
                        : (CTypeQual (CInlineQual internalNode))
                        : if is_static then static_specifier:[] else []
        declarator =
            let ident = internalIdent ("__access__" ++ variable_name)
                indirections =    CFunDeclr (Left []) [] internalNode
                                : CPtrDeclr [] internalNode
                                : variable_indirections
            in CDeclr (Just ident) indirections Nothing [] internalNode
        statement =
            let cast_declr = CDeclr Nothing (CPtrDeclr [] internalNode:variable_indirections) Nothing [] internalNode
                cast_decl = CDecl
                                [typedef_spec]
                                [(Just cast_declr,Nothing,Nothing)]
                                internalNode
                pointer_expr = CBinary CAddOp
                                    (CCall (makeVariableExpr module_data_access_function_name) [] internalNode)
                                    (makeIntegerExpr module_data_offset)
                                    internalNode
                cast_expr = CCast cast_decl pointer_expr internalNode
                return_stmt = CReturn (Just cast_expr) internalNode
            in CCompound [] [CBlockStmt return_stmt] internalNode
    in CFunDef specification declarator [] statement internalNode
-- @-node:gcross.20090708193517.3:makeAccessor
-- @+node:gcross.20090709200011.16:makeInitializer
makeInitializer :: String -> String -> [CDerivedDeclr] -> Maybe CInit -> CFunDef
makeInitializer variable_name variable_type_name variable_indirections maybe_init =
    let specifiers = [static_specifier,void_specifier]
        declarator =
            let maybe_ident = (Just . internalIdent . ("__initialize__" ++) $ variable_name)
                indirections = [CFunDeclr (Left []) [] internalNode]
            in CDeclr maybe_ident indirections Nothing [] internalNode
        block_items =
            case maybe_init of
                Nothing -> []
                Just init ->
                    let declare_default_value = 
                            let specifiers = [static_specifier, makeTypedefSpecifier variable_type_name]
                                declarator = CDeclr (Just . internalIdent $ "__initial__value__") variable_indirections Nothing [] internalNode
                            in CDecl specifiers [(Just declarator ,maybe_init,Nothing)] internalNode
                        copy_into_variable = flip CExpr internalNode . Just $
                            CCall
                                (makeVariableExpr "memcpy")
                                [   CCall (makeVariableExpr $ "__access__" ++ variable_name) [] internalNode
                                ,   CUnary CAdrOp (makeVariableExpr "__initial__value__") internalNode
                                ,   CSizeofExpr (makeVariableExpr "__initial__value__") internalNode
                                ]
                                internalNode
                    in  [CBlockDecl declare_default_value
                        ,CBlockStmt copy_into_variable
                        ]
        statement = CCompound [] block_items internalNode
    in CFunDef specifiers declarator [] statement internalNode
-- @-node:gcross.20090709200011.16:makeInitializer
-- @+node:gcross.20090709200011.31:privatizeExpr
privatizeExpr :: (Set String,Set String) -> CExpr -> CExpr
privatizeExpr (global_variables,local_static_variables) var@(CVar ident _)
    |   Set.member name local_static_variables
            = CUnary CIndOp var internalNode
    |   Set.member name global_variables
            = CUnary CIndOp (CCall (makeVariableExpr $ "__access__" ++ name) [] internalNode) internalNode
    |   otherwise
            = var
  where
    name = identToString ident

privatizeExpr special_variables expr = gmapT (mkT (privatizeExpr special_variables)) expr
-- @-node:gcross.20090709200011.31:privatizeExpr
-- @-node:gcross.20090709200011.20:Processing
-- @-others
-- @-node:gcross.20090708193517.2:@thin Privatization.hs
-- @-leo
