-- @+leo-ver=4-thin
-- @+node:gcross.20090708193517.2:@thin Privatization.hs
-- @@language Haskell

module Algorithm.GlobalVariablePrivatization.Privatization where

-- @<< Imports >>
-- @+node:gcross.20090709200011.2:<< Imports >>
import Language.C
-- @-node:gcross.20090709200011.2:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090708193517.3:makeAccessor
makeAccessor :: String -> Bool -> String -> String -> [CDerivedDeclr] -> Int -> CFunDef
makeAccessor module_data_access_function_name is_static variable_name variable_type_name variable_indirections module_data_offset =
    let typedef_spec = CTypeSpec (CTypeDef (internalIdent variable_type_name) internalNode)
        specification =   typedef_spec
                        : (CTypeQual (CInlineQual internalNode))
                        : if is_static then (CStorageSpec (CStatic internalNode):[]) else []
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
                                    (CCall (CVar (internalIdent module_data_access_function_name) internalNode) [] internalNode)
                                    (CConst (CIntConst (cInteger.toInteger $ module_data_offset) internalNode))
                                    internalNode
                cast_expr = CCast cast_decl pointer_expr internalNode
                return_stmt = CReturn (Just cast_expr) internalNode
            in CCompound [] [CBlockStmt return_stmt] internalNode
    in CFunDef specification declarator [] statement internalNode
-- @-node:gcross.20090708193517.3:makeAccessor
-- @-others
-- @-node:gcross.20090708193517.2:@thin Privatization.hs
-- @-leo
