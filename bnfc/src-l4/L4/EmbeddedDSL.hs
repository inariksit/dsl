{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module EmbeddedDSL where

import AbsL
import Control.Monad.Trans.Writer
import Data.String (IsString(..))
import Data.List.Split
import Data.Char (isUpper)

type CuteList a = Writer [a] ()

mkList :: a -> CuteList a
mkList = tell . pure

getList :: CuteList a -> [a]
getList = execWriter

program :: CuteList Rule -> Tops
program = Toplevel . map ToplevelsRule . getList

rule :: Integer -> String -> RuleBody -> CuteList Rule
rule n name inner = mkList $
   SimpleRule (RNumIDName n name) inner

potato :: Tops
potato =
  program do
    rule 2 "edible" $
      define [ ("Item" `Is` "EdiblePotato")
                         `And` "TastyFood"
          --- ^ Intentionally wrong placement of parentheses, to match the current parser
             ]
        $ When $ ("Item" `Is` "Potato")
           `And` ("Item" `Is` "Edible")
    rule 3 "isPotato" $
      define [ "Item" `Is` "Potato"
             ]
        $ When $ "Item.species"
            ~~ listOrE [ S "Solanum tuberosum", S "Mister Potatohead"]

-- This corresponds to

{-
RULE  2 edible
 DEFINE Item IS EdiblePotato
            AND TastyFood
   WHEN Item IS Potato
    AND Item IS Edible

RULE  3 isPotato
 DEFINE Item IS Potato
   WHEN Item.species ~ ["Solanum tuberosum" | "Mister Potatohead"]
-}

-- >>> potato
-- Toplevel [ToplevelsRule (Rule (RNumID 2 (OA_dots [ObjAttrElemIdent (Ident "edible")])) (RName OptLangStrings1) AsofNull Meta0 (RuleDeem (GUGiven GivenLimb0 UponLimb1) [DefLimb DefDefine [CComma (Op2E (BBool_And3 (Op2E (BRel_Is (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Item"))])) (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "EdiblePotato"))])))) (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "TastyFood"))]))))] WithLimb1 AsofNull] (WHW (WhenLimb1 (Op2E (BBool_And3 (Op2E (BRel_Is (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Item"))])) (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Potato"))])))) (Op2E (BRel_Is (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Item"))])) (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Edible"))]))))))) DNoHence WhereLimb0))),ToplevelsRule (Rule (RNumID 3 (OA_dots [ObjAttrElemIdent (Ident "isPotato")])) (RName OptLangStrings1) AsofNull Meta0 (RuleDeem (GUGiven GivenLimb0 UponLimb1) [DefLimb DefDefine [CComma (Op2E (BRel_Is (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Item"))])) (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Potato"))]))))] WithLimb1 AsofNull] (WHW (WhenLimb1 (Op2E (BCmp_Match1 (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent (UIdent "Item")),UnifyElemObjAttrElem (ObjAttrElemIdent (Ident "species"))])) (ListE (ListOr [ConstE (StringV "Solanum tuberosum")] (ConstE (StringV "Mister Potatohead"))))))) DNoHence WhereLimb0)))]

-- TODO: Make a real test out of this

pattern S :: String -> Exp
pattern S s = ConstE (StringV s)

listOrE :: [Exp] -> Exp
listOrE l = ListE (ListOr (init l) (last l))

-- pattern ListOrE l = ListE (ListOr l)

(~~) :: Exp -> Exp -> Exp
(~~) = Match

pattern Match a b = Op2E (BCmp_Match1 a b)

pattern When :: Exp -> WhenHenceWhere
pattern When c = WHW (WhenLimb1 c) DNoHence WhereLimb0

instance IsString Exp where
--   fromString = IdentE
  fromString = UnifyE . splitObjAttr


splitObjAttr :: String -> UnifyExpr
splitObjAttr s = UnifyExpr1 . map objAttrIdent $ splitOn "." s

objAttrIdent :: String -> UnifyElem
objAttrIdent s@(x:_) | isUpper x = AttrUIdent s
                     | otherwise = AttrIdent s
objAttrIdent "" = error "objAttrIdent: Empty string"

    --   define
    --     do
    --         _ `And` _
    --         "Item" `And` "EdiblePotato"
    --     _

pattern AttrUIdent s = UnifyElemObjAttrElem
                ( ObjAttrElemUIdent ( UIdent s ) )

pattern AttrIdent s = UnifyElemObjAttrElem
                ( ObjAttrElemIdent ( Ident s ) )

pattern IdentE s = UnifyE
        ( UnifyExpr1
            [ UnifyElemObjAttrElem
                ( ObjAttrElemUIdent ( UIdent s ) )
            ]
        )

pattern And :: Exp -> Exp -> Exp
pattern And a b = Op2E (BBool_And3 a b)

pattern Is :: Exp -> Exp -> Exp
pattern Is a b = Op2E (BRel_Is a b)

deemRule0 = SimpleDeem

-- define constrs cond = SimpleDeem _
define :: [Exp] -> WhenHenceWhere -> RuleBody
define constrs = SimpleDeem [Define (fmap CComma constrs) WithLimb1 AsofNull]
-- define :: CuteList Exp -> WhenHenceWhere -> RuleBody
-- define constrs = SimpleDeem [Define (fmap CComma  . getList $ constrs) WithLimb1 AsofNull]

pattern Define :: [ConstraintComma] -> WithLimb -> Asof -> DefineLimb
pattern Define aa bb cc = DefLimb DefDefine aa bb cc

pattern SimpleDeem :: [DefineLimb] -> WhenHenceWhere -> RuleBody
pattern SimpleDeem defLimbs whenHence = RuleDeem ( GUGiven GivenLimb0 UponLimb1) defLimbs whenHence

pattern RNumIDName :: Integer -> String -> RuleDef
pattern RNumIDName n name = RNumID n (RuleName name)

pattern SimpleRule :: RuleDef -> RuleBody -> Rule
pattern SimpleRule name inner =
   Rule name NoRuleName AsofNull Meta0 inner

pattern NoRuleName =  RName OptLangStrings1

pattern RuleName :: String -> ObjAttr
pattern RuleName name = OA_dots [ ObjAttrElemIdent ( Ident name ) ]
