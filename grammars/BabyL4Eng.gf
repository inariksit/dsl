concrete BabyL4Eng of BabyL4 = open SyntaxEng, ParadigmsEng, SymbolicEng, WordNetEng, ExtendEng in {
  lincat
    S = Utt ;
    L4Type,
      L4Obj,
      L4Prop -- needs to become QS so need to keep open Question or not
      = {subj : NP ; pred : VPS} ;
    L4Query = QS ;

    -- Attr = VPS ;
    -- [Attr] = {s : ListVPS ; first : VPS ; size : Size} ;
    Attr = NP ;
    [Attr] = {s : ListNP ; first : NP ; size : Size} ;

    Ind = {s : NP ; whichInd : IndType} ;

  param
    Size = Empty | One | More ;
    IndType = IndTrue | IndFalse | IndMegaCorp ;

  lin
    BoolType = {s = "Boolean"} ;

    -- : String -> WNId -> [Attr] -> L4Type ;
    TypeDef name _wnId attrs = {
      subj = symb name ;
      pred = mkVPS' positivePol (mkVP (
         case attrs.size of {
           Empty => mkNP (mkPN "a class with no fields") ;
           One => mkNP (mkCN (mkN2 "a class with the field") attrs.first) ;
           More => mkNP (mkCN (mkN2 "a class with the fields") (mkNP and_Conj attrs.s))
        }))
      } ;

  oper
    emptyNP : NP = it_NP ** {s = \\_ => []} ;
    listVPS  : ListVPS = lin ListVPS {s1,s2 = \\_ => []} ;
    emptyVPS : VPS     = lin VPS {s = \\_ => []} ;
    mkVPS' : Pol -> VP -> VPS = MkVPS (mkTemp presentTense simultaneousAnt) ;

  lin
    -- BaseAttr = {s = listVPS ; first = emptyVPS ; size = Empty} ;
    -- ConsAttr a as =
    --   case as.size of {
    --     Empty => as ** {first = a ; size = One} ;
    --     One => as ** {s = BaseVPS a as.first ; size = More} ;
    --     More => as ** {s = ConsVPS a as.s}
    --   } ;
    BaseAttr = {s = mkListNP emptyNP emptyNP ; first = emptyNP ; size = Empty} ;
    ConsAttr a as =
      case as.size of {
        Empty => as ** {first = a ; size = One} ;
        One => as ** {s = mkListNP a as.first ; size = More} ;
        More => as ** {s = mkListNP a as.s}
      } ;


    -- : String -> WNId -> BuiltinType -> Attr ; -- is_legal (23798) : boolean
    TypeAttr name wnId typ = symb name ;


    -- : Ind -> Ind -> L4Obj ;
    ObjDef a b = {subj = a.s ; pred = mkVPS' positivePol (mkVP b.s)} ;

    -- : String -> Ind -> Ind -> L4Prop ;
    BinOp f x a =
      case a.whichInd of {
        IndTrue => {subj = x.s ; pred = mkVPS' positivePol (mkVP (symb f)) } ;
        _IndFalse => {subj = x.s ; pred = mkVPS' negativePol (mkVP (symb f)) }
        -- _ => {subj = mkNP (GenNP)}
        } ;


    -- : L4Prop -> L4Query ;
    Query prop = SQuestVPS prop.subj prop.pred ;

    -- : Ind
    True = {s = mkNP (mkPN "true") ; whichInd = IndTrue} ;
    False = {s = mkNP (mkPN "false") ; whichInd = IndFalse} ;
    MegaCorp = {s = mkNP (mkPN "MegaCorp") ; whichInd = IndMegaCorp} ;

    -- : String -> WNId ;
    WN s = s ;

    -- The coercions

    sObj,
    sType,
    sProp = \r -> mkUtt (PredVPS r.subj r.pred) ;
    sQuery = mkUtt  ;
}
