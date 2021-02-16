concrete BabyL4L4 of BabyL4 = {
  lincat
    [Attr] = {s : Str ; isEmpty : IsEmpty} ;

  param
    IsEmpty = Empty | NonEmpty ;

  lin
    BoolType = ss "boolean" ;
    -- : String -> WNId -> [Attr] -> L4Type ;
    TypeDef name wnId attrs = ss
      ("Type" ++ name.s ++ wn wnId ++
         case attrs.isEmpty of {
           Empty => attrs.s ;
           NonEmpty => ": {" ++  attrs.s ++ "}"
         }) ;

    BaseAttr = ss "" ** {isEmpty = Empty} ;
    ConsAttr a as =
      let sep : Str = case as.isEmpty of {
            Empty => [] ;
            NonEmpty => ";"
            } ;
       in {s = a.s ++ sep ++ as.s ; isEmpty = NonEmpty} ;

    -- : String -> WNId -> BuiltinType -> Attr ; -- is_legal (23798) : boolean
    TypeAttr name wnId typ = ss (name.s ++ wn wnId ++ ":" ++ typ.s) ;

    -- : Ind -> Ind -> L4Obj ;
    ObjDef a b = ss (a.s ++ ":" ++ b.s) ;

    -- : String -> Ind -> Ind -> L4Prop ;
    BinOp f x y = ss (x.s ++ "." ++ f.s ++ "=" ++ y.s) ;

    -- : L4Prop -> L4Query ;
    Query prop = ss ("?" ++ prop.s) ;

    -- : Ind
    True = ss "true" ;
    False = ss "false" ;
    MegaCorp = ss "MegaCorp" ;

    -- : String -> WNId ;
    WN s = s ;

    -- The coercions

    sObj,
    sType,
    sProp,
    sQuery = id ;

  oper
    SS : Type = {s : Str} ;
    ss : Str -> SS = \s -> {s=s} ;
    wn : SS -> Str = \ss -> "(" ++ ss.s ++ ")" ;

    id : SS -> SS = \s -> s ;
}
