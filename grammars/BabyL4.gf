abstract BabyL4 = {
  cat
    S ;
    L4Type ;
    L4Obj ;
    L4Prop ;
    L4Query ;
    Attr ;
    [Attr]{0} ;

    Ind ;
    BuiltinType ;
    WNId ;

  fun
    BoolType : BuiltinType ; -- do we need the distinction?

    TypeDef : String -> WNId -> [Attr] -> L4Type ; -- Business (45612) { attr1 (WNid) : Type ; attrN : Type}
    TypeAttr : String -> WNId -> BuiltinType -> Attr ; -- is_legal (23798) : boolean

    ObjDef : Ind -> Ind -> L4Obj ;

    BinOp : String -> Ind -> Ind -> L4Prop ;
    Query : L4Prop -> L4Query ;

    True, False,
      MegaCorp : Ind ;

    WN : String -> WNId ;

    sObj : L4Obj -> S ;
    sType : L4Type -> S ;
    sProp : L4Prop -> S ;
    sQuery : L4Query -> S ;

}
