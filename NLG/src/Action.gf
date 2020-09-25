abstract Action = Term ** {
  cat
    Action ;           -- A complete action: "raise capital"
    Action_Dir ;       -- Action that is missing a direct object
    Action_Indir ;     -- Action that is missing an indirect object
    Action_Dir_Indir ; -- Action that is missing both

    -- Lists of actions
    [Action]{2} ;             -- sells stock to Acme and raises capital
    [Action_Dir]{2} ;         -- sells today and issues (stock) at fixed valuation
    [Action_Indir]{2} ;       -- sells widgets and issues stock (at fixed valuation)
    [Action_Dir_Indir]{2} ;   -- sells and issues (stock) (at fixed valuation)

  fun
    -- Complements
    AComplDir   : Action_Dir -> Term -> Action ;
    AComplIndir : Action_Indir -> Term -> Action ;
    ASlashDir   : Action_Dir_Indir -> Term -> Action_Indir ; -- sell stock (at fixed valuation)
    ASlashIndir : Action_Dir_Indir -> Term -> Action_Dir ;   -- sell (stock) at fixed valuation

    -- Valency changes
    ANoComplDir   : Action_Dir -> Action ;   -- refund _ -> "issue a refund" ; return _ -> "return the purchase"
    ANoComplIndir : Action_Indir -> Action ; -- same but for indirect object
    PursuantTo    : Action -> Action_Indir ;

    -- Negation of a whole Action: doesnt sell X / doesnt sell X and Y
    ANeg : Action -> Action ;

    -- Negation regarding the complements
    AComplNoneDir   : Action_Dir -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneIndir : Action_Indir -> [Term] -> Action ; -- sells (X) neither to B nor to B

    -- Conjunctions
    ConjAction : Conjunction -> [Action] -> Action ;
    ConjSlashDir : Conjunction -> [Action_Dir] -> Action_Dir ;
    ConjSlashIndir : Conjunction -> [Action_Indir] -> Action_Indir ;
    ConjSlashDirIndir : Conjunction -> [Action_Dir_Indir] -> Action_Dir_Indir ;

}