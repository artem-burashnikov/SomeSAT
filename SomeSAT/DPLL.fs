module SomeSAT.DPLL

open Definitions

let neg =
    function
    | Positive l -> Negative l
    | Negative l -> Positive l

let propagate unitLiteral cnf =
    let negUnitLiteral = neg unitLiteral
    let unitClause = Set.empty.Add(unitLiteral)
    let negUnitClause = Set.empty.Add(negUnitLiteral)
    
    let folder =
        fun (state: Set<Set<_>>) (clause: Set<_>) ->
            // Eliminate true clauses.
            if (unitClause = clause) || (unitClause.IsSubsetOf clause) then
                state
            // Remove not(unitLiteral) from clauses.
            elif (negUnitClause.IsSubsetOf clause) then
                state.Add(clause.Remove negUnitLiteral)
            // Otherwise accumulate.
            else
                state.Add(clause)
                
    Set.fold folder Set.empty cnf
