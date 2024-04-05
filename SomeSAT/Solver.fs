module SomeSAT.Solver

let propagate unitLiteral cnf =
    let negUnitLiteral = -unitLiteral
    
    let folder =
        fun (state: Set<Set<_>>) (clause: Set<_>) ->
            // Eliminate true clauses.
            if clause.Contains(unitLiteral) then
                state
            // Remove not(unitLiteral) from clauses.
            elif clause.Contains(negUnitLiteral) then
                state.Add(clause.Remove negUnitLiteral)
            // Otherwise accumulate.
            else
                state.Add(clause)
                
    Set.fold folder Set.empty cnf
