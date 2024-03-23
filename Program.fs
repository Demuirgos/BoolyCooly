open SAT.Engine

[<EntryPoint>]
let main _ = 
    printfn "%A" (solve (And((Var "x"), (Not (Var "x")))))
    printfn "%A" (solve (Or((Var "x"), (Not (Var "x")))))
    0