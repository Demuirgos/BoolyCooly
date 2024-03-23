namespace SAT.Engine

[<AutoOpen>]
module Engine = 

    type Expression = 
        | And of Expression * Expression 
        | Or of Expression * Expression 
        | Not of Expression
        | Imply of Expression * Expression 
        | Equiv of Expression * Expression
        | Const of bool
        | Var of string

    let rec freeVariable e = 
        let (<?>) (l: 'a option * 'b option) (r:Lazy<'a option * 'b option>) = 
            match l with 
            | Some v , _ as result -> result 
            | _ -> r.Value 
        match e with 
        | Const value -> None, Some value
        | Var v -> Some  v, None
        | Not expr -> freeVariable expr
        | And(lhs, rhs) ->
            freeVariable lhs <?> (Lazy.Create(fun _ -> freeVariable rhs))
        | Or (lhs, rhs) ->
            freeVariable lhs <?> (Lazy.Create(fun _ -> freeVariable rhs))
        | Imply(lhs, rhs) ->
            freeVariable lhs <?> (Lazy.Create(fun _ -> freeVariable rhs))
        | Equiv(lhs, rhs) ->
            freeVariable lhs <?> (Lazy.Create(fun _ -> freeVariable rhs))


    let rec guessVariable var value e = 
        let guessVariable'  = guessVariable var value 
        match e with 
        | Const _ -> e
        | Var v -> 
            if v = var 
            then Const value 
            else e
        | Not expr -> Not(guessVariable' expr)
        | And(lhs, rhs) ->
            And(guessVariable' lhs, guessVariable' rhs)
        | Or (lhs, rhs) ->
            Or(guessVariable' lhs, guessVariable' rhs)
        | Imply(lhs, rhs) ->
            Imply(guessVariable' lhs, guessVariable' rhs)
        | Equiv(lhs, rhs) ->
            Equiv(guessVariable' lhs, guessVariable' rhs)

    let rec simplifyExpression e = 
        match e with 
        | Const expr -> Const expr
        | Var v -> Var v 
        | Not expr -> 
            match simplifyExpression expr with 
            | Const result -> Const(not result)
            | result -> Not result
        | Or(lhs, rhs) -> 
            let lSimplified = simplifyExpression lhs
            let rSimplified = simplifyExpression rhs
            match (lSimplified, rSimplified) with 
            | (Const true,  _) | (_, Const true) -> Const true
            | (expr, Const false) | (Const false, expr) -> expr
            | (lExpr, rExpr) -> Or(lExpr, rExpr)  
        | And(lhs, rhs) -> 
            let lSimplified = simplifyExpression lhs
            let rSimplified = simplifyExpression rhs
            match (lSimplified, rSimplified) with 
            | (Const false,  _) | (_, Const false) -> Const false
            | (expr, Const true) | (Const true, expr) -> expr
            | (lExpr, rExpr) -> And(lExpr, rExpr)  

    let rec solve e = 
        let resolve = (solve << simplifyExpression) 
        match freeVariable e with 
        | None, Some result -> result
        | Some(v), _ -> 
            let trueGuess = guessVariable v true e 
            let falseGuess = guessVariable v false e
            resolve trueGuess || resolve falseGuess
