type tree = 
    | Node of tree * tree
    | Leaf of int

type Sexp =
    | Num of int
    | Error of string

let rec helperLeft(t : tree) : Sexp =
    match t with
        | Leaf n -> Num n
        | Node (lt, rt) ->
            let minL = helperLeft(lt)
            let minR = helperRight(rt)
            match (minL, minR) with
                | (Num l, Num r) ->
                    if l <= r then
                        minR
                    else Error "()"
                | _ -> Error "()"

and helperRight(t : tree) : Sexp =
    match t with
        | Leaf n -> Num n
        | Node (lt, rt) ->
            let minL = helperLeft(lt)
            let minR = helperRight(rt)
            match (minL, minR) with
                | (Num l, Num r) ->
                    if l <= r then
                        minL
                    else Error "()"
                | _ -> Error "()"

let rec getLeft(t : tree) : Sexp =
    match t with
        | Leaf n -> Num n
        | Node (lt, _) ->
            getLeft(lt)

let rec minOrder(t : tree) : Sexp = 
    match t with
        | Leaf n -> Num n
        | Node (lt, rt) ->
            let minL = helperLeft(lt)
            let minR = helperRight(rt)
            match (minL, minR) with
                | (Num l, Num r) -> 
                    if l <= r then
                        getLeft(t)
                    else Error "()"
                | _ -> Error "()"

let test1 = Node (Node (Leaf 2, Leaf 3), Leaf 6)
let test2 = Node (Node (Leaf 3, Leaf 2), Leaf 6)
let test3 = Node (Node (Leaf 1, Leaf 4), Leaf 3)

printfn "TestCorrect: %A" (minOrder test1)
printfn "TestError: %A" (minOrder test2)
printfn "TestError: %A" (minOrder test3)