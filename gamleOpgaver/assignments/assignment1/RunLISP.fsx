
// REPL for LISP variant (v. 2.0)
// Compile with "source compile.sh"
// Run with "mono lisp.exe"

// Type in S-expression at the "> " prompt.
// It may span several lines -- a new prompt is shown for each line. 
// When (after an "Enter" is typed) a complete S-expression is found
// it is evaluated and the result shown after a "= ".
// If an error is found during parsing or evaluation,
// a message is displayed after a "! ".
// In both cases, the "> " prompt is shown again.
// Close session by pressing ^D (control-D) at the prompt

open Sexp;;

// Global environment (initially empty)

let mutable globalEnv = []

// look up variable in environment
// environment is represented as list of (name,value) pairs

let rec lookup env x =
  match env with
  | [] -> None
  | ((y,v) :: env1) -> if x=y then Some v else lookup env1 x

// update binding in environment, adding new if not bound
// only used for global environment

let rec update env x v =
  match env with
  | [] -> [(x,v)]
  | ((y,w) :: env1) ->
       if x=y then (x,v) :: env1 else (y,w) :: update env1 x v

// update global environment

let updateGlobal x v =
  globalEnv <- update globalEnv x v

// exception for LISP evaluation errors

exception Lerror of string

// specials do not evaluate (all) their arguments
let specials =  ["quote"; "lambda"; "define"; "save"; "load"]

// unary operators
let unops = ["number?"; "symbol?"]

// binary operators 
let binops = ["cons"; "apply"; "/"; "%"; "!="]

// variadic operators 
let varops = ["+"; "-"; "*"; "<"; "="; "<="; ">"; ">="]

let operators = unops @ binops @ varops

let predefined = specials @ operators

// evaluate Sexp to Sexp in local environment
// if error raise exception Lerror message

let rec eval s localEnv =
  match s with
  | Nil -> Nil
  | Num _ -> s // numbers evaluate to themselves
  | Symbol x -> // predefined or variable
      if List.contains x operators
      then s  // operators evaluate to themselves
      else
        match lookup (localEnv @ globalEnv) x with
        | Some v -> v
        | None -> raise (Lerror ("Undefined variable " + x))
  | Cons (Symbol "quote", Cons (v, Nil)) -> v
  | Cons (Symbol "lambda", rules) -> Closure (s, localEnv)
  | Closure _ -> s // closures evaluate to themselves
  | Cons (Symbol "define", Cons (Symbol x, Cons (e, Nil))) ->
      if List.contains x predefined then
        raise (Lerror (x + " can not be redefined"))
      else
        (updateGlobal x (eval e localEnv); Symbol x)
  | Cons (Symbol "cons", Cons (e1, Cons (e2, Nil))) ->
      Cons (eval e1 localEnv, eval e2 localEnv)
  | Cons (Symbol "save", Cons (Symbol f, Nil)) ->
      try
        let envText = // create definitions as text, one per line
              String.concat "\n"
                (List.map
                  (fun (x,v) ->
                     showSexp 
                       (makeList [Symbol "define"; Symbol x; quoteExp v]))
                   globalEnv)
        let outfile = System.IO.File.CreateText (f + ".le")
        outfile.Write envText; outfile.Close (); Nil
      with _ -> raise (Lerror ("Could not open file " + f + ".le"))
  | Cons (Symbol "load", Cons (Symbol f, Nil)) ->
      let infile = try new System.IO.StreamReader (f + ".le")
                   with _ -> raise (Lerror ("Could not open file " + f + ".le"))
      try repl infile ()
      with EndOfFile -> infile.Close ();
      Nil
  | Cons (e1, args) -> // function application
      applyFun (eval e1 localEnv, evalList args localEnv)

// apply function to arguments

and applyFun (fnc, pars) =
      match fnc with
      | Symbol x when (List.contains x unops) ->
          match pars with
          | Cons (v, Nil) -> applyUnop x v
          | _ -> raise (Lerror ("Wrong number of arguments to " + x))
      | Symbol x when (List.contains x binops) ->
          match pars with
          | Cons (v1, Cons (v2, Nil)) ->
                applyBinop x (v1, v2)
          | _ -> raise (Lerror ("Wrong number of arguments to " + x))
      | Symbol x when (List.contains x varops) ->
          applyVarop x pars
        // applying a closure
      | Closure (Cons (Symbol "lambda", rules), closureEnv) ->
          tryRules rules pars closureEnv
      | _ -> raise (Lerror (showSexp fnc + " can not be applied as a function"))

// evaluate argument list

and evalList es localEnv =
  match es with
  | Nil -> Nil
  | Cons (e1, es1) -> Cons (eval e1 localEnv, evalList es1 localEnv)
  | _ -> raise (Lerror ("arguments are not a list: " + showSexp es))

// apply unary operator (predicate)
and applyUnop x v =
  match (x, v) with
  | ("number?", _) -> v
  | ("symbol?", Symbol _) -> v
  | _ -> Nil  // test failed

// apply binary operator
and applyBinop x (v, w) =
  match (x, v, w) with
  | ("cons", _, _) -> Cons (v, w)
  | ("apply", _, _) -> applyFun (v, w)
  | ("/", Num m, Num n) -> if n <> 0 then Num (m / n) else Nil
  | ("%", Num m, Num n) -> if n <> 0 then Num (m % n) else Nil
  | ("!=", Num m, Num n) -> if m = n then Num m else Nil
  | _ -> Nil

// apply variadic operator
and applyVarop x vs =
  match (x, vs) with
  | ("+", Nil) -> Num 0
  | ("+", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                | Num m -> Num (n + m)
                                | _ -> Nil
  | ("-", Nil) -> Num 0
  | ("-", Cons (Num n, Nil)) -> Num (-n)
  | ("-", Cons (Num n, vs1)) -> match applyVarop "+" vs1 with
                                | Num m -> Num (n - m)
                                | _ -> Nil
  | ("*", Nil) -> Num 1
  | ("*", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                | Num m -> Num (n * m)
                                | _ -> Nil
  // comparisons return first element if arguments sorted, otherwise Nil
  | (("<" | "=" | "<=" | ">" | ">="), Cons (Num n, Nil)) -> Num n
  | ("<", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                | Num m when n < m -> Num n
                                | _ -> Nil
  | ("=", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                | Num m when n = m -> Num n
                                | _ -> Nil
  | ("<=", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                 | Num m when n <= m -> Num n
                                 | _ -> Nil
  | (">=", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                 | Num m when n >= m -> Num n
                                 | _ -> Nil
  | (">", Cons (Num n, vs1)) -> match applyVarop x vs1 with
                                 | Num m when n > m -> Num n
                                 | _ -> Nil
  | _ -> Nil

// tries rules of a lambda, choosing first matching rule (if any)

and tryRules rs args localEnv =
  match rs with
  | Cons (p, Cons (e, rs1)) ->
      match matchPattern p args with
      | Some env -> eval e (env @ localEnv)
      | None -> tryRules rs1 args localEnv
  | Nil -> raise (Lerror ("No patterns matched arguments " + showSexp args))
  | _ ->  raise (Lerror ("Malformed rules " + showSexp rs))

// match pattern to argument list
// returns Some environment if matches, None if not

and matchPattern p v =
  match (p, v) with
  | (Nil, Nil) -> Some []
  | (Num m, Num n) when m = n -> Some []
  | (Symbol x, w) ->
       if List.contains x specials
       then raise (Lerror (x + " can not be used as pattern"))
       else if List.contains x operators
       then (if Symbol x = w then Some [] else None)
       else Some [(x, w)]
  | (Cons (Symbol "quote", Cons (v, Nil)), w) ->
       if v = w then Some [] else None
  | (Cons (p1, p2), Cons (v1, v2)) ->
       match (matchPattern p1 v1, matchPattern p2 v2) with
       | (Some env1, Some env2) -> combine env1 env2 
       | _ -> None
  | _ -> None

// combine environments and check if repeated variables have same value

and combine env1 env2 =
  match env1 with
  | [] -> Some env2
  | (x,v) :: env3 ->
       match lookup env2 x with
       | Some w -> if v = w then combine env3 env2 else None
       | None -> combine env3 ((x,v) :: env2)

// creates constant expression from value
// used when saving global environment
and quoteExp v =
  match v with
  | Nil -> v
  | Num n -> v
  | Symbol x when List.contains x operators -> v
  | Cons (Symbol "quote", _) -> v
  | Cons (Symbol "lambda", _) -> v
  | Closure (w, []) -> quoteExp w
  | Closure (w, env) ->
      makeList
        (makeList [Symbol "lambda";
                   makeList (List.map (Symbol << fst) env);
                   quoteExp w]
         :: List.map (quoteExp << snd) env)         
  | _ -> makeList [Symbol "quote"; v]

// read-eval-print loop (REPL) for LISP variant
// See functionality at top of this file

and repl infile () =
  printf "> " ;
  match readFromStream infile "" with
  | Success (e, p) ->
     if p=0 then
       (try printf "= %s\n" (showSexp (eval e []))
        with Lerror mess -> printf "! %s \n" mess)
     else
       printf "! %s\n" "Input is not a single S-expression"
     ; repl infile ()
  | ErrorAt i ->
      printf "! %s \n" ("parse error at position " + string i);
      repl infile ()

// Start REPL

let infile = new System.IO.StreamReader (System.Console.OpenStandardInput ())
printf "PLD LISP version 2020\n";
try repl infile () with EndOfFile -> printf "\n"
