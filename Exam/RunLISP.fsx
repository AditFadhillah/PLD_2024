#r "Sexp.dll"

// REPL for LISP variant (v. 3.0)
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

exception Serror of Sexp

// specials do not evaluate (all) their arguments
let specials =
  ["quote"; "lambda"; "\\"; "if"; "define"; "save"; "load"; "let"; "raise"; "handle"]

// unary operators
let unops = ["number?"; "symbol?"]

// binary operators 
let binops = ["::"; "apply"; "/"; "%"; "!="]

// variadic operators 
let varops = ["+"; "-"; "*"; "<"; "="; "<="; ">"; ">="]

let operators = specials @ unops @ binops @ varops

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
         // quote returns its argument unevaluated
  | Cons (Symbol "\\", rules) -> Closure (s, localEnv)
         // a statically scoped function builds a closure
         // of the function and the local environment at its definition
  | Cons (Symbol "lambda", rules) -> s
        // multi-way conditional
  | Cons (Symbol "if", es) ->
       match es with
       | Nil -> Nil
       | Cons (e1, Nil) -> eval e1 localEnv
       | Cons (e1, Cons (e2, ess)) ->
           if eval e1 localEnv <> Nil
           then eval e2 localEnv
           else eval (Cons (Symbol "if", ess)) localEnv
       | _ -> raise (Lerror ("Malformed if-expression " + Sexp.showSexp s))
         // a dynamically scoped function just evaluates to itself.
  | Closure _ -> s // closures evaluate to themselves
  | Cons (Symbol "define", Cons (Symbol x, Cons (e, Nil))) ->
         // binds name in the global environment unless it is a predefined name
      if List.contains x predefined then
        raise (Lerror (x + " can not be redefined"))
      else
        (updateGlobal x (eval e localEnv); Symbol x)
  | Cons (Symbol "fun", Cons (Symbol x, rules)) ->
         // defines global function unless it is a predefined name
      if List.contains x predefined then
        raise (Lerror (x + " can not be redefined"))
      else
        (updateGlobal x (Closure (Cons (Symbol "\\", rules), localEnv));
         Symbol x)
  | Cons (Symbol "::", Cons (e1, Cons (e2, Nil))) -> // builds a pair
      Cons (eval e1 localEnv, eval e2 localEnv)
  | Cons (Symbol "save", Cons (Symbol f, Nil)) ->
         // saves global envirenment to file
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
         // loads global envirenment from file
      let infile = try new System.IO.StreamReader (f + ".le")
                   with _ -> raise (Lerror ("Could not open file " + f + ".le"))
      try repl infile ()
      with EndOfFile s ->
        (if skipWhite s 0 s.Length <> s.Length then
           printf "! %s\n" "File ended before expression was complete"
         ; infile.Close ())
      ; Nil

  | Cons (Symbol "let", Cons (e1, Nil)) -> eval e1 localEnv

  | Cons (Symbol "let", Cons (p, Cons (e1, rest))) ->
      let v = eval e1 localEnv
      (match matchPattern p v with
       | None -> raise (Lerror ("The pattern "
                                + showSexpIndent p 12 12
                                + " did not match the value "
                                + showSexpIndent v 30 30))
       | Some env -> eval (Cons (Symbol "let", rest)) (env @ localEnv))

  | Cons (Symbol "raise", Cons(sexp, Nil)) ->
      let err = eval sexp localEnv
      // modified raise, that now return the malformed argument
      raise (Serror err) 

  | Cons (Symbol "handle", Cons(e1, Cons(handler, Nil))) ->
      try
        eval e1 localEnv
      with
        // evaluate the handler function with e as its argument
        | Serror e -> eval (Cons(handler, Cons(e, Nil))) localEnv
        | _ -> eval (Cons(handler, Nil)) localEnv

  | Cons (e1, args) -> // function application
      applyFun (eval e1 localEnv, evalList args localEnv, localEnv)

  

// apply function to arguments

and applyFun (fnc, pars, localEnv) =
      match fnc with
      | Symbol x when (List.contains x unops) ->
          // apply unary operator to one argument
          match pars with
          | Cons (v, Nil) -> applyUnop x v
          | _ -> raise (Lerror ("Wrong number of arguments to " + x))
      | Symbol x when (List.contains x binops) ->
          // apply binary operator to two arguments
          match pars with
          | Cons (v1, Cons (v2, Nil)) ->
                applyBinop x (v1, v2, localEnv)
          | _ -> raise (Lerror ("Wrong number of arguments to " + x))
      | Symbol x when (List.contains x varops) ->
          // apply variable-arity operator to all arguments
          applyVarop x pars
      | Closure (Cons (Symbol "\\", rules), closureEnv) ->
        // apply a closure to all arguments,
        // using the environment from closure
          try
            tryRules rules pars closureEnv
          with Lerror message ->
            raise (Lerror (message + "\n! when applying "
                           + showSexpIndent (Cons (Symbol "\\", rules))
                                            16 16
                           + "\n! to " + showSexpIndent pars 5 5))
      | Cons (Symbol "lambda", rules) ->
        // apply a dynamically scoped function to all arguments,
        // using the current environment
          try
            tryRules rules pars localEnv
          with Lerror message ->
            raise (Lerror (message + "\n! when applying "
                           + showSexpIndent fnc 16 16 + "\n! to "
                           + showSexpIndent pars 5 5))
      
        // not a function or operator -- report error
      | _ -> raise (Lerror (showSexp fnc + " can not be applied as a function"))

// evaluate argument list

and evalList es localEnv =
  match es with
  | Nil -> Nil
  | Cons (e1, es1) -> Cons (eval e1 localEnv, evalList es1 localEnv)
  | _ -> raise (Lerror ("arguments are not a list: "
                        + showSexpIndent es 25 25))

// apply unary operator (predicate)
and applyUnop x v =
  match (x, v) with
  | ("number?", Num _) -> v
  | ("symbol?", Symbol _) -> v
  | _ -> Nil  // test failed

// apply binary operator
and applyBinop x (v, w, localEnv) =
  match (x, v, w) with
  | ("::", _, _) -> Cons (v, w)
  | ("apply", _, _) -> applyFun (v, w, localEnv)
  | ("/", Num m, Num n) -> if n <> 0 then Num (m / n) else Nil
  | ("%", Num m, Num n) -> if n <> 0 then Num (m % n) else Nil
  | ("!=", Num m, Num n) -> if m <> n then Num m else Nil
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

// tries rules of a / or lambda, choosing first matching rule (if any)

and tryRules rs args localEnv =
  match rs with
  | Cons (p, Cons (e, rs1)) ->
      match matchPattern p args with
      | Some env -> eval e (env @ localEnv)
      | None -> tryRules rs1 args localEnv
  | Nil -> raise (Lerror ("No patterns matched arguments "
                          + showSexpIndent args 29 29))
  | _ ->  raise (Lerror ("Malformed rules " + showSexpIndent rs 16 16))

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
  | Cons (Symbol "\\", _) -> v
  | Cons (Symbol "lambda", _) -> v
  | Closure (w, []) -> quoteExp w
  | Closure (w, env) ->
      makeList
        (makeList [Symbol "\\";
                   makeList (List.map (Symbol << fst) env);
                   quoteExp w]
         :: List.map (quoteExp << snd) env)         
  | _ -> makeList [Symbol "quote"; v]

// read-eval-print loop (REPL) for LISP variant
// See functionality at top of this file

and repl infile () =
  printf "> " ;
  match readFromStream infile " " with
  | Success (e, p) ->
     if p=0 then
       (try printf "= %s\n" (showSexpIndent (eval e []) 2 2)
        with
        | Lerror message -> printf "! %s \n" message
        | Serror sexp -> printf "! %s \n" (showSexp sexp)
        )
     else
       printf "! %s\n" "Input is not a single S-expression"
     ; repl infile ()
  | ErrorAt i ->
      printf "! %s \n" ("parse error at position " + string i);
      repl infile ()

// Start REPL

let infile = new System.IO.StreamReader (System.Console.OpenStandardInput ())
printf "PLD LISP version 3.0\n";
try repl infile () with EndOfFile s -> printf "\n"
