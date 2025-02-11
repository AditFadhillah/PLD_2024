// Sexp.fs: S-expressions for LISP variant

module Sexp

// Type for S-expressions

type Sexp = Symbol of string
          | Nil
          | Num of int
          | Cons of Sexp * Sexp
          | Closure of Sexp * (string * Sexp) list // expression + environment

// creates an Sexp list from an F# list of Sexps

let makeList l = List.foldBack (fun a b -> Cons (a,b)) l Nil

// Characters allowed in symbols and numbers
// Any sequence of such that matches an F# number string is a number
// Otherwise it is a symbol

let symbolChars
  = set (['a'..'z'] @ ['A'..'Z'] @ ['0'..'9'] @
         [for c in "æøåöäÆØÅÖÄ+-*/%=<>?!#,:{}\\&@~_$" -> c])

// whitespace characters

let whitespace = [for c in " \b\n\r\t" -> c]

let spaces n =
  "                                                        ".[0..(n-1)]

// Convert Sexp to string

let rec showSexpIndent s ind curr =
  match s with
  | Symbol name -> name
  | Num n -> string n
  | Nil  -> "()"
  | Cons (Symbol "quote", Cons (s, Nil)) ->
      "'" + showSexpIndent s (ind+1) (curr+1)
  | Cons (a,d) ->
      let str1 = showSexpIndent a (ind+1) (curr+1)
      let ll1 = String.length str1
      let str2 = showTail d ind (curr+ll1)
      let ll2 = String.length str2
      "(" + str1 + str2
  | Closure _ -> "λ"  // closures are opaque

and showTail d ind curr =
  if curr > 70 && d <> Nil then
    "\n" + spaces ind + showTail d ind ind
  else
    match d with
    | Symbol name -> " . " + name + ")"
    | Num n -> " . " + string n + ")"
    | Nil  -> ")"
    | Cons (Symbol "quote", Cons (s, Nil)) ->
      " . '" + showSexpIndent s ind (curr+4) + ")"
    | Cons (a,d1) ->
        let str1 =  showSexpIndent a ind (curr+1)
        let ll1 = String.length str1
        let str2 = showTail d1 ind (curr+ll1+1)
        let ll2 = String.length str2
        " " + str1 + str2
    | Closure _ -> "λ)"  // closures are opague

let showSexp s = showSexpIndent s 0 0

// Type for success or failure for parsing

type ParseResult = Success of Sexp * int // expression and position
                 | ErrorAt of int // position


// Read Sexp from string
// Return Success (s, 0) if input matches an Sexp s with no "junk" afterwards
// Return Success (s, i) if prefix of input matches,
// but there is "junk" at position i
// Return ErrorAt i if a parse error is found at position i

let rec readSexp cs =
  let len = String.length cs
  match readExp cs 0 len with
  | Success (s, i) ->
      let j = skipWhite cs i len
      if j = len then Success (s, 0) else Success (s, j)
  | ErrorAt i -> ErrorAt i

// skip whitespace

and skipWhite cs i len  =
  if i < len && (List.contains cs.[i] whitespace)
  then skipWhite cs (i+1) len
  else if i < len && cs.[i] = ';' // comment
  then skipUntilLineBreak cs (i+1) len
  else i

and skipUntilLineBreak cs i len =
  if i = len then i
  else if cs.[i] <> '\n'
  then skipUntilLineBreak cs (i+1) len
  else skipWhite cs (i+1) len

// read Sexp from position i in string
// and return either ErrorAt j (if string didn't match at position j) or
// Success (s, j) where s is the read Sexp and j is position after read Sexp

and readExp cs i len =
  if i = len then ErrorAt i
  else
    let c = cs.[i]
    if Set.contains c symbolChars then // read symbol
      readSymbol (string c) cs (i+1) len
    else if c = '(' then // this starts a list or Nil, read the rest
      readTail cs (i+1) len
    else if c = '\'' then // read quoted expression
      match readExp cs (i+1) len with
      | Success (s, j) -> Success (makeList [Symbol "quote"; s], j)
      | other -> other
    else if List.contains c whitespace  || c = '\\' then // skip whitespace
      readExp cs (skipWhite cs i len) len
    else ErrorAt i

// read symbol
// add characters to name (already read characters)
// stop when end of string or non-letter reached
// convert to number if legal number-string

and readSymbol name cs i len =
  // makeSymbol tries to convert name to an integer,
  // and if this fails, makes it  a symbol.
  // This makes all F# integer notation (including hexadecimal) legal
  let makeSymbol n = try Num (int name) with _ -> Symbol name
  if i = len then
    Success (makeSymbol name, i)
  else
    let c = cs.[i]
    if Set.contains c symbolChars
    then readSymbol (name + string c) cs (i+1) len
    else Success (makeSymbol name, i)

// read list tail
// handles shorthand (a1 a2 ... an) = (a1 . (a2 . ... an))

and readTail cs i len  =
  if i = len then ErrorAt i
  else
    let c = cs.[i]
    if c = ')' then Success (Nil, i+1)  // end of list
    else if Set.contains c symbolChars then  // read symbol and then tail
      match readSymbol (string c) cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (sym, j) ->
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) -> Success (Cons (sym, list), j)
    else if c = '(' then // read Sexp and then tail
      match readTail cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (s, j) ->
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) -> Success (Cons (s, list), j)
    else if c = '\'' then // read 'Sexp and then tail
      match readExp cs (i+1) len with
      | Success (s, j) -> 
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) ->
             Success (Cons (makeList [Symbol "quote"; s], list), j)
      | other -> other
    else if c = '.' then // read Sexp and then ")"
      match readExp cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (s, j) ->
         match readClose cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (_, j) -> Success (s, j)
    else if List.contains c whitespace  || c = '\\' then // skip whitespace
      readTail cs (skipWhite cs i len) len
    else ErrorAt i

// read close parenthesis (possibly preceeded by whitespace) 

and readClose cs i len  =
  if i = len then ErrorAt i
  else
    let c = cs.[i]
    if c = ')' then Success (Nil, i+1)
    else if List.contains c whitespace  || c = '\\' then // skip whitespace
      readClose cs (skipWhite cs i len) len
    else ErrorAt i

// read single Sexp from stream
// the Sexp can span multiple lines,
// but there can not be any non-whitespace characters after the Sexp

exception EndOfFile of string

let rec readFromStream (stream : System.IO.StreamReader) prefix =
  let input = stream.ReadLine()
  if input = null then raise (EndOfFile prefix)
  let s = prefix + input
  let len = String.length s
  match readSexp s with
  | Success (e, p) -> Success (e, p)
  | ErrorAt i ->
      if i=len then readFromStream stream (s+"\n") else ErrorAt i