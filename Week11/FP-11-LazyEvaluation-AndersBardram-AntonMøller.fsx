// IT UNIVERSITY KSD FUNCTIONAL PROGRAMMING SPRING 2024

// Assignment 11
// Version 2.0 of 2024-04-15 nh@itu.dk
// Version 1.0 of 2020-03-19 sestoft@itu.dk

// These exercises concern computation expressions (or monads) in F#, and
// more precisely, the expression evaluator example.

// You should build on the lecture's example code, found in file

// evalCompExp.fsx

// found in the code repository, lecture 11.

// Exercise 1. Extend the expression language and monadic evaluators with
// single-argument functions such as ABS(e1) which evaluates e1 and
// produces its absolute value.  Do this by adding a new case Prim1 of
// string * expr to the expr datatype.  Create suitable variants of all
// the monadic evaluators; you should not have to change the monad
// definitions (OptionBuilder, SetBuilder, TraceBuilder) at all.
type expr =
    | CstI of int
    | Prim of string * expr * expr
    | Prim1 of string * expr

let ex = Prim("+", CstI 7, Prim("*", CstI 9, CstI 10))


let opEval op v1 v2 =
  match op with
    "+" -> v1 + v2
  | "*" -> v1 * v2
  | "/" -> v1 / v2

let opEval1 op v =
    match op with 
    | "ABS" -> abs v


let rec eval e =
  match e with
    | CstI i -> i
    | Prim(op, e1, e2) ->
        let v1 = eval e1
        let v2 = eval e2
        opEval op v1 v2
    | Prim1(op ,e) -> 
        let v = eval e
        opEval1 op v


let opEvalOpt op v1 v2 =
    match op with
    | "+" -> Some (v1 + v2)
    | "*" -> Some (v1 * v2)
    | "/" -> if v2 = 0 then None else Some (v1 / v2)

let opEvalOpt1 op v =
    match op with
    | "ABS" -> Some(abs v)


let rec evalOpt e =
    match e with
    | CstI i -> Some i
    | Prim(op, e1, e2) ->
        match evalOpt e1 with
        | None -> None
        | Some v1 ->
            match evalOpt e2 with
            | None -> None
            | Some v2 -> opEvalOpt op v1 v2
    | Prim1(op, e) -> 
        match evalOpt e with
        | None -> None
        | Some v -> opEvalOpt1 op v



let opEvalSet op v1 v2 =
    match op with
    | "+" -> Set [v1 + v2]
    | "*" -> Set [v1 * v2]
    | "/" -> if v2 = 0 then Set.empty else Set [v1 / v2]
    | "choose" -> Set [v1;v2]


// Work on https://learnit.itu.dk/mod/assign/view.php?id=193468 
// Hvad faen er det for en opgave du laver her?

// Der er assignment 10 der skal afleveres senere i dag
// Hahahhaa for fanden
// Væk med det her pis så


// Abstract out the action of ABS on its argument in new auxiliary
// functions opEvalOpt1, opEvalSet1 and opEvalTrace1 similar to the
// existing functions opEvalOpt, opEvalSet and opEvalTrace for
// two-argument primitives.  Try the new evaluators on eg these
// expressions:

// let expr10 = Prim1("ABS", Prim("+", CstI(7), Prim("*", CstI(-9), CstI(10))))
// let expr11 = Prim1("ABS", Prim("+", CstI(7), Prim("/", CstI(9), CstI(0))))
// let expr12 = Prim("+", CstI(7), Prim("choose", Prim1("ABS", CstI(-9)), CstI(10)))

// Exercise 2. Extend the expression language and the monadic evaluators
// with a three-argument function such as +(e1, e2, e3) that is basically
// two applications of "+", as in, +(+(e1,e2),e3).  Do this by adding a
// new constructor Prim3 of string * expr * expr * expr to the expr type.

// You may alternatively add a more general facility for functions with
// n>=1 arguments, such as SUM(e1, ..., en), adding a suitable
// constructor to the expr type.

// Implement evaluation of such three-argument (or multi-argument)
// constructs in the monadic evaluators.

// Exercise 3. Create a new family of evaluation functions
// optionTraceEval.  These evaluators should combine the effect of the
// original optional evaluator (optionEval) and the original tracing
// evaluator (traceEval).

// This can be done in several ways, for instance corresponding to (A)
// return type int trace option, for an evaluator that returns no trace
// if a computation fails; or (B) the result type int option trace, for
// an evaluator that returns a partial trace up until some computation
// (eg division by zero) fails.

// 3.1: Make both a standard explicit version of (A) and a monadic
// version.  You need to create a new monad OptionTraceABuilder, among
// other things.

// 3.2: Make both a standard explicit version of (B) and a monadic
// version.  You need to create a new monad OptionTraceBBuilder, among
// other things.