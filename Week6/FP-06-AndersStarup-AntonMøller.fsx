// 6.1 (HR 6.2)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;


let rec fexprToString expr = 
    match expr with 
    | Const n -> string n
    | X -> "x"
    | Add (expr1,expr2) -> fexprToString expr1 + fexprToString expr2 + "+"
    | Sub (expr1,expr2) -> fexprToString expr1 + fexprToString expr2 + "-"
    | Mul (expr1,expr2) -> fexprToString expr1 + fexprToString expr2 + "*"
    | Div (expr1,expr2) -> fexprToString expr1 + fexprToString expr2 + "/"
    | Sin (expr) -> fexprToString expr + "Sin"
    | Cos (expr) -> fexprToString expr + "Cos"
    | Log (expr) -> fexprToString expr + "Log"
    | Exp (expr) -> fexprToString expr + "Exp"    
     
// 6.2 (HR 6.8)

type Stack = S of float list

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float


let intpInstr s i = 
    match (s,i) with 
    | (s''::s'::s,ADD) -> (s' + s'')::s
    | (s''::s'::s,SUB) -> (s' - s'')::s
    | (s''::s'::s,MULT) -> (s' * s'')::s
    | (s''::s'::s,DIV) -> (s' / s'')::s
    | (s'::s,SIN) -> (sin s')::s
    | (s'::s,COS) -> (cos s')::s
    | (s'::s,LOG) -> (log s')::s
    | (s'::s,EXP) -> (exp s')::s
    | (_,PUSH x) -> x::s
    | _ -> failwith "Somethings is wrong, maybe empty stack I don't know"



let intpProg i = 
    let rec intpProgRec s i = 
        match i with
        | [] -> s
        | instr::i -> intpProgRec (intpInstr s instr) i 
    match intpProgRec [] i with 
    | [] -> failwith "oh no"
    | s'::s -> s'


let rec trans (expr,n) = 
    match expr with
    | Const c -> [PUSH c]
    | X -> [PUSH n]
    | Add(x1,x2) -> trans(x1,n) @ trans(x2,n) @ [ADD]
    | Sub (x1,x2) -> trans(x1,n) @ trans(x2,n) @ [SUB]
    | Mul (x1,x2) -> trans(x1,n) @ trans(x2,n) @ [MULT]
    | Div (x1,x2) -> trans(x1,n) @ trans(x2,n) @ [DIV]
    | Sin (x) -> trans(x,n) @ [SIN]
    | Cos (x) -> trans(x,n) @ [COS]
    | Log (x) -> trans(x,n) @ [LOG]
    | Exp (x) -> trans(x,n) @ [EXP]

// 6.3 (HR 7.2)

// 3.5
// type ComplexNumber = {real:int; im:int}


module Complex
type Complex
val ( make ) : float -> float -> Complex
val ( -. ) : Complex -> Complex 
val ( +. ) : Complex -> Complex -> Complex
val ( /. ) : Complex -> Complex -> Complex
val ( *. ) : Complex -> Complex -> Complex
val ( invAdd ) : Complex -> Complex
val ( invMult ) : Complex -> Complex


type Complex = C of float * float
let ( make ) (C(real,im)) = C(real,im)
let ( invAdd ) ((C(real,im))) = C(-real,-im)
let ( invMult ) ((C(real,im))) = C(real/(real*real+im*im),(-im)/im*im+real*real)
let ( +. ) (C(re1,im1)) (C(re2,im2)) = C(re1+re2,im1+im2)
let ( -. ) (C(re1,im1)) (C(re2,im2)) = C(re1,im1) +. invAdd C(re2,im2) 
let ( *. ) (C(re1,im1)) (C(re2,im2)) = C(re1*re2-im1*im2,im1*re2+re1*im2)
let ( /. ) (C(re1,im1)) (C(re2,im2)) = C(re1,im1) *. invMult C(re2,im2)
