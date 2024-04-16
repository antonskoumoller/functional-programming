// Exercise 5.1 
type 'a BinTree = 
     Leaf 
    |Node of  'a * 'a BinTree * 'a BinTree

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),Node(562, Leaf, Node(78, Leaf, Leaf)))


let rec inOrder tree = 
    match tree with
    | Leaf -> []
    | Node(n,tree1,tree2) -> inOrder tree1 @ [n] @ inOrder tree2;;

inOrder intBinTree

// Exercise 5.2
let rec mapInOrder f tree  = 
    match tree with
    | Leaf -> Leaf
    | Node (n, tree1, tree2) -> 
        let new_tree1 = mapInOrder f tree1 in
        let new_value = f n in
        let new_tree2 = mapInOrder f tree2 in
        Node (new_value, new_tree1, new_tree2);;

mapInOrder (fun x -> x + 1) intBinTree

// Exercise 5.3
let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

let rec foldInOrder f acc tree =
    match tree with
    | Leaf -> acc
    | Node(value, left, right) -> 
        let left_acc = foldInOrder f acc left in
        let node_acc = f value left_acc in
        foldInOrder f node_acc right

foldInOrder (fun n a -> a + n) 0.0 floatBinTree

// Exercise 5.4 

type aExp = (*Arithmetic expressions*)
    | N of int (* numbers *)
    | V of string (* variables *)
    | Add of aExp * aExp (* addition *)
    | Mul of aExp * aExp (* multiplication *)
    | Sub of aExp * aExp (* subtraction *)



let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s
 

type bExp = (* Boolean expressions *)
    | TT (* true *)
    | FF (* false *)
    | Eq of aExp * aExp (* equality *)
    | Lt of aExp * aExp (* less than *)
    | Neg of bExp (* negation *)
    | Con of bExp * bExp (* conjunction *)

let rec B b s = 
    match b with
    | TT            -> true
    | FF            -> false
    | Eq(a1, a2) -> A a1 s = A a2 s 
    | Lt(a1, a2) -> A a1 s < A a2 s
    | Neg(b)-> not (B b s)
    | Con(b1, b2) -> B b1 s && B b2 s


// Exercise 5.5
type stm = (* statements *)
    | Ass of string * aExp (* assignment *)
    | Skip
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else *)
    | While of bExp * stm (* while *)
    | IT of bExp * stm (* if then*)
    | RU of bExp * stm (* repeat until *)

let rec I statement s =
    let update x a s = Map.add x (A a s) s
    match statement with
    | Ass(x,a) -> update x a s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s)
    | ITE(b, stm1, stm2) -> if B b s then I stm1 s else I stm2 s
    | While(b,stm) -> if B b s then I (While(b,stm)) s else s
    | IT(b, stm) -> if B b s then I stm s else I Skip s
    | RU(b, stm) -> if not (B b s) then I stm s else I Skip s
    
let stmt = Ass("res",(Add(N 10, N 30)))
let state = Map.empty

I stmt state

// Exercise 5.6
(*  First we would add the Inc type to the arithmetic expressions. Then we would 
    add the Inc to the recursive interpreter function. Just incrementing the variable 
    and returning would not be enough. We would need to update the state and then return 
    the variable from there. *) 


// Example 1: Assign values to two variables and evaluate in sequence.
let stmt0 =  Ass("en",N 1)
let stmt1 =  Ass("to",N 2)
let statement = Seq(stmt0,stmt1)
let state0 = Map.empty
// This below is us understanding environments/states. Doing the same with a slightly different state
// made good sense in terms of understanding states.
let stmt00 =  Ass("en",N 5)
let stmt11 =  Ass("to",N 5)
let statementx = Seq(stmt00,stmt11)
let state1 = I statement state0
let state11 = I statementx state0
// Example 2: Add two variables
let stmt2 = Add(V "en", V "to")
// Using our two different states where we have "the same variables" "en" and "to".
A stmt2 state1
A stmt2 state11
// Example 3: While loop increment variable "en" until it is not less than 10.
let stmt3 = While(Lt(V "en", N 10), Ass("en", Add(V "en",N 1)))
I stmt3 state1
// Example 4: Use If-then-else and boolean. If "en" is less than "to", then assign 3 to "en" and if not
//              assign 1 to "en".
let stmt4 = ITE(Lt(V "en", V "to"), Ass("en", N 3), Ass("en", N 1))
let stat2 = I stmt4 state1
// Example 5: If variable "en" is not 1 then set it to 1, else Skip.
let stmt5 = ITE(Neg(Eq(N 1, V "en")),Ass("var",N 1),Skip)
let state5 = I stmt5 stat2