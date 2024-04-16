(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>

// When we encounter a leaf, return the value and sum it to previous calls accumulator
// recursively and increment
let rec countA t n = 
    match t with
    | Leaf -> n 
    | Node(tl,_,tr) -> countA tl (1 + (countA tr n))

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
let t2 = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Node(Leaf,2,Leaf),4,Leaf))

countA t 0

(* Assignment 8.2, HR 9.9 *)

//  The function increments the node count 'n' upon encountering any node (Leaf or Node). For Leaf nodes, the
//  continuation function 'c' is called with the current count 'n'. For Node types, the function first recursively
//  processes the left child, increasing the count, and constructs a new continuation function that will subsequently
//  call 'countAC' on the right child, and increment the count. 
//  The continuation function 'c' is applied when a leaf in the right tree is reached.
let rec countAC t n c = 
  match t with 
  | Leaf -> c n
  | Node(tl,_,tr) -> countAC tl (n+1) (fun con -> countAC tr con c)


(* Example *)
countAC t 0 id

(* Assignment 8.3, HR 9.10 *)
// Decrement counter every call and cons an element (1) to anonymous continuation
// function until the anonymous functions return an empty array
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))
// When k is big bigListK will call (fun res -> 1::k(res)) 
// recursively which will result in k-1 new closures of the function 
// which will result in the stack overflowing. 

bigListK 10 id



(* Assignment 8.4, HR 9.11 *)
// Not sure if these satisfy tail recursion?
let rec leftTree n =
    let rec buildTree value remaining subtree =
      match remaining with
      | 0 -> subtree
      | _ -> buildTree (value - 1) (remaining - 1) (Node(subtree, value, Leaf))
    match n with
    | 0 -> Leaf
    | _ -> buildTree n n Leaf

leftTree 0
leftTree 10
leftTree 360000

let rec rightTree n =
    let rec buildTree value remaining subtree =
      match remaining with
      | 0 -> subtree
      | _ -> buildTree (value - 1) (remaining - 1) (Node(Leaf, value, subtree))
    match n with
    | 0 -> Leaf
    | _ -> buildTree n n Leaf
(* Examples *)
rightTree 0
rightTree 1
rightTree 2
rightTree 360000

let rec count = function (* from page HR 214 *)
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c = (* from page HR 215 *)
  match t with
    Leaf -> c 0
  | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

(* Assignment 8.5, HR 11.1 *)
let nat = Seq.initInfinite (fun i-> i);;
// filter the natural numbers sequence.
let odd=Seq.filter(fun n -> n % 2 = 1) nat;;


(* Assignment 8.6, HR 11.2 *)

let rec fact = function 
    | 0->1
    | n when n > 0 -> n * fact(n-1);;
// Use fact function to compute the sequence.
let fac=Seq.initInfinite(fun i -> fact i);;
(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac

Seq.toList(Seq.take 10 fac) 