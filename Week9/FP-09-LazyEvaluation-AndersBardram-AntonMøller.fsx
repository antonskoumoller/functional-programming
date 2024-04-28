
type Heap<'a when 'a: equality> =
| EmptyHP
| HP of 'a * Heap<'a> * Heap<'a>

let ex3 = HP(1,HP(2,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),HP(4,EmptyHP,EmptyHP))

// Type of ex3 is Heap<int>. It is monomorphic because to fulfil the heap properties, 
// the data in the nodes must be comparable. Else we cannot keep the heap order. 

let eh1 = EmptyHP

exception HeapError of string;;
// Testing HeapError with beautiful code
let hej a =
    if a = 0 then raise (HeapError "shit")
    else printfn "hej"

hej 1 

let isEmpty heap = 
    if heap = EmptyHP then true else false

let rec size heap n = 
    match heap with
    | EmptyHP -> n
    | HP(_,hp1,hp2) -> size hp1 (size hp2 (n+1))

size ex3 0

let find h = 
    match h with
    | HP(x,_,_) -> x


find ex3

let rec chkHeapProperty h = 
    match h with
    | EmptyHP -> true
    | HP(x, hl, hr) -> 
        match hl, hr with
        | EmptyHP, _ -> chkHeapProperty hr
        | _, EmptyHP -> chkHeapProperty hl
        | HP(lx, _, _), HP(rx, _, _) -> 
            x < lx && x < rx && chkHeapProperty hl && chkHeapProperty hr



chkHeapProperty ex3
// Question 1.3
// Q 1.3.1
let rec map f h = 
    match h with
    | EmptyHP -> h
    | HP(x, hl, hr) -> HP(f x, map f hl, map f hr)

let ff x = x + 1


ex3

map ff ex3

ex3

// Q 1.3.2
let f x = x/2
chkHeapProperty (map f ex3)

// Question 3.1
// Declare the infinite sequence triNum of triangular numbers. The type of triNum is seq<int>.
// The triangular numbers are defined as xn = n(n+1)/2 where xn is the nth element in the sequence.
// The first element has index n = 0.

let triNum = Seq.initInfinite(fun x -> x*(x+1)/2)
Seq.toList(Seq.take 100 triNum) 

let triNumC = Seq.cache triNum


let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s))
        (filterOddIndex (Seq.skip 2 s))

// Checking types for understanding
Seq.take
filterOddIndex
Seq.singleton
// Does not work
// Seq.toList(Seq.take 4 (filterOddIndex triNum))

// Function takes a sequence and produces a sequence. The first element is produced with Seq.head
// the next line appends the recursive call to the filter function to the element just produced.
// The Sequence the is used as an argument in the recursive call, is skipping the first 2 elements of the current sequence
// and returning the rest of the requence, which is then passed to the myFilterOddIndex recursive call.
let rec myFilterOddIndex s =
    seq {
        yield Seq.head s
        yield! myFilterOddIndex (Seq.skip 2 s)
    }
Seq.toList(Seq.take 10 (myFilterOddIndex triNum))


// Using the yield and yield! functions to produce the first element and append the rest of the sequence 
// skipping the first element in the recursive call
let rec zipSeq s1 s2 =
    seq { let e1 = Seq.head s1
          let e2 = Seq.head s2
          match e1,e2 with
          | Some x, Some y -> 
                yield(x,y)
                yield! zipSeq (Seq.skip 1 s1) (Seq.skip 1 s2)
          | _ -> ()
    }


