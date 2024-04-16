// Exercise 7.1

// Exercise 7.2
let rec sum(m,n) = 
    match(m,n) with
    | (m,0) -> m
    | (m,n) -> m+sum(m,n-1)
// Understanding the sum function:
// sum 5 5
// 5 + sum(5,4)     -> 5 + 25 = 30
// 5 + sum(5,3)     -> 5 + 20
// 5 + sum(5,2)     -> 5 + 15
// 5 + sum(5,1)     -> 5 + 10
// 5 + sum(5,0)     -> 5 + 5

// Solution: Not accumulating function calls on the stack as each call to SumA 
// is finalized and not waiting for the next call

let rec sumA = function
    | (m,0,acc) -> acc+m
    | (m,n,acc) -> sumA(m,n-1,acc+m) 

sumA(10,10,0)

// Exercise 7.3
// 9.4: Give iterative declarations of the list function List.length.

let rec listLength = function
    | ([],acc) -> acc
    | (x::tail,acc) -> listLength(tail,acc+1) // removing one element at a time and counting removals empty list

listLength([1;2;3],0)

// Exercise 7.4
// 9.6  Declare a continuation-based version of the factorial function and compare the run time with 
//      the results in Section 9.4.

let rec factA = function
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m) 
factA(5,1)


let rec factC n c =
    if n = 1 then c n
    else factC (n-1) (fun res -> c(n*res))  // decrementing n all the way to 1 and then returning 1 multiplied with 
                                            // the previous computations 
factC 5 id

// Exercise 7.5
// Declare a function for computing Fibonacci numbers Fn (see Exercise 1.5) using a while
// loop. Hint: introduce variables to contain the two previously computed Fibonacci numbers.

let fib n =
    let mutable prev = 0
    let mutable current = 1
    let mutable counter = 0
    while counter < n do
        let temp = prev // store previous
        prev <- current // previous is now current
        current <- temp + current // compute next value from previous and current
        counter <- counter + 1 // count rounds 
    prev // Always one ahead to we return prev to include 0 in the sequence

#time
fib 30

// 0 -> 1
// 1 -> 2
// 1 -> 3
// 2 -> 4
// 3 -> 5
// 5 -> 6
// 8 -> 7
// 13 -> 8

// Exercise 7.5

let rec fibA n n1 n2 = 
    match (n,n1,n2) with
    | (0,_,_) -> n2
    | (n,n1,n2) -> fibA (n-1) (n1+n2) n1
// F -2 accumulator has to be 0 to get 0 in the sequence.
let n1 = 1
let n2 = 0
#time
fibA 30 n1 n2



let rec fibC n c =
    if n <= 0 then c 0
    elif n = 1 then c 1
    else fibC (n - 1) (fun x -> fibC (n - 2) (fun y -> c (x + y)))
#time
fibC 7 id

// We cannot get #time to work :( )

