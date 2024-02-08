module a1

// 1.1
let sqr (n: float) = n * n
// 1.2
let pow a b = System.Math.Pow(a, b)

// 1.3 / HR 1.1
let g n = n + 4

// 1.4 / HR 1.2
let h(x:float, y:float) = System.Math.Sqrt(sqr x + sqr y)

// 1.5 / HR 1.4
let rec f = function
    | 0 -> 0
    | n -> n + f(n-1)


// 1.6 / HR 1.5
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + f(n-2)


// 1.7 / HR 1.6
let rec sum(m,n) = 
    match(m,n) with
    | (m,0) -> m
    | (m,n) -> m+sum(m,n-1)



// 1.8 / HR 1.7

let rec fact = function
    | 0 -> 1 
    | n -> n * fact(n-1)

let rec power = function
    | (_,0) -> 1.0 
    | (x,n) -> x * power(x,n-1)

// Cannot fact a negative number??
(System.Math.PI, fact -1)

// This is an int
fact ( fact 4)

// This is a float
power(System.Math.PI, fact 2)

// This is a tuple of to functions
// (float * int -> float) * (int -> int)
// or float * int
(power,fact)

// 1.9 / HR 1.8
let a = 5
let ff a = a + 1
ff 3
// Env: a   |-> 3
//      ff  |-> 4

// Evaluation: 3 + 1 = 4
let gg b = (ff b) + a
gg 3
// Env: b   |-> 3
//      a   |-> 5
//      gg  |-> 9

// Evaluation: (3 + 1) + 5 = 9



// 1.10 Duplicate strings: dup:string -> string
let dup a = a + a


let rec dupn (text:string) n = 
    match (text, n) with
    | (text,0) -> ""
    | (text, n) -> text + dupn text (n-1)
