// 2.1
let timediff (hh1, mm1) (hh2,mm2) =
    let hd = (hh2 - hh1) * 60
    let md = mm2 - mm1
    hd + md;;

// 2.2
let minutes (hh,mm) = hh*60 + mm

// 2.3
let rec pow (x, n) =
    match n with
    | 0 -> ""
    | n -> x  + pow(x,n-1)

// 2.4 
let rec bin (n,k) = 
    match (n,k) with
    | (n,0) -> 1
    | (n,k) when n = k -> 1
    | (n,k) -> bin(n-1,k-1) + bin(n-1,k)

// 2.5
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y)

// 1:   f: int * int -> int. 
//      f takes a pair of integers and returns an integer

// 2: The function terminates a x = 0

// 3:   f(2,3) = f(1,6)
//      f(1,6) = f(0,6)
//      f(0,6) = 6

// 4: The mathematical meaning of f(x,y) is x!*y

// 2.6
let test(c,e) = if !c then e else 0
// 1: It takes a boolean and an integer and returns an integer
// 2: It fails because fact(-1) is not defined.
// 3: Evaluating the statement manually does not require us to compute fact(-1), since the if statement is false,
//    hence the statement evaluates to 0.
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)


// 2.7
// f is defined further up.
let curry f x y = f(x,y)
curry f 2 2

let ff a b = a+b+1
let uncurry ff (x,y) = ff x y
uncurry ff (2,2)