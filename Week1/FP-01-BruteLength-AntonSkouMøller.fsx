module a1

// 1.1
let sqr n = n * n
// 1.2
let pow a b = System.Math.Pow(a, b)

// 1.3 / HR 1.1
let g n = n + 4

// 1.4 / HR 1.2
let h(x:float, y:float) = System.Math.Sqrt(x * x + y *y)

let result = h (2.0 2.0)
printfn "Result: %f" result

// 1.5 / HR 1.4
let rec f = failwith "not implemented"

// 1.6 / HR 1.5
let rec fib = failwith "not implemented"

// 1.7 / HR 1.6
let rec sum(m,n) = failwith "not implemented"

// 1.8 / HR 1.7

// 1.9 / HR 1.8

// 1.10 Duplicate strings: dup:string -> string
let dup a:string = failwith "not implemented"

// 1.11 Duplicate string n times.
let rec dupn (text:string) n = failwith "not implemented"