// 4.1
let explode (s:string) : char list = 
    s.ToCharArray() |> List.ofArray
explode "star"

let rec explode2 (s:string) = 
    match s with
    | "" -> []
    | s -> s.Chars(0)::explode2(s.Remove(0,1))
explode2 "star"

// 4.2
// First solution
let implode (x: char list) : string = List.foldBack(fun char acc -> string char + acc) x ""
implode ['a';'b';'c']

let implodeRev (x: char list) : string = List.foldBack(fun char acc -> acc + string char) x ""
implodeRev ['a';'b';'c']

// 4.3
let toUpper (s:string)  = 
    s |> explode |> List.map System.Char.ToUpper |> implode
toUpper "QejekRok"

let toUpper1 s =
    (explode >> (List.map System.Char.ToUpper) >> implode) s

let toUpper2 s=
    s |> explode |> List.map System.Char.ToUpper |> implode

// 4.4 
let palindrome s = 
    let word = toUpper s
    let s' = (explode >> (List.map System.Char.ToUpper) >> implodeRev) s
    word.Equals(s')
palindrome "AnNA"

// 4.5
let rec ack (m,n) = 
    if n < 0 || m < 0 then failwith "The Ackermann function is defined for non negative numbers only"
    match (m, n) with 
    | (0,n) -> n+1
    | (m,0) when m > 0 -> ack(m-1,1)
    | (m,n) when m > 0 && n > 0 -> ack(m-1, ack(m,n-1))

// 4.6
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);

time (fun () -> ack(3,11))

let timeArg1 f a = 
    time (fun() -> ack(a))

timeArg1 ack (3,11)

// 4.7
let rec downto1 f n e = 
    if n <= 0 then e 
    else downto1 f (n-1) (f n e)

let fact n =  downto1 (fun n e -> if n = 0 then 1 else n * e) n 1 

let buildList g n = 
    downto1 (fun n e -> g n :: e) n []

buildList fact 8