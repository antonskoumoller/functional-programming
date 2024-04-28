// Exercise 11.1 Do assignment 2 in exam set from June 2018.

// Question 2 (30%)
// We shall now consider a binary divide–and–conquer algorithm. We will use the algorithm to implement
// mergesort. You do not need to know how mergesort works to do this.


// Question 2.1

// • Declare a function genRandoms n of type int -> int[] that returns an array of n random
// integers. The random integers are larger than or equal to 1 and less than 10000. For instance,
// genRandoms 4 may return [|8803;8686;2936;2521|].
// Hint: You can use below to define a generator random of type unit -> int to generate the
// random numbers.

// This function uses the system.random libary to generate a random number between 1 and 10.000
let random =
    let rnd = new System.Random()
    fun () -> rnd.Next(1,10000)

// This function generates a list of n random numbers by recursively calling itselve and consting the 
// newly generated number to a list
let rec genRandoms n =
    match n with
    | 0 -> []
    | n -> random()::genRandoms(n-1)


genRandoms 5

// • Declare a function genRandomsP n of type int -> int[] that is similar to genRandom
// except that the numbers are generated in parallel to speed up the process.
// Hint: You may use the Array.Parallel library as explained in Section 13.6 in the book HR.

// this function generates a array of random numbers like genRandoms but instead of doing it recursively
// it usses the Array.Parallel to generate the array.
let genRandomsP n =
    Array.Parallel.init n (fun x -> random())

genRandomsP 5

// This fucntion finds the halfway point in a list a splits it at the middel. If the list has an uneven number 
// of elements then the second list will have one more element than the other
let split xs =
    let mid = (xs |> List.length) / 2
    xs |> List.splitAt mid

// Test for even number of elements
let test11 = [ 1..2 ]

// Test for odd number of elements
let test12 = [ 1..3 ]

// Test for one element list
let test13 = [1]
split test11
split test12
split test13

// This function check if the size of the list is less than or equal to 1 and returns true and otherwise false.
let indivisible xs =
    if xs |> List.length <= 1 then true else false

let test21 = []
let test22 = [1]
let test23 = [1..2]
indivisible test21
indivisible test22
indivisible test23


let rec merge xs ys = 
    match xs, ys with
    | [], [] -> []
    | [], ys -> ys
    | xs, [] -> xs 
    | x::xs', y::ys' -> 
        if x < y then 
            x::merge xs' ys
        else 
            y::merge xs ys'
    
let xtest31 = []
let ytest31 = [1..3]
let xtest32 = [1..5]

let ytest32 = [1..5]
let xtest33 = []
let ytest33 = []

merge xtest31 ytest31
merge xtest32 ytest32
merge ([]: int list) ([]: int list)

let divideAndConquer split merge indivisible p =
    let rec dc p = 
        if indivisible p
        then p
        else 
            let left, right = split p
            let sortedLeft = dc left
            let sortedRight = dc right
            merge sortedLeft sortedRight
    dc p

divideAndConquer split merge indivisible [22;746;931;975;200]

