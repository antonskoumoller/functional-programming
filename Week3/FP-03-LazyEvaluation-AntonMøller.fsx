// 3.1 
let rec downTo p = if p = 1 then [1] else p::downTo(p-1)

downTo 3
// Understanding the evaluation
// downTo(3) = 3 :: downTo(3-1)
// downTo(2) = 2 :: downTo(2-1)
// downTo(1) = [1] 

let rec downTo2 = function
    | 1 -> [1]
    | p -> p::downTo2(p-1)

downTo2 3

// 3.2
let rec removeOddIdx = function
    | [] -> []
    | [x] -> [x]
    | x::_::xs' -> x::removeOddIdx xs'

removeOddIdx [1;2;3;4;5;6]

// 3.3
let rec combinePair = function
    | [] -> []
    | [n] -> []
    | x::y::xs' -> (x,y)::combinePair xs'
// Making sure it works with both an even and odd number of elements
combinePair [1;2;3;4;5;6;7]

// // 3.4
// Creating type
type Amount = {pounds:int; shillings:int; pence:int}

// With records
let (.+) amount1  amount2 = 
    // Transforming into pence
    let totalPence1 = amount1.pounds * 20 * 12 + amount1.shillings * 12 + amount1.pence
    let totalPence2 = amount2.pounds * 20 * 12 + amount2.shillings * 12 + amount2.pence
    let totalPence = totalPence1 + totalPence2
    // Going back to pounds, shillings, pence
    let pence = totalPence % 12
    let shillings' = totalPence / 12
    let shillings = shillings' % 20
    let pounds = shillings'/20
    {pounds = pounds; shillings = shillings; pence = pence}

let (.-) amount1  amount2 = 
    // Transforming into pence
    let totalPence1 = amount1.pounds * 20 * 12 + amount1.shillings * 12 + amount1.pence
    let totalPence2 = amount2.pounds * 20 * 12 + amount2.shillings * 12 + amount2.pence
    let totalPence = totalPence1 - totalPence2
    // Going back to pounds, shillings, pence
    let pence = totalPence % 12
    let shillings' = totalPence / 12
    let shillings = shillings' % 20
    let pounds = shillings'/20
    {pounds = pounds; shillings = shillings; pence = pence}

// With triples
let (..+) (pound1, shilling1, pence1) (pound2, shilling2, pence2) = 
    let amount1 = {pounds = pound1;shillings = shilling1;pence = pence1}
    let amount2 = {pounds = pound2;shillings = shilling2;pence = pence2}
    amount1 .+ amount2

let (..-) (pound1, shilling1, pence1) (pound2, shilling2, pence2) = 
    let amount1 = {pounds = pound1;shillings = shilling1;pence = pence1}
    let amount2 = {pounds = pound2;shillings = shilling2;pence = pence2}
    amount1 .- amount2
// Checking results
let amount1 = {pounds = 1; shillings = 19; pence = 6}
let amount2 = {pounds = 0; shillings = 5; pence = 8}
let resultAdd1 = amount1 .+ amount2
let resultDeduct1 = amount1 .- amount2

let resultAdd2 = (1,19,6) ..+ (0,5,8)
let resultDeduct2 = (1,19,6) ..- (0,5,8)

// 3.5
type ComplexNumber = {real:int; im:int}

let (...+) complex1 complex2 = 
    let re = complex1.real + complex2.real
    let im = complex1.im + complex2.im
    {real= re; im = im}

let (.*.) complex1 complex2 = 
    let re = complex1.real * complex2.real - complex1.im * complex2.im
    let im = complex1.im * complex2.real + complex1.real * complex2.im
    {real= re; im = im}

// Helper function to get the inverse so we can use the (...+) infix function instead of making a (-) infix function
let invAdd complexNumber = 
    {real = - complexNumber.real; im = -complexNumber.im}

let (...-) complex1 complex2 =
    complex1 ...+ invAdd complex2
// Same type of helper function to we can multiply with the inverse complex number
let invMult complexNumber =
    let re = complexNumber.real / (complexNumber.real*complexNumber.real+ complexNumber.im * complexNumber.im)
    let im = (-complexNumber.im) / (complexNumber.im*complexNumber.im + complexNumber.real*complexNumber.real)
    {real = re; im = im}

let (./) complex1 complex2 = 
    complex1 .*. invMult complex2

// 1/2 / 2/1
// 1/2 

// 3.6
// Only two pattern matching clauses in the start. We don't know if this suffices but all we could come up with. 
// We tried :) 
let rec altsum = function
    | [] -> 0
    | x0::xs' -> 
        match xs' with
        | [] -> x0
        | x1::xs'' -> x0 - x1 + altsum xs''
// Check result
altsum [2; -1; 3]



