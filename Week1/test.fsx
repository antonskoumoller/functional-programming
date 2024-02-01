let h (x: float, y: float) = System.Math.Sqrt(x * x + y * y)

let float1 : float = 1.0

let float2 : float = 2.0
let result = h float1 float2  
printfn "Result: %f" result
