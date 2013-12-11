// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Romberg.fs"
open NumericalAnalysis.Integration
open System

// Define your library scripting code here

//let f x = 8.0 * (Math.Sqrt(1.0 - x*x) - x)
//romberg f 0.0 (1.0/Math.Sqrt(2.0)) 9 |> printfn "%.12f"

let erf' t = 2.0 / Math.Sqrt(Math.PI) * (Math.E**(-(t**2.0)))
let erf x = romberg erf' 0.0 x 9
