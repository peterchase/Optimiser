// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Optimiser

module Program =
    [<EntryPoint>]
    let main argv = 
        let f x = x**3.0 - 100.0*x
        printfn "Solution x = %f" (Solve.solution 1.0e-10 1.0e-6 f 20.0)
        0 // return an integer exit code
