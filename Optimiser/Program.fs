namespace Optimiser

module Program =
    [<EntryPoint>]
    let main argv = 
        let f x = x**3.0 - 100.0*x
        let settings = { zeroDerivativeTolerance = 1.0e-10; dxForDerivative = 1.0e-6 }
        printfn "Solution x = %f" (Solve.solution settings f 20.0)
        0 // return an integer exit code
