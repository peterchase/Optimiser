namespace Optimiser

module Program =
    [<EntryPoint>]
    let main argv = 

        let f x = x**3.0 - 100.0*x
        let xInitial = 20.0

        let settings = { zeroDerivativeTolerance = 1.0e-10; dxForDerivative = 1.0e-6 }

        Output.Header

        let solution = Solve.solution settings [] f xInitial
        
        let steps = Output.History solution

        printfn "Steps = %d" steps
        0 // return an integer exit code
