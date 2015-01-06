namespace Optimiser

module Program =
    [<EntryPoint>]
    let main argv = 

        let f x = x**3.0 - 100.0*x
        let xInitial = 20.0

        let settings = { zeroDerivativeTolerance = 1.0e-10; dxForDerivative = 1.0e-6 }

        Output.Header

        let history = Solve.solution settings [] f xInitial
        let solution = List.head history

        let steps = Output.History history

        printfn "Solution found: x = %f, in %d steps" solution.x steps

        0 // return an integer exit code
