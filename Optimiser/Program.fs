namespace Optimiser

open TwoTrack

module Program =
    [<EntryPoint>]
    let main argv = 

        let f x = x**3.0 - 100.0*x
        let xInitial = 20.0

        let settings = { zeroDerivativeTolerance = 1.0e-10; dxForDerivative = 1.0e-6 }
        let nextStep = Step.nextStep settings

        Output.Header

        let historyOrFailure = Solve.solution nextStep f xInitial
        match historyOrFailure with
        | Success history ->
            let solution = List.head history
            let steps = Output.history history
            printfn "Solution found: x = %f, in %d steps" solution.x steps
            0 // exit code
        | Failure f ->
            printfn "Failure: %s" f
            1 // exit code
            
