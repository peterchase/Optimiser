namespace Optimiser

open System
open TwoTrack
open OptimiserLib
open FSharp.Charting

module Program =

    let private outputToConsole history =
        Output.Header
        let solution = List.head history
        let numSteps = Output.history history
        printfn "Solution found: x = %f, in %d steps" solution.x numSteps
        history

    let private outputToGraph graphSettings f history =
        let graph = Graph.doGraph graphSettings f history
        match graph with
        | Success g ->
            let form = g.ShowChart()
            System.Windows.Forms.Application.Run(form)
            Success history
        | Failure failure -> Failure failure

    [<EntryPoint>]
    [<STAThread>]
    let main argv = 

        let f x = x**3.0 - 100.0*x
        let xInitial = 20.0

        let graphSettings = { numSteps = 100; extraDomain = 0.5 }
        let settings = { zeroDerivativeTolerance = 1.0e-10; dxForDerivative = 1.0e-6 }

        let derivs = Derivative.derivs settings
        let nextStep = Step.nextStep settings derivs
        let solution = Solve.solution nextStep f xInitial

        let doGraph = TwoTrack.Binding.bindTwoTrack (outputToGraph graphSettings f)
        let doConsole = TwoTrack.Binding.bindSimple outputToConsole
        let doOutput = doGraph >> doConsole
        let overallResult = solution |> doOutput

        match overallResult with
        | Success history ->
            0 // exit code
        | Failure f ->
            printfn "Failure: %s" f
            1 // exit code
