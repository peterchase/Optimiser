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

    let private showGraph (graph: ChartTypes.GenericChart) =
        let form = graph.ShowChart()
        System.Windows.Forms.Application.Run(form)

    let private outputToGraph graphSettings f history =
        Graph.doGraph graphSettings f history |> TwoTrack.Binding.bindSimple showGraph

    [<EntryPoint>]
    let main argv = 

        let f x = 0.3*x**4.0 - 0.1*x**3.0 - 100.0*x + 12.0
        let xInitial = -6.0

        let graphSettings = { numSteps = 100; extraDomain = 0.5 }
        let settings = { zeroDerivativeTolerance = 1.0e-10; dxForDerivative = 1.0e-6; minStepFactor = 1.0e-5; stepReductionFactor = 0.1 }

        let derivs = Derivative.derivs settings
        let nextStep = Step.nextStep settings derivs
        let solution = Solve.solution nextStep f xInitial

        let doGraph = TwoTrack.Binding.bindTwoTrack (outputToGraph graphSettings f)
        let doConsole = TwoTrack.Binding.bindSimple outputToConsole
        let doOutput = doConsole >> doGraph
        let overallResult = solution |> doOutput

        match overallResult with
        | Success _ ->
            0 // exit code
        | Failure f ->
            printfn "Failure: %s" f
            1 // exit code
