namespace OptimiserLib

open TwoTrack
open FSharp.Charting

type GraphSettings = { numSteps: int; extraDomain: double }

module Graph =

    let private functionCurve settings f history =
        try
            let xValues = history |> List.map (fun sd -> sd.x)
            let xMin = List.min xValues
            let xMax = List.max xValues
            let domain = xMax - xMin

            let extraDomain = domain * settings.extraDomain;
            let xGraphMin = xMin - extraDomain
            let xGraphMax = xMax + extraDomain
            let graphDomain = xGraphMax - xGraphMin

            let graphPointIndexes = seq { 0 .. settings.numSteps }
            let x index = xGraphMin + (graphDomain * (double index))/(double settings.numSteps)

            let curve = graphPointIndexes |> Seq.map (fun i -> x i) |> Seq.map (fun x -> (x, f x))
            Success curve
        with
            | ex -> Failure ex.Message

    let private stepsCurve history =
        history |> List.map (fun sd -> (sd.x, sd.valueAndDerivatives.value))
        
    let private combinedLineChart (solution: seq<double * double>) (func: seq<double * double>) =
        try
            let line1 = Chart.Line(func, Name="Function")
            let line2 = Chart.Line(solution, Name="Solution").WithMarkers(Size=10, Style=ChartTypes.MarkerStyle.Circle)
            let lines = [ line1; line2 ]
            let xGraphMin = Seq.head func |> fst |> floor
            let xGraphMax = Seq.last func |> fst |> ceil
            let combined = (Chart.Combine lines).WithTitle("Solving by Newton's Method").WithXAxis(Title="X", Min=xGraphMin, Max=xGraphMax).WithYAxis(Title="F(X)")
            Success combined
        with
            | ex -> Failure ex.Message

    let doGraph settings f history =
        let solution = stepsCurve history
        functionCurve settings f history |> TwoTrack.Binding.bindTwoTrack (combinedLineChart solution)
