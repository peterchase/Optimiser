namespace OptimiserLib

open TwoTrack

type GraphSettings = { numSteps: int; extraDomain: double }

module Graph =

    let fCurve settings f history =
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

    let stepsCurve history =
        let curve = history |> List.map (fun sd -> (sd.x, sd.valueAndDerivatives.value))
        Success curve
        