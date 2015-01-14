namespace OptimiserLib

open TwoTrack

module Solve =

    let rec private solutionImpl calcNextStep f x history stepData =
        let newHistory = stepData :: history
        match stepData.step with
        | Some step -> solutionWithErrorHandling calcNextStep f (x + step) newHistory
        | None _ -> Success newHistory

    and private solutionWithErrorHandling calcNextStep f x history =
        calcNextStep f x |> TwoTrack.Binding.bindTwoTrack (solutionImpl calcNextStep f x history)

    let solution calcNextStep f x = solutionWithErrorHandling calcNextStep f x []
