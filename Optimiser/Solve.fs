namespace Optimiser

open TwoTrack

module Solve =

    let rec private solutionWithErrorHandling settings history f x =
        let stepDataOrFailure = Step.nextStep settings f x

        let solutionImpl stepData =
            let newHistory = stepData :: history
            match stepData.step with
            | Some step -> solutionWithErrorHandling settings newHistory f (x + step)
            | None _ -> Success newHistory

        match stepDataOrFailure with
        | Success s -> solutionImpl s
        | Failure f -> Failure f

    let solution settings f x = solutionWithErrorHandling settings [] f x
