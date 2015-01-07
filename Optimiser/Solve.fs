namespace Optimiser

open TwoTrack

module Solve =

    let rec private solutionWithErrorHandling (nextStep: (double -> double) -> double -> Result<StepData, string>) history f x =
        let stepDataOrFailure = nextStep f x

        let solutionImpl stepData =
            let newHistory = stepData :: history
            match stepData.step with
            | Some step -> solutionWithErrorHandling nextStep newHistory f (x + step)
            | None _ -> Success newHistory

        match stepDataOrFailure with
        | Success s -> solutionImpl s
        | Failure f -> Failure f

    let solution nextStep f x = solutionWithErrorHandling nextStep [] f x
