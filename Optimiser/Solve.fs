namespace Optimiser

module Solve =

    let rec private solutionImpl history settings f x =

        let stepData = Step.nextStep settings f x
        let newHistory = stepData :: history

        match stepData.step with
        | Some(s) -> solutionImpl newHistory settings f (x + s)
        | None _ -> newHistory

    let solution (settings: Settings) f x = solutionImpl [] settings f x
