namespace Optimiser

module Solve =

    let rec solution (settings: Settings) history f x =

        let stepData = Step.nextStep settings f x
        let newHistory = stepData :: history

        match stepData.step with
        | Some(s) -> solution settings newHistory f (x + s)
        | None _ -> newHistory
