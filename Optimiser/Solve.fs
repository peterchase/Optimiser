namespace Optimiser

module Solve =

    let rec performStep (settings: Settings) f x valueAndDerivatives step =
        Output.Step x valueAndDerivatives step
        solution settings f (x + step)

    and solution (settings: Settings) f x =

        let stepData = Step.nextStep settings f x
        let fx = stepData.valueAndDerivatives.[0]

        match stepData.step with
        | Some(s) -> performStep settings f x stepData.valueAndDerivatives s
        | None _ -> (x, stepData)
