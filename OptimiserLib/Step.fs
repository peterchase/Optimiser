namespace OptimiserLib

open TwoTrack

type StepData = { x: double; valueAndDerivatives: ValueAndDerivatives; step: double Option }

module Step =

    let private calcNewtonStep derivatives = -derivatives.first / derivatives.second

    let rec private calcSafeStep settings derivatives f x stepFactor =
        let newtonStep = calcNewtonStep derivatives
        let adjustedStep = newtonStep * stepFactor
        let xNew = x + adjustedStep
        let fxNew = f xNew
        let deltaFx = fxNew - derivatives.value
        let expectedChangeDirection = derivatives.first * adjustedStep |> sign
        let actualChangeDirection = deltaFx |> sign
        if expectedChangeDirection = actualChangeDirection then
            Success adjustedStep
        else if stepFactor > settings.minStepFactor then
            let reducedStepFactor = stepFactor * settings.stepReductionFactor
            calcSafeStep settings derivatives f x reducedStepFactor
        else
            Failure "Unable to find a safe step. Is the objective function discontinuous?"

    let private calcStep settings f x derivatives =
        let tol = settings.zeroDerivativeTolerance
        try
            match derivatives with
                | d when abs d.first < tol -> Success { x = x; step = None; valueAndDerivatives = derivatives }
                | d -> let safeStepOrFailure = calcSafeStep settings d f x 1.0
                       match safeStepOrFailure with
                       | Success safeStep -> Success { x = x; step = Some safeStep; valueAndDerivatives = derivatives }
                       | Failure f -> Failure f
        with
            | _ -> Failure "Could not calculate step"

    let nextStep settings calcDerivs f x =
        calcDerivs f x |> Binding.bindTwoTrack (calcStep settings f x)
