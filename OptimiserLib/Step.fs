namespace OptimiserLib

open TwoTrack

type StepData = { x: double; valueAndDerivatives: ValueAndDerivatives; step: double Option }

module Step =

    let private calcStep settings x derivatives =
        let tol = settings.zeroDerivativeTolerance
        try
            match derivatives with
                | d when abs d.first < tol -> Success { x = x; step = None; valueAndDerivatives = derivatives }
                | d -> Success { x = x; step = Some (-d.first/d.second); valueAndDerivatives = derivatives }
        with
            | _ -> Failure "Could not calculate step"

    let nextStep settings derivs f x =
        derivs f x |> Binding.bindTwoTrack (calcStep settings x)
