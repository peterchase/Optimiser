namespace Optimiser

open TwoTrack

type StepData = { x: double; valueAndDerivatives: ValueAndDerivatives; step: double Option }

module Step =

    let nextStep settings f x =

        let calcStep derivatives =
            let tol = settings.zeroDerivativeTolerance
            try
                match derivatives with
                    | d when abs d.first < tol -> Success { x = x; step = None; valueAndDerivatives = derivatives }
                    | d -> Success { x = x; step = Some (-d.first/d.second); valueAndDerivatives = derivatives }
            with
                | _ -> Failure "Could not calculate step"
        
        let twoTrackDerivatives = Derivative.derivs settings f x
        Binding.bindTwoTrack calcStep twoTrackDerivatives
