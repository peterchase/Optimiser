namespace Optimiser

type StepData = { x: double; valueAndDerivatives: ValueAndDerivatives; step: double Option }

module Step =

    let nextStep (settings: Settings) f x =

        let tol = settings.zeroDerivativeTolerance
        
        let derivatives = Derivative.derivs settings f x
        
        match derivatives with
            | d when abs d.first < tol -> { x = x; step = None; valueAndDerivatives = derivatives }
            | d -> { x = x; step = Some (-d.first/d.second); valueAndDerivatives = derivatives }
