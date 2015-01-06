namespace Optimiser

type StepData = { step: double Option; valueAndDerivatives: ValueAndDerivatives }

module Step =

    let nextStep (settings: Settings) f x =

        let tol = settings.zeroDerivativeTolerance
        
        let derivatives = Derivative.derivs settings f x
        
        match derivatives with
            | d when abs d.first < tol -> { step = None; valueAndDerivatives = derivatives }
            | d -> { step = Some (-d.first/d.second); valueAndDerivatives = derivatives }
