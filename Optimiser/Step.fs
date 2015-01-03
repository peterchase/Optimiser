namespace Optimiser

type StepData = { step: double Option; valueAndDerivatives: double list }

module Step =

    let nextStep (settings: Settings) f x =

        let tol = settings.zeroDerivativeTolerance
        
        let derivatives = Derivative.derivs settings f x
        
        match derivatives with
            | d when abs d.[1] < tol -> { step = None; valueAndDerivatives = derivatives }
            | d -> { step = Some (-d.[1]/d.[2]); valueAndDerivatives = derivatives }
