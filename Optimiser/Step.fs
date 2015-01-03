namespace Optimiser

module Step =

    let nextStep (settings: Settings) f x =

        let tol = settings.zeroDerivativeTolerance
        
        let derivatives = Derivative.derivs settings f x
        
        match derivatives with
            | d when abs d.[0] < tol -> None
            | d -> Some (-d.[0]/d.[1])
