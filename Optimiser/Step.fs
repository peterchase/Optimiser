namespace Optimiser

module Step =

    let nextStep (settings: Settings) f x =

        let tol = settings.zeroDerivativeTolerance
        
        let derivative = Derivative.deriv settings f x
        
        match derivative with
            | d when abs d < tol -> None
            | d -> Some (-d/(Derivative.deriv2 settings f x))
