namespace Optimiser

module Step =

    let nextStep tol dx f x =

        let derivative = Derivative.deriv dx f x
        
        match derivative with
            | d when abs d < tol -> None
            | d -> Some (-d/(Derivative.deriv2 dx f x))
