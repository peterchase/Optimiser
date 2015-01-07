namespace OptimiserLib

open TwoTrack

type ValueAndDerivatives = { value: double; first: double; second: double }

module Derivative =

    let derivs settings f x =
        let dx = settings.dxForDerivative
        let fx: double = f x
        let fxPlusDx = f (x + dx)
        let fxMinusDx = f (x - dx)

        try
            // Simple central differences. There may be better formulae, if we ever come to care enough!
            let firstDerivative = (fxPlusDx - fxMinusDx)/(2.0 * dx)
            let secondDerivative = (fxPlusDx - 2.0*fx + fxMinusDx)/(dx*dx)

            Success { value = fx; first = firstDerivative; second = secondDerivative }
        with
            | _ -> Failure "Could not calculate derivatives"
