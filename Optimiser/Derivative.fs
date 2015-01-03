namespace Optimiser

module Derivative =

    let derivs (settings: Settings) f x : double list =
        let dx = settings.dxForDerivative
        let fx = f x
        let fxPlusDx = f (x + dx)
        let fxMinusDx = f (x - dx)

        // Simple central differences. There may be better formulae, if we ever come to care enough!
        let firstDerivative = (fxPlusDx - fxMinusDx)/(2.0 * dx)
        let secondDerivative = (fxPlusDx - 2.0*fx + fxMinusDx)/(dx*dx)

        [ firstDerivative; secondDerivative ]
