namespace Optimiser

module Derivative =

    let deriv (settings: Settings) f x : double =
        let dx = settings.dxForDerivative
        (f (x + dx) - f x)/dx

    let deriv2 (settings: Settings) f x = deriv settings (deriv settings f) x