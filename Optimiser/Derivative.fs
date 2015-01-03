namespace Optimiser

module Derivative =

    let deriv dx f x : double = (f (x + dx) - f x)/dx

    let deriv2 dx f x = deriv dx (deriv dx f) x