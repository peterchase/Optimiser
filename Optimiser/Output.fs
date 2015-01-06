namespace Optimiser

module Output =

    let Header =
        printfn "%15s %15s %15s %15s %15s" "x" "y" "dy/dx" "d2y/dx2" "xStep"

    let Step x (valueAndDerivatives : ValueAndDerivatives) step =
        let fx = valueAndDerivatives.value
        let firstDerivative = valueAndDerivatives.first
        let secondDerivative = valueAndDerivatives.second
        let step = (-firstDerivative/secondDerivative)
        printfn "%15.10f %15.10f %15.10f %15.10f %15.10f" x fx firstDerivative secondDerivative step
