namespace Optimiser

module Output =

    let Header =
        printfn "%15s %15s %15s %15s %15s" "x" "y" "dy/dx" "d2y/dx2" "xStep"

    let Step x (valueAndDerivatives : double list) step =
        let fx = valueAndDerivatives.[0]
        let firstDerivative = valueAndDerivatives.[1]
        let secondDerivative = valueAndDerivatives.[2]
        let step = (-firstDerivative/secondDerivative)
        printfn "%15.10f %15.10f %15.10f %15.10f %15.10f" x fx firstDerivative secondDerivative step
