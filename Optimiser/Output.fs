namespace Optimiser

module Output =

    let Header =
        printfn "%15s %15s %15s %15s %15s" "x" "y" "dy/dx" "d2y/dx2" "xStep"

    let private step (step: StepData) =
        let fx = step.valueAndDerivatives.value
        let firstDerivative = step.valueAndDerivatives.first
        let secondDerivative = step.valueAndDerivatives.second
        match step.step with
        | Some(s) -> printfn "%15.10f %15.10f %15.10f %15.10f %15.10f" step.x fx firstDerivative secondDerivative s
        | None _ -> printfn "%15.10f %15.10f %15.10f %15.10f %15s" step.x fx firstDerivative secondDerivative "SOLVED"

    let rec private historyImpl (history: StepData list) count =
        match history with
        | [] -> count
        | head :: tail -> historyStep head tail (count + 1)

    and historyStep head tail count =
        let nextCount = historyImpl tail count
        step head
        nextCount

    let history history = historyImpl history 0
