namespace Optimiser

module Output =

    let Header =
        printfn "%15s %15s %15s %15s %15s" "x" "y" "dy/dx" "d2y/dx2" "xStep"

    let private Step (step: StepData) =
        let fx = step.valueAndDerivatives.value
        let firstDerivative = step.valueAndDerivatives.first
        let secondDerivative = step.valueAndDerivatives.second
        match step.step with
        | Some(s) -> printfn "%15.10f %15.10f %15.10f %15.10f %15.10f" step.x fx firstDerivative secondDerivative s
        | None _ -> printfn "%15.10f %15.10f %15.10f %15.10f %15s" step.x fx firstDerivative secondDerivative "SOLVED"

    let rec private HistoryImpl (history: StepData list) count =
        match history with
        | [] -> count
        | head :: tail -> HistoryStep head tail (count + 1)

    and HistoryStep head tail count =
        let nextCount = HistoryImpl tail count
        Step head
        nextCount

    let History history = HistoryImpl history 0
