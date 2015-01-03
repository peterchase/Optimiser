namespace Optimiser

module Solve =

    let rec solution (settings: Settings) f x =

        let step = Step.nextStep settings f x
        let fx = f x

        printfn "%15.10f %15.10f" x fx 

        match step with
        | Some s -> solution settings f (x + s)
        | None _ -> x
